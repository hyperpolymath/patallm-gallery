//! Voting system for proposal approval
//!
//! Multiple agents can vote on proposals using configurable strategies.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use ulid::Ulid;

use crate::schema::AgentId;
use super::proposal::{ProposalId, Proposal, ProposalManager};
use super::capabilities::CapabilityConfig;

pub type VoteId = Ulid;

/// A vote on a proposal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Vote {
    pub id: VoteId,
    pub proposal_id: ProposalId,
    pub voter: AgentId,
    pub decision: VoteDecision,
    pub weight: f32,
    pub reason: Option<String>,
    pub timestamp: DateTime<Utc>,
}

/// Vote decision
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum VoteDecision {
    Approve,
    Reject,
    Abstain,
}

impl Vote {
    pub fn new(proposal_id: ProposalId, voter: AgentId, decision: VoteDecision) -> Self {
        Self {
            id: Ulid::new(),
            proposal_id,
            voter,
            decision,
            weight: 1.0,
            reason: None,
            timestamp: Utc::now(),
        }
    }

    pub fn with_weight(mut self, weight: f32) -> Self {
        self.weight = weight;
        self
    }

    pub fn with_reason(mut self, reason: impl Into<String>) -> Self {
        self.reason = Some(reason.into());
        self
    }
}

/// Voting strategy for determining approval
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "snake_case")]
pub enum VotingStrategy {
    /// All voters must approve
    Unanimous,
    /// More than half must approve
    #[default]
    SimpleMajority,
    /// More than 2/3 must approve
    Supermajority,
    /// Use weighted votes, threshold is sum of approve weights
    Weighted { threshold: f32 },
    /// First vote wins (for fast iteration)
    FirstVote,
    /// Single designated approver
    SingleApprover { approver: AgentId },
}

impl std::fmt::Display for VotingStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VotingStrategy::Unanimous => write!(f, "unanimous"),
            VotingStrategy::SimpleMajority => write!(f, "simple_majority"),
            VotingStrategy::Supermajority => write!(f, "supermajority"),
            VotingStrategy::Weighted { threshold } => write!(f, "weighted({})", threshold),
            VotingStrategy::FirstVote => write!(f, "first_vote"),
            VotingStrategy::SingleApprover { approver } => write!(f, "single_approver({})", approver),
        }
    }
}

/// Result of evaluating votes
#[derive(Debug, Clone, PartialEq)]
pub enum VotingResult {
    /// Proposal approved
    Approved { reason: String },
    /// Proposal rejected
    Rejected { reason: String },
    /// Still waiting for votes
    Pending { votes_for: f32, votes_against: f32, votes_needed: f32 },
}

/// Coordinates voting on proposals
pub struct VotingCoordinator {
    strategy: VotingStrategy,
    votes: HashMap<ProposalId, Vec<Vote>>,
    min_voters: usize,
}

impl Default for VotingCoordinator {
    fn default() -> Self {
        Self::new(VotingStrategy::default())
    }
}

impl VotingCoordinator {
    /// Create a new voting coordinator
    pub fn new(strategy: VotingStrategy) -> Self {
        Self {
            strategy,
            votes: HashMap::new(),
            min_voters: 1,
        }
    }

    /// Set minimum number of voters required
    pub fn with_min_voters(mut self, min: usize) -> Self {
        self.min_voters = min;
        self
    }

    /// Change voting strategy
    pub fn set_strategy(&mut self, strategy: VotingStrategy) {
        self.strategy = strategy;
    }

    /// Get current strategy
    pub fn strategy(&self) -> &VotingStrategy {
        &self.strategy
    }

    /// Cast a vote
    pub fn cast_vote(
        &mut self,
        vote: Vote,
        capabilities: &CapabilityConfig,
    ) -> Result<(), String> {
        // Check if voter can vote
        if !capabilities.can_vote(&vote.voter) {
            return Err(format!("{} is not allowed to vote", vote.voter));
        }

        // Get or create vote list for this proposal
        let votes = self.votes.entry(vote.proposal_id).or_default();

        // Check for duplicate vote
        if votes.iter().any(|v| v.voter == vote.voter) {
            return Err(format!("{} has already voted on this proposal", vote.voter));
        }

        // Apply vote weight from capabilities
        let mut vote = vote;
        vote.weight = capabilities.get_capabilities(&vote.voter).vote_weight;

        votes.push(vote);
        Ok(())
    }

    /// Get votes for a proposal
    pub fn get_votes(&self, proposal_id: ProposalId) -> &[Vote] {
        self.votes.get(&proposal_id).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Evaluate votes and determine result
    pub fn evaluate(&self, proposal_id: ProposalId) -> VotingResult {
        let votes = self.get_votes(proposal_id);

        if votes.len() < self.min_voters {
            return VotingResult::Pending {
                votes_for: 0.0,
                votes_against: 0.0,
                votes_needed: self.min_voters as f32,
            };
        }

        let (approve_weight, reject_weight) = votes.iter().fold((0.0, 0.0), |(a, r), v| {
            match v.decision {
                VoteDecision::Approve => (a + v.weight, r),
                VoteDecision::Reject => (a, r + v.weight),
                VoteDecision::Abstain => (a, r),
            }
        });

        let total_weight = approve_weight + reject_weight;

        match self.strategy {
            VotingStrategy::Unanimous => {
                if reject_weight > 0.0 {
                    VotingResult::Rejected {
                        reason: format!("Unanimous approval required, but {} weight rejected", reject_weight),
                    }
                } else if approve_weight > 0.0 {
                    VotingResult::Approved {
                        reason: "Unanimous approval".into(),
                    }
                } else {
                    VotingResult::Pending {
                        votes_for: approve_weight,
                        votes_against: reject_weight,
                        votes_needed: 1.0,
                    }
                }
            }

            VotingStrategy::SimpleMajority => {
                if total_weight == 0.0 {
                    return VotingResult::Pending {
                        votes_for: 0.0,
                        votes_against: 0.0,
                        votes_needed: 1.0,
                    };
                }
                let ratio = approve_weight / total_weight;
                if ratio > 0.5 {
                    VotingResult::Approved {
                        reason: format!("Simple majority: {:.1}% approval", ratio * 100.0),
                    }
                } else if reject_weight / total_weight > 0.5 {
                    VotingResult::Rejected {
                        reason: format!("Simple majority rejected: {:.1}% against", (reject_weight / total_weight) * 100.0),
                    }
                } else {
                    VotingResult::Pending {
                        votes_for: approve_weight,
                        votes_against: reject_weight,
                        votes_needed: total_weight * 0.5 - approve_weight + 0.01,
                    }
                }
            }

            VotingStrategy::Supermajority => {
                if total_weight == 0.0 {
                    return VotingResult::Pending {
                        votes_for: 0.0,
                        votes_against: 0.0,
                        votes_needed: 1.0,
                    };
                }
                let ratio = approve_weight / total_weight;
                if ratio >= 0.667 {
                    VotingResult::Approved {
                        reason: format!("Supermajority: {:.1}% approval", ratio * 100.0),
                    }
                } else if reject_weight / total_weight > 0.333 {
                    VotingResult::Rejected {
                        reason: format!("Supermajority blocked: {:.1}% against", (reject_weight / total_weight) * 100.0),
                    }
                } else {
                    VotingResult::Pending {
                        votes_for: approve_weight,
                        votes_against: reject_weight,
                        votes_needed: total_weight * 0.667 - approve_weight + 0.01,
                    }
                }
            }

            VotingStrategy::Weighted { threshold } => {
                if approve_weight >= threshold {
                    VotingResult::Approved {
                        reason: format!("Weighted threshold met: {} >= {}", approve_weight, threshold),
                    }
                } else if reject_weight >= threshold {
                    VotingResult::Rejected {
                        reason: format!("Weighted rejection: {} >= {}", reject_weight, threshold),
                    }
                } else {
                    VotingResult::Pending {
                        votes_for: approve_weight,
                        votes_against: reject_weight,
                        votes_needed: threshold - approve_weight,
                    }
                }
            }

            VotingStrategy::FirstVote => {
                if let Some(first) = votes.first() {
                    match first.decision {
                        VoteDecision::Approve => VotingResult::Approved {
                            reason: format!("First vote by {}: approve", first.voter),
                        },
                        VoteDecision::Reject => VotingResult::Rejected {
                            reason: format!("First vote by {}: reject", first.voter),
                        },
                        VoteDecision::Abstain => VotingResult::Pending {
                            votes_for: 0.0,
                            votes_against: 0.0,
                            votes_needed: 1.0,
                        },
                    }
                } else {
                    VotingResult::Pending {
                        votes_for: 0.0,
                        votes_against: 0.0,
                        votes_needed: 1.0,
                    }
                }
            }

            VotingStrategy::SingleApprover { ref approver } => {
                if let Some(vote) = votes.iter().find(|v| &v.voter == approver) {
                    match vote.decision {
                        VoteDecision::Approve => VotingResult::Approved {
                            reason: format!("Approved by designated approver: {}", approver),
                        },
                        VoteDecision::Reject => VotingResult::Rejected {
                            reason: format!("Rejected by designated approver: {}", approver),
                        },
                        VoteDecision::Abstain => VotingResult::Pending {
                            votes_for: 0.0,
                            votes_against: 0.0,
                            votes_needed: 1.0,
                        },
                    }
                } else {
                    VotingResult::Pending {
                        votes_for: 0.0,
                        votes_against: 0.0,
                        votes_needed: 1.0,
                    }
                }
            }
        }
    }

    /// Process a proposal through the voting system
    pub fn process_proposal(
        &self,
        proposal_id: ProposalId,
        proposal_manager: &mut ProposalManager,
    ) -> VotingResult {
        let result = self.evaluate(proposal_id);

        if let Some(proposal) = proposal_manager.get_mut(proposal_id) {
            match &result {
                VotingResult::Approved { reason } => proposal.approve(Some(reason.clone())),
                VotingResult::Rejected { reason } => proposal.reject(Some(reason.clone())),
                VotingResult::Pending { .. } => {}
            }
        }

        result
    }

    /// Clean up votes for resolved proposals
    pub fn cleanup(&mut self, proposal_manager: &ProposalManager) {
        self.votes.retain(|id, _| {
            proposal_manager.get(*id).is_some_and(|p| p.is_pending())
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_majority() {
        let mut coordinator = VotingCoordinator::new(VotingStrategy::SimpleMajority);
        let config = CapabilityConfig::default();
        let proposal_id = ProposalId::new();

        coordinator.cast_vote(
            Vote::new(proposal_id, AgentId::User, VoteDecision::Approve),
            &config,
        ).unwrap();

        coordinator.cast_vote(
            Vote::new(proposal_id, AgentId::Claude, VoteDecision::Approve),
            &config,
        ).unwrap();

        coordinator.cast_vote(
            Vote::new(proposal_id, AgentId::Llama, VoteDecision::Reject),
            &config,
        ).unwrap();

        let result = coordinator.evaluate(proposal_id);
        assert!(matches!(result, VotingResult::Approved { .. }));
    }

    #[test]
    fn test_unanimous_rejection() {
        let mut coordinator = VotingCoordinator::new(VotingStrategy::Unanimous);
        let config = CapabilityConfig::default();
        let proposal_id = ProposalId::new();

        coordinator.cast_vote(
            Vote::new(proposal_id, AgentId::User, VoteDecision::Approve),
            &config,
        ).unwrap();

        coordinator.cast_vote(
            Vote::new(proposal_id, AgentId::Claude, VoteDecision::Reject),
            &config,
        ).unwrap();

        let result = coordinator.evaluate(proposal_id);
        assert!(matches!(result, VotingResult::Rejected { .. }));
    }

    #[test]
    fn test_weighted_voting() {
        let mut coordinator = VotingCoordinator::new(VotingStrategy::Weighted { threshold: 2.0 });
        let mut config = CapabilityConfig::default();

        // Give User higher weight
        let user_caps = super::super::capabilities::AgentCapabilities::new(AgentId::User)
            .with_vote_weight(2.0);
        config.set_capabilities(user_caps).unwrap();

        let proposal_id = ProposalId::new();

        coordinator.cast_vote(
            Vote::new(proposal_id, AgentId::User, VoteDecision::Approve),
            &config,
        ).unwrap();

        let result = coordinator.evaluate(proposal_id);
        assert!(matches!(result, VotingResult::Approved { .. }));
    }

    #[test]
    fn test_duplicate_vote_rejected() {
        let mut coordinator = VotingCoordinator::new(VotingStrategy::SimpleMajority);
        let config = CapabilityConfig::default();
        let proposal_id = ProposalId::new();

        coordinator.cast_vote(
            Vote::new(proposal_id, AgentId::User, VoteDecision::Approve),
            &config,
        ).unwrap();

        let result = coordinator.cast_vote(
            Vote::new(proposal_id, AgentId::User, VoteDecision::Reject),
            &config,
        );

        assert!(result.is_err());
    }
}
