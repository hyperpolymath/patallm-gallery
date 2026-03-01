//! Proposal management for mutation requests
//!
//! When agents operate in Proposal mode, their mutations become proposals
//! that require approval before execution.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use ulid::Ulid;

use crate::schema::{AgentId, Operation, NodeId, EdgeId};

pub type ProposalId = Ulid;

/// A proposed mutation to the state graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Proposal {
    pub id: ProposalId,
    pub proposer: AgentId,
    pub operation: Operation,
    pub target: ProposalTarget,
    pub payload: Value,
    pub rationale: Option<String>,
    pub status: ProposalStatus,
    pub created_at: DateTime<Utc>,
    pub resolved_at: Option<DateTime<Utc>>,
    pub resolution_reason: Option<String>,
}

/// Target of a proposed mutation
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ProposalTarget {
    Node { id: Option<NodeId>, kind: Option<String> },
    Edge { id: Option<EdgeId>, from: Option<NodeId>, to: Option<NodeId> },
}

/// Status of a proposal
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "snake_case")]
pub enum ProposalStatus {
    /// Awaiting votes
    #[default]
    Pending,
    /// Approved and executed
    Approved,
    /// Rejected
    Rejected,
    /// Expired without resolution
    Expired,
    /// Withdrawn by proposer
    Withdrawn,
}

impl Proposal {
    /// Create a new proposal
    pub fn new(proposer: AgentId, operation: Operation, target: ProposalTarget, payload: Value) -> Self {
        Self {
            id: Ulid::new(),
            proposer,
            operation,
            target,
            payload,
            rationale: None,
            status: ProposalStatus::Pending,
            created_at: Utc::now(),
            resolved_at: None,
            resolution_reason: None,
        }
    }

    /// Add rationale to the proposal
    pub fn with_rationale(mut self, rationale: impl Into<String>) -> Self {
        self.rationale = Some(rationale.into());
        self
    }

    /// Check if proposal is still pending
    pub fn is_pending(&self) -> bool {
        self.status == ProposalStatus::Pending
    }

    /// Approve the proposal
    pub fn approve(&mut self, reason: Option<String>) {
        self.status = ProposalStatus::Approved;
        self.resolved_at = Some(Utc::now());
        self.resolution_reason = reason;
    }

    /// Reject the proposal
    pub fn reject(&mut self, reason: Option<String>) {
        self.status = ProposalStatus::Rejected;
        self.resolved_at = Some(Utc::now());
        self.resolution_reason = reason;
    }

    /// Withdraw the proposal
    pub fn withdraw(&mut self) {
        self.status = ProposalStatus::Withdrawn;
        self.resolved_at = Some(Utc::now());
    }
}

/// Manages pending proposals
pub struct ProposalManager {
    proposals: HashMap<ProposalId, Proposal>,
    /// Maximum time a proposal can be pending (in seconds)
    expiry_seconds: i64,
}

impl Default for ProposalManager {
    fn default() -> Self {
        Self::new()
    }
}

impl ProposalManager {
    /// Create a new proposal manager
    pub fn new() -> Self {
        Self {
            proposals: HashMap::new(),
            expiry_seconds: 3600, // 1 hour default
        }
    }

    /// Set proposal expiry time
    pub fn with_expiry(mut self, seconds: i64) -> Self {
        self.expiry_seconds = seconds;
        self
    }

    /// Submit a new proposal
    pub fn submit(&mut self, proposal: Proposal) -> ProposalId {
        let id = proposal.id;
        self.proposals.insert(id, proposal);
        id
    }

    /// Get a proposal by ID
    pub fn get(&self, id: ProposalId) -> Option<&Proposal> {
        self.proposals.get(&id)
    }

    /// Get a mutable proposal by ID
    pub fn get_mut(&mut self, id: ProposalId) -> Option<&mut Proposal> {
        self.proposals.get_mut(&id)
    }

    /// List pending proposals
    pub fn pending(&self) -> Vec<&Proposal> {
        self.proposals
            .values()
            .filter(|p| p.is_pending())
            .collect()
    }

    /// List all proposals
    pub fn all(&self) -> Vec<&Proposal> {
        self.proposals.values().collect()
    }

    /// List proposals by proposer
    pub fn by_proposer(&self, agent: &AgentId) -> Vec<&Proposal> {
        self.proposals
            .values()
            .filter(|p| &p.proposer == agent)
            .collect()
    }

    /// Expire old pending proposals
    pub fn expire_old(&mut self) {
        let now = Utc::now();
        let expiry = chrono::Duration::seconds(self.expiry_seconds);

        for proposal in self.proposals.values_mut() {
            if proposal.is_pending() && now - proposal.created_at > expiry {
                proposal.status = ProposalStatus::Expired;
                proposal.resolved_at = Some(now);
                proposal.resolution_reason = Some("Expired".into());
            }
        }
    }

    /// Clean up resolved proposals older than given duration
    pub fn cleanup(&mut self, max_age: chrono::Duration) {
        let now = Utc::now();
        self.proposals.retain(|_, p| {
            if let Some(resolved_at) = p.resolved_at {
                now - resolved_at < max_age
            } else {
                true // Keep pending proposals
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_proposal_lifecycle() {
        let mut manager = ProposalManager::new();

        let proposal = Proposal::new(
            AgentId::Claude,
            Operation::Create,
            ProposalTarget::Node { id: None, kind: Some("insight".into()) },
            json!({"text": "hello"}),
        );

        let id = manager.submit(proposal);

        assert_eq!(manager.pending().len(), 1);

        let p = manager.get_mut(id).unwrap();
        p.approve(Some("Looks good".into()));

        assert_eq!(manager.pending().len(), 0);
        assert_eq!(manager.get(id).unwrap().status, ProposalStatus::Approved);
    }

    #[test]
    fn test_proposal_with_rationale() {
        let proposal = Proposal::new(
            AgentId::Llama,
            Operation::Update,
            ProposalTarget::Node { id: Some(Ulid::new()), kind: None },
            json!({"status": "done"}),
        )
        .with_rationale("Task completed successfully");

        assert!(proposal.rationale.is_some());
    }
}
