//! Agent reputation tracking
//!
//! Tracks voting history and adjusts agent reputation based on outcomes.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::schema::AgentId;
use super::voting::{VoteDecision, VotingResult};
use super::proposal::ProposalId;

/// Reputation score for an agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Reputation {
    pub agent: AgentId,
    pub score: f32,
    pub total_votes: u32,
    pub correct_votes: u32,
    pub last_updated: DateTime<Utc>,
}

impl Reputation {
    /// Create a new reputation with default score
    pub fn new(agent: AgentId) -> Self {
        Self {
            agent,
            score: 0.5, // Start neutral
            total_votes: 0,
            correct_votes: 0,
            last_updated: Utc::now(),
        }
    }

    /// Calculate accuracy as a percentage
    pub fn accuracy(&self) -> f32 {
        if self.total_votes == 0 {
            0.5 // Neutral when no history
        } else {
            self.correct_votes as f32 / self.total_votes as f32
        }
    }

    /// Update reputation based on vote outcome
    pub fn record_vote_outcome(&mut self, was_correct: bool) {
        self.total_votes += 1;
        if was_correct {
            self.correct_votes += 1;
        }

        // Exponential moving average for score
        let alpha = 0.1; // Learning rate
        let outcome = if was_correct { 1.0 } else { 0.0 };
        self.score = self.score * (1.0 - alpha) + outcome * alpha;

        // Clamp to valid range
        self.score = self.score.clamp(0.0, 1.0);

        self.last_updated = Utc::now();
    }

    /// Apply time decay to reputation (call periodically)
    pub fn apply_decay(&mut self, decay_factor: f32) {
        // Move towards neutral (0.5) over time
        self.score = 0.5 + (self.score - 0.5) * decay_factor;
        self.last_updated = Utc::now();
    }
}

/// Tracks reputation for all agents
pub struct ReputationTracker {
    reputations: HashMap<String, Reputation>,
    /// Weight given to reputation in vote weight calculation
    reputation_weight: f32,
    /// Base vote weight before reputation modifier
    base_weight: f32,
}

impl Default for ReputationTracker {
    fn default() -> Self {
        Self::new()
    }
}

impl ReputationTracker {
    /// Create a new reputation tracker
    pub fn new() -> Self {
        Self {
            reputations: HashMap::new(),
            reputation_weight: 0.5,
            base_weight: 1.0,
        }
    }

    /// Set the weight given to reputation
    pub fn with_reputation_weight(mut self, weight: f32) -> Self {
        self.reputation_weight = weight.clamp(0.0, 1.0);
        self
    }

    /// Get or create reputation for an agent
    pub fn get_or_create(&mut self, agent: &AgentId) -> &mut Reputation {
        let key = agent.to_string();
        self.reputations
            .entry(key)
            .or_insert_with(|| Reputation::new(agent.clone()))
    }

    /// Get reputation for an agent (read-only)
    pub fn get(&self, agent: &AgentId) -> Option<&Reputation> {
        self.reputations.get(&agent.to_string())
    }

    /// Calculate vote weight based on reputation
    pub fn calculate_vote_weight(&self, agent: &AgentId) -> f32 {
        let reputation = self
            .reputations
            .get(&agent.to_string())
            .map(|r| r.score)
            .unwrap_or(0.5);

        // Linear interpolation between base weight and reputation-modified weight
        let reputation_modifier = 0.5 + reputation; // Range: 0.5 to 1.5
        self.base_weight * (1.0 - self.reputation_weight)
            + self.base_weight * reputation_modifier * self.reputation_weight
    }

    /// Record outcome of a vote
    pub fn record_outcome(
        &mut self,
        agent: &AgentId,
        decision: VoteDecision,
        result: &VotingResult,
    ) {
        let was_correct = match (decision, result) {
            (VoteDecision::Approve, VotingResult::Approved { .. }) => true,
            (VoteDecision::Reject, VotingResult::Rejected { .. }) => true,
            (VoteDecision::Abstain, _) => return, // Abstain doesn't affect reputation
            _ => false,
        };

        let reputation = self.get_or_create(agent);
        reputation.record_vote_outcome(was_correct);
    }

    /// Apply decay to all reputations
    pub fn apply_decay_all(&mut self, decay_factor: f32) {
        for reputation in self.reputations.values_mut() {
            reputation.apply_decay(decay_factor);
        }
    }

    /// Get all reputations sorted by score (descending)
    pub fn leaderboard(&self) -> Vec<&Reputation> {
        let mut reps: Vec<_> = self.reputations.values().collect();
        reps.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));
        reps
    }

    /// Export reputations to JSON
    pub fn export(&self) -> serde_json::Value {
        serde_json::to_value(&self.reputations).unwrap_or_default()
    }

    /// Import reputations from JSON
    pub fn import(&mut self, data: serde_json::Value) -> Result<(), String> {
        let imported: HashMap<String, Reputation> =
            serde_json::from_value(data).map_err(|e| e.to_string())?;
        self.reputations.extend(imported);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reputation_starts_neutral() {
        let rep = Reputation::new(AgentId::Claude);
        assert!((rep.score - 0.5).abs() < f32::EPSILON);
    }

    #[test]
    fn test_reputation_increases_on_correct_votes() {
        let mut rep = Reputation::new(AgentId::Claude);

        for _ in 0..10 {
            rep.record_vote_outcome(true);
        }

        assert!(rep.score > 0.5);
        assert_eq!(rep.total_votes, 10);
        assert_eq!(rep.correct_votes, 10);
    }

    #[test]
    fn test_reputation_decreases_on_incorrect_votes() {
        let mut rep = Reputation::new(AgentId::Claude);

        for _ in 0..10 {
            rep.record_vote_outcome(false);
        }

        assert!(rep.score < 0.5);
    }

    #[test]
    fn test_vote_weight_calculation() {
        let mut tracker = ReputationTracker::new().with_reputation_weight(1.0);

        // High reputation agent
        let rep = tracker.get_or_create(&AgentId::User);
        for _ in 0..20 {
            rep.record_vote_outcome(true);
        }

        let high_weight = tracker.calculate_vote_weight(&AgentId::User);

        // Low reputation agent
        let rep = tracker.get_or_create(&AgentId::Llama);
        for _ in 0..20 {
            rep.record_vote_outcome(false);
        }

        let low_weight = tracker.calculate_vote_weight(&AgentId::Llama);

        assert!(high_weight > low_weight);
    }

    #[test]
    fn test_leaderboard() {
        let mut tracker = ReputationTracker::new();

        // Create agents with different scores
        let rep = tracker.get_or_create(&AgentId::User);
        for _ in 0..10 {
            rep.record_vote_outcome(true);
        }

        let rep = tracker.get_or_create(&AgentId::Claude);
        for _ in 0..10 {
            rep.record_vote_outcome(false);
        }

        let leaderboard = tracker.leaderboard();
        assert_eq!(leaderboard.len(), 2);
        assert!(leaderboard[0].score > leaderboard[1].score);
    }
}
