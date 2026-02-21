//! Coordinator module for multi-agent state management
//!
//! Provides:
//! - Proposal mode (Direct vs Proposal capabilities)
//! - Voting system for proposal approval
//! - Agent reputation tracking

mod capabilities;
mod proposal;
mod voting;
mod reputation;

pub use capabilities::{CapabilityMode, AgentCapabilities, CapabilityConfig};
pub use proposal::{Proposal, ProposalStatus, ProposalManager};
pub use voting::{Vote, VoteDecision, VotingStrategy, VotingCoordinator};
pub use reputation::{Reputation, ReputationTracker};
