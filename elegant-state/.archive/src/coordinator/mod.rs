// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
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
