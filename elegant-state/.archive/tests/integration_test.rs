//! Integration tests for elegant-STATE
//!
//! Tests the high-level library API and coordination features.

use elegant_state::{
    AgentId, NodeKind, StateNode,
    CapabilityMode, CapabilityConfig,
    Proposal, ProposalStatus, ProposalManager, ProposalTarget,
    Vote, VoteDecision, VotingStrategy, VotingCoordinator, VotingResult,
    ReputationTracker, Operation,
};
use serde_json::json;

#[test]
fn test_state_node_creation() {
    let node = StateNode::new(NodeKind::Insight, json!({"text": "hello world"}));

    assert!(matches!(node.kind, NodeKind::Insight));
    assert_eq!(node.content["text"], "hello world");
}

#[test]
fn test_capability_modes() {
    // Default config uses Proposal mode for all agents
    let config = CapabilityConfig::default();

    let caps = config.get_capabilities(&AgentId::User);
    assert!(matches!(caps.mode, CapabilityMode::Proposal));

    let caps = config.get_capabilities(&AgentId::Claude);
    assert!(matches!(caps.mode, CapabilityMode::Proposal));

    // Test that we can check write capability
    assert!(!config.can_write_directly(&AgentId::User));
    assert!(!config.can_write_directly(&AgentId::Claude));
}

#[test]
fn test_proposal_workflow() {
    let mut manager = ProposalManager::new();

    let proposal = Proposal::new(
        AgentId::Claude,
        Operation::Create,
        ProposalTarget::Node { id: None, kind: Some("insight".into()) },
        json!({"text": "test insight"}),
    );

    let id = manager.submit(proposal);
    assert!(manager.get(id).is_some());
    assert!(manager.pending().len() == 1);

    // Approve the proposal
    if let Some(p) = manager.get_mut(id) {
        p.approve(Some("Looks good".into()));
    }

    assert!(manager.pending().is_empty());
    assert!(matches!(manager.get(id).unwrap().status, ProposalStatus::Approved));
}

#[test]
fn test_voting_simple_majority() {
    let mut coordinator = VotingCoordinator::new(VotingStrategy::SimpleMajority);
    let config = CapabilityConfig::default();
    let proposal_id = ulid::Ulid::new();

    // Cast approving votes
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

    // Should be approved (2 vs 1)
    let result = coordinator.evaluate(proposal_id);
    assert!(matches!(result, VotingResult::Approved { .. }));
}

#[test]
fn test_reputation_tracking() {
    let mut tracker = ReputationTracker::new();

    let rep = tracker.get_or_create(&AgentId::Claude);
    assert!((rep.score - 0.5).abs() < 0.01); // Start neutral

    // Record correct votes
    for _ in 0..5 {
        rep.record_vote_outcome(true);
    }

    assert!(rep.score > 0.5); // Score should increase
    assert_eq!(rep.total_votes, 5);
    assert_eq!(rep.correct_votes, 5);
}

#[test]
fn test_agent_ids() {
    assert_eq!(AgentId::User.to_string(), "user");
    assert_eq!(AgentId::Claude.to_string(), "claude");
    assert_eq!(AgentId::Llama.to_string(), "llama");
    assert_eq!(AgentId::System.to_string(), "system");
    assert_eq!(AgentId::Module("test".into()).to_string(), "module:test");
}

#[test]
fn test_node_kinds() {
    let kinds = [
        NodeKind::Conversation,
        NodeKind::Project,
        NodeKind::Insight,
        NodeKind::Task,
        NodeKind::Context,
        NodeKind::Module,
        NodeKind::Agent,
        NodeKind::Custom("test".into()),
    ];

    for kind in kinds {
        let node = StateNode::new(kind.clone(), json!({}));
        assert_eq!(node.kind, kind);
    }
}

#[test]
fn test_proposal_with_rationale() {
    let proposal = Proposal::new(
        AgentId::User,
        Operation::Update,
        ProposalTarget::Node { id: Some(ulid::Ulid::new()), kind: None },
        json!({"status": "done"}),
    )
    .with_rationale("Task completed successfully");

    assert!(proposal.rationale.is_some());
    assert_eq!(proposal.rationale.unwrap(), "Task completed successfully");
}
