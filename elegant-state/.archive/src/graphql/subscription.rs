//! GraphQL subscriptions for real-time event streaming
//!
//! Provides subscriptions for:
//! - Node changes
//! - Edge changes
//! - Event stream
//! - Proposal updates
//! - Vote notifications

use async_graphql::{Context, Subscription, Result, ID};
use futures_util::Stream;
use tokio::sync::broadcast;
use std::pin::Pin;

use super::types::{StateEvent, StateNode, NodeKind};

/// Event types for subscriptions
#[derive(Debug, Clone)]
pub enum SubscriptionEvent {
    NodeCreated(crate::schema::StateNode),
    NodeUpdated(crate::schema::StateNode),
    NodeDeleted(crate::schema::NodeId),
    EdgeCreated(crate::schema::StateEdge),
    EdgeDeleted(crate::schema::EdgeId),
    Event(crate::schema::StateEvent),
}

/// Broadcast channel for events
pub type EventSender = broadcast::Sender<SubscriptionEvent>;
pub type EventReceiver = broadcast::Receiver<SubscriptionEvent>;

/// Create a new event broadcast channel
pub fn create_event_channel(capacity: usize) -> (EventSender, EventReceiver) {
    broadcast::channel(capacity)
}

pub struct SubscriptionRoot;

#[Subscription]
impl SubscriptionRoot {
    /// Subscribe to node changes
    async fn node_changed<'ctx>(
        &self,
        ctx: &Context<'ctx>,
        kinds: Option<Vec<NodeKind>>,
    ) -> Result<Pin<Box<dyn Stream<Item = StateNode> + Send + 'ctx>>> {
        let receiver = ctx.data::<EventSender>()?.subscribe();

        let stream = async_stream::stream! {
            let mut receiver = receiver;
            while let Ok(event) = receiver.recv().await {
                match event {
                    SubscriptionEvent::NodeCreated(node) | SubscriptionEvent::NodeUpdated(node) => {
                        // Filter by kind if specified
                        if let Some(ref ks) = kinds {
                            let node_kind: NodeKind = node.kind.clone().into();
                            if !ks.contains(&node_kind) {
                                continue;
                            }
                        }
                        yield node.into();
                    }
                    _ => {}
                }
            }
        };

        Ok(Box::pin(stream))
    }

    /// Subscribe to all events
    async fn event_stream<'ctx>(
        &self,
        ctx: &Context<'ctx>,
    ) -> Result<Pin<Box<dyn Stream<Item = StateEvent> + Send + 'ctx>>> {
        let receiver = ctx.data::<EventSender>()?.subscribe();

        let stream = async_stream::stream! {
            let mut receiver = receiver;
            while let Ok(event) = receiver.recv().await {
                if let SubscriptionEvent::Event(e) = event {
                    yield e.into();
                }
            }
        };

        Ok(Box::pin(stream))
    }Nickel package for Guix — currently not in Guix, may need to package it

    /// Subscribe to events by specific agent
    async fn events_by_agent<'ctx>(
        &self,
        ctx: &Context<'ctx>,
        agent: String,
    ) -> Result<Pin<Box<dyn Stream<Item = StateEvent> + Send + 'ctx>>> {
        let receiver = ctx.data::<EventSender>()?.subscribe();

        let stream = async_stream::stream! {
            let mut receiver = receiver;
            while let Ok(event) = receiver.recv().await {
                if let SubscriptionEvent::Event(e) = event {
                    if e.agent.to_string() == agent {
                        yield e.into();
                    }p
                }
            }
        };

        Ok(Box::pin(stream))
    }
}

/// Helper to publish events to subscribers
pub struct EventPublisher {
    sender: EventSender,
}

impl EventPublisher {
    pub fn new(sender: EventSender) -> Self {
        Self { sender }
    }

    pub fn publish(&self, event: SubscriptionEvent) {
        // Ignore send errors (no subscribers)
        let _ = self.sender.send(event);
    }

    pub fn node_created(&self, node: crate::schema::StateNode) {
        self.publish(SubscriptionEvent::NodeCreated(node));
    }

    pub fn node_updated(&self, node: crate::schema::StateNode) {
        self.publish(SubscriptionEvent::NodeUpdated(node));
    }

    pub fn node_deleted(&self, id: crate::schema::NodeId) {
        self.publish(SubscriptionEvent::NodeDeleted(id));
    }

    pub fn edge_created(&self, edge: crate::schema::StateEdge) {
        self.publish(SubscriptionEvent::EdgeCreated(edge));
    }

    pub fn edge_deleted(&self, id: crate::schema::EdgeId) {
        self.publish(SubscriptionEvent::EdgeDeleted(id));
    }

    pub fn event(&self, event: crate::schema::StateEvent) {
        self.publish(SubscriptionEvent::Event(event));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_event_channel() {
        let (sender, mut receiver) = create_event_channel(16);

        let node = crate::schema::StateNode::new(
            crate::schema::NodeKind::Insight,
            serde_json::json!({"test": true}),
        );

        sender.send(SubscriptionEvent::NodeCreated(node.clone())).unwrap();

        match receiver.try_recv().unwrap() {
            SubscriptionEvent::NodeCreated(n) => assert_eq!(n.id, node.id),
            _ => panic!("Wrong event type"),
        }
    }
}
