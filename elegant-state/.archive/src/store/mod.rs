mod sled_store;
mod indices;

pub use sled_store::SledStore;
pub use indices::Indices;

use crate::schema::*;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum StoreError {
    #[error("Node not found: {0}")]
    NodeNotFound(NodeId),

    #[error("Edge not found: {0}")]
    EdgeNotFound(EdgeId),

    #[error("Database error: {0}")]
    Database(#[from] sled::Error),

    #[error("Serialization error: {0}")]
    Serialization(String),

    #[error("Invalid operation: {0}")]
    InvalidOperation(String),
}

pub type Result<T> = std::result::Result<T, StoreError>;

/// Core trait for state storage backends
pub trait Store: Send + Sync {
    // Node operations
    fn create_node(&self, node: StateNode, agent: AgentId) -> Result<StateNode>;
    fn get_node(&self, id: NodeId) -> Result<Option<StateNode>>;
    fn update_node(&self, id: NodeId, content: serde_json::Value, agent: AgentId) -> Result<StateNode>;
    fn delete_node(&self, id: NodeId, agent: AgentId) -> Result<()>;
    fn list_nodes(&self, kind: Option<NodeKind>, limit: usize) -> Result<Vec<StateNode>>;

    // Edge operations
    fn create_edge(&self, edge: StateEdge, agent: AgentId) -> Result<StateEdge>;
    fn get_edge(&self, id: EdgeId) -> Result<Option<StateEdge>>;
    fn delete_edge(&self, id: EdgeId, agent: AgentId) -> Result<()>;
    fn edges_from(&self, node_id: NodeId) -> Result<Vec<StateEdge>>;
    fn edges_to(&self, node_id: NodeId) -> Result<Vec<StateEdge>>;

    // Event operations
    fn get_events(&self, since: Option<chrono::DateTime<chrono::Utc>>, limit: usize) -> Result<Vec<StateEvent>>;

    // Search
    fn search(&self, query: &str, kinds: Option<Vec<NodeKind>>) -> Result<Vec<StateNode>>;

    // Graph traversal
    fn neighbors(&self, id: NodeId, depth: usize) -> Result<Vec<StateNode>>;
}
