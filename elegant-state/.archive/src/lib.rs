//! elegant-STATE: Local-first state graph for multi-agent orchestration
//!
//! This library provides a persistent knowledge graph that multiple agents
//! (Claude, Llama, custom modules) can query and modify via GraphQL.

pub mod schema;
pub mod store;
pub mod graphql;
pub mod event;

pub use schema::{StateNode, StateEdge, StateEvent, NodeKind, EdgeKind, AgentId};
pub use store::{SledStore, Store, StoreError};
pub use graphql::{build_schema, StateSchema};
pub use event::EventSourcer;

/// Library version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
