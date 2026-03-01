use async_graphql::{Enum, InputObject, SimpleObject, ID};
use crate::schema::{self as domain, NodeKind as DomainNodeKind, EdgeKind as DomainEdgeKind, AgentId as DomainAgentId};

// GraphQL enum for NodeKind
#[derive(Enum, Copy, Clone, Eq, PartialEq)]
pub enum NodeKind {
    Conversation,
    Project,
    Insight,
    Task,
    Context,
    Module,
    Agent,
}

impl From<NodeKind> for DomainNodeKind {
    fn from(k: NodeKind) -> Self {
        match k {
            NodeKind::Conversation => DomainNodeKind::Conversation,
            NodeKind::Project => DomainNodeKind::Project,
            NodeKind::Insight => DomainNodeKind::Insight,
            NodeKind::Task => DomainNodeKind::Task,
            NodeKind::Context => DomainNodeKind::Context,
            NodeKind::Module => DomainNodeKind::Module,
            NodeKind::Agent => DomainNodeKind::Agent,
        }
    }
}

impl From<DomainNodeKind> for NodeKind {
    fn from(k: DomainNodeKind) -> Self {
        match k {
            DomainNodeKind::Conversation => NodeKind::Conversation,
            DomainNodeKind::Project => NodeKind::Project,
            DomainNodeKind::Insight => NodeKind::Insight,
            DomainNodeKind::Task => NodeKind::Task,
            DomainNodeKind::Context => NodeKind::Context,
            DomainNodeKind::Module => NodeKind::Module,
            DomainNodeKind::Agent => NodeKind::Agent,
            DomainNodeKind::Custom(_) => NodeKind::Context, // Fallback
        }
    }
}

// GraphQL enum for EdgeKind
#[derive(Enum, Copy, Clone, Eq, PartialEq)]
pub enum EdgeKind {
    References,
    DerivedFrom,
    RelatedTo,
    PartOf,
    Blocks,
    Enables,
    Supersedes,
}

impl From<EdgeKind> for DomainEdgeKind {
    fn from(k: EdgeKind) -> Self {
        match k {
            EdgeKind::References => DomainEdgeKind::References,
            EdgeKind::DerivedFrom => DomainEdgeKind::DerivedFrom,
            EdgeKind::RelatedTo => DomainEdgeKind::RelatedTo,
            EdgeKind::PartOf => DomainEdgeKind::PartOf,
            EdgeKind::Blocks => DomainEdgeKind::Blocks,
            EdgeKind::Enables => DomainEdgeKind::Enables,
            EdgeKind::Supersedes => DomainEdgeKind::Supersedes,
        }
    }
}

impl From<DomainEdgeKind> for EdgeKind {
    fn from(k: DomainEdgeKind) -> Self {
        match k {
            DomainEdgeKind::References => EdgeKind::References,
            DomainEdgeKind::DerivedFrom => EdgeKind::DerivedFrom,
            DomainEdgeKind::RelatedTo => EdgeKind::RelatedTo,
            DomainEdgeKind::PartOf => EdgeKind::PartOf,
            DomainEdgeKind::Blocks => EdgeKind::Blocks,
            DomainEdgeKind::Enables => EdgeKind::Enables,
            DomainEdgeKind::Supersedes => EdgeKind::Supersedes,
            DomainEdgeKind::Custom(_) => EdgeKind::RelatedTo, // Fallback
        }
    }
}

#[derive(Enum, Copy, Clone, Eq, PartialEq)]
pub enum AgentKind {
    User,
    Claude,
    Llama,
    System,
}

impl From<AgentKind> for DomainAgentId {
    fn from(a: AgentKind) -> Self {
        match a {
            AgentKind::User => DomainAgentId::User,
            AgentKind::Claude => DomainAgentId::Claude,
            AgentKind::Llama => DomainAgentId::Llama,
            AgentKind::System => DomainAgentId::System,
        }
    }
}

// GraphQL output types
#[derive(SimpleObject)]
pub struct StateNode {
    pub id: ID,
    pub kind: NodeKind,
    pub content: async_graphql::Json<serde_json::Value>,
    pub metadata: async_graphql::Json<serde_json::Value>,
    pub created_at: String,
    pub updated_at: String,
}

impl From<domain::StateNode> for StateNode {
    fn from(n: domain::StateNode) -> Self {
        Self {
            id: ID(n.id.to_string()),
            kind: n.kind.into(),
            content: async_graphql::Json(n.content),
            metadata: async_graphql::Json(serde_json::to_value(&n.metadata).unwrap_or_default()),
            created_at: n.created_at.to_rfc3339(),
            updated_at: n.updated_at.to_rfc3339(),
        }
    }
}

#[derive(SimpleObject)]
pub struct StateEdge {
    pub id: ID,
    pub from: ID,
    pub to: ID,
    pub kind: EdgeKind,
    pub weight: f32,
    pub created_at: String,
}

impl From<domain::StateEdge> for StateEdge {
    fn from(e: domain::StateEdge) -> Self {
        Self {
            id: ID(e.id.to_string()),
            from: ID(e.from.to_string()),
            to: ID(e.to.to_string()),
            kind: e.kind.into(),
            weight: e.weight,
            created_at: e.created_at.to_rfc3339(),
        }
    }
}

#[derive(SimpleObject)]
pub struct StateEvent {
    pub id: ID,
    pub timestamp: String,
    pub agent: String,
    pub operation: String,
    pub before: Option<async_graphql::Json<serde_json::Value>>,
    pub after: Option<async_graphql::Json<serde_json::Value>>,
}

impl From<domain::StateEvent> for StateEvent {
    fn from(e: domain::StateEvent) -> Self {
        Self {
            id: ID(e.id.to_string()),
            timestamp: e.timestamp.to_rfc3339(),
            agent: e.agent.to_string(),
            operation: format!("{:?}", e.operation),
            before: e.before.map(async_graphql::Json),
            after: e.after.map(async_graphql::Json),
        }
    }
}

// Input types
#[derive(InputObject)]
pub struct CreateNodeInput {
    pub kind: NodeKind,
    pub content: async_graphql::Json<serde_json::Value>,
    pub metadata: Option<async_graphql::Json<serde_json::Value>>,
}

#[derive(InputObject)]
pub struct UpdateNodeInput {
    pub id: ID,
    pub content: async_graphql::Json<serde_json::Value>,
}

#[derive(InputObject)]
pub struct CreateEdgeInput {
    pub from: ID,
    pub to: ID,
    pub kind: EdgeKind,
    pub weight: Option<f32>,
}
