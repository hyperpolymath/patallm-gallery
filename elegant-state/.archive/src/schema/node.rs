use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use ulid::Ulid;

pub type NodeId = Ulid;
pub type Metadata = HashMap<String, Value>;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "snake_case")]
pub enum NodeKind {
    Conversation,
    Project,
    Insight,
    Task,
    Context,
    Module,
    Agent,
    Custom(String),
}

impl std::fmt::Display for NodeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NodeKind::Conversation => write!(f, "conversation"),
            NodeKind::Project => write!(f, "project"),
            NodeKind::Insight => write!(f, "insight"),
            NodeKind::Task => write!(f, "task"),
            NodeKind::Context => write!(f, "context"),
            NodeKind::Module => write!(f, "module"),
            NodeKind::Agent => write!(f, "agent"),
            NodeKind::Custom(s) => write!(f, "custom:{}", s),
        }
    }
}

impl std::str::FromStr for NodeKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "conversation" => Ok(NodeKind::Conversation),
            "project" => Ok(NodeKind::Project),
            "insight" => Ok(NodeKind::Insight),
            "task" => Ok(NodeKind::Task),
            "context" => Ok(NodeKind::Context),
            "module" => Ok(NodeKind::Module),
            "agent" => Ok(NodeKind::Agent),
            s if s.starts_with("custom:") => Ok(NodeKind::Custom(s[7..].to_string())),
            _ => Err(format!("Unknown node kind: {}", s)),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateNode {
    pub id: NodeId,
    pub kind: NodeKind,
    pub content: Value,
    pub metadata: Metadata,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

impl StateNode {
    pub fn new(kind: NodeKind, content: Value) -> Self {
        let now = Utc::now();
        Self {
            id: Ulid::new(),
            kind,
            content,
            metadata: HashMap::new(),
            created_at: now,
            updated_at: now,
        }
    }

    pub fn with_metadata(mut self, metadata: Metadata) -> Self {
        self.metadata = metadata;
        self
    }

    pub fn with_id(mut self, id: NodeId) -> Self {
        self.id = id;
        self
    }
}
