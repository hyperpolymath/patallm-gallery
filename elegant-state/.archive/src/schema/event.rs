use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use ulid::Ulid;

use super::{EdgeId, NodeId};

pub type EventId = Ulid;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum AgentId {
    User,
    Claude,
    Llama,
    Module(String),
    System,
}

impl std::fmt::Display for AgentId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AgentId::User => write!(f, "user"),
            AgentId::Claude => write!(f, "claude"),
            AgentId::Llama => write!(f, "llama"),
            AgentId::Module(s) => write!(f, "module:{}", s),
            AgentId::System => write!(f, "system"),
        }
    }
}

impl std::str::FromStr for AgentId {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "user" => Ok(AgentId::User),
            "claude" => Ok(AgentId::Claude),
            "llama" => Ok(AgentId::Llama),
            "system" => Ok(AgentId::System),
            s if s.starts_with("module:") => Ok(AgentId::Module(s[7..].to_string())),
            _ => Err(format!("Unknown agent: {}", s)),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum Operation {
    Create,
    Update,
    Delete,
    Link,
    Unlink,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Target {
    Node(NodeId),
    Edge(EdgeId),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateEvent {
    pub id: EventId,
    pub timestamp: DateTime<Utc>,
    pub agent: AgentId,
    pub operation: Operation,
    pub target: Target,
    pub before: Option<Value>,
    pub after: Option<Value>,
}

impl StateEvent {
    pub fn new(agent: AgentId, operation: Operation, target: Target) -> Self {
        Self {
            id: Ulid::new(),
            timestamp: Utc::now(),
            agent,
            operation,
            target,
            before: None,
            after: None,
        }
    }

    pub fn with_before(mut self, before: Value) -> Self {
        self.before = Some(before);
        self
    }

    pub fn with_after(mut self, after: Value) -> Self {
        self.after = Some(after);
        self
    }
}
