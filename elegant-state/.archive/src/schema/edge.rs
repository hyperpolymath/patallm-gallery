use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use ulid::Ulid;

use super::{Metadata, NodeId};

pub type EdgeId = Ulid;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "snake_case")]
pub enum EdgeKind {
    References,
    DerivedFrom,
    RelatedTo,
    PartOf,
    Blocks,
    Enables,
    Supersedes,
    Custom(String),
}

impl std::fmt::Display for EdgeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EdgeKind::References => write!(f, "references"),
            EdgeKind::DerivedFrom => write!(f, "derived_from"),
            EdgeKind::RelatedTo => write!(f, "related_to"),
            EdgeKind::PartOf => write!(f, "part_of"),
            EdgeKind::Blocks => write!(f, "blocks"),
            EdgeKind::Enables => write!(f, "enables"),
            EdgeKind::Supersedes => write!(f, "supersedes"),
            EdgeKind::Custom(s) => write!(f, "custom:{}", s),
        }
    }
}

impl std::str::FromStr for EdgeKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "references" => Ok(EdgeKind::References),
            "derived_from" => Ok(EdgeKind::DerivedFrom),
            "related_to" => Ok(EdgeKind::RelatedTo),
            "part_of" => Ok(EdgeKind::PartOf),
            "blocks" => Ok(EdgeKind::Blocks),
            "enables" => Ok(EdgeKind::Enables),
            "supersedes" => Ok(EdgeKind::Supersedes),
            s if s.starts_with("custom:") => Ok(EdgeKind::Custom(s[7..].to_string())),
            _ => Err(format!("Unknown edge kind: {}", s)),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateEdge {
    pub id: EdgeId,
    pub from: NodeId,
    pub to: NodeId,
    pub kind: EdgeKind,
    pub weight: f32,
    pub metadata: Metadata,
    pub created_at: DateTime<Utc>,
}

impl StateEdge {
    pub fn new(from: NodeId, to: NodeId, kind: EdgeKind) -> Self {
        Self {
            id: Ulid::new(),
            from,
            to,
            kind,
            weight: 1.0,
            metadata: std::collections::HashMap::new(),
            created_at: Utc::now(),
        }
    }

    pub fn with_weight(mut self, weight: f32) -> Self {
        self.weight = weight;
        self
    }

    pub fn with_metadata(mut self, metadata: Metadata) -> Self {
        self.metadata = metadata;
        self
    }
}
