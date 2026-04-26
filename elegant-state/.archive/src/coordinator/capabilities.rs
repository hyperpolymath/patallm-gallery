//! Agent capability modes
//!
//! Controls whether agents have direct write access or must propose changes.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use crate::schema::AgentId;

/// Capability mode for an agent
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "snake_case")]
pub enum CapabilityMode {
    /// Agent can directly mutate state
    Direct,
    /// Agent must propose changes for approval
    #[default]
    Proposal,
    /// Agent can only observe (read-only)
    Observer,
}

impl std::fmt::Display for CapabilityMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CapabilityMode::Direct => write!(f, "direct"),
            CapabilityMode::Proposal => write!(f, "proposal"),
            CapabilityMode::Observer => write!(f, "observer"),
        }
    }
}

impl std::str::FromStr for CapabilityMode {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "direct" => Ok(CapabilityMode::Direct),
            "proposal" => Ok(CapabilityMode::Proposal),
            "observer" => Ok(CapabilityMode::Observer),
            _ => Err(format!("Unknown capability mode: {}", s)),
        }
    }
}

/// Capabilities for a specific agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentCapabilities {
    pub agent: AgentId,
    pub mode: CapabilityMode,
    pub can_vote: bool,
    pub vote_weight: f32,
}

impl AgentCapabilities {
    pub fn new(agent: AgentId) -> Self {
        Self {
            agent,
            mode: CapabilityMode::default(),
            can_vote: true,
            vote_weight: 1.0,
        }
    }

    pub fn with_mode(mut self, mode: CapabilityMode) -> Self {
        self.mode = mode;
        self
    }

    pub fn with_vote_weight(mut self, weight: f32) -> Self {
        self.vote_weight = weight;
        self
    }

    pub fn as_observer(mut self) -> Self {
        self.mode = CapabilityMode::Observer;
        self.can_vote = false;
        self
    }
}

/// System-wide capability configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CapabilityConfig {
    /// Default mode for new agents
    pub default_mode: CapabilityMode,
    /// Per-agent overrides
    pub agent_overrides: HashMap<String, AgentCapabilities>,
    /// Whether to allow runtime mode changes
    pub allow_runtime_changes: bool,
}

impl Default for CapabilityConfig {
    fn default() -> Self {
        Self {
            default_mode: CapabilityMode::Proposal,
            agent_overrides: HashMap::new(),
            allow_runtime_changes: true,
        }
    }
}

impl CapabilityConfig {
    /// Create a permissive development config (all direct)
    pub fn development() -> Self {
        Self {
            default_mode: CapabilityMode::Direct,
            agent_overrides: HashMap::new(),
            allow_runtime_changes: true,
        }
    }

    /// Create a restrictive production config (all proposal)
    pub fn production() -> Self {
        Self {
            default_mode: CapabilityMode::Proposal,
            agent_overrides: HashMap::new(),
            allow_runtime_changes: false,
        }
    }

    /// Get capabilities for an agent
    pub fn get_capabilities(&self, agent: &AgentId) -> AgentCapabilities {
        let key = agent.to_string();
        self.agent_overrides
            .get(&key)
            .cloned()
            .unwrap_or_else(|| AgentCapabilities::new(agent.clone()).with_mode(self.default_mode))
    }

    /// Set capabilities for an agent
    pub fn set_capabilities(&mut self, capabilities: AgentCapabilities) -> Result<(), String> {
        if !self.allow_runtime_changes {
            return Err("Runtime capability changes are disabled".into());
        }
        let key = capabilities.agent.to_string();
        self.agent_overrides.insert(key, capabilities);
        Ok(())
    }

    /// Check if an agent can write directly
    pub fn can_write_directly(&self, agent: &AgentId) -> bool {
        matches!(
            self.get_capabilities(agent).mode,
            CapabilityMode::Direct
        )
    }

    /// Check if an agent can vote
    pub fn can_vote(&self, agent: &AgentId) -> bool {
        self.get_capabilities(agent).can_vote
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_capabilities() {
        let config = CapabilityConfig::default();
        let caps = config.get_capabilities(&AgentId::Claude);

        assert_eq!(caps.mode, CapabilityMode::Proposal);
        assert!(caps.can_vote);
    }

    #[test]
    fn test_development_config() {
        let config = CapabilityConfig::development();

        assert!(config.can_write_directly(&AgentId::Claude));
        assert!(config.can_write_directly(&AgentId::Llama));
    }

    #[test]
    fn test_override_capabilities() {
        let mut config = CapabilityConfig::default();

        let caps = AgentCapabilities::new(AgentId::User).with_mode(CapabilityMode::Direct);
        config.set_capabilities(caps).unwrap();

        assert!(config.can_write_directly(&AgentId::User));
        assert!(!config.can_write_directly(&AgentId::Claude));
    }
}
