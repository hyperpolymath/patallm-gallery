use super::{Result, Store, StoreError};
use crate::schema::*;
use serde_json::Value;
use sled::Db;
use std::path::Path;

const NODES_TREE: &str = "nodes";
const EDGES_TREE: &str = "edges";
const EVENTS_TREE: &str = "events";
const NODES_BY_KIND_TREE: &str = "nodes_by_kind";
const EDGES_BY_FROM_TREE: &str = "edges_by_from";
const EDGES_BY_TO_TREE: &str = "edges_by_to";

pub struct SledStore {
    db: Db,
}

impl SledStore {
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
        let db = sled::open(path)?;
        Ok(Self { db })
    }

    pub fn open_temporary() -> Result<Self> {
        let db = sled::Config::new().temporary(true).open()?;
        Ok(Self { db })
    }

    fn nodes_tree(&self) -> Result<sled::Tree> {
        Ok(self.db.open_tree(NODES_TREE)?)
    }

    fn edges_tree(&self) -> Result<sled::Tree> {
        Ok(self.db.open_tree(EDGES_TREE)?)
    }

    fn events_tree(&self) -> Result<sled::Tree> {
        Ok(self.db.open_tree(EVENTS_TREE)?)
    }

    fn nodes_by_kind_tree(&self) -> Result<sled::Tree> {
        Ok(self.db.open_tree(NODES_BY_KIND_TREE)?)
    }

    fn edges_by_from_tree(&self) -> Result<sled::Tree> {
        Ok(self.db.open_tree(EDGES_BY_FROM_TREE)?)
    }

    fn edges_by_to_tree(&self) -> Result<sled::Tree> {
        Ok(self.db.open_tree(EDGES_BY_TO_TREE)?)
    }

    fn serialize<T: serde::Serialize>(value: &T) -> Result<Vec<u8>> {
        bincode::serialize(value).map_err(|e| StoreError::Serialization(e.to_string()))
    }

    fn deserialize<T: serde::de::DeserializeOwned>(bytes: &[u8]) -> Result<T> {
        bincode::deserialize(bytes).map_err(|e| StoreError::Serialization(e.to_string()))
    }

    fn log_event(&self, event: StateEvent) -> Result<()> {
        let events = self.events_tree()?;
        let key = event.id.to_bytes();
        let value = Self::serialize(&event)?;
        events.insert(key, value)?;
        Ok(())
    }

    fn add_to_index(&self, tree: &sled::Tree, index_key: &[u8], id: &[u8]) -> Result<()> {
        let mut ids: Vec<Vec<u8>> = tree
            .get(index_key)?
            .map(|v| Self::deserialize(&v))
            .transpose()?
            .unwrap_or_default();

        if !ids.iter().any(|existing| existing == id) {
            ids.push(id.to_vec());
            tree.insert(index_key, Self::serialize(&ids)?)?;
        }
        Ok(())
    }

    fn remove_from_index(&self, tree: &sled::Tree, index_key: &[u8], id: &[u8]) -> Result<()> {
        if let Some(value) = tree.get(index_key)? {
            let mut ids: Vec<Vec<u8>> = Self::deserialize(&value)?;
            ids.retain(|existing| existing != id);
            if ids.is_empty() {
                tree.remove(index_key)?;
            } else {
                tree.insert(index_key, Self::serialize(&ids)?)?;
            }
        }
        Ok(())
    }
}

impl Store for SledStore {
    fn create_node(&self, node: StateNode, agent: AgentId) -> Result<StateNode> {
        let nodes = self.nodes_tree()?;
        let nodes_by_kind = self.nodes_by_kind_tree()?;

        let key = node.id.to_bytes();
        let value = Self::serialize(&node)?;

        nodes.insert(&key, value)?;

        // Index by kind
        let kind_key = node.kind.to_string();
        self.add_to_index(&nodes_by_kind, kind_key.as_bytes(), &key)?;

        // Log event
        let event = StateEvent::new(agent, Operation::Create, Target::Node(node.id))
            .with_after(serde_json::to_value(&node).unwrap());
        self.log_event(event)?;

        Ok(node)
    }

    fn get_node(&self, id: NodeId) -> Result<Option<StateNode>> {
        let nodes = self.nodes_tree()?;
        let key = id.to_bytes();

        match nodes.get(key)? {
            Some(bytes) => Ok(Some(Self::deserialize(&bytes)?)),
            None => Ok(None),
        }
    }

    fn update_node(&self, id: NodeId, content: Value, agent: AgentId) -> Result<StateNode> {
        let nodes = self.nodes_tree()?;
        let key = id.to_bytes();

        let old_node: StateNode = nodes
            .get(&key)?
            .map(|bytes| Self::deserialize(&bytes))
            .transpose()?
            .ok_or(StoreError::NodeNotFound(id))?;

        let mut new_node = old_node.clone();
        new_node.content = content;
        new_node.updated_at = chrono::Utc::now();

        nodes.insert(&key, Self::serialize(&new_node)?)?;

        // Log event
        let event = StateEvent::new(agent, Operation::Update, Target::Node(id))
            .with_before(serde_json::to_value(&old_node).unwrap())
            .with_after(serde_json::to_value(&new_node).unwrap());
        self.log_event(event)?;

        Ok(new_node)
    }

    fn delete_node(&self, id: NodeId, agent: AgentId) -> Result<()> {
        let nodes = self.nodes_tree()?;
        let nodes_by_kind = self.nodes_by_kind_tree()?;
        let key = id.to_bytes();

        let old_node: StateNode = nodes
            .get(&key)?
            .map(|bytes| Self::deserialize(&bytes))
            .transpose()?
            .ok_or(StoreError::NodeNotFound(id))?;

        // Remove from kind index
        let kind_key = old_node.kind.to_string();
        self.remove_from_index(&nodes_by_kind, kind_key.as_bytes(), &key)?;

        // Delete connected edges
        for edge in self.edges_from(id)? {
            self.delete_edge(edge.id, agent.clone())?;
        }
        for edge in self.edges_to(id)? {
            self.delete_edge(edge.id, agent.clone())?;
        }

        nodes.remove(&key)?;

        // Log event
        let event = StateEvent::new(agent, Operation::Delete, Target::Node(id))
            .with_before(serde_json::to_value(&old_node).unwrap());
        self.log_event(event)?;

        Ok(())
    }

    fn list_nodes(&self, kind: Option<NodeKind>, limit: usize) -> Result<Vec<StateNode>> {
        let nodes = self.nodes_tree()?;

        match kind {
            Some(k) => {
                let nodes_by_kind = self.nodes_by_kind_tree()?;
                let kind_key = k.to_string();

                let ids: Vec<Vec<u8>> = nodes_by_kind
                    .get(kind_key.as_bytes())?
                    .map(|v| Self::deserialize(&v))
                    .transpose()?
                    .unwrap_or_default();

                ids.into_iter()
                    .take(limit)
                    .filter_map(|id| nodes.get(&id).ok().flatten())
                    .map(|bytes| Self::deserialize(&bytes))
                    .collect()
            }
            None => nodes
                .iter()
                .take(limit)
                .filter_map(|r| r.ok())
                .map(|(_, bytes)| Self::deserialize(&bytes))
                .collect(),
        }
    }

    fn create_edge(&self, edge: StateEdge, agent: AgentId) -> Result<StateEdge> {
        let edges = self.edges_tree()?;
        let edges_by_from = self.edges_by_from_tree()?;
        let edges_by_to = self.edges_by_to_tree()?;

        let key = edge.id.to_bytes();
        let value = Self::serialize(&edge)?;

        edges.insert(&key, value)?;

        // Index by from/to
        self.add_to_index(&edges_by_from, &edge.from.to_bytes(), &key)?;
        self.add_to_index(&edges_by_to, &edge.to.to_bytes(), &key)?;

        // Log event
        let event = StateEvent::new(agent, Operation::Link, Target::Edge(edge.id))
            .with_after(serde_json::to_value(&edge).unwrap());
        self.log_event(event)?;

        Ok(edge)
    }

    fn get_edge(&self, id: EdgeId) -> Result<Option<StateEdge>> {
        let edges = self.edges_tree()?;
        let key = id.to_bytes();

        match edges.get(key)? {
            Some(bytes) => Ok(Some(Self::deserialize(&bytes)?)),
            None => Ok(None),
        }
    }

    fn delete_edge(&self, id: EdgeId, agent: AgentId) -> Result<()> {
        let edges = self.edges_tree()?;
        let edges_by_from = self.edges_by_from_tree()?;
        let edges_by_to = self.edges_by_to_tree()?;
        let key = id.to_bytes();

        let old_edge: StateEdge = edges
            .get(&key)?
            .map(|bytes| Self::deserialize(&bytes))
            .transpose()?
            .ok_or(StoreError::EdgeNotFound(id))?;

        // Remove from indices
        self.remove_from_index(&edges_by_from, &old_edge.from.to_bytes(), &key)?;
        self.remove_from_index(&edges_by_to, &old_edge.to.to_bytes(), &key)?;

        edges.remove(&key)?;

        // Log event
        let event = StateEvent::new(agent, Operation::Unlink, Target::Edge(id))
            .with_before(serde_json::to_value(&old_edge).unwrap());
        self.log_event(event)?;

        Ok(())
    }

    fn edges_from(&self, node_id: NodeId) -> Result<Vec<StateEdge>> {
        let edges = self.edges_tree()?;
        let edges_by_from = self.edges_by_from_tree()?;

        let ids: Vec<Vec<u8>> = edges_by_from
            .get(node_id.to_bytes())?
            .map(|v| Self::deserialize(&v))
            .transpose()?
            .unwrap_or_default();

        ids.into_iter()
            .filter_map(|id| edges.get(&id).ok().flatten())
            .map(|bytes| Self::deserialize(&bytes))
            .collect()
    }

    fn edges_to(&self, node_id: NodeId) -> Result<Vec<StateEdge>> {
        let edges = self.edges_tree()?;
        let edges_by_to = self.edges_by_to_tree()?;

        let ids: Vec<Vec<u8>> = edges_by_to
            .get(node_id.to_bytes())?
            .map(|v| Self::deserialize(&v))
            .transpose()?
            .unwrap_or_default();

        ids.into_iter()
            .filter_map(|id| edges.get(&id).ok().flatten())
            .map(|bytes| Self::deserialize(&bytes))
            .collect()
    }

    fn get_events(
        &self,
        since: Option<chrono::DateTime<chrono::Utc>>,
        limit: usize,
    ) -> Result<Vec<StateEvent>> {
        let events = self.events_tree()?;

        let iter = events.iter().rev(); // Newest first (ULID is time-sortable)

        let events: Vec<StateEvent> = iter
            .filter_map(|r| r.ok())
            .map(|(_, bytes)| Self::deserialize(&bytes))
            .filter_map(|r| r.ok())
            .filter(|e: &StateEvent| {
                since.map(|s| e.timestamp >= s).unwrap_or(true)
            })
            .take(limit)
            .collect();

        Ok(events)
    }

    fn search(&self, query: &str, kinds: Option<Vec<NodeKind>>) -> Result<Vec<StateNode>> {
        let nodes = self.nodes_tree()?;
        let query_lower = query.to_lowercase();

        let results: Vec<StateNode> = nodes
            .iter()
            .filter_map(|r| r.ok())
            .map(|(_, bytes)| Self::deserialize::<StateNode>(&bytes))
            .filter_map(|r| r.ok())
            .filter(|node| {
                // Filter by kind if specified
                if let Some(ref ks) = kinds {
                    if !ks.contains(&node.kind) {
                        return false;
                    }
                }
                // Simple text search in content
                node.content.to_string().to_lowercase().contains(&query_lower)
            })
            .collect();

        Ok(results)
    }

    fn neighbors(&self, id: NodeId, depth: usize) -> Result<Vec<StateNode>> {
        if depth == 0 {
            return Ok(vec![]);
        }

        let mut visited = std::collections::HashSet::new();
        let mut result = Vec::new();
        let mut current_level = vec![id];

        for _ in 0..depth {
            let mut next_level = Vec::new();

            for node_id in current_level {
                if visited.contains(&node_id) {
                    continue;
                }
                visited.insert(node_id);

                // Get outgoing edges
                for edge in self.edges_from(node_id)? {
                    if !visited.contains(&edge.to) {
                        if let Some(node) = self.get_node(edge.to)? {
                            result.push(node);
                            next_level.push(edge.to);
                        }
                    }
                }

                // Get incoming edges
                for edge in self.edges_to(node_id)? {
                    if !visited.contains(&edge.from) {
                        if let Some(node) = self.get_node(edge.from)? {
                            result.push(node);
                            next_level.push(edge.from);
                        }
                    }
                }
            }

            current_level = next_level;
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_node_crud() {
        let store = SledStore::open_temporary().unwrap();

        let node = StateNode::new(
            NodeKind::Project,
            serde_json::json!({"name": "test"}),
        );
        let id = node.id;

        // Create
        let created = store.create_node(node, AgentId::User).unwrap();
        assert_eq!(created.id, id);

        // Read
        let retrieved = store.get_node(id).unwrap().unwrap();
        assert_eq!(retrieved.id, id);

        // Update
        let updated = store
            .update_node(id, serde_json::json!({"name": "updated"}), AgentId::User)
            .unwrap();
        assert_eq!(updated.content["name"], "updated");

        // Delete
        store.delete_node(id, AgentId::User).unwrap();
        assert!(store.get_node(id).unwrap().is_none());
    }

    #[test]
    fn test_edge_operations() {
        let store = SledStore::open_temporary().unwrap();

        let node1 = store
            .create_node(
                StateNode::new(NodeKind::Project, serde_json::json!({"name": "A"})),
                AgentId::User,
            )
            .unwrap();
        let node2 = store
            .create_node(
                StateNode::new(NodeKind::Task, serde_json::json!({"name": "B"})),
                AgentId::User,
            )
            .unwrap();

        let edge = StateEdge::new(node1.id, node2.id, EdgeKind::PartOf);
        let created = store.create_edge(edge, AgentId::User).unwrap();

        let edges_from = store.edges_from(node1.id).unwrap();
        assert_eq!(edges_from.len(), 1);
        assert_eq!(edges_from[0].id, created.id);

        let edges_to = store.edges_to(node2.id).unwrap();
        assert_eq!(edges_to.len(), 1);
    }

    #[test]
    fn test_event_logging() {
        let store = SledStore::open_temporary().unwrap();

        let node = StateNode::new(NodeKind::Insight, serde_json::json!({"text": "hello"}));
        store.create_node(node, AgentId::Claude).unwrap();

        let events = store.get_events(None, 10).unwrap();
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].agent, AgentId::Claude);
        assert_eq!(events[0].operation, Operation::Create);
    }
}
