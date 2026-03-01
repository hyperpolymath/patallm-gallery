use async_graphql::{Context, Object, Result, ID};
use crate::store::{SledStore, Store};
use crate::schema::{
    self as domain,
    AgentId, NodeId, EdgeId,
};
use super::types::{StateNode, StateEdge, CreateNodeInput, UpdateNodeInput, CreateEdgeInput, AgentKind};
use std::sync::Arc;
use ulid::Ulid;

pub struct MutationRoot;

#[Object]
impl MutationRoot {
    /// Create a new node
    async fn create_node(
        &self,
        ctx: &Context<'_>,
        input: CreateNodeInput,
        #[graphql(default_with = "AgentKind::User")] agent: AgentKind,
    ) -> Result<StateNode> {
        let store = ctx.data::<Arc<SledStore>>()?;

        let mut node = domain::StateNode::new(input.kind.into(), input.content.0);
        if let Some(meta) = input.metadata {
            if let Ok(map) = serde_json::from_value(meta.0) {
                node = node.with_metadata(map);
            }
        }

        let created = store.create_node(node, agent.into())?;
        Ok(created.into())
    }

    /// Update an existing node
    async fn update_node(
        &self,
        ctx: &Context<'_>,
        input: UpdateNodeInput,
        #[graphql(default_with = "AgentKind::User")] agent: AgentKind,
    ) -> Result<StateNode> {
        let store = ctx.data::<Arc<SledStore>>()?;
        let node_id: NodeId = input.id.parse::<Ulid>().map_err(|e| format!("Invalid ID: {}", e))?;

        let updated = store.update_node(node_id, input.content.0, agent.into())?;
        Ok(updated.into())
    }

    /// Delete a node
    async fn delete_node(
        &self,
        ctx: &Context<'_>,
        id: ID,
        #[graphql(default_with = "AgentKind::User")] agent: AgentKind,
    ) -> Result<bool> {
        let store = ctx.data::<Arc<SledStore>>()?;
        let node_id: NodeId = id.parse::<Ulid>().map_err(|e| format!("Invalid ID: {}", e))?;

        store.delete_node(node_id, agent.into())?;
        Ok(true)
    }

    /// Create a new edge between nodes
    async fn create_edge(
        &self,
        ctx: &Context<'_>,
        input: CreateEdgeInput,
        #[graphql(default_with = "AgentKind::User")] agent: AgentKind,
    ) -> Result<StateEdge> {
        let store = ctx.data::<Arc<SledStore>>()?;

        let from_id: NodeId = input.from.parse::<Ulid>().map_err(|e| format!("Invalid from ID: {}", e))?;
        let to_id: NodeId = input.to.parse::<Ulid>().map_err(|e| format!("Invalid to ID: {}", e))?;

        let mut edge = domain::StateEdge::new(from_id, to_id, input.kind.into());
        if let Some(w) = input.weight {
            edge = edge.with_weight(w);
        }

        let created = store.create_edge(edge, agent.into())?;
        Ok(created.into())
    }

    /// Delete an edge
    async fn delete_edge(
        &self,
        ctx: &Context<'_>,
        id: ID,
        #[graphql(default_with = "AgentKind::User")] agent: AgentKind,
    ) -> Result<bool> {
        let store = ctx.data::<Arc<SledStore>>()?;
        let edge_id: EdgeId = id.parse::<Ulid>().map_err(|e| format!("Invalid ID: {}", e))?;

        store.delete_edge(edge_id, agent.into())?;
        Ok(true)
    }
}
