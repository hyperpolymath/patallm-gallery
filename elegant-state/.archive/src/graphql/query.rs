use async_graphql::{Context, Object, Result, ID};
use crate::store::{SledStore, Store};
use crate::schema::{NodeId, NodeKind as DomainNodeKind};
use super::types::{StateNode, StateEdge, StateEvent, NodeKind, EdgeKind};
use std::sync::Arc;
use ulid::Ulid;

pub struct QueryRoot;

#[Object]
impl QueryRoot {
    /// Get a node by ID
    async fn node(&self, ctx: &Context<'_>, id: ID) -> Result<Option<StateNode>> {
        let store = ctx.data::<Arc<SledStore>>()?;
        let node_id: NodeId = id.parse::<Ulid>().map_err(|e| format!("Invalid ID: {}", e))?;
        Ok(store.get_node(node_id)?.map(Into::into))
    }

    /// List nodes, optionally filtered by kind
    async fn nodes(
        &self,
        ctx: &Context<'_>,
        kind: Option<NodeKind>,
        #[graphql(default = 100)] limit: i32,
    ) -> Result<Vec<StateNode>> {
        let store = ctx.data::<Arc<SledStore>>()?;
        let domain_kind: Option<DomainNodeKind> = kind.map(Into::into);
        Ok(store
            .list_nodes(domain_kind, limit as usize)?
            .into_iter()
            .map(Into::into)
            .collect())
    }

    /// Get edges, optionally filtered by from, to, or kind
    async fn edges(
        &self,
        ctx: &Context<'_>,
        from: Option<ID>,
        to: Option<ID>,
        _kind: Option<EdgeKind>,
    ) -> Result<Vec<StateEdge>> {
        let store = ctx.data::<Arc<SledStore>>()?;

        let edges = if let Some(from_id) = from {
            let node_id: NodeId = from_id.parse::<Ulid>().map_err(|e| format!("Invalid ID: {}", e))?;
            store.edges_from(node_id)?
        } else if let Some(to_id) = to {
            let node_id: NodeId = to_id.parse::<Ulid>().map_err(|e| format!("Invalid ID: {}", e))?;
            store.edges_to(node_id)?
        } else {
            vec![] // Would need a list_edges method
        };

        Ok(edges.into_iter().map(Into::into).collect())
    }

    /// Get recent events
    async fn events(
        &self,
        ctx: &Context<'_>,
        #[graphql(default = 50)] limit: i32,
    ) -> Result<Vec<StateEvent>> {
        let store = ctx.data::<Arc<SledStore>>()?;
        Ok(store
            .get_events(None, limit as usize)?
            .into_iter()
            .map(Into::into)
            .collect())
    }

    /// Get neighbors of a node up to a certain depth
    async fn neighbors(
        &self,
        ctx: &Context<'_>,
        id: ID,
        #[graphql(default = 1)] depth: i32,
    ) -> Result<Vec<StateNode>> {
        let store = ctx.data::<Arc<SledStore>>()?;
        let node_id: NodeId = id.parse::<Ulid>().map_err(|e| format!("Invalid ID: {}", e))?;
        Ok(store
            .neighbors(node_id, depth as usize)?
            .into_iter()
            .map(Into::into)
            .collect())
    }

    /// Search nodes by content
    async fn search(
        &self,
        ctx: &Context<'_>,
        query: String,
        kinds: Option<Vec<NodeKind>>,
    ) -> Result<Vec<StateNode>> {
        let store = ctx.data::<Arc<SledStore>>()?;
        let domain_kinds: Option<Vec<DomainNodeKind>> =
            kinds.map(|ks| ks.into_iter().map(Into::into).collect());
        Ok(store
            .search(&query, domain_kinds)?
            .into_iter()
            .map(Into::into)
            .collect())
    }
}
