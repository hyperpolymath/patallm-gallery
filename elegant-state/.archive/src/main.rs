use anyhow::Result;
use clap::Parser;
use elegant_state::{
    build_schema, NodeKind, StateEdge, StateNode, SledStore, Store, AgentId, EdgeKind,
};
use std::sync::Arc;

mod cli;
use cli::{Cli, Commands, NodeCommands, EdgeCommands, ServeCommands};

fn expand_path(path: &str) -> String {
    if path.starts_with("~/") {
        if let Some(home) = dirs::home_dir() {
            return path.replacen("~", &home.to_string_lossy(), 1);
        }
    }
    path.to_string()
}

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt::init();

    let cli = Cli::parse();
    let db_path = expand_path(&cli.db_path);

    // Ensure parent directory exists
    if let Some(parent) = std::path::Path::new(&db_path).parent() {
        std::fs::create_dir_all(parent)?;
    }

    let store = Arc::new(SledStore::open(&db_path)?);

    match cli.command {
        Commands::Node { command } => handle_node_command(command, &store)?,
        Commands::Edge { command } => handle_edge_command(command, &store)?,
        Commands::Search { query, kinds } => {
            let kind_list: Option<Vec<NodeKind>> = kinds.map(|k| {
                k.split(',')
                    .filter_map(|s| s.trim().parse().ok())
                    .collect()
            });
            let results = store.search(&query, kind_list)?;
            for node in results {
                println!("{}", serde_json::to_string_pretty(&node)?);
            }
        }
        Commands::Events { limit, agent: _ } => {
            let events = store.get_events(None, limit)?;
            for event in events {
                println!(
                    "[{}] {} {:?} by {}",
                    event.timestamp.format("%Y-%m-%d %H:%M:%S"),
                    format!("{:?}", event.operation),
                    event.target,
                    event.agent
                );
            }
        }
        Commands::Export { format: _ } => {
            let nodes = store.list_nodes(None, usize::MAX)?;
            let export = serde_json::json!({
                "version": "0.1.0",
                "nodes": nodes,
            });
            println!("{}", serde_json::to_string_pretty(&export)?);
        }
        Commands::Import { file } => {
            let content = std::fs::read_to_string(&file)?;
            let import: serde_json::Value = serde_json::from_str(&content)?;
            if let Some(nodes) = import.get("nodes").and_then(|n| n.as_array()) {
                for node_value in nodes {
                    let node: StateNode = serde_json::from_value(node_value.clone())?;
                    store.create_node(node, AgentId::System)?;
                }
                println!("Imported {} nodes", nodes.len());
            }
        }
        Commands::Serve { command } => handle_serve_command(command, store).await?,
    }

    Ok(())
}

fn handle_node_command(command: NodeCommands, store: &Arc<SledStore>) -> Result<()> {
    match command {
        NodeCommands::Create { kind, content, metadata } => {
            let kind: NodeKind = kind.parse().map_err(|e: String| anyhow::anyhow!(e))?;
            let content: serde_json::Value = serde_json::from_str(&content)?;
            let mut node = StateNode::new(kind, content);
            if let Some(meta) = metadata {
                let meta_map = serde_json::from_str(&meta)?;
                node = node.with_metadata(meta_map);
            }
            let created = store.create_node(node, AgentId::User)?;
            println!("Created node: {}", created.id);
            println!("{}", serde_json::to_string_pretty(&created)?);
        }
        NodeCommands::Get { id } => {
            let node_id = id.parse().map_err(|e| anyhow::anyhow!("Invalid ID: {}", e))?;
            match store.get_node(node_id)? {
                Some(node) => println!("{}", serde_json::to_string_pretty(&node)?),
                None => println!("Node not found"),
            }
        }
        NodeCommands::List { kind, limit } => {
            let kind: Option<NodeKind> = kind
                .map(|k| k.parse().map_err(|e: String| anyhow::anyhow!(e)))
                .transpose()?;
            let nodes = store.list_nodes(kind, limit)?;
            for node in nodes {
                println!("{} [{}] {:?}", node.id, node.kind, node.content);
            }
        }
        NodeCommands::Update { id, content } => {
            let node_id = id.parse().map_err(|e| anyhow::anyhow!("Invalid ID: {}", e))?;
            let content: serde_json::Value = serde_json::from_str(&content)?;
            let updated = store.update_node(node_id, content, AgentId::User)?;
            println!("Updated node: {}", updated.id);
        }
        NodeCommands::Delete { id, force } => {
            if !force {
                print!("Are you sure you want to delete node {}? [y/N] ", id);
                std::io::Write::flush(&mut std::io::stdout())?;
                let mut input = String::new();
                std::io::stdin().read_line(&mut input)?;
                if !input.trim().eq_ignore_ascii_case("y") {
                    println!("Aborted");
                    return Ok(());
                }
            }
            let node_id = id.parse().map_err(|e| anyhow::anyhow!("Invalid ID: {}", e))?;
            store.delete_node(node_id, AgentId::User)?;
            println!("Deleted node: {}", id);
        }
    }
    Ok(())
}

fn handle_edge_command(command: EdgeCommands, store: &Arc<SledStore>) -> Result<()> {
    match command {
        EdgeCommands::Create { from, to, kind, weight } => {
            let from_id = from.parse().map_err(|e| anyhow::anyhow!("Invalid from ID: {}", e))?;
            let to_id = to.parse().map_err(|e| anyhow::anyhow!("Invalid to ID: {}", e))?;
            let kind: EdgeKind = kind.parse().map_err(|e: String| anyhow::anyhow!(e))?;
            let mut edge = StateEdge::new(from_id, to_id, kind);
            if let Some(w) = weight {
                edge = edge.with_weight(w);
            }
            let created = store.create_edge(edge, AgentId::User)?;
            println!("Created edge: {}", created.id);
        }
        EdgeCommands::From { id } => {
            let node_id = id.parse().map_err(|e| anyhow::anyhow!("Invalid ID: {}", e))?;
            let edges = store.edges_from(node_id)?;
            for edge in edges {
                println!("{} --[{}]--> {}", edge.from, edge.kind, edge.to);
            }
        }
        EdgeCommands::To { id } => {
            let node_id = id.parse().map_err(|e| anyhow::anyhow!("Invalid ID: {}", e))?;
            let edges = store.edges_to(node_id)?;
            for edge in edges {
                println!("{} --[{}]--> {}", edge.from, edge.kind, edge.to);
            }
        }
        EdgeCommands::Delete { id } => {
            let edge_id = id.parse().map_err(|e| anyhow::anyhow!("Invalid ID: {}", e))?;
            store.delete_edge(edge_id, AgentId::User)?;
            println!("Deleted edge: {}", id);
        }
    }
    Ok(())
}

async fn handle_serve_command(command: ServeCommands, store: Arc<SledStore>) -> Result<()> {
    match command {
        ServeCommands::Http { port, host } => {
            use async_graphql_axum::{GraphQLRequest, GraphQLResponse};
            use axum::{routing::post, Extension, Router};

            let schema = build_schema(store);

            async fn graphql_handler(
                Extension(schema): Extension<elegant_state::StateSchema>,
                req: GraphQLRequest,
            ) -> GraphQLResponse {
                schema.execute(req.into_inner()).await.into()
            }

            let app = Router::new()
                .route("/graphql", post(graphql_handler))
                .layer(Extension(schema));

            let addr = format!("{}:{}", host, port);
            println!("GraphQL server running at https://{}/graphql", addr);

            let listener = tokio::net::TcpListener::bind(&addr).await?;
            axum::serve(listener, app).await?;
        }
    }
    Ok(())
}
