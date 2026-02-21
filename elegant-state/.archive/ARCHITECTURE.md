# elegant-STATE: Architecture

## Tech Stack Decision

**Note**: CubDB is Elixir-native. For pure Rust, we use **sled** (similar embedded KV store, Rust-native).

| Component | Choice | Rationale |
|-----------|--------|-----------|
| Embedded DB | sled | Rust-native, ACID, embedded, no server |
| GraphQL | async-graphql | Best Rust GraphQL, async-native |
| CLI | clap | Standard Rust CLI |
| Serialization | serde + bincode | Fast binary, serde ecosystem |
| Async runtime | tokio | Industry standard |
| Packaging | Guix | Reproducible, declarative |

## MVP Scope

```
v0.1.0 - Core State Graph
├── StateNode: vertices in the knowledge graph
├── StateEdge: relationships between nodes
├── StateEvent: append-only changelog
├── GraphQL API: query/mutate the graph
├── CLI: state-cli for local interaction
└── Guix: reproducible package
```

## Core Schema

```rust
// Identity
type NodeId = Ulid;      // Universally unique, sortable
type EdgeId = Ulid;
type EventId = Ulid;

// The fundamental unit of state
struct StateNode {
    id: NodeId,
    kind: NodeKind,           // Conversation, Project, Insight, Task, etc.
    content: Value,           // serde_json::Value for flexibility
    metadata: Metadata,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

enum NodeKind {
    Conversation,  // A Claude conversation
    Project,       // A project grouping
    Insight,       // An extracted insight
    Task,          // A todo/task
    Context,       // Contextual information
    Custom(String),
}

// Relationships between nodes
struct StateEdge {
    id: EdgeId,
    from: NodeId,
    to: NodeId,
    kind: EdgeKind,
    weight: f32,              // Relevance/strength
    metadata: Metadata,
    created_at: DateTime<Utc>,
}

enum EdgeKind {
    References,    // A references B
    DerivedFrom,   // A was derived from B
    RelatedTo,     // A is related to B
    PartOf,        // A is part of B
    Blocks,        // A blocks B
    Enables,       // A enables B
    Custom(String),
}

// Every mutation is logged
struct StateEvent {
    id: EventId,
    timestamp: DateTime<Utc>,
    agent: AgentId,           // Who made this change
    operation: Operation,
    target: Target,           // Node or Edge affected
    before: Option<Value>,    // Previous state (for undo)
    after: Option<Value>,     // New state
}

enum Operation {
    Create,
    Update,
    Delete,
    Link,
    Unlink,
}

enum AgentId {
    User,
    Claude,
    Llama,
    Module(String),
    System,
}
```

## Storage Layout (sled)

```
sled database
├── nodes/              # Tree: NodeId -> StateNode
├── edges/              # Tree: EdgeId -> StateEdge
├── edges_by_from/      # Index: NodeId -> Vec<EdgeId>
├── edges_by_to/        # Index: NodeId -> Vec<EdgeId>
├── events/             # Tree: EventId -> StateEvent (append-only)
├── nodes_by_kind/      # Index: NodeKind -> Vec<NodeId>
└── metadata/           # Tree: key -> value (config, schema version)
```

## GraphQL Schema

```graphql
type Query {
  node(id: ID!): StateNode
  nodes(kind: NodeKind, limit: Int): [StateNode!]!
  edges(from: ID, to: ID, kind: EdgeKind): [StateEdge!]!
  events(since: DateTime, agent: AgentId, limit: Int): [StateEvent!]!

  # Graph traversal
  neighbors(id: ID!, direction: Direction, depth: Int): [StateNode!]!
  path(from: ID!, to: ID!): [StateEdge!]

  # Search
  search(query: String!, kinds: [NodeKind!]): [StateNode!]!
}

type Mutation {
  createNode(kind: NodeKind!, content: JSON!, metadata: JSON): StateNode!
  updateNode(id: ID!, content: JSON, metadata: JSON): StateNode!
  deleteNode(id: ID!): Boolean!

  createEdge(from: ID!, to: ID!, kind: EdgeKind!, weight: Float): StateEdge!
  deleteEdge(id: ID!): Boolean!

  # Batch operations
  importNodes(nodes: [NodeInput!]!): [StateNode!]!

  # For proposal mode (v0.2)
  # propose(mutation: MutationInput!): Proposal!
}

type Subscription {
  nodeChanged(kinds: [NodeKind!]): StateEvent!
  eventStream(agents: [AgentId!]): StateEvent!
}
```

## CLI Interface

```bash
# Node operations
state-cli node create --kind project --content '{"name": "NeuroPhone"}'
state-cli node list --kind conversation --limit 10
state-cli node get <node-id>
state-cli node update <node-id> --content '{"status": "active"}'
state-cli node delete <node-id>

# Edge operations
state-cli edge create --from <id> --to <id> --kind references
state-cli edge list --from <id>
state-cli edge delete <edge-id>

# Search
state-cli search "NeuroPhone" --kinds project,insight

# Events
state-cli events --since "1 hour ago" --agent claude
state-cli events replay --from <event-id>  # Replay for debugging

# Server
state-cli serve --port 4000  # Start GraphQL server
state-cli serve --socket /tmp/state.sock  # Unix socket for local

# Import/Export
state-cli export --format json > state-backup.json
state-cli import < state-backup.json
```

## Directory Structure

```
elegant-STATE/
├── Cargo.toml
├── src/
│   ├── main.rs           # CLI entrypoint
│   ├── lib.rs            # Library root
│   ├── schema/
│   │   ├── mod.rs
│   │   ├── node.rs       # StateNode
│   │   ├── edge.rs       # StateEdge
│   │   └── event.rs      # StateEvent
│   ├── store/
│   │   ├── mod.rs
│   │   ├── sled.rs       # sled backend
│   │   └── indices.rs    # Secondary indices
│   ├── graphql/
│   │   ├── mod.rs
│   │   ├── query.rs
│   │   ├── mutation.rs
│   │   └── subscription.rs
│   ├── cli/
│   │   ├── mod.rs
│   │   ├── node.rs
│   │   ├── edge.rs
│   │   └── serve.rs
│   └── event/
│       ├── mod.rs
│       └── sourcing.rs   # Event replay, undo
├── guix/
│   └── elegant-state.scm # Guix package
├── tests/
│   ├── integration/
│   └── fixtures/
└── docs/
    └── ARCHITECTURE.md
```

## MVP Feature Matrix

| Feature | v0.1 | v0.2 | v0.3 |
|---------|------|------|------|
| Node CRUD | ✅ | ✅ | ✅ |
| Edge CRUD | ✅ | ✅ | ✅ |
| Event logging | ✅ | ✅ | ✅ |
| GraphQL API | ✅ | ✅ | ✅ |
| CLI | ✅ | ✅ | ✅ |
| Guix package | ✅ | ✅ | ✅ |
| Search | Basic | Full-text | Semantic |
| Proposal mode | ❌ | ✅ | ✅ |
| Voting system | ❌ | ✅ | ✅ |
| Reputation | ❌ | ❌ | ✅ |
| Subscriptions | ❌ | ✅ | ✅ |
| GUI | ❌ | ❌ | TUI |

## Build Order for v0.1

1. **Schema types** - Define StateNode, StateEdge, StateEvent
2. **Store layer** - sled backend with indices
3. **Event sourcing** - Append-only event log
4. **GraphQL** - Query and Mutation resolvers
5. **CLI** - clap-based interface
6. **Guix** - Package definition

Let's start coding.
