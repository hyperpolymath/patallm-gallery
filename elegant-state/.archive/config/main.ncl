# elegant-STATE Configuration
# Nickel multipermutation config with full combinatorics

let types = import "types.ncl" in
let presets = import "presets.ncl" in
let validators = import "validators.ncl" in

# ══════════════════════════════════════════════════════════════════════════════
# BASE CONFIGURATION SCHEMA
# ══════════════════════════════════════════════════════════════════════════════

let Schema = {
  # Environment configuration
  env
    | types.Environment
    | default = "dev",

  # Database configuration
  database = {
    path
      | String
      | default = "~/.local/share/elegant-state/db",

    backup_dir
      | String
      | default = "~/.local/share/elegant-state/backups",

    compact_threshold_mb
      | Number
      | default = 100,

    max_size_gb
      | Number
      | default = 10,
  },

  # Server configuration
  server = {
    host
      | String
      | default = "127.0.0.1",

    port
      | Number
      | validators.ValidPort
      | default = 4000,

    workers
      | Number
      | default = 4,

    timeout_ms
      | Number
      | default = 30000,

    cors = {
      enabled
        | Bool
        | default = true,

      origins
        | Array String
        | default = ["*"],

      methods
        | Array String
        | default = ["GET", "POST", "OPTIONS"],
    },

    tls = {
      enabled
        | Bool
        | default = false,

      cert_path
        | String
        | optional,

      key_path
        | String
        | optional,
    },
  },

  # Agent configuration
  agents = {
    default_mode
      | types.CapabilityMode
      | default = "proposal",

    allow_runtime_changes
      | Bool
      | default = true,

    # Per-agent overrides
    overrides
      | { _ : types.AgentConfig }
      | default = {
          user = {
            mode = "direct",
            can_vote = true,
            vote_weight = 1.0,
          },
          claude = {
            mode = "proposal",
            can_vote = true,
            vote_weight = 1.0,
          },
          llama = {
            mode = "proposal",
            can_vote = true,
            vote_weight = 0.5,
          },
          system = {
            mode = "direct",
            can_vote = false,
            vote_weight = 0.0,
          },
        },
  },

  # Voting configuration
  voting = {
    strategy
      | types.VotingStrategy
      | default = "simple_majority",

    min_voters
      | Number
      | default = 1,

    timeout_seconds
      | Number
      | default = 3600,

    # For weighted strategy
    threshold
      | Number
      | default = 2.0,

    # For single_approver strategy
    approver
      | String
      | optional,
  },

  # Reputation configuration
  reputation = {
    enabled
      | Bool
      | default = true,

    initial_score
      | Number
      | default = 0.5,

    decay_factor
      | Number
      | default = 0.99,

    decay_interval_hours
      | Number
      | default = 24,

    weight_in_voting
      | Number
      | default = 0.5,
  },

  # Search configuration
  search = {
    fulltext = {
      enabled
        | Bool
        | default = true,

      index_path
        | String
        | default = "~/.local/share/elegant-state/index",

      heap_size_mb
        | Number
        | default = 50,
    },

    fuzzy = {
      enabled
        | Bool
        | default = true,

      algorithm
        | types.FuzzyAlgorithm
        | default = "skim",

      max_distance
        | Number
        | default = 2,
    },
  },

  # External tools configuration
  tools = {
    pandoc = {
      enabled
        | Bool
        | default = true,

      path
        | String
        | default = "pandoc",

      default_from
        | String
        | default = "markdown",

      default_to
        | String
        | default = "plain",
    },

    tesseract = {
      enabled
        | Bool
        | default = true,

      path
        | String
        | default = "tesseract",

      language
        | String
        | default = "eng",

      engine_mode
        | types.OcrEngineMode
        | default = "combined",
    },
  },

  # Logging configuration
  logging = {
    level
      | types.LogLevel
      | default = "info",

    format
      | types.LogFormat
      | default = "pretty",

    file
      | String
      | optional,

    json
      | Bool
      | default = false,
  },

  # GraphQL configuration
  graphql = {
    playground
      | Bool
      | default = true,

    introspection
      | Bool
      | default = true,

    depth_limit
      | Number
      | default = 10,

    complexity_limit
      | Number
      | default = 1000,

    subscriptions = {
      enabled
        | Bool
        | default = true,

      channel_capacity
        | Number
        | default = 256,
    },
  },
} in

# ══════════════════════════════════════════════════════════════════════════════
# CONFIGURATION MERGING
# ══════════════════════════════════════════════════════════════════════════════

let merge_config = fun base override =>
  std.record.merge_all [base, override]
in

# ══════════════════════════════════════════════════════════════════════════════
# PERMUTATION GENERATORS
# ══════════════════════════════════════════════════════════════════════════════

let environments = ["dev", "staging", "prod"] in
let modes = ["direct", "proposal", "observer"] in
let strategies = ["unanimous", "simple_majority", "supermajority", "weighted", "first_vote"] in
let log_levels = ["trace", "debug", "info", "warn", "error"] in

let generate_permutation = fun env mode strategy =>
  merge_config Schema {
    env = env,
    agents.default_mode = mode,
    voting.strategy = strategy,
    logging.level =
      if env == "dev" then "debug"
      else if env == "staging" then "info"
      else "warn",
    server.cors.enabled = env != "prod",
    graphql.playground = env != "prod",
    graphql.introspection = env != "prod",
  }
in

# ══════════════════════════════════════════════════════════════════════════════
# EXPORTS
# ══════════════════════════════════════════════════════════════════════════════

{
  # Default configuration
  default = Schema,

  # Environment presets
  dev = presets.dev,
  staging = presets.staging,
  prod = presets.prod,

  # Generate specific permutation
  permutation = generate_permutation,

  # All permutations (for matrix generation)
  matrix = {
    environments = environments,
    modes = modes,
    strategies = strategies,
    log_levels = log_levels,
  },

  # Schema for validation
  schema = Schema,
}
