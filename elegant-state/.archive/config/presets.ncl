# Environment presets for elegant-STATE

let base = import "main.ncl" in

{
  # Development preset - permissive, verbose
  dev = base.default & {
    env = "dev",

    database.path = "/tmp/elegant-state-dev/db",

    server = {
      host = "127.0.0.1",
      port = 4000,
      workers = 2,
      cors.enabled = true,
      cors.origins = ["*"],
      tls.enabled = false,
    },

    agents = {
      default_mode = "direct",
      allow_runtime_changes = true,
      overrides = {
        user = { mode = "direct", can_vote = true, vote_weight = 1.0 },
        claude = { mode = "direct", can_vote = true, vote_weight = 1.0 },
        llama = { mode = "direct", can_vote = true, vote_weight = 1.0 },
        system = { mode = "direct", can_vote = false, vote_weight = 0.0 },
      },
    },

    voting = {
      strategy = "first_vote",
      min_voters = 1,
      timeout_seconds = 60,
    },

    logging = {
      level = "debug",
      format = "pretty",
      json = false,
    },

    graphql = {
      playground = true,
      introspection = true,
      subscriptions.enabled = true,
    },
  },

  # Staging preset - balanced
  staging = base.default & {
    env = "staging",

    database.path = "~/.local/share/elegant-state-staging/db",

    server = {
      host = "0.0.0.0",
      port = 4000,
      workers = 4,
      cors.enabled = true,
      cors.origins = ["https://staging.example.com"],
      tls.enabled = true,
    },

    agents = {
      default_mode = "proposal",
      allow_runtime_changes = true,
      overrides = {
        user = { mode = "direct", can_vote = true, vote_weight = 1.0 },
        claude = { mode = "proposal", can_vote = true, vote_weight = 0.8 },
        llama = { mode = "proposal", can_vote = true, vote_weight = 0.5 },
        system = { mode = "direct", can_vote = false, vote_weight = 0.0 },
      },
    },

    voting = {
      strategy = "simple_majority",
      min_voters = 2,
      timeout_seconds = 1800,
    },

    logging = {
      level = "info",
      format = "compact",
      json = true,
    },

    graphql = {
      playground = true,
      introspection = true,
      subscriptions.enabled = true,
    },
  },

  # Production preset - strict, secure
  prod = base.default & {
    env = "prod",

    database = {
      path = "/var/lib/elegant-state/db",
      backup_dir = "/var/backups/elegant-state",
      compact_threshold_mb = 500,
      max_size_gb = 100,
    },

    server = {
      host = "0.0.0.0",
      port = 443,
      workers = 8,
      timeout_ms = 10000,
      cors.enabled = true,
      cors.origins = ["https://app.example.com"],
      cors.methods = ["GET", "POST"],
      tls.enabled = true,
    },

    agents = {
      default_mode = "proposal",
      allow_runtime_changes = false,
      overrides = {
        user = { mode = "direct", can_vote = true, vote_weight = 1.0 },
        claude = { mode = "proposal", can_vote = true, vote_weight = 0.8 },
        llama = { mode = "proposal", can_vote = true, vote_weight = 0.5 },
        system = { mode = "observer", can_vote = false, vote_weight = 0.0 },
      },
    },

    voting = {
      strategy = "supermajority",
      min_voters = 3,
      timeout_seconds = 86400,
    },

    reputation = {
      enabled = true,
      decay_interval_hours = 168,
      weight_in_voting = 0.7,
    },

    logging = {
      level = "warn",
      format = "json",
      file = "/var/log/elegant-state/app.log",
      json = true,
    },

    graphql = {
      playground = false,
      introspection = false,
      depth_limit = 5,
      complexity_limit = 500,
      subscriptions.enabled = true,
    },
  },

  # Minimal preset - for embedded/constrained environments
  minimal = base.default & {
    env = "prod",

    database = {
      path = "./state.db",
      max_size_gb = 1,
    },

    server = {
      host = "127.0.0.1",
      port = 4000,
      workers = 1,
      timeout_ms = 60000,
    },

    agents.default_mode = "direct",

    search = {
      fulltext.enabled = false,
      fuzzy.enabled = true,
    },

    tools = {
      pandoc.enabled = false,
      tesseract.enabled = false,
    },

    graphql = {
      playground = false,
      introspection = false,
      subscriptions.enabled = false,
    },
  },

  # NeuroPhone preset - for Termux/mobile
  neurophone = base.default & {
    env = "prod",

    database = {
      path = "/data/data/com.termux/files/home/.elegant-state/db",
      max_size_gb = 2,
    },

    server = {
      host = "127.0.0.1",
      port = 4000,
      workers = 2,
    },

    agents = {
      default_mode = "proposal",
      overrides = {
        user = { mode = "direct", can_vote = true, vote_weight = 1.0 },
        llama = { mode = "proposal", can_vote = true, vote_weight = 0.7 },
      },
    },

    voting = {
      strategy = "simple_majority",
      min_voters = 1,
    },

    search = {
      fulltext.heap_size_mb = 20,
    },

    logging.level = "info",
  },
}
