# Policy Schema for elegant-STATE
# Defines what project policies look like

let PolicyLevel = [| 'Tier1, 'Tier2, 'Forbidden, 'Exception |] in

let Language = {
  name | String,
  level | PolicyLevel,
  exceptions | Array String | default = [],
} in

let ToolchainRule = {
  tool | String,
  requires | String,
} in

let EnforcementConfig = {
  slm_weight | Number | default = 1.5,
  escalate_threshold | Number | default = 0.4,
  block_threshold | Number | default = 0.7,
} in

let Policy = {
  # Project identification
  project | {
    name | String,
    description | String,
    repository | String | optional,
  },

  # Language policy
  languages | {
    allowed | Array Language,
    forbidden | Array String,
  },

  # Toolchain constraints
  toolchain | {
    forbidden_without_alternative | Array ToolchainRule | default = [],
  },

  # Style rules
  style | {
    max_readme_meta_percentage | Number | default = 20,
    require_project_description_first | Bool | default = true,
  } | optional,

  # Enforcement tuning (for Conative Gating integration)
  enforcement | EnforcementConfig | default = {},
} in

# Export the schema types
{
  Policy,
  Language,
  ToolchainRule,
  EnforcementConfig,
  PolicyLevel,
}
