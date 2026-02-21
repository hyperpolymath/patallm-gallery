# Example Policy - hyperpolymath ecosystem
# Import the schema
let schema = import "../schema/policy.ncl" in

{
  project = {
    name = "hyperpolymath-standard",
    description = "Default policy for hyperpolymath projects",
    repository = "https://github.com/hyperpolymath",
  },

  languages = {
    allowed = [
      { name = "rust", level = 'Tier1 },
      { name = "elixir", level = 'Tier1 },
      { name = "zig", level = 'Tier1 },
      { name = "ada", level = 'Tier1 },
      { name = "haskell", level = 'Tier2 },
      { name = "rescript", level = 'Tier2 },
      { name = "racket", level = 'Tier2 },
      { name = "nickel", level = 'Tier2 },
      { name = "python", level = 'Exception, exceptions = ["salt/**", "training/**"] },
    ],
    forbidden = ["typescript", "go", "java", "c#"],
  },

  toolchain = {
    forbidden_without_alternative = [
      { tool = "npm", requires = "deno.json" },
      { tool = "pip", requires = "salt context or training script" },
    ],
  },

  style = {
    max_readme_meta_percentage = 20,
    require_project_description_first = true,
  },

  enforcement = {
    slm_weight = 1.5,
    escalate_threshold = 0.4,
    block_threshold = 0.7,
  },
} : schema.Policy
