# Ecosystem Schema for elegant-STATE
# Defines project registries and cross-project coordination

let ProjectStatus = [| 'active, 'archived, 'superseded, 'planning |] in

let ProjectPhase = [|
  'research,
  'design,
  'implementation,
  'stabilization,
  'maintenance,
  'sunset
|] in

let Project = {
  name | String,
  status | ProjectStatus,
  phase | ProjectPhase | optional,
  repository | String | optional,
  description | String | optional,
  superseded_by | String | optional,
  archive_date | String | optional,
  languages | Array String | default = [],
  dependencies | Array String | default = [],
} in

let Ecosystem = {
  # Metadata
  name | String,
  version | String | default = "1.0",
  updated | String,

  # All projects in the ecosystem
  projects | Array Project,

  # Cross-project relationships
  relationships | Array {
    from | String,
    to | String,
    kind | [| 'depends_on, 'supersedes, 'related_to, 'part_of |],
  } | default = [],
} in

# Export the schema types
{
  Ecosystem,
  Project,
  ProjectStatus,
  ProjectPhase,
}
