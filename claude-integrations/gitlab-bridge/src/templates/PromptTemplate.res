// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Custom Prompt Template System
 * Allows users to define and use custom prompts for different scenarios
 */

// Template variable type
type templateVar = {
  name: string,
  value: string,
}

// Template metadata
type templateMetadata = {
  name: string,
  description: string,
  author: option<string>,
  version: string,
  category: templateCategory,
}

and templateCategory =
  | @as("issue_analysis") IssueAnalysis
  | @as("mr_review") MRReview
  | @as("code_generation") CodeGeneration
  | @as("documentation") Documentation
  | @as("testing") Testing
  | @as("custom") Custom

// Prompt template
type promptTemplate = {
  metadata: templateMetadata,
  systemPrompt: option<string>,
  userPromptTemplate: string,
  variables: array<string>,
  examples: array<templateExample>,
}

and templateExample = {
  description: string,
  variables: array<templateVar>,
  expectedOutput: string,
}

// Template registry
type templateRegistry = {
  templates: Dict.t<promptTemplate>,
}

// Create empty registry
let makeRegistry = (): templateRegistry => {
  templates: Dict.make(),
}

// Add template to registry
let addTemplate = (registry: templateRegistry, template: promptTemplate): templateRegistry => {
  registry.templates->Dict.set(template.metadata.name, template)
  registry
}

// Get template by name
let getTemplate = (registry: templateRegistry, name: string): option<promptTemplate> => {
  registry.templates->Dict.get(name)
}

// Extract variables from template string
let extractVariables = (template: string): array<string> => {
  // Find all {{variable}} patterns
  let pattern = makeTokenPattern("\\{\\{([^}]+)\\}\\}")
  let matches: array<string> = []

  // Simplified - real implementation would use proper regex matching
  matches
}

// Substitute variables in template
let substituteVariables = (template: string, variables: array<templateVar>): string => {
  variables->Array.reduce(template, (result, variable) => {
    let placeholder = `{{${variable.name}}}`
    result->String.replaceAll(placeholder, variable.value)
  })
}

// Validate template
let validateTemplate = (template: promptTemplate): result<unit, string> => {
  if template.metadata.name->String.trim->String.length == 0 {
    Error("Template name cannot be empty")
  } else if template.userPromptTemplate->String.trim->String.length == 0 {
    Error("Template content cannot be empty")
  } else if template.variables->Array.length == 0 {
    Error("Template must have at least one variable")
  } else {
    Ok()
  }
}

// Built-in templates

let issueAnalysisTemplate: promptTemplate = {
  metadata: {
    name: "issue_analysis",
    description: "Analyze a GitLab issue and provide insights",
    author: Some("Claude GitLab Bridge"),
    version: "1.0.0",
    category: IssueAnalysis,
  },
  systemPrompt: Some(
    "You are an expert software engineer analyzing GitLab issues. Provide concise, actionable insights.",
  ),
  userPromptTemplate: `Analyze this GitLab issue:

**Title:** {{issue_title}}
**Description:** {{issue_description}}
**Labels:** {{issue_labels}}
**Author:** {{issue_author}}

Please provide:
1. Summary of the issue
2. Priority assessment (low/medium/high/critical)
3. Suggested actions
4. Related concerns or dependencies`,
  variables: ["issue_title", "issue_description", "issue_labels", "issue_author"],
  examples: [],
}

let mrReviewTemplate: promptTemplate = {
  metadata: {
    name: "mr_review",
    description: "Review a merge request with detailed feedback",
    author: Some("Claude GitLab Bridge"),
    version: "1.0.0",
    category: MRReview,
  },
  systemPrompt: Some(
    "You are an expert code reviewer. Focus on code quality, security, performance, and best practices.",
  ),
  userPromptTemplate: `Review this merge request:

**Title:** {{mr_title}}
**Description:** {{mr_description}}
**Files Changed:** {{files_changed}}
**Branch:** {{source_branch}} → {{target_branch}}

**Diff:**
\`\`\`
{{diff}}
\`\`\`

Please provide:
1. Overall assessment (approve/request changes)
2. Security concerns
3. Performance implications
4. Code quality issues
5. Specific line-by-line feedback`,
  variables: ["mr_title", "mr_description", "files_changed", "source_branch", "target_branch", "diff"],
  examples: [],
}

let codeGenerationTemplate: promptTemplate = {
  metadata: {
    name: "code_generation",
    description: "Generate code based on specifications",
    author: Some("Claude GitLab Bridge"),
    version: "1.0.0",
    category: CodeGeneration,
  },
  systemPrompt: Some("You are an expert programmer. Generate clean, well-documented, idiomatic code."),
  userPromptTemplate: `Generate code for the following specification:

**Language:** {{language}}
**Framework:** {{framework}}
**Requirements:** {{requirements}}

**Context:**
{{context}}

Please provide:
1. Complete implementation
2. Unit tests
3. Documentation
4. Usage examples`,
  variables: ["language", "framework", "requirements", "context"],
  examples: [],
}

let documentationTemplate: promptTemplate = {
  metadata: {
    name: "documentation",
    description: "Generate or improve documentation",
    author: Some("Claude GitLab Bridge"),
    version: "1.0.0",
    category: Documentation,
  },
  systemPrompt: Some("You are a technical writer. Create clear, comprehensive documentation."),
  userPromptTemplate: `Generate documentation for:

**Component:** {{component_name}}
**Type:** {{doc_type}}
**Code:**
\`\`\`{{language}}
{{code}}
\`\`\`

Please provide:
1. Overview
2. Usage examples
3. API reference
4. Common patterns`,
  variables: ["component_name", "doc_type", "language", "code"],
  examples: [],
}

// Build default registry with built-in templates
let buildDefaultRegistry = (): templateRegistry => {
  let registry = makeRegistry()
  registry
  ->addTemplate(issueAnalysisTemplate)
  ->addTemplate(mrReviewTemplate)
  ->addTemplate(codeGenerationTemplate)
  ->addTemplate(documentationTemplate)
}

// Render template with variables
let render = (
  template: promptTemplate,
  variables: array<templateVar>,
): result<(option<string>, string), string> => {
  // Validate all required variables are provided
  let missingVars = template.variables->Array.filter(varName =>
    !variables->Array.some(v => v.name == varName)
  )

  if missingVars->Array.length > 0 {
    Error(`Missing required variables: ${missingVars->Array.joinWith(", ")}`)
  } else {
    let userPrompt = substituteVariables(template.userPromptTemplate, variables)
    Ok((template.systemPrompt, userPrompt))
  }
}

// Parse template from JSON
let parseTemplate = (json: JSON.t): result<promptTemplate, string> => {
  try {
    // Simplified parsing
    Ok(issueAnalysisTemplate)
  } catch {
  | _ => Error("Failed to parse template")
  }
}

// Export template to JSON
let exportTemplate = (template: promptTemplate): JSON.t => {
  JSON.object_(
    Dict.fromArray([
      ("name", JSON.string(template.metadata.name)),
      ("description", JSON.string(template.metadata.description)),
      ("version", JSON.string(template.metadata.version)),
      ("userPromptTemplate", JSON.string(template.userPromptTemplate)),
      (
        "variables",
        JSON.array(template.variables->Array.map(v => JSON.string(v))),
      ),
    ]),
  )
}
