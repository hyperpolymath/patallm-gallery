# Type definitions for elegant-STATE configuration

{
  Environment = std.contract.from_predicate (fun x =>
    std.array.elem x ["dev", "staging", "prod"]
  ),

  CapabilityMode = std.contract.from_predicate (fun x =>
    std.array.elem x ["direct", "proposal", "observer"]
  ),

  VotingStrategy = std.contract.from_predicate (fun x =>
    std.array.elem x [
      "unanimous",
      "simple_majority",
      "supermajority",
      "weighted",
      "first_vote",
      "single_approver"
    ]
  ),

  LogLevel = std.contract.from_predicate (fun x =>
    std.array.elem x ["trace", "debug", "info", "warn", "error"]
  ),

  LogFormat = std.contract.from_predicate (fun x =>
    std.array.elem x ["pretty", "compact", "json"]
  ),

  FuzzyAlgorithm = std.contract.from_predicate (fun x =>
    std.array.elem x ["skim", "nucleo", "agrep"]
  ),

  OcrEngineMode = std.contract.from_predicate (fun x =>
    std.array.elem x ["legacy", "lstm", "combined", "default"]
  ),

  NodeKind = std.contract.from_predicate (fun x =>
    std.array.elem x [
      "conversation",
      "project",
      "insight",
      "task",
      "context",
      "module",
      "agent"
    ]
  ),

  EdgeKind = std.contract.from_predicate (fun x =>
    std.array.elem x [
      "references",
      "derived_from",
      "related_to",
      "part_of",
      "blocks",
      "enables",
      "supersedes"
    ]
  ),

  AgentConfig = {
    mode
      | CapabilityMode
      | default = "proposal",
    can_vote
      | Bool
      | default = true,
    vote_weight
      | Number
      | default = 1.0,
  },
}
