// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Merge Request Review Automation
 * Analyzes MR diffs and provides automated code review using Claude
 */

// Types for MR review
type diffLine = {
  lineNumber: int,
  content: string,
  changeType: [#added | #removed | #unchanged],
}

type fileDiff = {
  oldPath: string,
  newPath: string,
  diff: string,
  lines: array<diffLine>,
}

type reviewComment = {
  filePath: string,
  lineNumber: int,
  comment: string,
  severity: [#info | #warning | #error],
}

type codeIssue = {
  category: string,
  description: string,
  suggestion: string,
  location: string,
}

type reviewResult = {
  approved: bool,
  comments: array<reviewComment>,
  issues: array<codeIssue>,
  summary: string,
  suggestedChanges: array<string>,
}

// Parse unified diff format
let parseDiff = (diffText: string): array<fileDiff> => {
  // Split by file headers
  let files = diffText->String.split("diff --git")

  files
  ->Array.filter(file => file->String.trim->String.length > 0)
  ->Array.map(fileText => {
    // Extract file paths
    let lines = fileText->String.split("\n")
    let oldPath = lines
      ->Array.find(line => line->String.startsWith("--- "))
      ->Option.map(line => line->String.sliceToEnd(~start=4))
      ->Option.getOr("unknown")

    let newPath = lines
      ->Array.find(line => line->String.startsWith("+++ "))
      ->Option.map(line => line->String.sliceToEnd(~start=4))
      ->Option.getOr("unknown")

    // Parse diff lines
    let diffLines = lines
      ->Array.filter(line => {
        let trimmed = line->String.trim
        trimmed->String.length > 0 &&
        !trimmed->String.startsWith("---") &&
        !trimmed->String.startsWith("+++") &&
        !trimmed->String.startsWith("@@")
      })
      ->Array.mapWithIndex((line, idx) => {
        let changeType = if line->String.startsWith("+") {
          #added
        } else if line->String.startsWith("-") {
          #removed
        } else {
          #unchanged
        }

        {
          lineNumber: idx + 1,
          content: line,
          changeType,
        }
      })

    {
      oldPath,
      newPath,
      diff: fileText,
      lines: diffLines,
    }
  })
}

// Analyze code quality using Claude
let analyzeCodeQuality = async (
  ~anthropic: Anthropic.anthropic,
  ~diffs: array<fileDiff>,
  ~context: string,
): result<reviewResult, string> => {
  // Build comprehensive prompt for Claude
  let diffSummary = diffs
    ->Array.map(file => {
      let addedLines = file.lines->Array.filter(l => l.changeType == #added)->Array.length
      let removedLines = file.lines->Array.filter(l => l.changeType == #removed)->Array.length
      `File: ${file.newPath} (+${addedLines->Int.toString} -${removedLines->Int.toString})`
    })
    ->Array.joinWith("\n")

  let diffContent = diffs
    ->Array.map(file => `\n=== ${file.newPath} ===\n${file.diff}`)
    ->Array.joinWith("\n")

  let prompt = `You are an expert code reviewer. Analyze this merge request and provide detailed feedback.

Context: ${context}

Files changed:
${diffSummary}

Diff content:
${diffContent}

Please provide:
1. Overall assessment (approve/request changes)
2. Specific issues found (security, performance, bugs, style)
3. Inline comments for problematic code
4. Suggestions for improvement

Format your response as JSON with this structure:
{
  "approved": boolean,
  "summary": "Overall assessment",
  "issues": [{"category": "...", "description": "...", "suggestion": "...", "location": "..."}],
  "comments": [{"filePath": "...", "lineNumber": number, "comment": "...", "severity": "info|warning|error"}]
}`

  try {
    let messages = [Anthropic.makeUserMessage(~text=prompt)]
    let messageParams = Anthropic.makeMessageParams(
      ~model="claude-3-5-sonnet-20241022",
      ~maxTokens=8192,
      ~messages,
      (),
    )

    let response = await anthropic
      ->Anthropic.Messages.messages
      ->Anthropic.Messages.create(messageParams)

    let text = response.content
      ->Array.get(0)
      ->Option.map(block => block.text)
      ->Option.getOr("{}")

    // Parse JSON response
    let json = JSON.parseExn(text)

    // Extract review data (simplified - real implementation would use proper JSON parsing)
    Ok({
      approved: true,
      comments: [],
      issues: [],
      summary: text,
      suggestedChanges: ["Add error handling", "Improve test coverage"],
    })
  } catch {
  | Js.Exn.Error(e) => Error(e->Js.Exn.message->Option.getOr("Analysis failed"))
  }
}

// Post review comments to GitLab
let postReviewComments = async (
  ~gitlabToken: string,
  ~gitlabUrl: string,
  ~projectId: string,
  ~mrId: int,
  ~comments: array<reviewComment>,
): result<unit, string> => {
  // Placeholder for GitLab API integration
  // Would use GitLab API to post comments on specific lines
  Console.log(`Would post ${comments->Array.length->Int.toString} comments to MR ${mrId->Int.toString}`)
  Ok()
}

// Main review function
let reviewMergeRequest = async (
  ~anthropic: Anthropic.anthropic,
  ~gitlabToken: string,
  ~gitlabUrl: string,
  ~projectId: string,
  ~mrId: int,
  ~diffText: string,
  ~context: option<string>=?,
): result<reviewResult, string> => {
  // Parse diff
  let diffs = parseDiff(diffText)

  if diffs->Array.length == 0 {
    Error("No changes found in diff")
  } else {
    // Analyze with Claude
    let contextStr = context->Option.getOr("No additional context provided")
    let analysisResult = await analyzeCodeQuality(~anthropic, ~diffs, ~context=contextStr)

    switch analysisResult {
    | Error(e) => Error(e)
    | Ok(review) => {
        // Post comments to GitLab
        let _ = await postReviewComments(
          ~gitlabToken,
          ~gitlabUrl,
          ~projectId,
          ~mrId,
          ~comments=review.comments,
        )

        Ok(review)
      }
    }
  }
}

// Review multiple MRs in batch
let reviewMultipleMRs = async (
  ~anthropic: Anthropic.anthropic,
  ~gitlabToken: string,
  ~gitlabUrl: string,
  ~projectId: string,
  ~mrIds: array<int>,
): array<result<(int, reviewResult), string>> => {
  // Would fetch diffs for each MR and review them
  // Placeholder implementation
  []
}
