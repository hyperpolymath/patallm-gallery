//! Pandoc document conversion support
//!
//! Provides ability to convert various document formats when ingesting
//! content into the state graph.

use std::process::{Command, Stdio};
use std::io::Write;
use crate::store::StoreError;

/// Supported input formats for conversion
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputFormat {
    Markdown,
    Html,
    Latex,
    Rst,
    Org,
    Docx,
    Epub,
    Asciidoc,
    MediaWiki,
    Textile,
    Json,      // Pandoc AST JSON
    Auto,      // Let pandoc detect
}

impl InputFormat {
    fn as_pandoc_arg(self) -> Option<&'static str> {
        match self {
            InputFormat::Markdown => Some("markdown"),
            InputFormat::Html => Some("html"),
            InputFormat::Latex => Some("latex"),
            InputFormat::Rst => Some("rst"),
            InputFormat::Org => Some("org"),
            InputFormat::Docx => Some("docx"),
            InputFormat::Epub => Some("epub"),
            InputFormat::Asciidoc => Some("asciidoc"),
            InputFormat::MediaWiki => Some("mediawiki"),
            InputFormat::Textile => Some("textile"),
            InputFormat::Json => Some("json"),
            InputFormat::Auto => None,
        }
    }
}

/// Supported output formats
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    Markdown,
    Html,
    Plain,
    Json,      // Pandoc AST JSON - useful for structured analysis
    Gfm,       // GitHub-flavored markdown
    CommonMark,
}

impl OutputFormat {
    fn as_pandoc_arg(self) -> &'static str {
        match self {
            OutputFormat::Markdown => "markdown",
            OutputFormat::Html => "html",
            OutputFormat::Plain => "plain",
            OutputFormat::Json => "json",
            OutputFormat::Gfm => "gfm",
            OutputFormat::CommonMark => "commonmark",
        }
    }
}

/// Pandoc-based document converter
pub struct PandocConverter {
    pandoc_path: String,
}

impl Default for PandocConverter {
    fn default() -> Self {
        Self::new()
    }
}

impl PandocConverter {
    /// Create a new converter using system pandoc
    pub fn new() -> Self {
        Self {
            pandoc_path: "pandoc".to_string(),
        }
    }

    /// Create with custom pandoc path
    pub fn with_path(path: impl Into<String>) -> Self {
        Self {
            pandoc_path: path.into(),
        }
    }

    /// Check if pandoc is available
    pub fn is_available(&self) -> bool {
        Command::new(&self.pandoc_path)
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|s| s.success())
    }

    /// Get pandoc version
    pub fn version(&self) -> Result<String, StoreError> {
        let output = Command::new(&self.pandoc_path)
            .arg("--version")
            .output()
            .map_err(|e| StoreError::Serialization(format!("pandoc not found: {e}")))?;

        if !output.status.success() {
            return Err(StoreError::Serialization("pandoc --version failed".into()));
        }

        let version = String::from_utf8_lossy(&output.stdout);
        Ok(version.lines().next().unwrap_or("unknown").to_string())
    }

    /// Convert content from one format to another
    pub fn convert(
        &self,
        content: &str,
        from: InputFormat,
        to: OutputFormat,
    ) -> Result<String, StoreError> {
        let mut cmd = Command::new(&self.pandoc_path);

        if let Some(from_arg) = from.as_pandoc_arg() {
            cmd.args(["-f", from_arg]);
        }

        cmd.args(["-t", to.as_pandoc_arg()]);
        cmd.stdin(Stdio::piped());
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());

        let mut child = cmd
            .spawn()
            .map_err(|e| StoreError::Serialization(format!("failed to run pandoc: {e}")))?;

        if let Some(mut stdin) = child.stdin.take() {
            stdin
                .write_all(content.as_bytes())
                .map_err(|e| StoreError::Serialization(format!("failed to write to pandoc: {e}")))?;
        }

        let output = child
            .wait_with_output()
            .map_err(|e| StoreError::Serialization(format!("pandoc failed: {e}")))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(StoreError::Serialization(format!("pandoc error: {stderr}")));
        }

        Ok(String::from_utf8_lossy(&output.stdout).into_owned())
    }

    /// Convert to plain text (useful for indexing)
    pub fn to_plain_text(&self, content: &str, from: InputFormat) -> Result<String, StoreError> {
        self.convert(content, from, OutputFormat::Plain)
    }

    /// Convert to markdown (normalized form)
    pub fn to_markdown(&self, content: &str, from: InputFormat) -> Result<String, StoreError> {
        self.convert(content, from, OutputFormat::Markdown)
    }

    /// Convert to Pandoc AST JSON (for structured analysis)
    pub fn to_ast_json(&self, content: &str, from: InputFormat) -> Result<serde_json::Value, StoreError> {
        let json_str = self.convert(content, from, OutputFormat::Json)?;
        serde_json::from_str(&json_str)
            .map_err(|e| StoreError::Serialization(format!("failed to parse pandoc JSON: {e}")))
    }

    /// Extract plain text for full-text indexing
    pub fn extract_text_for_indexing(
        &self,
        content: &str,
        from: InputFormat,
    ) -> Result<String, StoreError> {
        // Convert to plain text, stripping all formatting
        let plain = self.to_plain_text(content, from)?;

        // Normalize whitespace
        let normalized: String = plain
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");

        Ok(normalized)
    }
}

/// Helper to detect format from file extension
pub fn detect_format(path: &str) -> InputFormat {
    let ext = path.rsplit('.').next().unwrap_or("").to_lowercase();
    match ext.as_str() {
        "md" | "markdown" => InputFormat::Markdown,
        "html" | "htm" => InputFormat::Html,
        "tex" | "latex" => InputFormat::Latex,
        "rst" => InputFormat::Rst,
        "org" => InputFormat::Org,
        "docx" => InputFormat::Docx,
        "epub" => InputFormat::Epub,
        "adoc" | "asciidoc" => InputFormat::Asciidoc,
        "wiki" => InputFormat::MediaWiki,
        "textile" => InputFormat::Textile,
        "json" => InputFormat::Json,
        _ => InputFormat::Auto,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_format() {
        assert_eq!(detect_format("README.md"), InputFormat::Markdown);
        assert_eq!(detect_format("doc.html"), InputFormat::Html);
        assert_eq!(detect_format("paper.tex"), InputFormat::Latex);
        assert_eq!(detect_format("notes.org"), InputFormat::Org);
        assert_eq!(detect_format("unknown.xyz"), InputFormat::Auto);
    }

    #[test]
    fn test_pandoc_available() {
        let converter = PandocConverter::new();
        // This test passes if pandoc is installed, which is expected in the Guix env
        if converter.is_available() {
            let version = converter.version().unwrap();
            assert!(version.contains("pandoc"));
        }
    }

    #[test]
    fn test_markdown_to_plain() {
        let converter = PandocConverter::new();
        if !converter.is_available() {
            return; // Skip if pandoc not installed
        }

        let markdown = "# Hello\n\nThis is **bold** and *italic*.";
        let plain = converter.to_plain_text(markdown, InputFormat::Markdown).unwrap();

        assert!(plain.contains("Hello"));
        assert!(plain.contains("bold"));
        assert!(!plain.contains("**"));
    }

    #[test]
    fn test_html_to_markdown() {
        let converter = PandocConverter::new();
        if !converter.is_available() {
            return;
        }

        let html = "<h1>Title</h1><p>Paragraph with <strong>bold</strong>.</p>";
        let markdown = converter.to_markdown(html, InputFormat::Html).unwrap();

        assert!(markdown.contains("Title"));
        assert!(markdown.contains("**bold**") || markdown.contains("__bold__"));
    }
}
