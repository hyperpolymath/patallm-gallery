// SPDX-License-Identifier: MPL-2.0
//! Async verification support for network-based evidence checks.
//!
//! This module provides async variants of the verifier for checking
//! network-based evidence like HTTP endpoints, API responses, etc.
//!
//! Enable with the `async` feature:
//! ```toml
//! [dependencies]
//! did-you-actually-do-that = { version = "0.1", features = ["async"] }
//! ```

use crate::Verdict;
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::time::Duration;

/// Network-specific evidence types for async verification
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "spec")]
pub enum NetworkEvidenceSpec {
    /// HTTP endpoint should return a successful status code (2xx)
    HttpReachable {
        url: String,
        #[serde(default = "default_timeout")]
        timeout_secs: u64,
    },

    /// HTTP endpoint should return a specific status code
    HttpStatus {
        url: String,
        expected_status: u16,
        #[serde(default = "default_timeout")]
        timeout_secs: u64,
    },

    /// HTTP response body should contain a substring
    HttpBodyContains {
        url: String,
        substring: String,
        #[serde(default = "default_timeout")]
        timeout_secs: u64,
    },

    /// HTTP response should match a JSON path query
    HttpJsonPath {
        url: String,
        /// JSONPath expression (simplified: supports `.field` and `[index]`)
        path: String,
        expected_value: serde_json::Value,
        #[serde(default = "default_timeout")]
        timeout_secs: u64,
    },

    /// TCP port should be reachable
    TcpReachable {
        host: String,
        port: u16,
        #[serde(default = "default_timeout")]
        timeout_secs: u64,
    },
}

fn default_timeout() -> u64 {
    30
}

/// Result of verifying network evidence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkEvidenceResult {
    pub spec: NetworkEvidenceSpec,
    pub verdict: Verdict,
    pub details: Option<String>,
    pub response_time_ms: Option<u64>,
}

/// Async verifier for network-based evidence
pub struct AsyncVerifier {
    #[allow(dead_code)]
    client: Client,
}

impl Default for AsyncVerifier {
    fn default() -> Self {
        Self::new()
    }
}

impl AsyncVerifier {
    pub fn new() -> Self {
        Self {
            client: Client::builder()
                .timeout(Duration::from_secs(30))
                .build()
                .expect("Failed to create HTTP client"),
        }
    }

    /// Create with custom HTTP client
    pub fn with_client(client: Client) -> Self {
        Self { client }
    }

    /// Check a single piece of network evidence
    pub async fn check_network_evidence(
        &self,
        evidence: &NetworkEvidenceSpec,
    ) -> NetworkEvidenceResult {
        let start = std::time::Instant::now();

        let (verdict, details) = match evidence {
            NetworkEvidenceSpec::HttpReachable { url, timeout_secs } => {
                self.check_http_reachable(url, *timeout_secs).await
            }

            NetworkEvidenceSpec::HttpStatus {
                url,
                expected_status,
                timeout_secs,
            } => {
                self.check_http_status(url, *expected_status, *timeout_secs)
                    .await
            }

            NetworkEvidenceSpec::HttpBodyContains {
                url,
                substring,
                timeout_secs,
            } => {
                self.check_http_body_contains(url, substring, *timeout_secs)
                    .await
            }

            NetworkEvidenceSpec::HttpJsonPath {
                url,
                path,
                expected_value,
                timeout_secs,
            } => {
                self.check_http_json_path(url, path, expected_value, *timeout_secs)
                    .await
            }

            NetworkEvidenceSpec::TcpReachable {
                host,
                port,
                timeout_secs,
            } => self.check_tcp_reachable(host, *port, *timeout_secs).await,
        };

        let response_time_ms = Some(start.elapsed().as_millis() as u64);

        NetworkEvidenceResult {
            spec: evidence.clone(),
            verdict,
            details,
            response_time_ms,
        }
    }

    async fn check_http_reachable(
        &self,
        url: &str,
        timeout_secs: u64,
    ) -> (Verdict, Option<String>) {
        let client = Client::builder()
            .timeout(Duration::from_secs(timeout_secs))
            .build();

        let client = match client {
            Ok(c) => c,
            Err(e) => return (Verdict::Unverifiable, Some(format!("Client error: {}", e))),
        };

        match client.get(url).send().await {
            Ok(resp) => {
                if resp.status().is_success() {
                    (
                        Verdict::Confirmed,
                        Some(format!("HTTP {} - {}", resp.status().as_u16(), url)),
                    )
                } else {
                    (
                        Verdict::Refuted,
                        Some(format!(
                            "HTTP {} (not 2xx) - {}",
                            resp.status().as_u16(),
                            url
                        )),
                    )
                }
            }
            Err(e) => {
                if e.is_timeout() {
                    (
                        Verdict::Refuted,
                        Some(format!("Timeout after {}s - {}", timeout_secs, url)),
                    )
                } else if e.is_connect() {
                    (
                        Verdict::Refuted,
                        Some(format!("Connection failed - {}", url)),
                    )
                } else {
                    (Verdict::Refuted, Some(format!("Request error: {}", e)))
                }
            }
        }
    }

    async fn check_http_status(
        &self,
        url: &str,
        expected_status: u16,
        timeout_secs: u64,
    ) -> (Verdict, Option<String>) {
        let client = Client::builder()
            .timeout(Duration::from_secs(timeout_secs))
            .build();

        let client = match client {
            Ok(c) => c,
            Err(e) => return (Verdict::Unverifiable, Some(format!("Client error: {}", e))),
        };

        match client.get(url).send().await {
            Ok(resp) => {
                let actual = resp.status().as_u16();
                if actual == expected_status {
                    (
                        Verdict::Confirmed,
                        Some(format!("HTTP {} matches expected", actual)),
                    )
                } else {
                    (
                        Verdict::Refuted,
                        Some(format!("HTTP {} (expected {})", actual, expected_status)),
                    )
                }
            }
            Err(e) => (Verdict::Refuted, Some(format!("Request error: {}", e))),
        }
    }

    async fn check_http_body_contains(
        &self,
        url: &str,
        substring: &str,
        timeout_secs: u64,
    ) -> (Verdict, Option<String>) {
        let client = Client::builder()
            .timeout(Duration::from_secs(timeout_secs))
            .build();

        let client = match client {
            Ok(c) => c,
            Err(e) => return (Verdict::Unverifiable, Some(format!("Client error: {}", e))),
        };

        match client.get(url).send().await {
            Ok(resp) => match resp.text().await {
                Ok(body) => {
                    if body.contains(substring) {
                        (
                            Verdict::Confirmed,
                            Some("Substring found in response".to_string()),
                        )
                    } else {
                        (
                            Verdict::Refuted,
                            Some("Substring not found in response".to_string()),
                        )
                    }
                }
                Err(e) => (
                    Verdict::Refuted,
                    Some(format!("Failed to read body: {}", e)),
                ),
            },
            Err(e) => (Verdict::Refuted, Some(format!("Request error: {}", e))),
        }
    }

    async fn check_http_json_path(
        &self,
        url: &str,
        path: &str,
        expected_value: &serde_json::Value,
        timeout_secs: u64,
    ) -> (Verdict, Option<String>) {
        let client = Client::builder()
            .timeout(Duration::from_secs(timeout_secs))
            .build();

        let client = match client {
            Ok(c) => c,
            Err(e) => return (Verdict::Unverifiable, Some(format!("Client error: {}", e))),
        };

        match client.get(url).send().await {
            Ok(resp) => match resp.json::<serde_json::Value>().await {
                Ok(json) => {
                    // Simple JSONPath implementation (supports .field and [index])
                    let actual = extract_json_path(&json, path);
                    match actual {
                        Some(value) => {
                            if value == expected_value {
                                (
                                    Verdict::Confirmed,
                                    Some(format!("JSON path {} matches", path)),
                                )
                            } else {
                                (
                                    Verdict::Refuted,
                                    Some(format!(
                                        "JSON path {} = {:?} (expected {:?})",
                                        path, value, expected_value
                                    )),
                                )
                            }
                        }
                        None => (
                            Verdict::Refuted,
                            Some(format!("JSON path {} not found", path)),
                        ),
                    }
                }
                Err(e) => (
                    Verdict::Refuted,
                    Some(format!("Failed to parse JSON: {}", e)),
                ),
            },
            Err(e) => (Verdict::Refuted, Some(format!("Request error: {}", e))),
        }
    }

    async fn check_tcp_reachable(
        &self,
        host: &str,
        port: u16,
        timeout_secs: u64,
    ) -> (Verdict, Option<String>) {
        use tokio::net::TcpStream;
        use tokio::time::timeout;

        let addr = format!("{}:{}", host, port);
        let duration = Duration::from_secs(timeout_secs);

        match timeout(duration, TcpStream::connect(&addr)).await {
            Ok(Ok(_)) => (
                Verdict::Confirmed,
                Some(format!("TCP connection to {} succeeded", addr)),
            ),
            Ok(Err(e)) => (
                Verdict::Refuted,
                Some(format!("TCP connection to {} failed: {}", addr, e)),
            ),
            Err(_) => (
                Verdict::Refuted,
                Some(format!("TCP connection to {} timed out", addr)),
            ),
        }
    }

    /// Verify all network evidence for a claim
    pub async fn verify_network_evidence(
        &self,
        evidence: &[NetworkEvidenceSpec],
    ) -> Vec<NetworkEvidenceResult> {
        let mut results = Vec::new();
        for e in evidence {
            results.push(self.check_network_evidence(e).await);
        }
        results
    }
}

/// Simple JSON path extraction
/// Supports paths like ".field", ".nested.field", "[0]", ".array[0].field"
fn extract_json_path<'a>(json: &'a serde_json::Value, path: &str) -> Option<&'a serde_json::Value> {
    let mut current = json;

    for segment in path.split('.').filter(|s| !s.is_empty()) {
        // Check for array index
        if let Some(bracket_pos) = segment.find('[') {
            let field_name = &segment[..bracket_pos];
            if !field_name.is_empty() {
                current = current.get(field_name)?;
            }

            // Extract index
            let end_bracket = segment.find(']')?;
            let index_str = &segment[bracket_pos + 1..end_bracket];
            let index: usize = index_str.parse().ok()?;
            current = current.get(index)?;
        } else {
            current = current.get(segment)?;
        }
    }

    Some(current)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_json_path_simple() {
        let json: serde_json::Value = serde_json::json!({
            "name": "test",
            "value": 42
        });

        assert_eq!(
            extract_json_path(&json, ".name"),
            Some(&serde_json::json!("test"))
        );
        assert_eq!(
            extract_json_path(&json, ".value"),
            Some(&serde_json::json!(42))
        );
        assert_eq!(extract_json_path(&json, ".missing"), None);
    }

    #[test]
    fn test_extract_json_path_nested() {
        let json: serde_json::Value = serde_json::json!({
            "outer": {
                "inner": {
                    "value": "found"
                }
            }
        });

        assert_eq!(
            extract_json_path(&json, ".outer.inner.value"),
            Some(&serde_json::json!("found"))
        );
    }

    #[test]
    fn test_extract_json_path_array() {
        let json: serde_json::Value = serde_json::json!({
            "items": ["a", "b", "c"]
        });

        assert_eq!(
            extract_json_path(&json, ".items[0]"),
            Some(&serde_json::json!("a"))
        );
        assert_eq!(
            extract_json_path(&json, ".items[2]"),
            Some(&serde_json::json!("c"))
        );
    }
}
