//! Fuzzy matching support (agrep-like functionality)
//!
//! Provides approximate string matching for node search.

use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use nucleo_matcher::{Config, Matcher, Utf32Str};
use nucleo_matcher::pattern::{Pattern, CaseMatching, Normalization};

/// Fuzzy search engine with multiple matching strategies
pub struct FuzzySearch {
    skim_matcher: SkimMatcherV2,
}

impl Default for FuzzySearch {
    fn default() -> Self {
        Self::new()
    }
}

impl FuzzySearch {
    /// Create a new fuzzy search engine
    pub fn new() -> Self {
        Self {
            skim_matcher: SkimMatcherV2::default(),
        }
    }

    /// Match using skim algorithm (fzf-like)
    /// Returns score if matched, None otherwise
    pub fn skim_match(&self, pattern: &str, text: &str) -> Option<i64> {
        self.skim_matcher.fuzzy_match(text, pattern)
    }

    /// Match using nucleo (faster, used by helix editor)
    /// Returns score if matched, None otherwise
    pub fn nucleo_match(&self, pattern: &str, text: &str) -> Option<u32> {
        let mut matcher = Matcher::new(Config::DEFAULT);

        let pat = Pattern::parse(pattern, CaseMatching::Smart, Normalization::Smart);

        let mut buf = Vec::new();
        let haystack = Utf32Str::new(text, &mut buf);

        pat.score(haystack, &mut matcher)
    }

    /// Agrep-style approximate matching with edit distance
    /// max_errors: maximum number of allowed errors (insertions, deletions, substitutions)
    pub fn agrep_match(&self, pattern: &str, text: &str, max_errors: usize) -> bool {
        if pattern.is_empty() {
            return true;
        }
        if text.is_empty() {
            return pattern.len() <= max_errors;
        }

        // Use simple Levenshtein-based window matching
        let pattern_chars: Vec<char> = pattern.chars().collect();
        let text_chars: Vec<char> = text.chars().collect();

        // Sliding window approach
        for start in 0..=text_chars.len().saturating_sub(pattern_chars.len().saturating_sub(max_errors)) {
            let end = (start + pattern_chars.len() + max_errors).min(text_chars.len());
            let window: String = text_chars[start..end].iter().collect();

            if levenshtein(&pattern_chars, &window.chars().collect::<Vec<_>>()) <= max_errors {
                return true;
            }
        }

        false
    }

    /// Search a list of items and return sorted by relevance
    pub fn search<'a, T, F>(&self, pattern: &str, items: &'a [T], extract: F) -> Vec<(&'a T, i64)>
    where
        F: Fn(&T) -> &str,
    {
        let mut matches: Vec<(&T, i64)> = items
            .iter()
            .filter_map(|item| {
                let text = extract(item);
                self.skim_match(pattern, text).map(|score| (item, score))
            })
            .collect();

        matches.sort_by(|a, b| b.1.cmp(&a.1));
        matches
    }
}

/// Calculate Levenshtein edit distance
fn levenshtein(a: &[char], b: &[char]) -> usize {
    let m = a.len();
    let n = b.len();

    if m == 0 {
        return n;
    }
    if n == 0 {
        return m;
    }

    let mut prev: Vec<usize> = (0..=n).collect();
    let mut curr = vec![0; n + 1];

    for (i, ca) in a.iter().enumerate() {
        curr[0] = i + 1;

        for (j, cb) in b.iter().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            curr[j + 1] = (prev[j + 1] + 1)
                .min(curr[j] + 1)
                .min(prev[j] + cost);
        }

        std::mem::swap(&mut prev, &mut curr);
    }

    prev[n]
}

/// Match result with position information
#[derive(Debug, Clone)]
pub struct FuzzyMatch {
    pub score: i64,
    pub positions: Vec<usize>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skim_match() {
        let fuzzy = FuzzySearch::new();

        assert!(fuzzy.skim_match("rust", "rust programming language").is_some());
        assert!(fuzzy.skim_match("rpl", "rust programming language").is_some());
        assert!(fuzzy.skim_match("xyz", "rust programming language").is_none());
    }

    #[test]
    fn test_nucleo_match() {
        let fuzzy = FuzzySearch::new();

        assert!(fuzzy.nucleo_match("rust", "rust programming").is_some());
        assert!(fuzzy.nucleo_match("rp", "rust programming").is_some());
    }

    #[test]
    fn test_agrep_match() {
        let fuzzy = FuzzySearch::new();

        // Exact match
        assert!(fuzzy.agrep_match("hello", "hello world", 0));

        // One error allowed
        assert!(fuzzy.agrep_match("helo", "hello world", 1));
        assert!(fuzzy.agrep_match("helloo", "hello world", 1));

        // Two errors
        assert!(fuzzy.agrep_match("hllo", "hello world", 2));

        // Too many errors
        assert!(!fuzzy.agrep_match("xyz", "hello world", 1));
    }

    #[test]
    fn test_search_ranking() {
        let fuzzy = FuzzySearch::new();
        let items = vec![
            "rust programming",
            "trust in rust",
            "javascript",
            "rustacean",
        ];

        let results = fuzzy.search("rust", &items, |s| s);

        assert!(!results.is_empty());
        // "rust programming" or "rustacean" should be top matches
        assert!(results[0].0.contains("rust"));
    }

    #[test]
    fn test_levenshtein() {
        assert_eq!(levenshtein(&['h', 'e', 'l', 'l', 'o'], &['h', 'e', 'l', 'l', 'o']), 0);
        assert_eq!(levenshtein(&['h', 'e', 'l', 'o'], &['h', 'e', 'l', 'l', 'o']), 1);
        assert_eq!(levenshtein(&['k', 'i', 't', 't', 'e', 'n'], &['s', 'i', 't', 't', 'i', 'n', 'g']), 3);
    }
}
