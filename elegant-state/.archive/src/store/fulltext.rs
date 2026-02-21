//! Full-text search using tantivy
//!
//! Provides indexing and querying capabilities for StateNodes.

use std::path::Path;
use tantivy::{
    collector::TopDocs,
    directory::MmapDirectory,
    query::QueryParser,
    schema::{Schema, STORED, TEXT, STRING, Field},
    Index, IndexWriter, IndexReader, TantivyDocument,
};
use crate::schema::{NodeId, NodeKind, StateNode};
use crate::store::StoreError;

/// Full-text search index for StateNodes
pub struct FullTextIndex {
    index: Index,
    reader: IndexReader,
    schema: Schema,
    // Fields
    id_field: Field,
    kind_field: Field,
    content_field: Field,
    metadata_field: Field,
}

impl FullTextIndex {
    /// Create or open a full-text index at the given path
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self, StoreError> {
        let mut schema_builder = Schema::builder();

        let id_field = schema_builder.add_text_field("id", STRING | STORED);
        let kind_field = schema_builder.add_text_field("kind", STRING | STORED);
        let content_field = schema_builder.add_text_field("content", TEXT | STORED);
        let metadata_field = schema_builder.add_text_field("metadata", TEXT);

        let schema = schema_builder.build();

        // Ensure directory exists
        std::fs::create_dir_all(path.as_ref())
            .map_err(|e| StoreError::Serialization(e.to_string()))?;

        let dir = MmapDirectory::open(path)
            .map_err(|e| StoreError::Serialization(e.to_string()))?;

        let index = Index::open_or_create(dir, schema.clone())
            .map_err(|e| StoreError::Serialization(e.to_string()))?;

        let reader = index
            .reader()
            .map_err(|e| StoreError::Serialization(e.to_string()))?;

        Ok(Self {
            index,
            reader,
            schema,
            id_field,
            kind_field,
            content_field,
            metadata_field,
        })
    }

    /// Create an in-memory index for testing
    pub fn open_in_memory() -> Result<Self, StoreError> {
        let mut schema_builder = Schema::builder();

        let id_field = schema_builder.add_text_field("id", STRING | STORED);
        let kind_field = schema_builder.add_text_field("kind", STRING | STORED);
        let content_field = schema_builder.add_text_field("content", TEXT | STORED);
        let metadata_field = schema_builder.add_text_field("metadata", TEXT);

        let schema = schema_builder.build();

        let index = Index::create_in_ram(schema.clone());

        let reader = index
            .reader()
            .map_err(|e| StoreError::Serialization(e.to_string()))?;

        Ok(Self {
            index,
            reader,
            schema,
            id_field,
            kind_field,
            content_field,
            metadata_field,
        })
    }

    /// Get an index writer
    pub fn writer(&self, heap_size: usize) -> Result<IndexWriter, StoreError> {
        self.index
            .writer(heap_size)
            .map_err(|e| StoreError::Serialization(e.to_string()))
    }

    /// Index a node
    pub fn index_node(&self, writer: &IndexWriter, node: &StateNode) -> Result<(), StoreError> {
        let mut doc = TantivyDocument::new();
        doc.add_text(self.id_field, node.id.to_string());
        doc.add_text(self.kind_field, node.kind.to_string());
        doc.add_text(self.content_field, node.content.to_string());
        doc.add_text(
            self.metadata_field,
            serde_json::to_string(&node.metadata).unwrap_or_default(),
        );

        writer
            .add_document(doc)
            .map_err(|e| StoreError::Serialization(e.to_string()))?;

        Ok(())
    }

    /// Remove a node from the index
    pub fn remove_node(&self, writer: &IndexWriter, id: NodeId) -> Result<(), StoreError> {
        let term = tantivy::Term::from_field_text(self.id_field, &id.to_string());
        writer.delete_term(term);
        Ok(())
    }

    /// Search for nodes matching the query
    pub fn search(
        &self,
        query: &str,
        kinds: Option<&[NodeKind]>,
        limit: usize,
    ) -> Result<Vec<SearchResult>, StoreError> {
        self.reader
            .reload()
            .map_err(|e| StoreError::Serialization(e.to_string()))?;

        let searcher = self.reader.searcher();

        let query_parser = QueryParser::for_index(&self.index, vec![self.content_field]);
        let parsed_query = query_parser
            .parse_query(query)
            .map_err(|e| StoreError::Serialization(e.to_string()))?;

        let top_docs = searcher
            .search(&parsed_query, &TopDocs::with_limit(limit))
            .map_err(|e| StoreError::Serialization(e.to_string()))?;

        let mut results = Vec::new();

        for (score, doc_address) in top_docs {
            let doc: TantivyDocument = searcher
                .doc(doc_address)
                .map_err(|e| StoreError::Serialization(e.to_string()))?;

            let id = doc
                .get_first(self.id_field)
                .and_then(|v| v.as_str())
                .unwrap_or_default()
                .to_string();

            let kind = doc
                .get_first(self.kind_field)
                .and_then(|v| v.as_str())
                .unwrap_or_default()
                .to_string();

            // Filter by kind if specified
            if let Some(allowed_kinds) = kinds {
                if !allowed_kinds.iter().any(|k| k.to_string() == kind) {
                    continue;
                }
            }

            let content = doc
                .get_first(self.content_field)
                .and_then(|v| v.as_str())
                .unwrap_or_default()
                .to_string();

            results.push(SearchResult {
                id,
                kind,
                content,
                score,
            });
        }

        Ok(results)
    }
}

/// A search result with relevance score
#[derive(Debug, Clone)]
pub struct SearchResult {
    pub id: String,
    pub kind: String,
    pub content: String,
    pub score: f32,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_fulltext_search() {
        let index = FullTextIndex::open_in_memory().unwrap();
        let mut writer = index.writer(50_000_000).unwrap();

        let node = StateNode::new(NodeKind::Insight, json!({"text": "hello world rust programming"}));
        index.index_node(&writer, &node).unwrap();
        writer.commit().unwrap();

        let results = index.search("rust", None, 10).unwrap();
        assert!(!results.is_empty());
        assert!(results[0].content.contains("rust"));
    }
}
