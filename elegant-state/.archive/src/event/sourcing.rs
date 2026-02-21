use crate::schema::StateEvent;
use crate::store::{SledStore, Store, Result};

/// Event sourcing utilities for replay and undo
pub struct EventSourcer<'a> {
    store: &'a SledStore,
}

impl<'a> EventSourcer<'a> {
    pub fn new(store: &'a SledStore) -> Self {
        Self { store }
    }

    /// Get all events since a given timestamp
    pub fn events_since(&self, since: chrono::DateTime<chrono::Utc>) -> Result<Vec<StateEvent>> {
        self.store.get_events(Some(since), usize::MAX)
    }

    /// Get the last N events
    pub fn last_events(&self, n: usize) -> Result<Vec<StateEvent>> {
        self.store.get_events(None, n)
    }

    // Future: replay events to rebuild state
    // Future: undo last N operations
    // Future: point-in-time recovery
}
