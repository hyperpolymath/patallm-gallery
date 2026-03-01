//! Application state management

use anyhow::Result;
use llm_unify_core::Conversation;
use llm_unify_storage::{ConversationRepository, Database};

pub struct App {
    pub conversations: Vec<Conversation>,
    pub selected: usize,
    pub search_query: String,
    pub searching: bool,
    #[allow(dead_code)] // Reserved for future use (refresh, search execution)
    db: Database,
}

impl App {
    pub async fn new(db: Database) -> Result<Self> {
        let repo = ConversationRepository::new(&db);
        let conversations = repo.list().await?;

        Ok(Self {
            conversations,
            selected: 0,
            search_query: String::new(),
            searching: false,
            db,
        })
    }

    pub fn next(&mut self) {
        if !self.conversations.is_empty() {
            self.selected = (self.selected + 1) % self.conversations.len();
        }
    }

    pub fn previous(&mut self) {
        if !self.conversations.is_empty() {
            if self.selected > 0 {
                self.selected -= 1;
            } else {
                self.selected = self.conversations.len().saturating_sub(1);
            }
        }
    }

    pub fn select(&mut self) {
        // Toggle conversation details
    }

    pub fn start_search(&mut self) {
        self.searching = true;
        self.search_query.clear();
    }

    pub fn cancel_search(&mut self) {
        self.searching = false;
        self.search_query.clear();
    }

    pub fn is_searching(&self) -> bool {
        self.searching
    }

    pub fn add_search_char(&mut self, c: char) {
        self.search_query.push(c);
    }

    pub fn remove_search_char(&mut self) {
        self.search_query.pop();
    }

    pub fn get_selected_conversation(&self) -> Option<&Conversation> {
        self.conversations.get(self.selected)
    }
}
