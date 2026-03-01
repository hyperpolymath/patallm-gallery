mod query;
mod mutation;
mod types;

pub use query::QueryRoot;
pub use mutation::MutationRoot;
pub use types::*;

use async_graphql::{EmptySubscription, Schema};
use crate::store::SledStore;
use std::sync::Arc;

pub type StateSchema = Schema<QueryRoot, MutationRoot, EmptySubscription>;

pub fn build_schema(store: Arc<SledStore>) -> StateSchema {
    Schema::build(QueryRoot, MutationRoot, EmptySubscription)
        .data(store)
        .finish()
}
