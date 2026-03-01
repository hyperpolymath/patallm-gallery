// SPDX-License-Identifier: AGPL-3.0-or-later
//! UI rendering

use crate::app::App;
use ratatui::{
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    widgets::{Block, Borders, List, ListItem, Paragraph},
    Frame,
};

pub fn draw(f: &mut Frame, app: &App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(1)
        .constraints(
            [
                Constraint::Length(3),
                Constraint::Min(0),
                Constraint::Length(3),
            ]
            .as_ref(),
        )
        .split(f.area());

    // Title
    let title = Paragraph::new("LLM Unify - Conversation Browser")
        .style(Style::default().fg(Color::Cyan))
        .block(Block::default().borders(Borders::ALL));
    f.render_widget(title, chunks[0]);

    // Conversation list
    let items: Vec<ListItem> = app
        .conversations
        .iter()
        .enumerate()
        .map(|(i, conv)| {
            let style = if i == app.selected {
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
            };

            let content = format!(
                "{} | {} | {} messages",
                conv.provider,
                conv.title,
                conv.message_count()
            );

            ListItem::new(content).style(style)
        })
        .collect();

    let list = List::new(items).block(Block::default().borders(Borders::ALL).title("Conversations"));
    f.render_widget(list, chunks[1]);

    // Status bar
    let status_text = if app.searching {
        format!("Search: {}_", app.search_query)
    } else {
        "Press 'q' to quit, 'j/k' or arrows to navigate, '/' to search".to_string()
    };

    let status = Paragraph::new(status_text)
        .style(Style::default().fg(Color::White))
        .block(Block::default().borders(Borders::ALL));
    f.render_widget(status, chunks[2]);
}
