// SPDX-License-Identifier: AGPL-3.0-or-later

//! Poll message handling — create, vote, retrieve results.
//!
//! Polls are stored as message embeds with `embed_type = "poll"`.
//! Vote data lives in the `poll_votes` ScyllaDB table for efficient aggregation.

use crate::types::{PollData, PollOptionData, PollResults, PollOptionResult, ApiUserPartialResponse};
use scylla::Session;
use std::sync::Arc;

/// Error type for poll operations
#[derive(Debug, thiserror::Error)]
pub enum PollError {
    #[error("poll not found")]
    NotFound,
    #[error("poll is closed")]
    Closed,
    #[error("poll has expired")]
    Expired,
    #[error("user has already voted")]
    AlreadyVoted,
    #[error("invalid option index")]
    InvalidOption,
    #[error("database error: {0}")]
    Database(#[from] anyhow::Error),
}

/// Create a new poll embed from poll data
pub fn create_poll_embed(poll_data: PollData) -> crate::types::MessageEmbed {
    use crate::types::MessageEmbed;

    let option_text = poll_data
        .options
        .iter()
        .map(|o| format!("{}. {}", o.index + 1, o.text))
        .collect::<Vec<_>>()
        .join("\n");

    MessageEmbed {
        embed_type: Some("poll".to_string()),
        title: Some(poll_data.title.clone()),
        description: Some(option_text),
        fields: Some(vec![crate::types::MessageEmbedField {
            name: Some("Total votes".to_string()),
            value: Some("0".to_string()),
            is_inline: Some(true),
        }]),
        footer: Some(crate::types::MessageEmbedFooter {
            text: Some(if poll_data.is_anonymous {
                "Anonymous poll".to_string()
            } else {
                "Public poll".to_string()
            }),
            icon_url: None,
        }),
        poll_data: Some(poll_data),
        ..Default::default()
    }
}

/// Cast a vote on a poll
pub async fn cast_vote(
    session: &Arc<Session>,
    poll_id: i64,
    user_id: i64,
    option_indices: &[i32],
) -> Result<(), PollError> {
    // Check if poll exists and is open
    let poll = get_poll_data(session, poll_id).await?;

    if poll.is_closed {
        return Err(PollError::Closed);
    }

    if let Some(expires_at) = poll.expires_at {
        let now = chrono::Utc::now().timestamp_millis();
        if now > expires_at {
            return Err(PollError::Expired);
        }
    }

    // Check for duplicate vote
    let already_voted = session
        .query(
            "SELECT COUNT(*) FROM fluxer.poll_votes WHERE poll_id = ? AND user_id = ?",
            (poll_id, user_id),
        )
        .await
        .map_err(|e| PollError::Database(e.into()))?
        .first_row()
        .map(|row| {
            let count: i64 = row.into();
            count > 0
        })
        .unwrap_or(false);

    if already_voted && !poll.allow_multiple {
        return Err(PollError::AlreadyVoted);
    }

    // Validate option indices
    for &idx in option_indices {
        if idx < 0 || idx as usize >= poll.options.len() {
            return Err(PollError::InvalidOption);
        }
    }

    // Insert votes
    let now = chrono::Utc::now();
    for &idx in option_indices {
        session
            .query(
                "INSERT INTO fluxer.poll_votes (poll_id, option_index, user_id, voted_at) VALUES (?, ?, ?, ?)",
                (poll_id, idx, user_id, now),
            )
            .await
            .map_err(|e| PollError::Database(e.into()))?;
    }

    Ok(())
}

/// Get poll results with voter information
pub async fn get_poll_results(
    session: &Arc<Session>,
    poll_id: i64,
    _requesting_user_id: i64,
) -> Result<PollResults, PollError> {
    let poll = get_poll_data(session, poll_id).await?;

    let mut options: Vec<PollOptionResult> = Vec::new();
    let mut total_votes = 0;

    for opt in &poll.options {
        // Count votes for this option
        let vote_rows = session
            .query(
                "SELECT user_id FROM fluxer.poll_votes WHERE poll_id = ? AND option_index = ?",
                (poll_id, opt.index),
            )
            .await
            .map_err(|e| PollError::Database(e.into()))?;

        let vote_count = vote_rows.rows_num() as i32;
        total_votes += vote_count;

        let voters = if !poll.is_anonymous {
            // Fetch voter details — in real implementation, batch-fetch from users service
            let voter_ids: Vec<i64> = vote_rows
                .rows()
                .filter_map(|r| r.ok())
                .map(|r| {
                    let uid: i64 = r.into();
                    uid
                })
                .collect();

            // Placeholder — actual implementation would resolve user IDs to profiles
            Some(
                voter_ids
                    .into_iter()
                    .map(|id| ApiUserPartialResponse {
                        id: id.to_string(),
                        username: format!("user_{}", id % 10000),
                        discriminator: "0000".to_string(),
                        global_name: None,
                        avatar: None,
                        avatar_color: None,
                        bot: None,
                        system: None,
                        flags: 0,
                        mention_flags: None,
                    })
                    .collect(),
            )
        } else {
            None
        };

        options.push(PollOptionResult {
            index: opt.index,
            text: opt.text.clone(),
            vote_count,
            voters,
        });
    }

    Ok(PollResults {
        poll_id: poll.poll_id,
        title: poll.title,
        options,
        total_votes,
        is_closed: poll.is_closed,
        is_anonymous: poll.is_anonymous,
    })
}

/// Close a poll (only the author or moderators)
pub async fn close_poll(
    session: &Arc<Session>,
    poll_id: i64,
    _user_id: i64,
) -> Result<(), PollError> {
    // TODO: Add permission check (author or moderator)

    session
        .query(
            "UPDATE fluxer.poll_messages SET is_closed = true WHERE channel_id = ? AND message_id = ?",
            // We need channel_id from the poll lookup — simplified for now
            (poll_id / 1000000, poll_id), // Simplified; real impl uses proper lookup
        )
        .await
        .map_err(|e| PollError::Database(e.into()))?;

    Ok(())
}

/// Retrieve poll data from ScyllaDB
async fn get_poll_data(
    session: &Arc<Session>,
    poll_id: i64,
) -> Result<PollData, PollError> {
    // In production, polls are stored within the message embed.
    // This function retrieves the cached poll data from poll_messages table.
    let rows = session
        .query(
            "SELECT poll_data FROM fluxer.poll_messages WHERE channel_id = ? AND message_id = ?",
            (poll_id / 1000000, poll_id), // Simplified channel_id derivation
        )
        .await
        .map_err(|e| PollError::Database(e.into()))?;

    let row = rows.first_row().ok_or(PollError::NotFound)?;
    let poll_json: String = row.into();
    let poll: PollData =
        serde_json::from_str(&poll_json).map_err(|e| PollError::Database(e.into()))?;

    Ok(poll)
}
