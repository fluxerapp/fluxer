// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::types::{MessageRequest, MessageResponse};
use fluxer_svc::router::RouterService;

pub struct MessagesRouter;

impl MessagesRouter {
    pub fn new() -> Self {
        Self
    }
}

impl Default for MessagesRouter {
    fn default() -> Self {
        Self::new()
    }
}

impl RouterService for MessagesRouter {
    type Request = MessageRequest;
    type Response = MessageResponse;

    const CACHES_RESPONSES: bool = false;

    fn service_name(&self) -> &str {
        "messages"
    }

    fn route_key(req: &MessageRequest) -> String {
        match req {
            MessageRequest::GetById { channel_id, .. } => channel_id.to_string(),
            MessageRequest::GetLatest { channel_id, .. } => channel_id.to_string(),
            MessageRequest::GetBefore { channel_id, .. } => channel_id.to_string(),
            MessageRequest::GetAfter { channel_id, .. } => channel_id.to_string(),
            MessageRequest::GetResponseById { channel_id, .. } => channel_id.to_string(),
            MessageRequest::BuildResponse { message, .. } => message.channel_id.to_string(),
            MessageRequest::BuildResponses { messages, .. } => messages
                .first()
                .map(|message| message.channel_id.to_string())
                .unwrap_or_else(|| "0".to_owned()),
            MessageRequest::ListResponses { channel_id, .. } => channel_id.to_string(),
            MessageRequest::ExtractMentions { .. } => "mentions".to_owned(),
        }
    }

    fn coalesce_key(req: &MessageRequest) -> Option<String> {
        match req {
            MessageRequest::GetById {
                channel_id,
                message_id,
            } => Some(format!("get:{channel_id}:{message_id}")),
            MessageRequest::GetLatest { channel_id, limit } => {
                Some(format!("latest:{channel_id}:{limit}"))
            }
            MessageRequest::GetBefore {
                channel_id,
                before_id,
                limit,
            } => Some(format!("before:{channel_id}:{before_id}:{limit}")),
            MessageRequest::GetAfter {
                channel_id,
                after_id,
                limit,
            } => Some(format!("after:{channel_id}:{after_id}:{limit}")),
            MessageRequest::GetResponseById {
                channel_id,
                message_id,
                viewer_user_id,
                source_guild_id,
                message_history_cutoff_ms,
                can_read_message_history,
                media_endpoint,
                include_reactions,
                nonce,
                tts,
                ..
            } => Some(format!(
                "api-get:{channel_id}:{message_id}:{viewer_user_id}:{source_guild_id:?}:{message_history_cutoff_ms:?}:{can_read_message_history}:{media_endpoint}:{include_reactions:?}:{nonce:?}:{tts:?}"
            )),
            MessageRequest::BuildResponse { .. } => None,
            MessageRequest::BuildResponses { .. } => None,
            MessageRequest::ListResponses {
                channel_id,
                viewer_user_id,
                limit,
                before_id,
                after_id,
                around_id,
                source_guild_id,
                message_history_cutoff_ms,
                can_read_message_history,
                media_endpoint,
                include_reactions,
                ..
            } => Some(format!(
                "api-list:{channel_id}:{viewer_user_id}:{limit}:{before_id:?}:{after_id:?}:{around_id:?}:{source_guild_id:?}:{message_history_cutoff_ms:?}:{can_read_message_history}:{media_endpoint}:{include_reactions:?}"
            )),
            MessageRequest::ExtractMentions { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn coalesce_key_includes_message_query_shape() {
        let latest = MessageRequest::GetLatest {
            channel_id: 42,
            limit: 50,
        };
        let before = MessageRequest::GetBefore {
            channel_id: 42,
            before_id: 100,
            limit: 50,
        };
        assert_ne!(
            MessagesRouter::coalesce_key(&latest),
            MessagesRouter::coalesce_key(&before)
        );
    }
}
