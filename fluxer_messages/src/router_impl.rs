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
            MessageRequest::GetResponseById {
                channel_id,
                message_id,
                viewer_user_id,
                source_guild_id,
                message_history_cutoff_ms,
                can_read_message_history,
                media_endpoint,
                include_reactions,
                attachment_decay,
                nonce,
                tts,
                include_hidden,
                threads_mask,
                instance_name,
                ..
            } => Some(format!(
                "api-get:{channel_id}:{message_id}:{viewer_user_id}:{source_guild_id:?}:{message_history_cutoff_ms:?}:{can_read_message_history}:{media_endpoint}:{include_reactions:?}:{attachment_decay:?}:{nonce:?}:{tts:?}:{include_hidden}:{threads_mask}:{instance_name}"
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
                attachment_decay,
                include_hidden,
                threads_mask,
                exclude_types,
                instance_name,
                ..
            } => Some(format!(
                "api-list:{channel_id}:{viewer_user_id}:{limit}:{before_id:?}:{after_id:?}:{around_id:?}:{source_guild_id:?}:{message_history_cutoff_ms:?}:{can_read_message_history}:{media_endpoint}:{include_reactions:?}:{attachment_decay:?}:{include_hidden}:{threads_mask}:{exclude_types:?}:{instance_name}"
            )),
            MessageRequest::ExtractMentions { .. } => None,
        }
    }
}
