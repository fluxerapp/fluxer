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
                nonce,
                tts,
                include_hidden,
                threads_mask,
                ..
            } => Some(format!(
                "api-get:{channel_id}:{message_id}:{viewer_user_id}:{source_guild_id:?}:{message_history_cutoff_ms:?}:{can_read_message_history}:{media_endpoint}:{include_reactions:?}:{nonce:?}:{tts:?}:{include_hidden}:{threads_mask}"
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
                include_hidden,
                threads_mask,
                exclude_types,
                ..
            } => Some(format!(
                "api-list:{channel_id}:{viewer_user_id}:{limit}:{before_id:?}:{after_id:?}:{around_id:?}:{source_guild_id:?}:{message_history_cutoff_ms:?}:{can_read_message_history}:{media_endpoint}:{include_reactions:?}:{include_hidden}:{threads_mask}:{exclude_types:?}"
            )),
            MessageRequest::ExtractMentions { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn list_request(threads_mask: bool, exclude_types: Vec<i32>) -> MessageRequest {
        MessageRequest::ListResponses {
            channel_id: "42".to_owned(),
            viewer_user_id: "7".to_owned(),
            limit: 50,
            before_id: None,
            after_id: None,
            around_id: None,
            source_guild_id: Some("9".to_owned()),
            message_history_cutoff_ms: None,
            can_read_message_history: true,
            media_endpoint: "https://media.test".to_owned(),
            media_proxy_secret_key: "secret".to_owned(),
            attachment_url_secret_base64: None,
            include_reactions: None,
            include_hidden: false,
            threads_mask,
            exclude_types,
        }
    }

    fn get_request(threads_mask: bool) -> MessageRequest {
        MessageRequest::GetResponseById {
            channel_id: "42".to_owned(),
            message_id: "43".to_owned(),
            viewer_user_id: "7".to_owned(),
            source_guild_id: Some("9".to_owned()),
            message_history_cutoff_ms: None,
            can_read_message_history: true,
            media_endpoint: "https://media.test".to_owned(),
            media_proxy_secret_key: "secret".to_owned(),
            attachment_url_secret_base64: None,
            include_reactions: None,
            nonce: None,
            tts: None,
            include_hidden: false,
            threads_mask,
        }
    }

    #[test]
    fn coalesce_key_separates_thread_masked_and_type_excluded_lists() {
        let control = MessagesRouter::coalesce_key(&list_request(false, Vec::new()));
        let masked = MessagesRouter::coalesce_key(&list_request(true, Vec::new()));
        let excluded = MessagesRouter::coalesce_key(&list_request(false, vec![18]));
        assert_ne!(control, masked);
        assert_ne!(control, excluded);
        assert_ne!(masked, excluded);
        assert_ne!(
            MessagesRouter::coalesce_key(&get_request(false)),
            MessagesRouter::coalesce_key(&get_request(true))
        );
    }
}
