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
                attachment_api_base,
                ..
            } => Some(format!(
                "api-get:{channel_id}:{message_id}:{viewer_user_id}:{source_guild_id:?}:{message_history_cutoff_ms:?}:{can_read_message_history}:{media_endpoint}:{include_reactions:?}:{attachment_decay:?}:{nonce:?}:{tts:?}:{include_hidden}:{threads_mask}:{instance_name}{}",
                attachment_api_base_suffix(attachment_api_base.as_deref())
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
                attachment_api_base,
                ..
            } => Some(format!(
                "api-list:{channel_id}:{viewer_user_id}:{limit}:{before_id:?}:{after_id:?}:{around_id:?}:{source_guild_id:?}:{message_history_cutoff_ms:?}:{can_read_message_history}:{media_endpoint}:{include_reactions:?}:{attachment_decay:?}:{include_hidden}:{threads_mask}:{exclude_types:?}:{instance_name}{}",
                attachment_api_base_suffix(attachment_api_base.as_deref())
            )),
            MessageRequest::ExtractMentions { .. } => None,
        }
    }
}

fn attachment_api_base_suffix(attachment_api_base: Option<&str>) -> String {
    attachment_api_base
        .map(|base| format!(":{base}"))
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn list_request(attachment_api_base: Option<&str>) -> MessageRequest {
        let mut request = json!({
            "op": "ListResponses",
            "channel_id": "11",
            "viewer_user_id": "22",
            "limit": 50,
            "before_id": null,
            "after_id": null,
            "around_id": null,
            "source_guild_id": null,
            "message_history_cutoff_ms": null,
            "can_read_message_history": true,
            "media_endpoint": "https://media.test",
            "media_proxy_secret_key": "",
            "include_reactions": null,
            "attachment_decay": null,
        });
        if let Some(base) = attachment_api_base {
            request["attachment_api_base"] = json!(base);
        }
        serde_json::from_value(request).unwrap()
    }

    #[test]
    fn a_request_without_an_api_base_keeps_its_cache_key_and_wire_form() {
        let request = list_request(None);
        assert_eq!(
            Some(
                "api-list:11:22:50:None:None:None:None:None:true:https://media.test:None:None:false:false:[]:Fluxer"
                    .to_owned()
            ),
            MessagesRouter::coalesce_key(&request)
        );
        let encoded = serde_json::to_value(&request).unwrap();
        assert!(encoded.get("attachment_api_base").is_none());
    }

    #[test]
    fn an_api_base_never_shares_a_cache_key_with_media_urls() {
        assert_ne!(
            MessagesRouter::coalesce_key(&list_request(None)),
            MessagesRouter::coalesce_key(&list_request(Some("https://api.test/api")))
        );
    }
}
