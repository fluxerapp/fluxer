// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::types::{UserRequest, UserResponse};
use fluxer_svc::router::RouterService;

pub struct UsersRouter;

impl RouterService for UsersRouter {
    type Request = UserRequest;
    type Response = UserResponse;

    const CACHES_RESPONSES: bool = false;

    fn service_name(&self) -> &str {
        "users"
    }

    fn route_key(req: &UserRequest) -> String {
        match req {
            UserRequest::Invalidate { user_id } => user_id.to_string(),
            UserRequest::GetPartialsByIds { user_ids, .. } => user_ids
                .iter()
                .min()
                .map(ToString::to_string)
                .unwrap_or_else(|| "0".to_owned()),
            UserRequest::GetApiPartialsByIds { user_ids, .. } => user_ids
                .iter()
                .min()
                .cloned()
                .unwrap_or_else(|| "0".to_owned()),
        }
    }

    fn coalesce_key(req: &UserRequest) -> Option<String> {
        match req {
            UserRequest::GetPartialsByIds {
                user_ids,
                instance_name,
            } => {
                let mut ids = user_ids.clone();
                ids.sort_unstable();
                ids.dedup();
                Some(format!(
                    "partials:{instance_name}:{}",
                    ids.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(",")
                ))
            }
            UserRequest::GetApiPartialsByIds {
                user_ids,
                instance_name,
            } => {
                let mut ids = user_ids.clone();
                ids.sort_unstable();
                ids.dedup();
                Some(format!("api_partials:{instance_name}:{}", ids.join(",")))
            }
            UserRequest::Invalidate { .. } => None,
        }
    }

    fn is_broadcast_request(req: &UserRequest) -> bool {
        matches!(req, UserRequest::Invalidate { .. })
    }

    fn is_broadcast_acknowledgement(response: &UserResponse) -> bool {
        matches!(response, UserResponse::Invalidated)
    }
}
