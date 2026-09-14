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
            UserRequest::GetById { user_id }
            | UserRequest::GetPartialById { user_id }
            | UserRequest::Invalidate { user_id } => user_id.to_string(),
            UserRequest::GetPartialsByIds { user_ids } => user_ids
                .iter()
                .min()
                .map(ToString::to_string)
                .unwrap_or_else(|| "0".to_owned()),
            UserRequest::GetApiPartialById { user_id } => user_id.clone(),
            UserRequest::GetApiPartialsByIds { user_ids } => user_ids
                .iter()
                .min()
                .cloned()
                .unwrap_or_else(|| "0".to_owned()),
        }
    }

    fn coalesce_key(req: &UserRequest) -> Option<String> {
        match req {
            UserRequest::GetById { user_id } => Some(format!("get:{user_id}")),
            UserRequest::GetPartialById { user_id } => Some(format!("partial:{user_id}")),
            UserRequest::GetPartialsByIds { user_ids } => {
                let mut ids = user_ids.clone();
                ids.sort_unstable();
                ids.dedup();
                Some(format!(
                    "partials:{}",
                    ids.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(",")
                ))
            }
            UserRequest::GetApiPartialById { user_id } => Some(format!("api_partial:{user_id}")),
            UserRequest::GetApiPartialsByIds { user_ids } => {
                let mut ids = user_ids.clone();
                ids.sort_unstable();
                ids.dedup();
                Some(format!("api_partials:{}", ids.join(",")))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn coalesce_key_ignores_batch_id_order() {
        let forward = UserRequest::GetPartialsByIds {
            user_ids: vec![7, 42, 7],
        };
        let reversed = UserRequest::GetPartialsByIds {
            user_ids: vec![42, 7],
        };
        assert_eq!(
            UsersRouter::coalesce_key(&forward),
            UsersRouter::coalesce_key(&reversed)
        );
    }

    #[test]
    fn coalesce_key_separates_batches_with_different_ids() {
        let left = UserRequest::GetApiPartialsByIds {
            user_ids: vec!["7".to_owned(), "42".to_owned()],
        };
        let right = UserRequest::GetApiPartialsByIds {
            user_ids: vec!["7".to_owned()],
        };
        assert_ne!(
            UsersRouter::coalesce_key(&left),
            UsersRouter::coalesce_key(&right)
        );
    }

    #[test]
    fn invalidations_are_never_coalesced() {
        assert_eq!(
            UsersRouter::coalesce_key(&UserRequest::Invalidate { user_id: 42 }),
            None
        );
    }
}
