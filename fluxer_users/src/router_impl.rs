// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::types::{UserRequest, UserResponse};
use fluxer_svc::router::RouterService;

pub struct UsersRouter {}

impl RouterService for UsersRouter {
    type Request = UserRequest;
    type Response = UserResponse;

    const CACHES_RESPONSES: bool = false;

    fn service_name(&self) -> &str {
        "users"
    }

    fn route_key(req: &UserRequest) -> String {
        match req {
            UserRequest::Invalidate { user_id }
            | UserRequest::AppAttestCreateChallenge { user_id, .. }
            | UserRequest::AppAttestRegisterKey { user_id, .. }
            | UserRequest::AppAttestSubmitAge { user_id, .. }
            | UserRequest::PlayIntegrityCreateChallenge { user_id }
            | UserRequest::PlayIntegritySubmitAge { user_id, .. } => user_id.to_string(),
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
            UserRequest::GetPersonaPartialById {
                persona_id,
                user_id: _,
            } => persona_id.to_string(),
            UserRequest::GetPersonaPartialsByIds { persona_ids } => persona_ids
                .iter()
                .map(|f| &f.0)
                .min()
                .map(ToString::to_string)
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
            UserRequest::GetPersonaPartialById {
                persona_id,
                user_id: _,
            } => Some(format!("persona_partial:{persona_id}")),
            UserRequest::GetPersonaPartialsByIds { persona_ids } => {
                let mut ids = persona_ids.clone();
                ids.sort_unstable();
                ids.dedup();
                Some(format!(
                    "persona_partials:{}",
                    ids.iter()
                        .map(|f| &f.0)
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(",")
                ))
            }
            UserRequest::Invalidate { .. }
            | UserRequest::AppAttestCreateChallenge { .. }
            | UserRequest::AppAttestRegisterKey { .. }
            | UserRequest::AppAttestSubmitAge { .. }
            | UserRequest::PlayIntegrityCreateChallenge { .. }
            | UserRequest::PlayIntegritySubmitAge { .. } => None,
        }
    }

    fn is_broadcast_request(req: &UserRequest) -> bool {
        matches!(req, UserRequest::Invalidate { .. })
    }

    fn is_broadcast_acknowledgement(response: &UserResponse) -> bool {
        matches!(response, UserResponse::Invalidated)
    }
}
