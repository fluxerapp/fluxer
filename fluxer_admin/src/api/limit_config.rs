// SPDX-License-Identifier: AGPL-3.0-or-later

use reqwest::Method;

use super::client::{AdminApiClient, ApiResult};
use super::types::{LimitConfigResponse, LimitConfigUpdateRequest};

impl AdminApiClient {
    pub async fn get_limit_config(&self) -> ApiResult<LimitConfigResponse> {
        self.get("/admin/limit-config", None).await
    }

    pub async fn update_limit_config(
        &self,
        request: &LimitConfigUpdateRequest,
    ) -> ApiResult<LimitConfigResponse> {
        self.send(Method::PUT, "/admin/limit-config", Some(request), None)
            .await
    }
}
