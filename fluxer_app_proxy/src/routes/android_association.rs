// SPDX-License-Identifier: AGPL-3.0-or-later

use axum::{
    Json,
    http::header,
    response::{IntoResponse, Response},
};
use serde_json::json;

pub async fn assetlinks() -> Response {
    let body = ["com.fluxer", "com.fluxer.canary"].map(|package_name| {
        json!({
            "relation": [
                "delegate_permission/common.handle_all_urls",
                "delegate_permission/common.get_login_creds"
            ],
            "target": {
                "namespace": "android_app",
                "package_name": package_name,
                "sha256_cert_fingerprints": [
                    "91:E4:98:E1:B8:A6:C8:BA:99:41:5E:DB:29:78:29:6B:6C:58:BA:A5:E2:D2:A6:49:CE:C6:2D:A7:A8:29:C7:BC"
                ]
            }
        })
    });

    (
        [(header::CACHE_CONTROL, "public, max-age=1800")],
        Json(body),
    )
        .into_response()
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::body::to_bytes;

    #[tokio::test]
    async fn assetlinks_serves_android_login_credentials_association() {
        let response = assetlinks().await;

        assert_eq!(response.status(), axum::http::StatusCode::OK);
        assert_eq!(
            response.headers().get(header::CACHE_CONTROL).unwrap(),
            "public, max-age=1800"
        );

        let body = to_bytes(response.into_body(), usize::MAX).await.unwrap();
        let actual: serde_json::Value = serde_json::from_slice(&body).unwrap();
        assert_eq!(
            actual,
            serde_json::json!([
                {
                    "relation": [
                        "delegate_permission/common.handle_all_urls",
                        "delegate_permission/common.get_login_creds"
                    ],
                    "target": {
                        "namespace": "android_app",
                        "package_name": "com.fluxer",
                        "sha256_cert_fingerprints": [
                            "91:E4:98:E1:B8:A6:C8:BA:99:41:5E:DB:29:78:29:6B:6C:58:BA:A5:E2:D2:A6:49:CE:C6:2D:A7:A8:29:C7:BC"
                        ]
                    }
                },
                {
                    "relation": [
                        "delegate_permission/common.handle_all_urls",
                        "delegate_permission/common.get_login_creds"
                    ],
                    "target": {
                        "namespace": "android_app",
                        "package_name": "com.fluxer.canary",
                        "sha256_cert_fingerprints": [
                            "91:E4:98:E1:B8:A6:C8:BA:99:41:5E:DB:29:78:29:6B:6C:58:BA:A5:E2:D2:A6:49:CE:C6:2D:A7:A8:29:C7:BC"
                        ]
                    }
                }
            ])
        );
    }
}
