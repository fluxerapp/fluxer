// SPDX-License-Identifier: AGPL-3.0-or-later

use super::TEST_ADMIN_SECRET;
use fluxer_admin::{
    build_router,
    config::{AdminConfig, ProxyConfig, RuntimeEnv},
};
use std::time::{Duration, Instant};
use tokio::{net::TcpListener, task::JoinHandle, time::sleep};

pub struct RunningRustAdmin {
    base_url: String,
    handle: JoinHandle<()>,
}

impl RunningRustAdmin {
    pub fn base_url(&self) -> &str {
        &self.base_url
    }
}

impl Drop for RunningRustAdmin {
    fn drop(&mut self) {
        self.handle.abort();
    }
}

pub async fn start(api_endpoint: &str) -> Result<RunningRustAdmin, String> {
    let listener = TcpListener::bind("127.0.0.1:0")
        .await
        .map_err(|error| format!("failed to bind Rust admin server: {error}"))?;
    let port = listener
        .local_addr()
        .map_err(|error| format!("failed to read Rust admin address: {error}"))?
        .port();
    let base_url = format!("http://127.0.0.1:{port}");
    let config = admin_config(port, api_endpoint, &base_url);
    let router = build_router(config);
    let handle = tokio::spawn(async move {
        let _ = axum::serve(listener, router).await;
    });
    wait_for_health(&base_url).await?;
    Ok(RunningRustAdmin { base_url, handle })
}

fn admin_config(port: u16, api_endpoint: &str, admin_endpoint: &str) -> AdminConfig {
    AdminConfig {
        env: RuntimeEnv::Test,
        host: "127.0.0.1".to_owned(),
        port,
        secret_key_base: TEST_ADMIN_SECRET.to_owned(),
        base_path: String::new(),
        api_endpoint: api_endpoint.to_owned(),
        media_endpoint: format!("{api_endpoint}/media"),
        static_cdn_endpoint: "https://static.example.test".to_owned(),
        admin_endpoint: admin_endpoint.to_owned(),
        web_app_endpoint: "http://127.0.0.1:8088".to_owned(),
        kv_url: "redis://127.0.0.1:6379/0".to_owned(),
        oauth_client_id: "1234567890123456789".to_owned(),
        oauth_client_secret: "test-admin-oauth-secret".to_owned(),
        oauth_redirect_uri: format!("{admin_endpoint}/oauth2_callback"),
        build_version: "parity".to_owned(),
        release_channel: "parity".to_owned(),
        self_hosted: false,
        proxy: ProxyConfig {
            trust_client_ip_header: false,
            client_ip_header_name: "x-forwarded-for".to_owned(),
        },
    }
}

async fn wait_for_health(base_url: &str) -> Result<(), String> {
    let client = reqwest::Client::builder()
        .redirect(reqwest::redirect::Policy::none())
        .build()
        .map_err(|error| format!("failed to build health client: {error}"))?;
    let deadline = Instant::now() + Duration::from_secs(30);
    let url = format!("{}/_health", base_url.trim_end_matches('/'));
    let mut last_error = String::new();
    while Instant::now() < deadline {
        match client.get(&url).send().await {
            Ok(response) if response.status().is_success() => return Ok(()),
            Ok(response) => last_error = format!("health returned {}", response.status()),
            Err(error) => last_error = error.to_string(),
        }
        sleep(Duration::from_millis(200)).await;
    }
    Err(format!("timed out waiting for {url}: {last_error}"))
}
