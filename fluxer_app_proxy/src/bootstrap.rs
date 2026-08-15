// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::AppProxyConfig;
use crate::discovery_cache::DiscoveryResponse;
use serde::Serialize;

#[derive(Serialize)]
pub struct BootstrapPayload<'a> {
    pub config: BootstrapConfig<'a>,
    pub instance: &'a serde_json::Value,
    pub geoip: &'a serde_json::Value,
}

#[derive(Serialize)]
pub struct BootstrapConfig<'a> {
    #[serde(rename = "releaseChannel")]
    pub release_channel: &'a str,
    #[serde(rename = "bootstrapApiEndpoint")]
    pub bootstrap_api_endpoint: &'a str,
    #[serde(
        rename = "bootstrapApiPublicEndpoint",
        skip_serializing_if = "Option::is_none"
    )]
    pub bootstrap_api_public_endpoint: Option<&'a str>,
}

#[derive(Serialize)]
struct LegacyConfig<'a> {
    #[serde(rename = "PUBLIC_RELEASE_CHANNEL")]
    release_channel: &'a str,
    #[serde(rename = "PUBLIC_BOOTSTRAP_API_ENDPOINT")]
    bootstrap_api_endpoint: &'a str,
    #[serde(
        rename = "PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT",
        skip_serializing_if = "Option::is_none"
    )]
    bootstrap_api_public_endpoint: Option<&'a str>,
}

pub fn build_bootstrap_script(
    config: &AppProxyConfig,
    discovery: &DiscoveryResponse,
    geoip: &serde_json::Value,
    nonce: &str,
) -> String {
    let payload = BootstrapPayload {
        config: BootstrapConfig {
            release_channel: config.release_channel.as_str(),
            bootstrap_api_endpoint: &config.bootstrap_api_endpoint,
            bootstrap_api_public_endpoint: config.bootstrap_api_public_endpoint.as_deref(),
        },
        instance: &discovery.data,
        geoip,
    };

    let legacy = LegacyConfig {
        release_channel: config.release_channel.as_str(),
        bootstrap_api_endpoint: &config.bootstrap_api_endpoint,
        bootstrap_api_public_endpoint: config.bootstrap_api_public_endpoint.as_deref(),
    };

    let bootstrap_json = escape_json_for_script(&serde_json::to_string(&payload).unwrap());
    let legacy_json = escape_json_for_script(&serde_json::to_string(&legacy).unwrap());

    format!(
        r#"<script nonce="{nonce}">window.__FLUXER_BOOTSTRAP__={bootstrap_json};window.__FLUXER_CONFIG__={legacy_json};</script>"#
    )
}

pub fn inject_bootstrap(
    html: &str,
    nonce: &str,
    script_tag: &str,
    static_cdn_endpoint: &str,
) -> String {
    let nonced = html.replace("{{CSP_NONCE_PLACEHOLDER}}", nonce);
    let nonced = nonced.replace(
        "{{STATIC_CDN_ENDPOINT}}",
        static_cdn_endpoint.trim_end_matches('/'),
    );

    if nonced.contains("<!--{{FLUXER_BOOTSTRAP}}-->") {
        return nonced.replace("<!--{{FLUXER_BOOTSTRAP}}-->", script_tag);
    }
    if nonced.contains("{{FLUXER_BOOTSTRAP}}") {
        return nonced.replace("{{FLUXER_BOOTSTRAP}}", script_tag);
    }

    if let Some(pos) = nonced.find("<head>") {
        let insert_at = pos + "<head>".len();
        let mut result = String::with_capacity(nonced.len() + script_tag.len() + 3);
        result.push_str(&nonced[..insert_at]);
        result.push_str("\n\t\t");
        result.push_str(script_tag);
        result.push_str(&nonced[insert_at..]);
        return result;
    }

    if let Some(pos) = nonced.find("<head ")
        && let Some(close) = nonced[pos..].find('>')
    {
        let insert_at = pos + close + 1;
        let mut result = String::with_capacity(nonced.len() + script_tag.len() + 3);
        result.push_str(&nonced[..insert_at]);
        result.push_str("\n\t\t");
        result.push_str(script_tag);
        result.push_str(&nonced[insert_at..]);
        return result;
    }

    nonced
}

fn escape_json_for_script(value: &str) -> String {
    value
        .replace("</", "<\\/")
        .replace('\u{2028}', "\\u2028")
        .replace('\u{2029}', "\\u2029")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inject_bootstrap_before_head_close() {
        let html = "<html><head><title>App</title></head><body></body></html>";
        let result = inject_bootstrap(html, "abc123", "<script>boot</script>", "");
        assert!(result.contains("<script>boot</script>"));
        assert!(result.contains("<head>"));
    }

    #[test]
    fn inject_bootstrap_fluxer_placeholder() {
        let html = "<html><head>{{FLUXER_BOOTSTRAP}}</head></html>";
        let result = inject_bootstrap(html, "n1", "<script>x</script>", "");
        assert!(result.contains("<script>x</script>"));
        assert!(!result.contains("{{FLUXER_BOOTSTRAP}}"));
    }

    #[test]
    fn inject_bootstrap_comment_placeholder() {
        let html = "<html><head><!--{{FLUXER_BOOTSTRAP}}--></head></html>";
        let result = inject_bootstrap(html, "n2", "<script>y</script>", "");
        assert!(result.contains("<script>y</script>"));
        assert!(!result.contains("<!--{{FLUXER_BOOTSTRAP}}-->"));
    }

    #[test]
    fn inject_bootstrap_replaces_csp_nonce_placeholder() {
        let html = r#"<html><head><script nonce="{{CSP_NONCE_PLACEHOLDER}}"></script>{{FLUXER_BOOTSTRAP}}</head></html>"#;
        let result = inject_bootstrap(html, "mynonce", "<script>z</script>", "");
        assert!(result.contains(r#"nonce="mynonce""#));
        assert!(!result.contains("{{CSP_NONCE_PLACEHOLDER}}"));
    }

    #[test]
    fn inject_bootstrap_replaces_static_cdn_endpoint_placeholder() {
        let html = r#"<html><head><link href="{{STATIC_CDN_ENDPOINT}}/web/favicon-32x32.png">{{FLUXER_BOOTSTRAP}}</head></html>"#;
        let result = inject_bootstrap(
            html,
            "nonce",
            "<script>boot</script>",
            "https://cdn.example.test/",
        );
        assert!(result.contains(r#"href="https://cdn.example.test/web/favicon-32x32.png""#));
        assert!(!result.contains("{{STATIC_CDN_ENDPOINT}}"));
    }

    #[test]
    fn escape_json_for_script_escapes_closing_script() {
        assert_eq!(escape_json_for_script("</script>"), "<\\/script>");
    }

    #[test]
    fn escape_json_for_script_escapes_line_separators() {
        let input = "a\u{2028}b\u{2029}c";
        let result = escape_json_for_script(input);
        assert_eq!(result, "a\\u2028b\\u2029c");
    }

    #[test]
    fn escape_json_for_script_no_change_for_safe_input() {
        assert_eq!(
            escape_json_for_script(r#"{"key":"value"}"#),
            r#"{"key":"value"}"#
        );
    }

    #[test]
    fn bootstrap_payload_serialization_field_names() {
        let instance = serde_json::json!({"name": "test"});
        let geoip = serde_json::json!({"country": "SE"});
        let payload = BootstrapPayload {
            config: BootstrapConfig {
                release_channel: "stable",
                bootstrap_api_endpoint: "/api",
                bootstrap_api_public_endpoint: None,
            },
            instance: &instance,
            geoip: &geoip,
        };
        let json = serde_json::to_string(&payload).unwrap();
        assert!(json.contains(r#""releaseChannel""#));
        assert!(json.contains(r#""bootstrapApiEndpoint""#));
        assert!(json.contains(r#""config""#));
        assert!(json.contains(r#""instance""#));
        assert!(json.contains(r#""geoip""#));
    }

    #[test]
    fn bootstrap_config_serializes_public_endpoint_when_present() {
        let instance = serde_json::json!({});
        let geoip = serde_json::json!({});
        let payload = BootstrapPayload {
            config: BootstrapConfig {
                release_channel: "canary",
                bootstrap_api_endpoint: "/api",
                bootstrap_api_public_endpoint: Some("https://pub.example.com/api"),
            },
            instance: &instance,
            geoip: &geoip,
        };
        let json = serde_json::to_string(&payload).unwrap();
        assert!(json.contains(r#""bootstrapApiPublicEndpoint""#));
    }

    #[test]
    fn bootstrap_config_omits_public_endpoint_when_none() {
        let instance = serde_json::json!({});
        let geoip = serde_json::json!({});
        let payload = BootstrapPayload {
            config: BootstrapConfig {
                release_channel: "stable",
                bootstrap_api_endpoint: "/api",
                bootstrap_api_public_endpoint: None,
            },
            instance: &instance,
            geoip: &geoip,
        };
        let json = serde_json::to_string(&payload).unwrap();
        assert!(!json.contains("bootstrapApiPublicEndpoint"));
    }
}
