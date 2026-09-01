// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::AppProxyConfig;
use crate::discovery_cache::DiscoveryResponse;
use reqwest::Url;
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
    let api_public_endpoint =
        api_public_endpoint(config.bootstrap_api_public_endpoint.as_deref(), discovery);

    let payload = BootstrapPayload {
        config: BootstrapConfig {
            release_channel: config.release_channel.as_str(),
            bootstrap_api_endpoint: &config.bootstrap_api_endpoint,
            bootstrap_api_public_endpoint: api_public_endpoint,
        },
        instance: &discovery.data,
        geoip,
    };

    let legacy = LegacyConfig {
        release_channel: config.release_channel.as_str(),
        bootstrap_api_endpoint: &config.bootstrap_api_endpoint,
        bootstrap_api_public_endpoint: api_public_endpoint,
    };

    let bootstrap_json = escape_json_for_script(&serde_json::to_string(&payload).unwrap());
    let legacy_json = escape_json_for_script(&serde_json::to_string(&legacy).unwrap());

    format!(
        r#"<script nonce="{nonce}">window.__FLUXER_BOOTSTRAP__={bootstrap_json};window.__FLUXER_CONFIG__={legacy_json};</script>"#
    )
}

fn api_public_endpoint<'a>(
    configured: Option<&'a str>,
    discovery: &'a DiscoveryResponse,
) -> Option<&'a str> {
    let configured = configured?;
    let Some(discovered) = discovered_api_public(discovery) else {
        return Some(configured);
    };
    if has_explicit_port(configured) || !has_explicit_port(discovered) {
        return Some(configured);
    }
    let (Ok(configured_url), Ok(discovered_url)) = (Url::parse(configured), Url::parse(discovered))
    else {
        return Some(configured);
    };
    if configured_url.scheme() != discovered_url.scheme()
        || configured_url.host_str() != discovered_url.host_str()
        || configured_url.path() != discovered_url.path()
    {
        return Some(configured);
    }
    Some(discovered)
}

fn discovered_api_public(discovery: &DiscoveryResponse) -> Option<&str> {
    discovery
        .data
        .get("endpoints")
        .and_then(|endpoints| endpoints.get("api_public"))
        .and_then(serde_json::Value::as_str)
        .map(str::trim)
        .filter(|value| !value.is_empty())
}

fn has_explicit_port(url: &str) -> bool {
    let Some(scheme_end) = url.find("://") else {
        return false;
    };
    let rest = &url[scheme_end + 3..];
    let authority_end = rest.find(['/', '\\', '?', '#']).unwrap_or(rest.len());
    let host = rest[..authority_end].rsplit('@').next().unwrap_or_default();
    host[host.rfind(']').map_or(0, |index| index + 1)..].contains(':')
}

const MEDIA_PRECONNECT_TAG: &str = r#"<link rel="preconnect" href="{{MEDIA_ENDPOINT}}">"#;
const STATIC_PRECONNECT_TAGS: [&str; 2] = [
    r#"<link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}">"#,
    r#"<link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}" crossorigin>"#,
];

pub fn inject_bootstrap(
    html: &str,
    nonce: &str,
    script_tag: &str,
    static_cdn_endpoint: &str,
    media_endpoint: &str,
) -> String {
    let static_cdn = static_cdn_endpoint.trim_end_matches('/');
    let media = media_endpoint.trim_end_matches('/');

    let nonced = html.replace("{{CSP_NONCE_PLACEHOLDER}}", nonce);
    let nonced = apply_static_preconnect(&nonced, static_cdn);
    let nonced = nonced.replace("{{STATIC_CDN_ENDPOINT}}", static_cdn);
    let nonced = apply_media_preconnect(&nonced, media, static_cdn);

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

fn apply_static_preconnect(html: &str, static_cdn: &str) -> String {
    if !static_cdn.is_empty() {
        return html.to_owned();
    }
    let mut stripped = html.to_owned();
    for tag in STATIC_PRECONNECT_TAGS {
        stripped = stripped.replace(&format!("{tag}\n"), "").replace(tag, "");
    }
    stripped
}

fn apply_media_preconnect(html: &str, media: &str, static_cdn: &str) -> String {
    if media.is_empty() || media == static_cdn {
        return html
            .replace(&format!("{MEDIA_PRECONNECT_TAG}\n"), "")
            .replace(MEDIA_PRECONNECT_TAG, "")
            .replace("{{MEDIA_ENDPOINT}}", "");
    }
    html.replace("{{MEDIA_ENDPOINT}}", media)
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

    const SHIPPED_APP_SHELL: &str = include_str!("../../fluxer_app/index.html");

    fn inject_into_shipped_shell() -> String {
        inject_bootstrap(
            SHIPPED_APP_SHELL,
            "shellnonce",
            "<script>boot</script>",
            "https://cdn.example.test/",
            "https://media.example.test/",
        )
    }

    #[test]
    fn shipped_shell_declares_both_static_cdn_socket_pools() {
        assert!(
            SHIPPED_APP_SHELL
                .contains(r#"<link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}" crossorigin>"#),
            "fluxer_app/index.html has no anonymous preconnect for its module scripts and fonts"
        );
        assert!(
            SHIPPED_APP_SHELL.contains(r#"<link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}">"#),
            "fluxer_app/index.html lost the credentialed preconnect its stylesheet and icons use"
        );
    }

    #[test]
    fn shipped_shell_serves_three_distinct_preconnects() {
        let result = inject_into_shipped_shell();
        assert!(result.contains(r#"<link rel="preconnect" href="https://cdn.example.test">"#));
        assert!(
            result
                .contains(r#"<link rel="preconnect" href="https://cdn.example.test" crossorigin>"#)
        );
        assert!(result.contains(r#"<link rel="preconnect" href="https://media.example.test">"#));
        assert_eq!(result.matches("preconnect").count(), 3);
    }

    #[test]
    fn shipped_shell_ships_no_unsubstituted_placeholder() {
        let result = inject_into_shipped_shell();
        assert!(!result.contains("{{STATIC_CDN_ENDPOINT}}"));
        assert!(!result.contains("{{MEDIA_ENDPOINT}}"));
        assert!(!result.contains("{{CSP_NONCE_PLACEHOLDER}}"));
        assert!(!result.contains("{{FLUXER_BOOTSTRAP}}"));
        assert!(result.contains("<script>boot</script>"));
        assert!(result.contains(r#"nonce="shellnonce""#));
    }

    #[test]
    fn inject_bootstrap_before_head_close() {
        let html = "<html><head><title>App</title></head><body></body></html>";
        let result = inject_bootstrap(html, "abc123", "<script>boot</script>", "", "");
        assert!(result.contains("<script>boot</script>"));
        assert!(result.contains("<head>"));
    }

    #[test]
    fn inject_bootstrap_fluxer_placeholder() {
        let html = "<html><head>{{FLUXER_BOOTSTRAP}}</head></html>";
        let result = inject_bootstrap(html, "n1", "<script>x</script>", "", "");
        assert!(result.contains("<script>x</script>"));
        assert!(!result.contains("{{FLUXER_BOOTSTRAP}}"));
    }

    #[test]
    fn inject_bootstrap_comment_placeholder() {
        let html = "<html><head><!--{{FLUXER_BOOTSTRAP}}--></head></html>";
        let result = inject_bootstrap(html, "n2", "<script>y</script>", "", "");
        assert!(result.contains("<script>y</script>"));
        assert!(!result.contains("<!--{{FLUXER_BOOTSTRAP}}-->"));
    }

    #[test]
    fn inject_bootstrap_replaces_csp_nonce_placeholder() {
        let html = r#"<html><head><script nonce="{{CSP_NONCE_PLACEHOLDER}}"></script>{{FLUXER_BOOTSTRAP}}</head></html>"#;
        let result = inject_bootstrap(html, "mynonce", "<script>z</script>", "", "");
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
            "",
        );
        assert!(result.contains(r#"href="https://cdn.example.test/web/favicon-32x32.png""#));
        assert!(!result.contains("{{STATIC_CDN_ENDPOINT}}"));
    }

    #[test]
    fn inject_bootstrap_replaces_media_endpoint_placeholder() {
        let html = r#"<html><head><link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}">
<link rel="preconnect" href="{{MEDIA_ENDPOINT}}">
{{FLUXER_BOOTSTRAP}}</head></html>"#;
        let result = inject_bootstrap(
            html,
            "nonce",
            "<script>boot</script>",
            "https://cdn.example.test/",
            "https://media.example.test/",
        );
        assert!(result.contains(r#"<link rel="preconnect" href="https://media.example.test">"#));
        assert!(result.contains(r#"<link rel="preconnect" href="https://cdn.example.test">"#));
        assert!(!result.contains("{{MEDIA_ENDPOINT}}"));
    }

    #[test]
    fn inject_bootstrap_drops_media_preconnect_when_endpoint_is_empty() {
        let html = r#"<html><head><link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}">
<link rel="preconnect" href="{{MEDIA_ENDPOINT}}">
{{FLUXER_BOOTSTRAP}}</head></html>"#;
        let result = inject_bootstrap(
            html,
            "nonce",
            "<script>boot</script>",
            "https://cdn.example.test",
            "",
        );
        assert_eq!(result.matches("preconnect").count(), 1);
        assert!(!result.contains("{{MEDIA_ENDPOINT}}"));
        assert!(!result.contains(r#"href="">"#));
    }

    #[test]
    fn inject_bootstrap_drops_media_preconnect_when_it_matches_the_static_cdn() {
        let html = r#"<html><head><link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}">
<link rel="preconnect" href="{{MEDIA_ENDPOINT}}">
{{FLUXER_BOOTSTRAP}}</head></html>"#;
        let result = inject_bootstrap(
            html,
            "nonce",
            "<script>boot</script>",
            "https://cdn.example.test",
            "https://cdn.example.test/",
        );
        assert_eq!(result.matches("preconnect").count(), 1);
        assert!(!result.contains("{{MEDIA_ENDPOINT}}"));
    }

    const SHELL_PRECONNECT_HEAD: &str = r#"<html><head><link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}">
<link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}" crossorigin>
<link rel="preconnect" href="{{MEDIA_ENDPOINT}}">
{{FLUXER_BOOTSTRAP}}</head></html>"#;

    #[test]
    fn static_cdn_keeps_a_credentialed_and_an_anonymous_preconnect() {
        let result = inject_bootstrap(
            SHELL_PRECONNECT_HEAD,
            "nonce",
            "<script>boot</script>",
            "https://cdn.example.test/",
            "https://media.example.test",
        );
        assert!(result.contains(r#"<link rel="preconnect" href="https://cdn.example.test">"#));
        assert!(
            result
                .contains(r#"<link rel="preconnect" href="https://cdn.example.test" crossorigin>"#)
        );
        assert_eq!(result.matches("preconnect").count(), 3);
    }

    #[test]
    fn both_static_preconnects_are_dropped_when_the_endpoint_is_empty() {
        let result = inject_bootstrap(
            SHELL_PRECONNECT_HEAD,
            "nonce",
            "<script>boot</script>",
            "",
            "https://media.example.test",
        );
        assert_eq!(result.matches("preconnect").count(), 1);
        assert!(!result.contains(r#"href="""#));
        assert!(!result.contains("{{STATIC_CDN_ENDPOINT}}"));
    }

    #[test]
    fn media_preconnect_carries_no_crossorigin_attribute() {
        assert!(!MEDIA_PRECONNECT_TAG.contains("crossorigin"));
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

    fn discovery_offering(api_public: &str) -> DiscoveryResponse {
        DiscoveryResponse {
            data: serde_json::json!({"endpoints": {"api_public": api_public}}),
        }
    }

    #[test]
    fn the_discovered_port_repairs_a_portless_configured_endpoint() {
        let discovery = discovery_offering("https://chat.example.test:8443/api");
        assert_eq!(
            api_public_endpoint(Some("https://chat.example.test/api"), &discovery),
            Some("https://chat.example.test:8443/api")
        );
    }

    #[test]
    fn a_configured_endpoint_that_already_carries_a_port_is_left_alone() {
        let discovery = discovery_offering("https://chat.example.test:8443/api");
        assert_eq!(
            api_public_endpoint(Some("https://chat.example.test:9443/api"), &discovery),
            Some("https://chat.example.test:9443/api")
        );
    }

    #[test]
    fn a_discovered_endpoint_on_another_host_is_ignored() {
        let discovery = discovery_offering("https://api.example.test:8443/api");
        assert_eq!(
            api_public_endpoint(Some("https://chat.example.test/api"), &discovery),
            Some("https://chat.example.test/api")
        );
    }

    #[test]
    fn a_discovered_endpoint_on_another_path_is_ignored() {
        let discovery = discovery_offering("https://chat.example.test:8443/v9/api");
        assert_eq!(
            api_public_endpoint(Some("https://chat.example.test/api"), &discovery),
            Some("https://chat.example.test/api")
        );
    }

    #[test]
    fn a_discovered_endpoint_on_another_scheme_is_ignored() {
        let discovery = discovery_offering("http://chat.example.test:8443/api");
        assert_eq!(
            api_public_endpoint(Some("https://chat.example.test/api"), &discovery),
            Some("https://chat.example.test/api")
        );
    }

    #[test]
    fn a_discovery_document_without_a_snapshot_leaves_the_configured_endpoint_alone() {
        let discovery = DiscoveryResponse {
            data: serde_json::json!({}),
        };
        assert_eq!(
            api_public_endpoint(Some("https://chat.example.test/api"), &discovery),
            Some("https://chat.example.test/api")
        );
    }

    #[test]
    fn a_discovery_document_without_a_public_api_endpoint_changes_nothing() {
        let discovery = DiscoveryResponse {
            data: serde_json::json!({"endpoints": {"api_public": "   "}}),
        };
        assert_eq!(
            api_public_endpoint(Some("https://chat.example.test/api"), &discovery),
            Some("https://chat.example.test/api")
        );
    }

    #[test]
    fn a_portless_discovered_endpoint_leaves_a_default_install_alone() {
        let discovery = discovery_offering("https://chat.example.test/api");
        assert_eq!(
            api_public_endpoint(Some("https://chat.example.test/api"), &discovery),
            Some("https://chat.example.test/api")
        );
    }

    #[test]
    fn an_unconfigured_public_endpoint_is_never_invented_from_discovery() {
        let discovery = discovery_offering("https://chat.example.test:8443/api");
        assert_eq!(api_public_endpoint(None, &discovery), None);
    }

    #[test]
    fn the_boot_script_hands_the_repaired_endpoint_to_both_globals() {
        let discovery = discovery_offering("https://chat.example.test:8443/api");
        let mut config = AppProxyConfig::from_env();
        config.bootstrap_api_public_endpoint = Some("https://chat.example.test/api".to_owned());
        let script =
            build_bootstrap_script(&config, &discovery, &serde_json::json!({}), "scriptnonce");
        assert!(
            script.contains(r#""bootstrapApiPublicEndpoint":"https://chat.example.test:8443/api""#)
        );
        assert!(script.contains(
            r#""PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT":"https://chat.example.test:8443/api""#
        ));
        assert!(!script.contains(r#""https://chat.example.test/api""#));
    }
}
