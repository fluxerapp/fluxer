// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::manifest::{
    ANY_HOST, DEV_PROXY_GATEWAY_PORTS_ENV, DEV_PROXY_PORT, GATEWAY_PORT, LOCAL_APP_URL,
    PROXY_ROUTES, ProxyRoute,
};
use anyhow::{Context, Result, bail};
use axum::{
    Router,
    body::Body,
    extract::{ConnectInfo, State},
    http::{HeaderMap, HeaderValue, Method, Request, Response, StatusCode, header},
    response::IntoResponse,
    routing::any,
};
use hyper_util::rt::TokioIo;
use std::collections::HashMap;
use std::env;
use std::net::SocketAddr;
use std::sync::{Arc, LazyLock, Mutex};
use std::time::Duration;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{TcpListener, TcpStream};
use url::Url;

static ROUTE_CURSORS: LazyLock<Mutex<HashMap<&'static str, usize>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

#[derive(Clone)]
struct ProxyState {
    http_client: reqwest::Client,
    gateway_ports: Arc<[u16]>,
}

const BLOCKED_REQUEST_HEADERS: &[&str] = &[
    "connection",
    "content-length",
    "host",
    "keep-alive",
    "proxy-authenticate",
    "proxy-authorization",
    "te",
    "trailer",
    "trailers",
    "transfer-encoding",
    "upgrade",
];

const BLOCKED_RESPONSE_HEADERS: &[&str] = &[
    "connection",
    "keep-alive",
    "proxy-authenticate",
    "proxy-authorization",
    "te",
    "trailer",
    "trailers",
    "transfer-encoding",
    "upgrade",
];
const X_FORWARDED_FOR_HEADER: &str = "x-forwarded-for";

pub async fn run_proxy(host: &str, port: u16) -> Result<()> {
    let bind = format!("{host}:{port}");
    let listener = TcpListener::bind(&bind).await?;
    println!("Fluxer dev proxy listening on {}", listener.local_addr()?);

    let http_client = reqwest::Client::builder()
        .pool_max_idle_per_host(64)
        .pool_idle_timeout(Duration::from_secs(90))
        .redirect(reqwest::redirect::Policy::none())
        .tcp_nodelay(true)
        .build()
        .context("failed to build dev proxy HTTP client")?;
    let gateway_ports = gateway_proxy_ports()?;
    println!(
        "Fluxer dev proxy gateway ports: {}",
        gateway_ports
            .iter()
            .map(u16::to_string)
            .collect::<Vec<_>>()
            .join(",")
    );
    let app = Router::new()
        .fallback(any(proxy_request))
        .with_state(ProxyState {
            http_client,
            gateway_ports,
        });

    axum::serve(
        listener,
        app.into_make_service_with_connect_info::<SocketAddr>(),
    )
    .await
    .context("dev proxy server exited unexpectedly")
}

async fn proxy_request(
    State(state): State<ProxyState>,
    ConnectInfo(client_addr): ConnectInfo<SocketAddr>,
    request: Request<Body>,
) -> Response<Body> {
    let request_head = request_head_from_request(&request);

    if let Some(location) = tunnel_public_redirect_location(&request_head) {
        return redirect_response(&request_head, &location).into_response();
    }

    let route = route_for_path(&request_head.path);
    let (target_host, target_port) = target_for_route(route, &state.gateway_ports);
    if is_upgrade_request(&request_head) {
        return proxy_upgrade(
            request,
            request_head,
            route,
            target_host,
            target_port,
            client_addr,
        )
        .await;
    }

    proxy_http(
        state,
        request,
        request_head,
        route,
        target_host,
        target_port,
        client_addr,
    )
    .await
}

async fn proxy_http(
    state: ProxyState,
    request: Request<Body>,
    request_head: RequestHead,
    route: &'static ProxyRoute,
    target_host: &'static str,
    target_port: u16,
    client_addr: SocketAddr,
) -> Response<Body> {
    let target_url = upstream_http_url(&request_head.path, route, target_host, target_port);
    let (parts, body) = request.into_parts();
    let method = parts.method.clone();
    let mut request_builder = state.http_client.request(method.clone(), &target_url);
    if method != Method::GET && method != Method::HEAD {
        request_builder = request_builder.body(reqwest::Body::wrap_stream(body.into_data_stream()));
    }
    let mut saw_forwarded_for = false;

    for (name, value) in &parts.headers {
        let name_str = name.as_str();
        if BLOCKED_REQUEST_HEADERS.contains(&name_str) {
            continue;
        }
        if name.as_str().eq_ignore_ascii_case(X_FORWARDED_FOR_HEADER) {
            saw_forwarded_for = true;
            if let Some(value) = append_forwarded_for(value, client_addr) {
                request_builder = request_builder.header(name.clone(), value);
            }
        } else {
            request_builder = request_builder.header(name.clone(), value.clone());
        }
    }

    if !saw_forwarded_for {
        request_builder =
            request_builder.header(X_FORWARDED_FOR_HEADER, client_addr.ip().to_string());
    }

    let upstream_response = match request_builder.send().await {
        Ok(response) => response,
        Err(error) => {
            eprintln!("dev proxy HTTP request failed for {target_url}: {error:#}");
            return bad_gateway_response(error);
        }
    };

    let status = upstream_response.status();
    let headers = upstream_response.headers().clone();
    let mut response = Response::new(Body::from_stream(upstream_response.bytes_stream()));
    *response.status_mut() = status;
    copy_response_headers(&headers, response.headers_mut());
    response
}

async fn proxy_upgrade(
    mut request: Request<Body>,
    request_head: RequestHead,
    route: &'static ProxyRoute,
    target_host: &'static str,
    target_port: u16,
    client_addr: SocketAddr,
) -> Response<Body> {
    let on_upgrade = hyper::upgrade::on(&mut request);
    let mut target = match TcpStream::connect((target_host, target_port)).await {
        Ok(target) => target,
        Err(error) => return bad_gateway_response(error),
    };

    if let Err(error) = target
        .write_all(&rewrite_head(
            &request_head,
            route,
            Some(target_host),
            Some(target_port),
            Some(&client_addr.ip().to_string()),
        ))
        .await
    {
        return bad_gateway_response(error);
    }

    let (head, body_prefix) = match read_http_head(&mut target).await {
        Ok(value) => value,
        Err(error) => return bad_gateway_response(error),
    };
    let upstream_response = match parse_response_head(&head) {
        Ok(value) => value,
        Err(error) => return bad_gateway_response(error),
    };

    let mut response = Response::new(Body::empty());
    *response.status_mut() = upstream_response.status;
    copy_upgrade_response_headers(&upstream_response.headers, response.headers_mut());

    if upstream_response.status != StatusCode::SWITCHING_PROTOCOLS {
        *response.body_mut() = Body::from(body_prefix);
        return response;
    }

    tokio::spawn(async move {
        let result: Result<()> = async move {
            let upgraded = on_upgrade.await?;
            let mut client = TokioIo::new(upgraded);
            if !body_prefix.is_empty() {
                client.write_all(&body_prefix).await?;
            }
            tokio::io::copy_bidirectional(&mut client, &mut target).await?;
            Ok(())
        }
        .await;
        if let Err(error) = result {
            eprintln!("dev proxy upgrade bridge failed: {error:#}");
        }
    });

    response
}

fn upstream_http_url(
    request_path: &str,
    route: &ProxyRoute,
    target_host: &str,
    target_port: u16,
) -> String {
    format!(
        "http://{target_host}:{target_port}{}",
        rewrite_path(request_path, route)
    )
}

fn request_head_from_request(request: &Request<Body>) -> RequestHead {
    RequestHead {
        method: request.method().as_str().to_owned(),
        path: request
            .uri()
            .path_and_query()
            .map(|value| value.as_str().to_owned())
            .unwrap_or_else(|| request.uri().to_string()),
        version: format!("{:?}", request.version()),
        headers: request
            .headers()
            .iter()
            .map(|(name, value)| {
                (
                    name.as_str().to_owned(),
                    value.to_str().unwrap_or_default().to_owned(),
                )
            })
            .collect(),
    }
}

fn append_forwarded_for(value: &HeaderValue, client_addr: SocketAddr) -> Option<HeaderValue> {
    let value = value.to_str().ok()?;
    HeaderValue::from_str(&format!("{value}, {}", client_addr.ip())).ok()
}

fn copy_response_headers(source: &HeaderMap, target: &mut HeaderMap) {
    for (name, value) in source {
        if BLOCKED_RESPONSE_HEADERS.contains(&name.as_str()) {
            continue;
        }
        target.append(name.clone(), value.clone());
    }
}

fn copy_upgrade_response_headers(source: &[(String, String)], target: &mut HeaderMap) {
    for (name, value) in source {
        let Ok(name) = axum::http::HeaderName::from_bytes(name.as_bytes()) else {
            continue;
        };
        let Ok(value) = HeaderValue::from_str(value) else {
            continue;
        };
        target.append(name, value);
    }
}

fn bad_gateway_response(error: impl std::fmt::Display) -> Response<Body> {
    (
        StatusCode::BAD_GATEWAY,
        [(
            header::CONTENT_TYPE,
            HeaderValue::from_static("text/plain; charset=utf-8"),
        )],
        format!("{error}\n"),
    )
        .into_response()
}

async fn read_http_head(stream: &mut TcpStream) -> Result<(Vec<u8>, Vec<u8>)> {
    let mut head = Vec::new();
    let mut buf = [0_u8; 4096];
    loop {
        let count = stream.read(&mut buf).await?;
        if count == 0 {
            bail!("connection closed before response head");
        }
        head.extend_from_slice(&buf[..count]);
        if head.windows(4).any(|window| window == b"\r\n\r\n") {
            break;
        }
        if head.len() > 64 * 1024 {
            bail!("response head exceeded 64KiB");
        }
    }
    let header_end = head
        .windows(4)
        .position(|window| window == b"\r\n\r\n")
        .map(|index| index + 4)
        .context("missing response header terminator")?;
    let body_prefix = head.split_off(header_end);
    Ok((head, body_prefix))
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ResponseHead {
    status: StatusCode,
    headers: Vec<(String, String)>,
}

fn parse_response_head(head: &[u8]) -> Result<ResponseHead> {
    let text = String::from_utf8_lossy(head);
    let mut lines = text.split("\r\n");
    let status_line = lines.next().unwrap_or_default();
    let parts = status_line.splitn(3, ' ').collect::<Vec<_>>();
    if parts.len() < 2 {
        bail!("invalid response status line: {status_line:?}");
    }
    let status_code = parts[1]
        .parse::<u16>()
        .with_context(|| format!("invalid response status code: {:?}", parts[1]))?;
    let status = StatusCode::from_u16(status_code)
        .with_context(|| format!("unsupported response status code: {status_code}"))?;
    let headers = lines
        .filter_map(|line| {
            if line.is_empty() {
                return None;
            }
            let (name, value) = line.split_once(':')?;
            (!name.is_empty()).then(|| (name.to_owned(), value.trim().to_owned()))
        })
        .collect();
    Ok(ResponseHead { status, headers })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RequestHead {
    pub method: String,
    pub path: String,
    pub version: String,
    pub headers: Vec<(String, String)>,
}

pub fn parse_request_head(head: &[u8]) -> Result<RequestHead> {
    let text = String::from_utf8_lossy(head);
    let mut lines = text.split("\r\n");
    let request_line = lines.next().unwrap_or_default();
    let parts = request_line.splitn(3, ' ').collect::<Vec<_>>();
    if parts.len() != 3 {
        bail!("invalid request line: {request_line:?}");
    }
    let headers = lines
        .filter_map(|line| {
            if line.is_empty() {
                return None;
            }
            let (name, value) = line.split_once(':')?;
            (!name.is_empty()).then(|| (name.to_owned(), value.trim().to_owned()))
        })
        .collect();
    Ok(RequestHead {
        method: parts[0].to_owned(),
        path: parts[1].to_owned(),
        version: parts[2].to_owned(),
        headers,
    })
}

pub fn tunnel_public_redirect_location(request: &RequestHead) -> Option<String> {
    let public_url = env::var("FLUXER_PUBLIC_URL").ok()?;
    redirect_location_for_public_url(request, &public_url)
}

pub fn redirect_location_for_public_url(request: &RequestHead, public_url: &str) -> Option<String> {
    if !request.method.eq_ignore_ascii_case("GET") && !request.method.eq_ignore_ascii_case("HEAD") {
        return None;
    }
    if !is_local_dev_host(request) || !accepts_html(request) {
        return None;
    }
    let parsed_public_url = Url::parse(public_url).ok()?;
    if is_local_url(&parsed_public_url) {
        return None;
    }
    let path = normalize_request_target(&request.path);
    Some(format!(
        "{}{}",
        parsed_public_url.as_str().trim_end_matches('/'),
        path
    ))
}

fn redirect_response(request: &RequestHead, location: &str) -> Response<Body> {
    let body = if request.method.eq_ignore_ascii_case("HEAD") {
        String::new()
    } else {
        format!("Redirecting to {location}\n")
    };
    let mut response = Response::new(Body::from(body));
    *response.status_mut() = StatusCode::FOUND;
    if let Ok(value) = HeaderValue::from_str(location) {
        response.headers_mut().insert(header::LOCATION, value);
    }
    response
        .headers_mut()
        .insert(header::CACHE_CONTROL, HeaderValue::from_static("no-store"));
    response.headers_mut().insert(
        header::CONTENT_TYPE,
        HeaderValue::from_static("text/plain; charset=utf-8"),
    );
    response
}

fn is_local_dev_host(request: &RequestHead) -> bool {
    let Some(host) = header_value(request, "host") else {
        return false;
    };
    matches!(
        host_without_port(host).as_deref(),
        Some("localhost" | "127.0.0.1" | "::1" | "0.0.0.0")
    )
}

fn is_local_url(url: &Url) -> bool {
    if url.as_str().trim_end_matches('/') == LOCAL_APP_URL {
        return true;
    }
    matches!(
        url.host_str(),
        Some("localhost" | "127.0.0.1" | "::1" | "0.0.0.0")
    )
}

fn accepts_html(request: &RequestHead) -> bool {
    header_value(request, "accept")
        .map(|value| value.to_ascii_lowercase().contains("text/html"))
        .unwrap_or(false)
}

fn is_upgrade_request(request: &RequestHead) -> bool {
    let connection_upgrade = header_value(request, "connection")
        .map(|value| value.to_ascii_lowercase().contains("upgrade"))
        .unwrap_or(false);
    connection_upgrade && header_value(request, "upgrade").is_some()
}

fn header_value<'a>(request: &'a RequestHead, name: &str) -> Option<&'a str> {
    request
        .headers
        .iter()
        .find(|(header_name, _)| header_name.eq_ignore_ascii_case(name))
        .map(|(_, value)| value.as_str())
}

fn host_without_port(host: &str) -> Option<String> {
    let host = host.trim().to_ascii_lowercase();
    if host.is_empty() {
        return None;
    }
    if let Some(rest) = host.strip_prefix('[') {
        return rest.split_once(']').map(|(host, _)| host.to_owned());
    }
    Some(host.split(':').next().unwrap_or(&host).to_owned())
}

pub fn route_for_path(path: &str) -> &'static ProxyRoute {
    let normalized_path = normalize_request_target(path);
    let parsed_path = Url::parse(&format!("http://fluxer.local{normalized_path}"))
        .map(|url| url.path().to_owned())
        .unwrap_or_else(|_| {
            normalized_path
                .split('?')
                .next()
                .unwrap_or(&normalized_path)
                .to_owned()
        });
    for route in PROXY_ROUTES {
        if route.prefix == "/" {
            return route;
        }
        if parsed_path == route.prefix
            || parsed_path.starts_with(&format!("{}/", route.prefix))
            || (route.prefix.ends_with('-') && parsed_path.starts_with(route.prefix))
        {
            return route;
        }
    }
    PROXY_ROUTES.last().expect("proxy has fallback route")
}

pub fn target_for_route(route: &'static ProxyRoute, gateway_ports: &[u16]) -> (&'static str, u16) {
    let uses_gateway_ports = route.prefix == "/gateway";
    let port_count = if uses_gateway_ports {
        gateway_ports.len()
    } else {
        route.alternate_ports.len() + 1
    };
    assert!(
        port_count > 0,
        "proxy route must have at least one target port"
    );
    if port_count == 1 {
        if uses_gateway_ports {
            return (route.host, gateway_ports[0]);
        }
        return (route.host, route.port);
    }
    let mut cursors = ROUTE_CURSORS.lock().expect("route cursor lock poisoned");
    let cursor = *cursors.get(route.prefix).unwrap_or(&0);
    cursors.insert(route.prefix, (cursor + 1) % port_count);
    let port = if uses_gateway_ports {
        gateway_ports[cursor]
    } else if cursor == 0 {
        route.port
    } else {
        route.alternate_ports[cursor - 1]
    };
    (route.host, port)
}

fn gateway_proxy_ports() -> Result<Arc<[u16]>> {
    let raw = env::var(DEV_PROXY_GATEWAY_PORTS_ENV).unwrap_or_else(|_| GATEWAY_PORT.to_string());
    let mut ports = Vec::new();
    for token in raw.split(',') {
        let token = token.trim();
        if token.is_empty() {
            bail!("{DEV_PROXY_GATEWAY_PORTS_ENV} contains an empty port");
        }
        let port = token
            .parse::<u16>()
            .with_context(|| format!("Invalid port {token:?} in {DEV_PROXY_GATEWAY_PORTS_ENV}"))?;
        if port == 0 {
            bail!("{DEV_PROXY_GATEWAY_PORTS_ENV} ports must be greater than zero");
        }
        if ports.contains(&port) {
            bail!("{DEV_PROXY_GATEWAY_PORTS_ENV} contains duplicate port {port}");
        }
        ports.push(port);
    }
    if ports.len() > 3 {
        bail!("{DEV_PROXY_GATEWAY_PORTS_ENV} supports at most three ports");
    }
    Ok(ports.into())
}

pub fn rewrite_path(path: &str, route: &ProxyRoute) -> String {
    let normalized_path = normalize_request_target(path);
    if !route.strip_prefix {
        return normalized_path;
    }
    let (parsed_path, query) = split_path_query(&normalized_path);
    let next_path = parsed_path
        .strip_prefix(route.prefix)
        .unwrap_or(parsed_path);
    let next_path = if next_path.is_empty() { "/" } else { next_path };
    match query {
        Some(query) => format!("{next_path}?{query}"),
        None => next_path.to_owned(),
    }
}

fn normalize_request_target(request_target: &str) -> String {
    let Ok(url) = Url::parse(request_target) else {
        return request_target.to_owned();
    };
    if url.scheme() != "http" && url.scheme() != "https" {
        return request_target.to_owned();
    }
    let mut path = url.path().to_owned();
    if path.is_empty() {
        path.push('/');
    }
    if let Some(query) = url.query() {
        path.push('?');
        path.push_str(query);
    }
    path
}

fn split_path_query(path: &str) -> (&str, Option<&str>) {
    path.split_once('?')
        .map(|(path, query)| (path, Some(query)))
        .unwrap_or((path, None))
}

pub fn rewrite_head(
    request: &RequestHead,
    route: &ProxyRoute,
    target_host: Option<&str>,
    target_port: Option<u16>,
    client_ip: Option<&str>,
) -> Vec<u8> {
    let host = target_host.unwrap_or(route.host);
    let port = target_port.unwrap_or(route.port);
    let is_upgrade = request
        .headers
        .iter()
        .any(|(name, _)| name.eq_ignore_ascii_case("upgrade"));
    let mut lines = vec![format!(
        "{} {} {}",
        request.method,
        rewrite_path(&request.path, route),
        request.version
    )];
    let mut saw_forwarded_for = false;
    let mut saw_host = false;
    let mut saw_connection = false;
    for (name, value) in &request.headers {
        if name.eq_ignore_ascii_case("host") {
            saw_host = true;
            lines.push(format!("Host: {host}:{port}"));
        } else if name.eq_ignore_ascii_case("connection") {
            saw_connection = true;
            lines.push(format!(
                "Connection: {}",
                if is_upgrade { "Upgrade" } else { "close" }
            ));
        } else if name.eq_ignore_ascii_case("x-forwarded-for") {
            saw_forwarded_for = true;
            if let Some(client_ip) = client_ip {
                lines.push(format!("{name}: {value}, {client_ip}"));
            } else {
                lines.push(format!("{name}: {value}"));
            }
        } else {
            lines.push(format!("{name}: {value}"));
        }
    }
    if !saw_host {
        lines.push(format!("Host: {host}:{port}"));
    }
    if !saw_connection {
        lines.push(format!(
            "Connection: {}",
            if is_upgrade { "Upgrade" } else { "close" }
        ));
    }
    if let Some(client_ip) = client_ip
        && !saw_forwarded_for
    {
        lines.push(format!("X-Forwarded-For: {client_ip}"));
    }
    lines.push(String::new());
    lines.push(String::new());
    lines.join("\r\n").into_bytes()
}

#[allow(dead_code)]
pub fn default_bind() -> (&'static str, u16) {
    (ANY_HOST, DEV_PROXY_PORT)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn routes_and_rewrites_prefixes() {
        let api = route_for_path("/api/users?x=1");
        assert_eq!(api.prefix, "/api");
        assert_eq!(rewrite_path("/api/users?x=1", api), "/users?x=1");
        let asset = route_for_path("/assets/app.js");
        assert_eq!(rewrite_path("/assets/app.js", asset), "/assets/app.js");
        let lazy = route_for_path("/lazy-compilation-using-foo");
        assert_eq!(lazy.prefix, "/lazy-compilation-using-");
        let devmail = route_for_path("/devmail/messages");
        assert_eq!(devmail.prefix, "/devmail");
        assert_eq!(
            rewrite_path("/devmail/messages?x=1", devmail),
            "/devmail/messages?x=1"
        );
        let admin = route_for_path("/admin/users");
        assert_eq!(admin.prefix, "/admin");
        assert_eq!(rewrite_path("/admin/users", admin), "/users");
        let absolute_gateway = route_for_path("https://dev.example.test/gateway?v=1&encoding=json");
        assert_eq!(absolute_gateway.prefix, "/gateway");
        assert_eq!(
            rewrite_path(
                "https://dev.example.test/gateway?v=1&encoding=json",
                absolute_gateway
            ),
            "/?v=1&encoding=json"
        );
    }

    #[test]
    fn rewrites_host_connection_and_forwarded_for() {
        let request = RequestHead {
            method: "GET".to_owned(),
            path: "/api/_health".to_owned(),
            version: "HTTP/1.1".to_owned(),
            headers: vec![
                ("Host".to_owned(), "localhost:8088".to_owned()),
                ("Connection".to_owned(), "keep-alive".to_owned()),
                ("X-Forwarded-For".to_owned(), "1.1.1.1".to_owned()),
            ],
        };
        let head = String::from_utf8(rewrite_head(
            &request,
            route_for_path("/api/_health"),
            Some("api"),
            Some(8080),
            Some("2.2.2.2"),
        ))
        .unwrap();
        assert!(head.contains("GET /_health HTTP/1.1\r\n"));
        assert!(head.contains("Host: api:8080\r\n"));
        assert!(head.contains("Connection: close\r\n"));
        assert!(head.contains("X-Forwarded-For: 1.1.1.1, 2.2.2.2\r\n"));
    }

    #[test]
    fn redirects_local_html_navigation_to_public_tunnel_url() {
        let request = RequestHead {
            method: "GET".to_owned(),
            path: "/channels/@me?x=1".to_owned(),
            version: "HTTP/1.1".to_owned(),
            headers: vec![
                ("Host".to_owned(), "localhost:8088".to_owned()),
                (
                    "Accept".to_owned(),
                    "text/html,application/xhtml+xml".to_owned(),
                ),
            ],
        };
        assert_eq!(
            redirect_location_for_public_url(&request, "https://dev.example.test"),
            Some("https://dev.example.test/channels/@me?x=1".to_owned())
        );
    }

    #[test]
    fn public_tunnel_redirect_ignores_assets_and_local_public_url() {
        let asset = RequestHead {
            method: "GET".to_owned(),
            path: "/avatars/4.png".to_owned(),
            version: "HTTP/1.1".to_owned(),
            headers: vec![
                ("Host".to_owned(), "localhost:8088".to_owned()),
                (
                    "Accept".to_owned(),
                    "image/avif,image/webp,image/apng,image/*,*/*;q=0.8".to_owned(),
                ),
            ],
        };
        assert_eq!(
            redirect_location_for_public_url(&asset, "https://dev.example.test"),
            None
        );

        let local_public_url = RequestHead {
            method: "GET".to_owned(),
            path: "/channels/@me".to_owned(),
            version: "HTTP/1.1".to_owned(),
            headers: vec![
                ("Host".to_owned(), "localhost:8088".to_owned()),
                ("Accept".to_owned(), "text/html".to_owned()),
            ],
        };
        assert_eq!(
            redirect_location_for_public_url(&local_public_url, LOCAL_APP_URL),
            None
        );
    }
}
