// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::paths::ROOT;
use std::path::PathBuf;

pub const LOOPBACK_HOST: &str = "127.0.0.1";
pub const ANY_HOST: &str = "0.0.0.0";

pub const DEV_PROXY_PORT: u16 = 8088;
pub const APP_PORT: u16 = 3000;
pub const APP_PROXY_PORT: u16 = 8773;
pub const ADMIN_PORT: u16 = 3020;
pub const API_PORT: u16 = 8080;
pub const GATEWAY_PORT: u16 = 8771;
pub const GATEWAY_WEBSOCKET_PORTS: &[u16] = &[8771, 8772, 8774];
pub const MEDIA_PROXY_PORT: u16 = 8082;
pub const MARKETING_PORT: u16 = 3010;
pub const LIVEKIT_PORT: u16 = 7880;
pub const DEVMAIL_PORT: u16 = 8025;

pub const LOCAL_APP_URL: &str = "http://localhost:8088";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProxyRoute {
    pub prefix: &'static str,
    pub host: &'static str,
    pub port: u16,
    pub strip_prefix: bool,
    pub alternate_ports: &'static [u16],
}

pub const PROXY_ROUTES: &[ProxyRoute] = &[
    ProxyRoute {
        prefix: "/api",
        host: LOOPBACK_HOST,
        port: API_PORT,
        strip_prefix: true,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/gateway",
        host: LOOPBACK_HOST,
        port: GATEWAY_PORT,
        strip_prefix: true,
        alternate_ports: &[8772, 8774],
    },
    ProxyRoute {
        prefix: "/media",
        host: LOOPBACK_HOST,
        port: MEDIA_PROXY_PORT,
        strip_prefix: true,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/fluxer-uploads",
        host: LOOPBACK_HOST,
        port: 8333,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/livekit",
        host: "livekit",
        port: LIVEKIT_PORT,
        strip_prefix: true,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/devmail",
        host: "mailpit",
        port: DEVMAIL_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/admin",
        host: LOOPBACK_HOST,
        port: ADMIN_PORT,
        strip_prefix: true,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/marketing/branding",
        host: LOOPBACK_HOST,
        port: APP_PROXY_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/marketing/flags",
        host: LOOPBACK_HOST,
        port: APP_PROXY_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/marketing/pwa-install",
        host: LOOPBACK_HOST,
        port: APP_PROXY_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/marketing/screenshots",
        host: LOOPBACK_HOST,
        port: APP_PROXY_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/marketing",
        host: LOOPBACK_HOST,
        port: MARKETING_PORT,
        strip_prefix: true,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/assets",
        host: LOOPBACK_HOST,
        port: APP_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/lazy-compilation-using-",
        host: LOOPBACK_HOST,
        port: APP_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/manifest.json",
        host: LOOPBACK_HOST,
        port: APP_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/browserconfig.xml",
        host: LOOPBACK_HOST,
        port: APP_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/robots.txt",
        host: LOOPBACK_HOST,
        port: APP_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/sw.js",
        host: LOOPBACK_HOST,
        port: APP_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/sw.js.map",
        host: LOOPBACK_HOST,
        port: APP_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/version.json",
        host: LOOPBACK_HOST,
        port: APP_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
    ProxyRoute {
        prefix: "/",
        host: LOOPBACK_HOST,
        port: APP_PROXY_PORT,
        strip_prefix: false,
        alternate_ports: &[],
    },
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustServiceSpec {
    pub name: &'static str,
    pub package: &'static str,
    pub path: PathBuf,
    pub port_base: u16,
}

pub fn rust_services() -> Vec<RustServiceSpec> {
    vec![
        RustServiceSpec {
            name: "gifs",
            package: "fluxer-gifs",
            path: ROOT.join("fluxer_gifs"),
            port_base: 8110,
        },
        RustServiceSpec {
            name: "messages",
            package: "fluxer-messages",
            path: ROOT.join("fluxer_messages"),
            port_base: 8112,
        },
        RustServiceSpec {
            name: "snowflakes",
            package: "fluxer-snowflakes",
            path: ROOT.join("fluxer_snowflakes"),
            port_base: 8120,
        },
        RustServiceSpec {
            name: "unfurl",
            package: "fluxer-unfurl",
            path: ROOT.join("fluxer_unfurl"),
            port_base: 8122,
        },
        RustServiceSpec {
            name: "users",
            package: "fluxer-users",
            path: ROOT.join("fluxer_users"),
            port_base: 8124,
        },
    ]
}
