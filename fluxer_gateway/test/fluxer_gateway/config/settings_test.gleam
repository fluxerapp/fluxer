// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/env
import fluxer_gateway/config/settings
import fluxer_gateway/core/id
import fluxer_gateway/core/node
import gleam/dict
import gleam/list
import gleam/option.{None, Some}

fn source(
  vars: List(#(String, String)),
  files: List(#(String, BitArray)),
) -> env.Source {
  let vars = dict.from_list(vars)
  let files = dict.from_list(files)
  env.Source(getenv: fn(name) { dict.get(vars, name) }, read_file: fn(path) {
    case dict.get(files, path) {
      Ok(contents) -> Ok(contents)
      Error(Nil) -> Error("enoent")
    }
  })
}

fn read(vars: List(#(String, String))) -> settings.Settings {
  let assert Ok(loaded) = settings.read(source(vars, []))
  loaded
}

pub fn empty_environment_gives_defaults_test() {
  assert read([]) == settings.defaults()
}

pub fn defaults_match_the_original_test() {
  let loaded = read([])
  assert loaded.port == 8771
  assert loaded.role == node.AllRoles
  assert loaded.api_internal_url == "http://127.0.0.1:8080"
  assert loaded.nats_core_url == "nats://nats:4222"
  assert loaded.media_proxy_endpoint == "http://localhost:8088/media"
  assert loaded.static_cdn_endpoint == "http://localhost:8088"
  assert loaded.push_outbox_request_timeout_ms == 100_000
  assert loaded.guild_visitor_sessions_max == 10_000
  assert loaded.zstd_window_log == 14
  assert loaded.logger_level == settings.Info
}

pub fn role_text_test() {
  assert settings.parse_role(Some("websocket")) == node.Websocket
  assert settings.parse_role(Some("sessions")) == node.Sessions
  assert settings.parse_role(Some("websocket+sessions"))
    == node.WebsocketSessions
  assert settings.parse_role(Some("presence")) == node.Presence
  assert settings.parse_role(Some("guilds")) == node.Guilds
  assert settings.parse_role(Some("calls")) == node.Calls
  assert settings.parse_role(Some("push")) == node.Push
  assert settings.parse_role(Some("all")) == node.AllRoles
  assert settings.parse_role(Some(" guilds ")) == node.Guilds
  assert settings.parse_role(None) == node.AllRoles
}

pub fn unknown_role_falls_back_to_all_test() {
  assert settings.parse_role(Some("Guilds")) == node.AllRoles
  assert settings.parse_role(Some("All")) == node.AllRoles
  assert settings.parse_role(Some("guild")) == node.AllRoles
  assert settings.parse_role(Some("bogus")) == node.AllRoles
  assert settings.parse_role(Some("bouncer")) == node.AllRoles
  assert settings.parse_role(Some("")) == node.AllRoles
  let assert Ok(loaded) =
    settings.read(source([#("FLUXER_GATEWAY_ROLE", "websockets")], []))
  assert loaded.role == node.AllRoles
}

pub fn bind_host_defaults_to_every_interface_test() {
  assert read([]).bind_host == "0.0.0.0"
  assert read([#("FLUXER_GATEWAY_BIND_HOST", "  ")]).bind_host == "0.0.0.0"
}

pub fn bind_host_accepts_ip_literals_test() {
  assert read([#("FLUXER_GATEWAY_BIND_HOST", "127.0.0.1")]).bind_host
    == "127.0.0.1"
  assert read([#("FLUXER_GATEWAY_BIND_HOST", " 10.0.0.5 ")]).bind_host
    == "10.0.0.5"
  assert read([#("FLUXER_GATEWAY_BIND_HOST", "::1")]).bind_host == "::1"
}

pub fn bind_host_that_is_not_an_ip_is_rejected_test() {
  assert settings.read(source([#("FLUXER_GATEWAY_BIND_HOST", "localhost")], []))
    == Error(settings.InvalidBindHost("localhost"))
  assert settings.read(source([#("FLUXER_GATEWAY_BIND_HOST", "300.1.1.1")], []))
    == Error(settings.InvalidBindHost("300.1.1.1"))
}

pub fn role_from_environment_test() {
  assert read([#("FLUXER_GATEWAY_ROLE", "guilds")]).role == node.Guilds
  assert read([#("FLUXER_GATEWAY_ROLE", "   ")]).role == node.AllRoles
}

pub fn http_failure_threshold_test() {
  assert read([]).gateway_http_failure_threshold == 0
  assert read([#("FLUXER_GATEWAY_HTTP_FAILURE_THRESHOLD", "100000")]).gateway_http_failure_threshold
    == 100_000
}

pub fn booleans_test() {
  assert read([#("FLUXER_GATEWAY_CLUSTER_ENABLED", " YES ")]).cluster_enabled
  assert !read([#("FLUXER_GATEWAY_PUSH_ENABLED", "0")]).push_enabled
  assert read([#("FLUXER_GATEWAY_PUSH_ENABLED", "maybe")]).push_enabled
}

pub fn integers_test() {
  assert read([#("FLUXER_GATEWAY_PORT", " 9000 ")]).port == 9000
  assert read([]).status_port == 8082
  assert read([#("FLUXER_GATEWAY_STATUS_PORT", "9200")]).status_port == 9200
  assert read([#("FLUXER_GATEWAY_STATUS_PORT", "0")]).status_port == 0
  let assert Error(settings.EnvVar(env.InvalidIntegerEnv(
    name: "FLUXER_GATEWAY_PORT",
    value: "nine",
  ))) = settings.read(source([#("FLUXER_GATEWAY_PORT", "nine")], []))
}

pub fn env_file_test() {
  let loaded =
    settings.read(
      source([#("FLUXER_GATEWAY_RPC_AUTH_TOKEN_FILE", "/run/token")], [
        #("/run/token", <<"secret\r\n":utf8>>),
      ]),
    )
  let assert Ok(loaded) = loaded
  assert loaded.rpc_auth_token == "secret"
}

pub fn ambiguous_env_file_test() {
  let assert Error(settings.EnvVar(env.AmbiguousEnv(..))) =
    settings.read(
      source(
        [
          #("FLUXER_NATS_AUTH_TOKEN", "a"),
          #("FLUXER_NATS_AUTH_TOKEN_FILE", "/run/token"),
        ],
        [],
      ),
    )
}

pub fn unreadable_env_file_test() {
  let assert Error(settings.EnvVar(env.UnreadableEnvFile(..))) =
    settings.read(source([#("FLUXER_NATS_AUTH_TOKEN_FILE", "/missing")], []))
}

pub fn client_ip_header_is_normalised_test() {
  assert read([#("FLUXER_CLIENT_IP_HEADER_NAME", " X-Real-IP ")]).client_ip_header
    == "x-real-ip"
}

pub fn static_peers_test() {
  assert read([
      #("FLUXER_GATEWAY_CLUSTER_STATIC_PEERS", "gw@10.0.0.1, gw@10.0.0.2,,"),
    ]).cluster_static_peers
    == ["gw@10.0.0.1", "gw@10.0.0.2"]
  let assert Error(settings.InvalidStaticPeer(_)) =
    settings.read(
      source([#("FLUXER_GATEWAY_CLUSTER_STATIC_PEERS", "no-at-sign")], []),
    )
}

pub fn pinned_guild_ids_test() {
  let pinned =
    read([#("FLUXER_GATEWAY_PINNED_GUILD_IDS", "30, 10 20,10")]).pinned_guild_ids
    |> list.map(id.guild_id_to_int)
  assert pinned == [10, 20, 30]
  let assert Error(settings.InvalidPinnedGuildId("0")) =
    settings.read(source([#("FLUXER_GATEWAY_PINNED_GUILD_IDS", "0")], []))
}

pub fn logger_level_test() {
  assert read([#("FLUXER_GATEWAY_LOGGER_LEVEL", "DEBUG")]).logger_level
    == settings.Debug
  assert read([#("FLUXER_GATEWAY_LOGGER_LEVEL", "loud")]).logger_level
    == settings.Info
  assert read([
      #("FLUXER_GATEWAY_LOGGER_LEVEL", "debug"),
      #("LOGGER_LEVEL", "error"),
    ]).logger_level
    == settings.Error
}

pub fn public_port_is_written_into_endpoints_test() {
  let loaded =
    read([
      #("FLUXER_BASE_DOMAIN", "example.com."),
      #("FLUXER_PUBLIC_PORT", "8443"),
      #("FLUXER_GATEWAY_MEDIA_PROXY_ENDPOINT", "https://example.com/media"),
      #("FLUXER_GATEWAY_STATIC_CDN_ENDPOINT", "https://cdn.example.com"),
    ])
  assert loaded.media_proxy_endpoint == "https://example.com:8443/media"
  assert loaded.static_cdn_endpoint == "https://cdn.example.com"
}
