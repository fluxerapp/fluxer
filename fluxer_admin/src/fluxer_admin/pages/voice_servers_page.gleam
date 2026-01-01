//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

import fluxer_admin/api/common
import fluxer_admin/api/voice
import fluxer_admin/components/errors
import fluxer_admin/components/flash
import fluxer_admin/components/helpers
import fluxer_admin/components/layout
import fluxer_admin/components/ui
import fluxer_admin/components/voice as voice_components
import fluxer_admin/web.{type Context, type Session, href}
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import wisp.{type Request, type Response}

pub fn view(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  region_id: option.Option(String),
  flash_data: option.Option(flash.Flash),
) -> Response {
  case region_id {
    option.None -> view_no_region(ctx, session, current_admin)
    option.Some(rid) ->
      view_servers(ctx, session, current_admin, rid, flash_data)
  }
}

fn view_no_region(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
) -> Response {
  let content =
    h.div([a.class("space-y-6")], [
      h.div([a.class("mb-6")], [ui.heading_page("Voice Servers")]),
      h.div(
        [
          a.class(
            "bg-yellow-50 border border-yellow-200 rounded-lg p-6 text-center",
          ),
        ],
        [
          h.p([a.class("text-yellow-800 mb-4")], [
            element.text("Please select a region first."),
          ]),
          h.a(
            [
              href(ctx, "/voice-regions"),
              a.class(
                "inline-block px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors",
              ),
            ],
            [element.text("Go to Voice Regions")],
          ),
        ],
      ),
    ])

  let html =
    layout.page(
      "Voice Servers",
      "voice-servers",
      ctx,
      session,
      current_admin,
      option.None,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

fn view_servers(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  region_id: String,
  flash_data: option.Option(flash.Flash),
) -> Response {
  let servers_result = voice.list_voice_servers(ctx, session, region_id)
  let region_result = voice.get_voice_region(ctx, session, region_id, False)

  let content = case servers_result, region_result {
    Ok(servers_response), Ok(region_response) -> {
      let region_name = case region_response.region {
        option.Some(r) -> r.name
        option.None -> region_id
      }

      h.div([a.class("space-y-6")], [
        ui.flex_row_between([
          h.div([], [
            h.a(
              [
                href(ctx, "/voice-regions"),
                a.class(
                  "body-sm text-neutral-600 hover:text-neutral-900 mb-2 inline-block",
                ),
              ],
              [element.text("← Back to Regions")],
            ),
            ui.heading_page("Servers: " <> region_name),
          ]),
          h.a(
            [
              a.href("#create"),
              a.class(
                "px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors",
              ),
            ],
            [element.text("Add Server")],
          ),
        ]),
        render_servers_list(ctx, region_id, servers_response.servers),
        h.div([a.id("create"), a.class("mt-8")], [
          render_create_form(ctx, region_id),
        ]),
      ])
    }
    _, _ -> errors.error_view(common.ServerError)
  }

  let html =
    layout.page(
      "Voice Servers",
      "voice-servers",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

fn render_servers_list(
  ctx: Context,
  region_id: String,
  servers: List(voice.VoiceServer),
) {
  case list.is_empty(servers) {
    True ->
      ui.card_empty([
        ui.text_muted("No servers configured for this region yet."),
        ui.text_small_muted("Add your first server to get started."),
      ])
    False ->
      h.div(
        [a.class("space-y-4")],
        list.map(servers, fn(s) { render_server_card(ctx, region_id, s) }),
      )
  }
}

fn render_server_card(
  ctx: Context,
  region_id: String,
  server: voice.VoiceServer,
) {
  h.div(
    [a.class("bg-white border border-neutral-200 rounded-lg p-6 shadow-sm")],
    [
      h.div([a.class("flex items-start justify-between mb-4")], [
        h.div([], [
          h.h3([a.class("text-base font-semibold text-neutral-900")], [
            element.text(server.server_id),
          ]),
          h.p([a.class("text-sm text-neutral-600 mt-1")], [
            element.text(server.endpoint),
          ]),
        ]),
        h.div([a.class("flex items-center gap-2 flex-wrap")], [
          case server.is_active {
            True ->
              h.span(
                [
                  a.class(
                    "px-2 py-1 bg-green-100 text-green-800 text-xs rounded",
                  ),
                ],
                [element.text("ACTIVE")],
              )
            False ->
              h.span(
                [
                  a.class(
                    "px-2 py-1 bg-neutral-200 text-neutral-700 text-xs rounded",
                  ),
                ],
                [element.text("INACTIVE")],
              )
          },
          voice_components.status_badges(
            server.vip_only,
            !list.is_empty(server.required_guild_features),
            !list.is_empty(server.allowed_guild_ids),
          ),
        ]),
      ]),
      h.div([a.class("grid grid-cols-2 md:grid-cols-2 gap-4 mb-4")], [
        helpers.info_item("Region", server.region_id),
        helpers.info_item("Status", case server.is_active {
          True -> "Active"
          False -> "Inactive"
        }),
      ]),
      voice_components.features_list(server.required_guild_features),
      voice_components.guild_ids_list(server.allowed_guild_ids),
      h.div([a.class("flex gap-2 flex-wrap")], [
        h.form(
          [
            a.method("POST"),
            a.action(
              "?action=toggle&region="
              <> region_id
              <> "&server="
              <> server.server_id,
            ),
          ],
          [
            h.button(
              [
                a.type_("submit"),
                a.class(
                  "px-3 py-1.5 "
                  <> case server.is_active {
                    True ->
                      "bg-yellow-600 hover:bg-yellow-700 disabled:bg-yellow-400 disabled:cursor-not-allowed"
                    False ->
                      "bg-green-600 hover:bg-green-700 disabled:bg-green-400 disabled:cursor-not-allowed"
                  }
                  <> " text-white text-sm rounded transition-colors",
                ),
              ],
              [
                element.text(case server.is_active {
                  True -> "Deactivate"
                  False -> "Activate"
                }),
              ],
            ),
          ],
        ),
        h.form(
          [
            a.method("POST"),
            a.action(
              "?action=delete&region="
              <> region_id
              <> "&server="
              <> server.server_id,
            ),
          ],
          [
            h.button(
              [
                a.type_("submit"),
                a.class(
                  "px-3 py-1.5 bg-red-600 text-white text-sm rounded hover:bg-red-700 transition-colors",
                ),
                a.attribute(
                  "onclick",
                  "return confirm('Are you sure you want to delete this server?')",
                ),
              ],
              [element.text("Delete")],
            ),
          ],
        ),
      ]),
      h.details([a.class("mt-6")], [
        h.summary(
          [
            a.class(
              "cursor-pointer px-4 py-2 bg-blue-50 text-blue-700 rounded hover:bg-blue-100 transition-colors text-sm font-medium",
            ),
          ],
          [
            element.text("Edit Server"),
          ],
        ),
        h.div([a.class("mt-3 pt-3 border-t border-neutral-200")], [
          render_edit_server_form(ctx, region_id, server),
        ]),
      ]),
    ],
  )
}

fn render_edit_server_form(
  _ctx: Context,
  region_id: String,
  server: voice.VoiceServer,
) {
  h.div([a.class("bg-neutral-50 rounded-lg p-4")], [
    h.form(
      [
        a.method("POST"),
        a.action(
          "?action=update&region="
          <> region_id
          <> "&server="
          <> server.server_id,
        ),
        a.class("space-y-3"),
      ],
      [
        h.div([a.class("grid grid-cols-1 gap-4")], [
          helpers.form_field_with_value(
            "Endpoint",
            "endpoint",
            "url",
            server.endpoint,
            False,
            "WebSocket URL",
          ),
        ]),
        h.div([a.class("space-y-1")], [
          h.label([a.class("text-sm font-medium text-neutral-700")], [
            element.text("API Key"),
          ]),
          h.input([
            a.type_("text"),
            a.name("api_key"),
            a.placeholder("Leave blank to keep current"),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900",
            ),
          ]),
          h.p([a.class("text-xs text-neutral-500")], [
            element.text("LiveKit API key (leave blank to keep unchanged)"),
          ]),
        ]),
        h.div([a.class("space-y-1")], [
          h.label([a.class("text-sm font-medium text-neutral-700")], [
            element.text("API Secret"),
          ]),
          h.input([
            a.type_("password"),
            a.name("api_secret"),
            a.placeholder("Leave blank to keep current"),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900",
            ),
          ]),
          h.p([a.class("text-xs text-neutral-500")], [
            element.text("LiveKit API secret (leave blank to keep unchanged)"),
          ]),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("flex items-center gap-2")], [
            h.input([
              a.type_("checkbox"),
              a.name("is_active"),
              a.value("true"),
              a.checked(server.is_active),
            ]),
            h.span([a.class("text-sm text-neutral-700")], [
              element.text("Server is active"),
            ]),
          ]),
        ]),
        voice_components.restriction_fields(
          server.vip_only,
          server.required_guild_features,
          server.allowed_guild_ids,
        ),
        h.button(
          [
            a.type_("submit"),
            a.class(
              "w-full px-4 py-2 bg-blue-600 text-white rounded text-sm font-medium hover:bg-blue-700 transition-colors",
            ),
          ],
          [element.text("Update Server")],
        ),
      ],
    ),
  ])
}

fn render_create_form(_ctx: Context, region_id: String) {
  h.div([a.class("bg-white border border-neutral-200 rounded-lg p-6")], [
    h.h2([a.class("text-base font-medium text-neutral-900 mb-4")], [
      element.text("Add Voice Server"),
    ]),
    h.form(
      [
        a.method("POST"),
        a.action("?action=create&region=" <> region_id),
        a.class("space-y-4"),
      ],
      [
        h.input([a.type_("hidden"), a.name("region_id"), a.value(region_id)]),
        h.div([a.class("grid grid-cols-1 md:grid-cols-2 gap-4")], [
          helpers.form_field(
            "Server ID",
            "server_id",
            "text",
            "livekit-us-east-1",
            True,
            "Unique identifier for this server",
          ),
          helpers.form_field(
            "Endpoint",
            "endpoint",
            "url",
            "wss://livekit.example.com",
            True,
            "WebSocket URL",
          ),
          helpers.form_field(
            "API Key",
            "api_key",
            "text",
            "",
            True,
            "LiveKit API key",
          ),
          helpers.form_field(
            "API Secret",
            "api_secret",
            "password",
            "",
            True,
            "LiveKit API secret",
          ),
        ]),
        h.div([a.class("space-y-3")], [
          h.label([a.class("flex items-center gap-2")], [
            h.input([
              a.type_("checkbox"),
              a.name("is_active"),
              a.value("true"),
              a.checked(True),
            ]),
            h.span([a.class("text-sm text-neutral-700")], [
              element.text("Server is active"),
            ]),
          ]),
        ]),
        voice_components.restriction_fields(False, [], []),
        h.button(
          [
            a.type_("submit"),
            a.class(
              "w-full px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors",
            ),
          ],
          [element.text("Add Server")],
        ),
      ],
    ),
  ])
}

pub fn handle_action(
  req: Request,
  ctx: Context,
  session: Session,
  region_id: String,
) -> Response {
  let query = wisp.get_query(req)
  let action = list.key_find(query, "action") |> result.unwrap("unknown")

  case action {
    "create" -> handle_create(req, ctx, session, region_id)
    "update" -> handle_update_server(req, ctx, session, region_id)
    "delete" -> handle_delete(req, ctx, session, region_id)
    "toggle" -> handle_toggle(req, ctx, session, region_id)
    _ ->
      wisp.redirect(web.prepend_base_path(
        ctx,
        "/voice-servers?region=" <> region_id,
      ))
  }
}

fn handle_create(
  req: Request,
  ctx: Context,
  session: Session,
  region_id: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let server_id =
    list.key_find(form_data.values, "server_id") |> result.unwrap("")
  let endpoint =
    list.key_find(form_data.values, "endpoint") |> result.unwrap("")
  let api_key = list.key_find(form_data.values, "api_key") |> result.unwrap("")
  let api_secret =
    list.key_find(form_data.values, "api_secret") |> result.unwrap("")
  let is_active_str =
    list.key_find(form_data.values, "is_active") |> result.unwrap("false")
  let vip_only_str =
    list.key_find(form_data.values, "vip_only") |> result.unwrap("false")
  let required_guild_features_str =
    list.key_find(form_data.values, "required_guild_features")
    |> result.unwrap("")
  let allowed_guild_ids_str =
    list.key_find(form_data.values, "allowed_guild_ids") |> result.unwrap("")

  let is_active = is_active_str == "true"
  let vip_only = vip_only_str == "true"

  let required_guild_features =
    string.split(required_guild_features_str, ",")
    |> list.map(string.trim)
    |> list.filter(fn(s) { !string.is_empty(s) })

  let allowed_guild_ids =
    string.split(allowed_guild_ids_str, ",")
    |> list.map(string.trim)
    |> list.filter(fn(s) { !string.is_empty(s) })

  case
    voice.create_voice_server(
      ctx,
      session,
      region_id,
      server_id,
      endpoint,
      api_key,
      api_secret,
      is_active,
      vip_only,
      required_guild_features,
      allowed_guild_ids,
      option.None,
    )
  {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        "/voice-servers?region=" <> region_id,
        "Server created successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        "/voice-servers?region=" <> region_id,
        "Failed to create server",
      )
  }
}

fn handle_update_server(
  req: Request,
  ctx: Context,
  session: Session,
  region_id: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let query = wisp.get_query(req)
  let server_id = list.key_find(query, "server") |> result.unwrap("")

  let endpoint = case list.key_find(form_data.values, "endpoint") {
    Ok(e) -> option.Some(e)
    Error(_) -> option.None
  }
  let api_key = case list.key_find(form_data.values, "api_key") {
    Ok(k) if k != "" -> option.Some(k)
    _ -> option.None
  }
  let api_secret = case list.key_find(form_data.values, "api_secret") {
    Ok(s) if s != "" -> option.Some(s)
    _ -> option.None
  }
  let is_active_str =
    list.key_find(form_data.values, "is_active") |> result.unwrap("false")
  let vip_only_str =
    list.key_find(form_data.values, "vip_only") |> result.unwrap("false")
  let required_guild_features_str =
    list.key_find(form_data.values, "required_guild_features")
    |> result.unwrap("")
  let allowed_guild_ids_str =
    list.key_find(form_data.values, "allowed_guild_ids") |> result.unwrap("")

  let is_active = case is_active_str {
    "true" -> option.Some(True)
    _ -> option.Some(False)
  }

  let vip_only = case vip_only_str {
    "true" -> option.Some(True)
    _ -> option.Some(False)
  }

  let required_guild_features =
    string.split(required_guild_features_str, ",")
    |> list.map(string.trim)
    |> list.filter(fn(s) { !string.is_empty(s) })
    |> option.Some

  let allowed_guild_ids =
    string.split(allowed_guild_ids_str, ",")
    |> list.map(string.trim)
    |> list.filter(fn(s) { !string.is_empty(s) })
    |> option.Some

  case
    voice.update_voice_server(
      ctx,
      session,
      region_id,
      server_id,
      endpoint,
      api_key,
      api_secret,
      is_active,
      vip_only,
      required_guild_features,
      allowed_guild_ids,
      option.None,
    )
  {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        "/voice-servers?region=" <> region_id,
        "Server updated successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        "/voice-servers?region=" <> region_id,
        "Failed to update server",
      )
  }
}

fn handle_delete(
  req: Request,
  ctx: Context,
  session: Session,
  region_id: String,
) -> Response {
  let query = wisp.get_query(req)
  let server_id = list.key_find(query, "server") |> result.unwrap("")

  case
    voice.delete_voice_server(ctx, session, region_id, server_id, option.None)
  {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        "/voice-servers?region=" <> region_id,
        "Server deleted successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        "/voice-servers?region=" <> region_id,
        "Failed to delete server",
      )
  }
}

fn handle_toggle(
  req: Request,
  ctx: Context,
  session: Session,
  region_id: String,
) -> Response {
  let query = wisp.get_query(req)
  let server_id = list.key_find(query, "server") |> result.unwrap("")

  let servers_result = voice.list_voice_servers(ctx, session, region_id)

  case servers_result {
    Ok(response) -> {
      let server =
        list.find(response.servers, fn(s) { s.server_id == server_id })

      case server {
        Ok(s) -> {
          let new_status = !s.is_active
          case
            voice.update_voice_server(
              ctx,
              session,
              region_id,
              server_id,
              option.None,
              option.None,
              option.None,
              option.Some(new_status),
              option.None,
              option.None,
              option.None,
              option.None,
            )
          {
            Ok(_) ->
              flash.redirect_with_success(
                ctx,
                "/voice-servers?region=" <> region_id,
                "Server status updated",
              )
            Error(_) ->
              flash.redirect_with_error(
                ctx,
                "/voice-servers?region=" <> region_id,
                "Failed to update server",
              )
          }
        }
        Error(_) ->
          flash.redirect_with_error(
            ctx,
            "/voice-servers?region=" <> region_id,
            "Server not found",
          )
      }
    }
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        "/voice-servers?region=" <> region_id,
        "Failed to fetch servers",
      )
  }
}
