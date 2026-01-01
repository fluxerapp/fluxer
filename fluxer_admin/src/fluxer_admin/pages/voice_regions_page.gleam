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
import gleam/float
import gleam/int
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
  flash_data: option.Option(flash.Flash),
) -> Response {
  let result = voice.list_voice_regions(ctx, session, True)

  let content = case result {
    Ok(response) ->
      h.div([a.class("space-y-6")], [
        ui.flex_row_between([
          ui.heading_page("Voice Regions"),
          h.a(
            [
              a.href("#create"),
              a.class(
                "px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors",
              ),
            ],
            [element.text("Create Region")],
          ),
        ]),
        render_regions_list(ctx, response.regions),
        h.div([a.id("create"), a.class("mt-8")], [render_create_form(ctx)]),
      ])
    Error(err) -> errors.error_view(err)
  }

  let html =
    layout.page(
      "Voice Regions",
      "voice-regions",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

fn render_regions_list(ctx: Context, regions: List(voice.VoiceRegion)) {
  case list.is_empty(regions) {
    True ->
      ui.card_empty([
        ui.text_muted("No voice regions configured yet."),
        ui.text_small_muted("Create your first region to get started."),
      ])
    False ->
      h.div(
        [a.class("space-y-4")],
        list.map(regions, fn(r) { render_region_card(ctx, r) }),
      )
  }
}

fn render_region_card(ctx: Context, region: voice.VoiceRegion) {
  h.div(
    [a.class("bg-white border border-neutral-200 rounded-lg p-6 shadow-sm")],
    [
      h.div([a.class("flex items-start justify-between mb-4")], [
        h.div([a.class("flex items-center gap-3")], [
          h.span([a.class("text-3xl")], [element.text(region.emoji)]),
          h.div([], [
            h.h3([a.class("text-base font-semibold text-neutral-900")], [
              element.text(region.name),
            ]),
            h.p([a.class("text-sm text-neutral-600")], [
              element.text("Region ID: " <> region.id),
            ]),
          ]),
        ]),
        h.div([a.class("flex items-center gap-2 flex-wrap")], [
          case region.is_default {
            True ->
              h.span(
                [
                  a.class("px-2 py-1 bg-blue-100 text-blue-800 text-xs rounded"),
                ],
                [element.text("DEFAULT")],
              )
            False -> element.none()
          },
          voice_components.status_badges(
            region.vip_only,
            !list.is_empty(region.required_guild_features),
            !list.is_empty(region.allowed_guild_ids),
          ),
        ]),
      ]),
      h.div([a.class("grid grid-cols-2 md:grid-cols-3 gap-4 mb-4")], [
        helpers.info_item("Latitude", float.to_string(region.latitude)),
        helpers.info_item("Longitude", float.to_string(region.longitude)),
        helpers.info_item("Servers", case region.servers {
          option.Some(servers) -> int.to_string(list.length(servers))
          option.None -> "0"
        }),
      ]),
      voice_components.features_list(region.required_guild_features),
      voice_components.guild_ids_list(region.allowed_guild_ids),
      case region.servers {
        option.Some(servers) ->
          case list.is_empty(servers) {
            True -> element.none()
            False ->
              h.div([a.class("mt-4 pt-4 border-t border-neutral-200")], [
                h.h4([a.class("text-sm font-medium text-neutral-700 mb-2")], [
                  element.text("Servers"),
                ]),
                h.div(
                  [a.class("space-y-2")],
                  list.map(servers, fn(s) { render_server_row(ctx, s) }),
                ),
              ])
          }
        option.None -> element.none()
      },
      h.div([a.class("mt-4 flex gap-2")], [
        h.form([a.method("POST"), a.action("?action=delete&id=" <> region.id)], [
          h.button(
            [
              a.type_("submit"),
              a.class(
                "px-3 py-1.5 bg-red-600 text-white text-sm rounded hover:bg-red-700 transition-colors",
              ),
              a.attribute(
                "onclick",
                "return confirm('Are you sure? This will delete all servers in this region.')",
              ),
            ],
            [element.text("Delete Region")],
          ),
        ]),
        h.a(
          [
            href(ctx, "/voice-servers?region=" <> region.id),
            a.class(
              "px-3 py-1.5 bg-neutral-100 text-neutral-900 text-sm rounded hover:bg-neutral-200 transition-colors",
            ),
          ],
          [element.text("Manage Servers")],
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
            element.text("Edit Region"),
          ],
        ),
        h.div([a.class("mt-3 pt-3 border-t border-neutral-200")], [
          render_edit_form(ctx, region),
        ]),
      ]),
    ],
  )
}

fn render_server_row(_ctx: Context, server: voice.VoiceServer) {
  h.div(
    [a.class("flex items-center justify-between p-3 bg-neutral-50 rounded")],
    [
      h.div([a.class("flex-1")], [
        h.p([a.class("text-sm text-neutral-900")], [
          element.text(server.server_id),
        ]),
        h.p([a.class("text-xs text-neutral-600")], [
          element.text(server.endpoint),
        ]),
      ]),
      h.div([a.class("flex items-center gap-2")], [
        case server.is_active {
          True ->
            h.span(
              [
                a.class("px-2 py-1 bg-green-100 text-green-800 text-xs rounded"),
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
      ]),
    ],
  )
}

fn render_edit_form(_ctx: Context, region: voice.VoiceRegion) {
  h.div([a.class("bg-neutral-50 rounded-lg p-4")], [
    h.form(
      [
        a.method("POST"),
        a.action("?action=update&id=" <> region.id),
        a.class("space-y-3"),
      ],
      [
        h.div([a.class("grid grid-cols-1 md:grid-cols-2 gap-4")], [
          helpers.form_field_with_value(
            "Region Name",
            "name",
            "text",
            region.name,
            False,
            "Display name for the region",
          ),
          helpers.form_field_with_value(
            "Emoji",
            "emoji",
            "text",
            region.emoji,
            False,
            "Flag or emoji for the region",
          ),
          helpers.form_field_with_value(
            "Latitude",
            "latitude",
            "number",
            float.to_string(region.latitude),
            False,
            "Geographic latitude",
          ),
          helpers.form_field_with_value(
            "Longitude",
            "longitude",
            "number",
            float.to_string(region.longitude),
            False,
            "Geographic longitude",
          ),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("flex items-center gap-2")], [
            h.input([
              a.type_("checkbox"),
              a.name("is_default"),
              a.value("true"),
              a.checked(region.is_default),
            ]),
            h.span([a.class("text-sm text-neutral-700")], [
              element.text("Set as default region"),
            ]),
          ]),
        ]),
        voice_components.restriction_fields(
          region.vip_only,
          region.required_guild_features,
          region.allowed_guild_ids,
        ),
        h.button(
          [
            a.type_("submit"),
            a.class(
              "w-full px-4 py-2 bg-blue-600 text-white rounded text-sm font-medium hover:bg-blue-700 transition-colors",
            ),
          ],
          [element.text("Update Region")],
        ),
      ],
    ),
  ])
}

fn render_create_form(_ctx: Context) {
  h.div([a.class("bg-white border border-neutral-200 rounded-lg p-6")], [
    h.h2([a.class("text-base font-medium text-neutral-900 mb-4")], [
      element.text("Create Voice Region"),
    ]),
    h.form(
      [a.method("POST"), a.action("?action=create"), a.class("space-y-4")],
      [
        h.div([a.class("grid grid-cols-1 md:grid-cols-2 gap-4")], [
          helpers.form_field(
            "Region ID",
            "id",
            "text",
            "us-east",
            True,
            "Unique identifier for the region",
          ),
          helpers.form_field(
            "Region Name",
            "name",
            "text",
            "US East",
            True,
            "Display name",
          ),
          helpers.form_field(
            "Emoji",
            "emoji",
            "text",
            "🇺🇸",
            True,
            "Flag or emoji",
          ),
          helpers.form_field(
            "Latitude",
            "latitude",
            "number",
            "40.7128",
            True,
            "Geographic latitude",
          ),
          helpers.form_field(
            "Longitude",
            "longitude",
            "number",
            "-74.0060",
            True,
            "Geographic longitude",
          ),
        ]),
        h.div([a.class("space-y-3")], [
          h.label([a.class("flex items-center gap-2")], [
            h.input([a.type_("checkbox"), a.name("is_default"), a.value("true")]),
            h.span([a.class("text-sm text-neutral-700")], [
              element.text("Set as default region"),
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
          [element.text("Create Region")],
        ),
      ],
    ),
  ])
}

pub fn handle_action(req: Request, ctx: Context, session: Session) -> Response {
  let query = wisp.get_query(req)
  let action = list.key_find(query, "action") |> result.unwrap("unknown")

  case action {
    "create" -> handle_create(req, ctx, session)
    "update" -> handle_update(req, ctx, session)
    "delete" -> handle_delete(req, ctx, session)
    _ -> wisp.redirect(web.prepend_base_path(ctx, "/voice-regions"))
  }
}

fn handle_create(req: Request, ctx: Context, session: Session) -> Response {
  use form_data <- wisp.require_form(req)

  let id = list.key_find(form_data.values, "id") |> result.unwrap("")
  let name = list.key_find(form_data.values, "name") |> result.unwrap("")
  let emoji = list.key_find(form_data.values, "emoji") |> result.unwrap("")
  let latitude_str =
    list.key_find(form_data.values, "latitude") |> result.unwrap("0.0")
  let longitude_str =
    list.key_find(form_data.values, "longitude") |> result.unwrap("0.0")
  let is_default_str =
    list.key_find(form_data.values, "is_default") |> result.unwrap("false")
  let vip_only_str =
    list.key_find(form_data.values, "vip_only") |> result.unwrap("false")
  let required_guild_features_str =
    list.key_find(form_data.values, "required_guild_features")
    |> result.unwrap("")
  let allowed_guild_ids_str =
    list.key_find(form_data.values, "allowed_guild_ids") |> result.unwrap("")

  let latitude = float.parse(latitude_str) |> result.unwrap(0.0)
  let longitude = float.parse(longitude_str) |> result.unwrap(0.0)
  let is_default = is_default_str == "true"
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
    voice.create_voice_region(
      ctx,
      session,
      id,
      name,
      emoji,
      latitude,
      longitude,
      is_default,
      vip_only,
      required_guild_features,
      allowed_guild_ids,
      option.None,
    )
  {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        "/voice-regions",
        "Region created successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        "/voice-regions",
        "Failed to create region",
      )
  }
}

fn handle_update(req: Request, ctx: Context, session: Session) -> Response {
  use form_data <- wisp.require_form(req)

  let query = wisp.get_query(req)
  let id = list.key_find(query, "id") |> result.unwrap("")

  let name = case list.key_find(form_data.values, "name") {
    Ok(n) -> option.Some(n)
    Error(_) -> option.None
  }
  let emoji = case list.key_find(form_data.values, "emoji") {
    Ok(e) -> option.Some(e)
    Error(_) -> option.None
  }
  let latitude_str = list.key_find(form_data.values, "latitude")
  let longitude_str = list.key_find(form_data.values, "longitude")
  let is_default_str =
    list.key_find(form_data.values, "is_default") |> result.unwrap("false")
  let vip_only_str =
    list.key_find(form_data.values, "vip_only") |> result.unwrap("false")
  let required_guild_features_str =
    list.key_find(form_data.values, "required_guild_features")
    |> result.unwrap("")
  let allowed_guild_ids_str =
    list.key_find(form_data.values, "allowed_guild_ids") |> result.unwrap("")

  let latitude = case latitude_str {
    Ok(s) ->
      float.parse(s) |> result.map(option.Some) |> result.unwrap(option.None)
    Error(_) -> option.None
  }

  let longitude = case longitude_str {
    Ok(s) ->
      float.parse(s) |> result.map(option.Some) |> result.unwrap(option.None)
    Error(_) -> option.None
  }

  let is_default = case is_default_str {
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
    voice.update_voice_region(
      ctx,
      session,
      id,
      name,
      emoji,
      latitude,
      longitude,
      is_default,
      vip_only,
      required_guild_features,
      allowed_guild_ids,
      option.None,
    )
  {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        "/voice-regions",
        "Region updated successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        "/voice-regions",
        "Failed to update region",
      )
  }
}

fn handle_delete(req: Request, ctx: Context, session: Session) -> Response {
  let query = wisp.get_query(req)
  let id = list.key_find(query, "id") |> result.unwrap("")

  case voice.delete_voice_region(ctx, session, id, option.None) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        "/voice-regions",
        "Region deleted successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        "/voice-regions",
        "Failed to delete region",
      )
  }
}
