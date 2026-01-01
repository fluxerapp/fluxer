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
import fluxer_admin/api/instance_config
import fluxer_admin/components/errors
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/ui
import fluxer_admin/web.{type Context, type Session, action}
import gleam/int
import gleam/list
import gleam/option
import gleam/result
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
  let result = instance_config.get_instance_config(ctx, session)

  let content = case result {
    Ok(config) ->
      h.div([a.class("space-y-6")], [
        ui.heading_page("Instance Configuration"),
        render_status_card(config),
        render_config_form(ctx, config),
      ])
    Error(err) -> errors.error_view(err)
  }

  let html =
    layout.page(
      "Instance Configuration",
      "instance-config",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

fn render_status_card(config: instance_config.InstanceConfig) {
  let status_color = case config.manual_review_active_now {
    True -> "bg-green-100 text-green-800 border-green-200"
    False -> "bg-amber-100 text-amber-800 border-amber-200"
  }

  let status_text = case config.manual_review_active_now {
    True -> "Manual review is currently ACTIVE"
    False -> "Manual review is currently INACTIVE"
  }

  h.div([a.class("p-4 rounded-lg border " <> status_color)], [
    h.div([a.class("flex items-center gap-2")], [
      h.span([a.class("text-lg font-semibold")], [element.text(status_text)]),
    ]),
    case config.manual_review_schedule_enabled {
      True ->
        h.p([a.class("mt-2 text-sm")], [
          element.text(
            "Schedule: "
            <> int.to_string(config.manual_review_schedule_start_hour_utc)
            <> ":00 UTC to "
            <> int.to_string(config.manual_review_schedule_end_hour_utc)
            <> ":00 UTC",
          ),
        ])
      False -> element.none()
    },
  ])
}

fn render_config_form(ctx: Context, config: instance_config.InstanceConfig) {
  ui.card(ui.PaddingMedium, [
    ui.heading_card_with_margin("Manual Review Settings"),
    h.p([a.class("text-sm text-neutral-600 mb-4")], [
      element.text(
        "Configure whether new registrations require manual review before the account is activated.",
      ),
    ]),
    h.form(
      [
        a.method("POST"),
        action(ctx, "/instance-config?action=update"),
        a.class("space-y-6"),
      ],
      [
        h.div([a.class("space-y-2")], [
          h.label([a.class("flex items-center gap-3 cursor-pointer")], [
            h.input([
              a.type_("checkbox"),
              a.name("manual_review_enabled"),
              a.value("true"),
              a.class("w-5 h-5 rounded border-neutral-300"),
              case config.manual_review_enabled {
                True -> a.checked(True)
                False -> a.attribute("", "")
              },
            ]),
            h.span([a.class("text-sm font-medium text-neutral-900")], [
              element.text("Enable manual review for new registrations"),
            ]),
          ]),
          h.p([a.class("text-xs text-neutral-500 ml-8")], [
            element.text(
              "When enabled, new accounts will require approval before they can use the platform.",
            ),
          ]),
        ]),
        h.div([a.class("border-t border-neutral-200 pt-6")], [
          h.label([a.class("flex items-center gap-3 cursor-pointer mb-4")], [
            h.input([
              a.type_("checkbox"),
              a.name("schedule_enabled"),
              a.value("true"),
              a.class("w-5 h-5 rounded border-neutral-300"),
              case config.manual_review_schedule_enabled {
                True -> a.checked(True)
                False -> a.attribute("", "")
              },
            ]),
            h.span([a.class("text-sm font-medium text-neutral-900")], [
              element.text("Enable schedule-based activation"),
            ]),
          ]),
          h.p([a.class("text-xs text-neutral-500 mb-4")], [
            element.text(
              "When enabled, manual review will only be active during the specified hours (UTC).",
            ),
          ]),
          h.div([a.class("grid grid-cols-2 gap-4")], [
            h.div([a.class("space-y-1")], [
              h.label(
                [
                  a.for("start_hour"),
                  a.class("text-sm font-medium text-neutral-700"),
                ],
                [element.text("Start Hour (UTC)")],
              ),
              h.select(
                [
                  a.name("start_hour"),
                  a.id("start_hour"),
                  a.class(
                    "w-full px-3 py-2 border border-neutral-300 rounded text-sm",
                  ),
                ],
                list.map(list.range(0, 23), fn(hour) {
                  h.option(
                    [
                      a.value(int.to_string(hour)),
                      case
                        hour == config.manual_review_schedule_start_hour_utc
                      {
                        True -> a.selected(True)
                        False -> a.attribute("", "")
                      },
                    ],
                    int.to_string(hour) <> ":00",
                  )
                }),
              ),
            ]),
            h.div([a.class("space-y-1")], [
              h.label(
                [
                  a.for("end_hour"),
                  a.class("text-sm font-medium text-neutral-700"),
                ],
                [element.text("End Hour (UTC)")],
              ),
              h.select(
                [
                  a.name("end_hour"),
                  a.id("end_hour"),
                  a.class(
                    "w-full px-3 py-2 border border-neutral-300 rounded text-sm",
                  ),
                ],
                list.map(list.range(0, 23), fn(hour) {
                  h.option(
                    [
                      a.value(int.to_string(hour)),
                      case hour == config.manual_review_schedule_end_hour_utc {
                        True -> a.selected(True)
                        False -> a.attribute("", "")
                      },
                    ],
                    int.to_string(hour) <> ":00",
                  )
                }),
              ),
            ]),
          ]),
        ]),
        h.div([a.class("border-t border-neutral-200 pt-6")], [
          h.div([a.class("space-y-4")], [
            h.div([a.class("space-y-1")], [
              h.label(
                [
                  a.for("registration_alerts_webhook_url"),
                  a.class("text-sm font-medium text-neutral-700"),
                ],
                [element.text("Registration Alerts Webhook URL")],
              ),
              h.input([
                a.type_("url"),
                a.name("registration_alerts_webhook_url"),
                a.id("registration_alerts_webhook_url"),
                a.value(config.registration_alerts_webhook_url),
                a.class(
                  "w-full px-3 py-2 border border-neutral-300 rounded text-sm",
                ),
              ]),
              h.p([a.class("text-xs text-neutral-500 mt-1")], [
                element.text(
                  "Webhook URL for receiving alerts about new user registrations.",
                ),
              ]),
            ]),
            h.div([a.class("space-y-1")], [
              h.label(
                [
                  a.for("system_alerts_webhook_url"),
                  a.class("text-sm font-medium text-neutral-700"),
                ],
                [element.text("System Alerts Webhook URL")],
              ),
              h.input([
                a.type_("url"),
                a.name("system_alerts_webhook_url"),
                a.id("system_alerts_webhook_url"),
                a.value(config.system_alerts_webhook_url),
                a.class(
                  "w-full px-3 py-2 border border-neutral-300 rounded text-sm",
                ),
              ]),
              h.p([a.class("text-xs text-neutral-500 mt-1")], [
                element.text(
                  "Webhook URL for receiving system alerts (virus scan failures, etc.).",
                ),
              ]),
            ]),
          ]),
        ]),
        h.div([a.class("pt-4 border-t border-neutral-200")], [
          ui.button_primary("Save Configuration", "submit", []),
        ]),
      ],
    ),
  ])
}

pub fn handle_action(
  req: Request,
  ctx: Context,
  session: Session,
  action_name: String,
) -> Response {
  case action_name {
    "update" -> handle_update(req, ctx, session)
    _ -> flash.redirect_with_error(ctx, "/instance-config", "Unknown action")
  }
}

fn handle_update(req: Request, ctx: Context, session: Session) -> Response {
  use form_data <- wisp.require_form(req)

  let manual_review_enabled =
    list.key_find(form_data.values, "manual_review_enabled")
    |> result.map(fn(v) { v == "true" })
    |> result.unwrap(False)

  let schedule_enabled =
    list.key_find(form_data.values, "schedule_enabled")
    |> result.map(fn(v) { v == "true" })
    |> result.unwrap(False)

  let start_hour =
    list.key_find(form_data.values, "start_hour")
    |> result.try(int.parse)
    |> result.unwrap(0)

  let end_hour =
    list.key_find(form_data.values, "end_hour")
    |> result.try(int.parse)
    |> result.unwrap(23)

  let registration_alerts_webhook_url =
    list.key_find(form_data.values, "registration_alerts_webhook_url")
    |> result.unwrap("")

  let system_alerts_webhook_url =
    list.key_find(form_data.values, "system_alerts_webhook_url")
    |> result.unwrap("")

  case
    instance_config.update_instance_config(
      ctx,
      session,
      manual_review_enabled,
      schedule_enabled,
      start_hour,
      end_hour,
      registration_alerts_webhook_url,
      system_alerts_webhook_url,
    )
  {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        "/instance-config",
        "Configuration updated successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        "/instance-config",
        "Failed to update configuration",
      )
  }
}
