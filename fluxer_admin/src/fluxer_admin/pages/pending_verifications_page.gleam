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
import fluxer_admin/api/verifications
import fluxer_admin/avatar
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/review_deck
import fluxer_admin/components/review_hintbar
import fluxer_admin/components/ui
import fluxer_admin/user
import fluxer_admin/web.{
  type Context, type Session, action, href, prepend_base_path,
}
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import wisp.{type Request, type Response}

const suspicious_user_agent_keywords = [
  "curl",
  "bot",
  "spider",
  "python",
  "java",
  "wget",
  "httpclient",
  "go-http-client",
]

pub fn view(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
) -> Response {
  let limit = 50
  let result = verifications.list_pending_verifications(ctx, session, limit)

  let content = case result {
    Ok(response) -> {
      let total = list.length(response.pending_verifications)

      h.div(
        [
          a.class("max-w-7xl mx-auto"),
          a.id("pending-verifications"),
        ],
        [
          ui.flex_row_between([
            ui.heading_page("Pending Verifications"),
            h.div([a.class("flex items-center gap-4")], [
              case total {
                0 -> element.none()
                count ->
                  h.span(
                    [
                      a.class("body-sm text-neutral-600"),
                      a.attribute("data-review-progress", ""),
                    ],
                    [
                      element.text(int.to_string(count) <> " remaining"),
                    ],
                  )
              },
            ]),
          ]),
          case list.is_empty(response.pending_verifications) {
            True -> empty_state()
            False ->
              h.div(
                [a.class("mt-4")],
                list.append(
                  [review_deck.styles()],
                  list.append(review_deck.script_tags(), [
                    h.div(
                      [
                        a.attribute("data-review-deck", "true"),
                        a.attribute(
                          "data-fragment-base",
                          prepend_base_path(
                            ctx,
                            "/pending-verifications/fragment",
                          ),
                        ),
                        a.attribute("data-next-page", "2"),
                        a.attribute("data-can-paginate", "true"),
                        a.attribute("data-prefetch-when-remaining", "6"),
                        a.attribute(
                          "data-empty-url",
                          prepend_base_path(ctx, "/pending-verifications"),
                        ),
                        a.tabindex(0),
                      ],
                      [
                        h.div(
                          [a.class("max-w-2xl mx-auto")],
                          list.map(response.pending_verifications, fn(pv) {
                            render_pending_verification_card(ctx, pv)
                          }),
                        ),
                        h.div(
                          [
                            a.attribute("data-review-progress", "true"),
                            a.class("text-center mt-4 body-sm text-neutral-600"),
                          ],
                          [
                            element.text(int.to_string(total) <> " remaining"),
                          ],
                        ),
                        review_hintbar.view(
                          "←",
                          "Reject",
                          "→",
                          "Approve",
                          "Esc",
                          "Exit",
                          option.Some("Swipe cards on touch devices"),
                        ),
                      ],
                    ),
                  ]),
                ),
              )
          },
        ],
      )
    }
    Error(err) -> error_view(err)
  }

  let html =
    layout.page(
      "Pending Verifications",
      "pending-verifications",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

pub fn view_fragment(ctx: Context, session: Session, page: Int) -> Response {
  let limit = 50
  let _offset = page * limit
  let result = verifications.list_pending_verifications(ctx, session, limit)

  case result {
    Ok(response) -> {
      let fragment =
        h.div(
          [
            a.attribute("data-review-fragment", "true"),
            a.attribute("data-page", int.to_string(page)),
          ],
          list.map(response.pending_verifications, fn(pv) {
            render_pending_verification_card(ctx, pv)
          }),
        )

      wisp.html_response(element.to_document_string(fragment), 200)
    }
    Error(_) -> {
      let empty = h.div([a.attribute("data-review-fragment", "true")], [])

      wisp.html_response(element.to_document_string(empty), 200)
    }
  }
}

pub fn view_single(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
  user_id: String,
) -> Response {
  let limit = 50
  let result = verifications.list_pending_verifications(ctx, session, limit)

  let content = case result {
    Ok(response) -> {
      case
        list.find(response.pending_verifications, fn(pv) {
          pv.user_id == user_id
        })
      {
        Ok(pv) -> {
          h.div(
            [
              a.class("max-w-7xl mx-auto"),
              a.id("pending-verifications"),
            ],
            [
              ui.flex_row_between([
                ui.heading_page("Pending Verifications"),
                h.a(
                  [
                    href(ctx, "/pending-verifications"),
                    a.class("text-sm text-neutral-600 hover:text-neutral-900"),
                  ],
                  [element.text("Back to all")],
                ),
              ]),
              h.div(
                [a.class("mt-4")],
                list.append(
                  [review_deck.styles()],
                  list.append(review_deck.script_tags(), [
                    h.div(
                      [
                        a.attribute("data-review-deck", "true"),
                        a.attribute(
                          "data-empty-url",
                          prepend_base_path(ctx, "/pending-verifications"),
                        ),
                        a.tabindex(0),
                      ],
                      [
                        h.div([a.class("max-w-2xl mx-auto")], [
                          render_pending_verification_card(ctx, pv),
                        ]),
                        h.div(
                          [
                            a.attribute("data-review-progress", "true"),
                            a.class("text-center mt-4 body-sm text-neutral-600"),
                          ],
                          [
                            element.text("1 remaining"),
                          ],
                        ),
                        review_hintbar.view(
                          "←",
                          "Reject",
                          "→",
                          "Approve",
                          "Esc",
                          "Exit",
                          option.Some("Swipe cards on touch devices"),
                        ),
                      ],
                    ),
                  ]),
                ),
              ),
            ],
          )
        }
        Error(_) -> {
          h.div([a.class("max-w-7xl mx-auto")], [
            h.div([a.class("bg-red-50 border border-red-200 rounded-lg p-8")], [
              h.div([a.class("text-sm text-red-900")], [
                element.text("Verification not found"),
              ]),
            ]),
          ])
        }
      }
    }
    Error(err) -> error_view(err)
  }

  let html =
    layout.page(
      "Pending Verification",
      "pending-verifications",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

pub fn handle_action(
  req: Request,
  ctx: Context,
  session: Session,
  action: option.Option(String),
  background: Bool,
) -> Response {
  use form_data <- wisp.require_form(req)

  let user_id = list.key_find(form_data.values, "user_id") |> option.from_result

  case action {
    option.Some("approve") ->
      handle_pending_verification_approval(ctx, session, user_id, background)
    option.Some("reject") ->
      handle_pending_verification_rejection(ctx, session, user_id, background)
    _ ->
      case background {
        True -> wisp.json_response("{\"error\": \"Unknown action\"}", 400)
        False ->
          flash.redirect_with_error(
            ctx,
            "/pending-verifications",
            "Unknown action",
          )
      }
  }
}

fn handle_pending_verification_approval(
  ctx: Context,
  session: Session,
  user_id: option.Option(String),
  background: Bool,
) -> Response {
  case user_id {
    option.Some(id) -> {
      case verifications.approve_registration(ctx, session, id) {
        Ok(_) ->
          case background {
            True -> wisp.json_response("{}", 204)
            False ->
              flash.redirect_with_success(
                ctx,
                "/pending-verifications",
                "Approved registration for " <> id,
              )
          }
        Error(err) ->
          case background {
            True ->
              wisp.json_response(
                "{\"error\": \"" <> api_error_message(err) <> "\"}",
                400,
              )
            False ->
              flash.redirect_with_error(
                ctx,
                "/pending-verifications",
                api_error_message(err),
              )
          }
      }
    }
    option.None ->
      case background {
        True -> wisp.json_response("{\"error\": \"Missing user_id\"}", 400)
        False ->
          flash.redirect_with_error(
            ctx,
            "/pending-verifications",
            "Missing user_id",
          )
      }
  }
}

fn handle_pending_verification_rejection(
  ctx: Context,
  session: Session,
  user_id: option.Option(String),
  background: Bool,
) -> Response {
  case user_id {
    option.Some(id) -> {
      case verifications.reject_registration(ctx, session, id) {
        Ok(_) ->
          case background {
            True -> wisp.json_response("{}", 204)
            False ->
              flash.redirect_with_success(
                ctx,
                "/pending-verifications",
                "Rejected registration for " <> id,
              )
          }
        Error(err) ->
          case background {
            True ->
              wisp.json_response(
                "{\"error\": \"" <> api_error_message(err) <> "\"}",
                400,
              )
            False ->
              flash.redirect_with_error(
                ctx,
                "/pending-verifications",
                api_error_message(err),
              )
          }
      }
    }
    option.None ->
      case background {
        True -> wisp.json_response("{\"error\": \"Missing user_id\"}", 400)
        False ->
          flash.redirect_with_error(
            ctx,
            "/pending-verifications",
            "Missing user_id",
          )
      }
  }
}

fn api_error_message(err: common.ApiError) -> String {
  case err {
    common.Unauthorized -> "Unauthorized"
    common.Forbidden(message) -> message
    common.NotFound -> "Not Found"
    common.NetworkError -> "Network error"
    common.ServerError -> "Server error"
  }
}

fn render_pending_verification_card(
  ctx: Context,
  pv: verifications.PendingVerification,
) -> element.Element(a) {
  let metadata_warning = user_agent_warning(pv.metadata)

  h.div(
    [
      a.attribute("data-review-card", "true"),
      a.attribute(
        "data-direct-url",
        prepend_base_path(ctx, "/pending-verifications/" <> pv.user_id),
      ),
      a.class(
        "bg-white border border-neutral-200 rounded-xl shadow-sm p-6 focus:outline-none focus:ring-2 focus:ring-neutral-900",
      ),
      a.tabindex(0),
    ],
    [
      h.div([a.class("flex items-start gap-4 mb-6")], [
        h.img([
          a.src(avatar.get_user_avatar_url(
            ctx.media_endpoint,
            ctx.cdn_endpoint,
            pv.user.id,
            pv.user.avatar,
            True,
            ctx.asset_version,
          )),
          a.alt(pv.user.username),
          a.class("w-16 h-16 rounded-full flex-shrink-0"),
        ]),
        h.div([a.class("flex-1 min-w-0")], [
          h.a(
            [
              href(ctx, "/users/" <> pv.user.id),
              a.class(
                "text-lg text-neutral-900 hover:text-neutral-600 underline decoration-neutral-300 hover:decoration-neutral-500 font-semibold",
              ),
            ],
            [
              element.text(
                pv.user.username
                <> "#"
                <> user.format_discriminator(pv.user.discriminator),
              ),
            ],
          ),
          h.div([a.class("text-sm text-neutral-600 mt-1 truncate")], [
            case pv.user.email {
              option.Some(email) -> element.text(email)
              option.None -> element.text("N/A")
            },
          ]),
          h.div([a.class("text-sm text-neutral-500 mt-1")], [
            element.text("Registered " <> format_timestamp(pv.created_at)),
          ]),
        ]),
      ]),
      h.details(
        [
          a.class("group mb-6"),
          a.attribute("open", ""),
        ],
        [
          h.summary(
            [
              a.class(
                "cursor-pointer text-sm text-neutral-600 hover:text-neutral-900 list-none flex items-center gap-2",
              ),
            ],
            [
              element.text("Registration metadata"),
              h.span([a.class("text-neutral-400 group-open:hidden")], [
                element.text("▼"),
              ]),
              h.span([a.class("text-neutral-400 hidden group-open:inline")], [
                element.text("▲"),
              ]),
            ],
          ),
          h.div([a.class("mt-3 text-sm text-neutral-600 space-y-1")], [
            render_registration_metadata(pv.metadata, metadata_warning),
          ]),
        ],
      ),
      h.form(
        [
          a.method("post"),
          action(ctx, "/pending-verifications?action=reject"),
          a.attribute("data-review-submit", "left"),
          a.class("inline-flex w-full"),
        ],
        [
          h.input([
            a.type_("hidden"),
            a.name("user_id"),
            a.value(pv.user_id),
          ]),
        ],
      ),
      h.form(
        [
          a.method("post"),
          action(ctx, "/pending-verifications?action=approve"),
          a.attribute("data-review-submit", "right"),
          a.class("inline-flex w-full"),
        ],
        [
          h.input([
            a.type_("hidden"),
            a.name("user_id"),
            a.value(pv.user_id),
          ]),
        ],
      ),
      h.div(
        [
          a.class(
            "flex items-center justify-between gap-4 pt-4 border-t border-neutral-200",
          ),
        ],
        [
          h.button(
            [
              a.attribute("data-review-action", "left"),
              a.class(
                "px-4 py-2 bg-red-600 text-white rounded-lg label hover:bg-red-700 transition-colors",
              ),
            ],
            [element.text("Reject")],
          ),
          h.button(
            [
              a.attribute("data-review-action", "right"),
              a.class(
                "px-4 py-2 bg-green-600 text-white rounded-lg label hover:bg-green-700 transition-colors",
              ),
            ],
            [element.text("Approve")],
          ),
        ],
      ),
    ],
  )
}

fn render_registration_metadata(
  metadata: List(verifications.PendingVerificationMetadata),
  warning: option.Option(String),
) -> element.Element(a) {
  let ip = option_or_default("Unknown", metadata_value(metadata, "ip_address"))

  let normalized_ip =
    option_or_default(ip, metadata_value(metadata, "normalized_ip"))

  let ip_display = case normalized_ip == ip {
    True -> ip
    False -> ip <> " (Normalized: " <> normalized_ip <> ")"
  }

  let geoip_reason =
    option_or_default("none", metadata_value(metadata, "geoip_reason"))

  let os = option_or_default("Unknown", metadata_value(metadata, "os"))

  let browser =
    option_or_default("Unknown", metadata_value(metadata, "browser"))

  let device = option_or_default("Unknown", metadata_value(metadata, "device"))

  let display_name =
    option_or_default("N/A", metadata_value(metadata, "display_name"))

  let user_agent =
    option_or_default("Not provided", metadata_value(metadata, "user_agent"))

  let location =
    option_or_default("Unknown Location", metadata_value(metadata, "location"))

  let ip_reverse = metadata_value(metadata, "ip_address_reverse")

  let geoip_note = case geoip_reason {
    "none" -> element.none()
    reason ->
      h.div([a.class("text-xs text-neutral-500")], [
        element.text("GeoIP hint: " <> reason),
      ])
  }

  h.div([a.class("flex flex-col gap-0.5 text-xs text-neutral-600")], [
    h.div([], [element.text("Display Name: " <> display_name)]),
    h.div([], [element.text("IP: " <> ip_display)]),
    h.div([], [element.text("Location: " <> location)]),
    case ip_reverse {
      option.Some(reverse) ->
        h.div([], [element.text("Reverse DNS: " <> reverse)])
      option.None -> element.none()
    },
    geoip_note,
    h.div([], [element.text("OS: " <> os)]),
    h.div([], [element.text("Browser: " <> browser)]),
    h.div([], [element.text("Device: " <> device)]),
    h.div([a.class("break-words")], [element.text("User Agent: " <> user_agent)]),
    render_user_agent_warning(warning),
  ])
}

fn render_user_agent_warning(
  warning: option.Option(String),
) -> element.Element(a) {
  case warning {
    option.Some(message) ->
      h.div([a.class("mt-2")], [
        ui.pill(message, ui.PillWarning),
      ])
    option.None -> element.none()
  }
}

fn user_agent_warning(
  metadata: List(verifications.PendingVerificationMetadata),
) -> option.Option(String) {
  let user_agent = option_or_default("", metadata_value(metadata, "user_agent"))

  let normalized = user_agent |> string.trim |> string.lowercase

  case string.is_empty(normalized) {
    True -> option.Some("Missing user agent")
    False ->
      case find_suspicious_keyword(normalized, suspicious_user_agent_keywords) {
        option.Some(keyword) ->
          option.Some("Suspicious user agent (" <> keyword <> ")")
        option.None -> option.None
      }
  }
}

fn metadata_value(
  metadata: List(verifications.PendingVerificationMetadata),
  key: String,
) -> option.Option(String) {
  list.fold(metadata, option.None, fn(acc, entry) {
    case acc {
      option.Some(_) -> acc
      option.None ->
        case entry {
          verifications.PendingVerificationMetadata(
            key: entry_key,
            value: entry_value,
          ) ->
            case entry_key == key {
              True -> option.Some(entry_value)
              False -> option.None
            }
        }
    }
  })
}

fn option_or_default(default: String, value: option.Option(String)) -> String {
  case value {
    option.Some(v) -> v
    option.None -> default
  }
}

fn find_suspicious_keyword(
  normalized: String,
  keywords: List(String),
) -> option.Option(String) {
  list.fold(keywords, option.None, fn(acc, keyword) {
    case acc {
      option.Some(_) -> acc
      option.None ->
        case string.contains(normalized, keyword) {
          True -> option.Some(keyword)
          False -> option.None
        }
    }
  })
}

fn format_timestamp(timestamp: String) -> String {
  case string.split(timestamp, "T") {
    [date_part, time_part] -> {
      let time_clean = case string.split(time_part, ".") {
        [hms, _] -> hms
        _ -> time_part
      }
      let time_clean = string.replace(time_clean, "Z", "")

      case string.split(time_clean, ":") {
        [hour, minute, _] -> date_part <> " " <> hour <> ":" <> minute
        _ -> timestamp
      }
    }
    _ -> timestamp
  }
}

fn empty_state() {
  ui.card_empty([
    ui.text_muted("No pending verifications"),
    ui.text_small_muted("All registration requests have been processed"),
  ])
}

fn error_view(err: common.ApiError) {
  let #(title, message) = case err {
    common.Unauthorized -> #(
      "Authentication Required",
      "Your session has expired. Please log in again.",
    )
    common.Forbidden(msg) -> #("Permission Denied", msg)
    common.NotFound -> #(
      "Not Found",
      "Pending verifications could not be retrieved.",
    )
    common.ServerError -> #(
      "Server Error",
      "An internal server error occurred. Please try again later.",
    )
    common.NetworkError -> #(
      "Network Error",
      "Could not connect to the API. Please try again later.",
    )
  }

  h.div([a.class("max-w-4xl mx-auto")], [
    h.div([a.class("bg-red-50 border border-red-200 rounded-lg p-8")], [
      h.div([a.class("flex items-start gap-4")], [
        h.div(
          [
            a.class(
              "flex-shrink-0 w-12 h-12 bg-red-100 rounded-full flex items-center justify-center",
            ),
          ],
          [
            h.span([a.class("text-red-600 title-sm")], [
              element.text("!"),
            ]),
          ],
        ),
        h.div([a.class("flex-1")], [
          h.h2([a.class("title-sm text-red-900 mb-2")], [
            element.text(title),
          ]),
          h.p([a.class("text-red-700")], [element.text(message)]),
        ]),
      ]),
    ]),
  ])
}
