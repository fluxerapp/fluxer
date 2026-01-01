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

import fluxer_admin/acl
import fluxer_admin/api/codes
import fluxer_admin/api/common
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/slider_control
import fluxer_admin/components/ui
import fluxer_admin/constants
import fluxer_admin/web.{type Context, type Session}
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import wisp.{type Request, type Response}

const max_gift_codes = 100

const default_gift_count = 10

const gift_product_options = [
  #("gift_1_month", "Gift - 1 Month subscription"),
  #("gift_1_year", "Gift - 1 Year subscription"),
  #("gift_visionary", "Gift - Visionary lifetime"),
]

pub fn view(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
  admin_acls: List(String),
) -> Response {
  render_page(
    ctx,
    session,
    current_admin,
    flash_data,
    admin_acls,
    default_gift_count,
    option.None,
    option.None,
  )
}

fn render_page(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
  admin_acls: List(String),
  selected_count: Int,
  generation_result: option.Option(flash.Flash),
  generated_codes: option.Option(List(String)),
) -> Response {
  let has_permission =
    acl.has_permission(admin_acls, constants.acl_gift_codes_generate)
  let content = case has_permission {
    True ->
      render_generator_card(generated_codes, generation_result, selected_count)
    False -> render_access_denied()
  }

  let html =
    layout.page(
      "Gift Codes",
      "gift-codes",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )

  wisp.html_response(element.to_document_string(html), 200)
}

fn render_generator_card(
  generated_codes: option.Option(List(String)),
  generation_result: option.Option(flash.Flash),
  selected_count: Int,
) -> element.Element(a) {
  let codes_value = case generated_codes {
    option.Some(codes) -> string.join(codes, "\n")
    option.None -> ""
  }

  let status_view = flash.view(generation_result)

  h.div([a.class("max-w-7xl mx-auto space-y-6")], [
    h.div([a.class("space-y-6")], [
      ui.card(ui.PaddingMedium, [
        h.div([a.class("space-y-2")], [
          h.h1([a.class("text-2xl font-semibold text-neutral-900")], [
            element.text("Generate Gift Codes"),
          ]),
        ]),
        status_view,
        h.form(
          [
            a.id("gift-form"),
            a.class("space-y-4"),
            a.method("POST"),
            a.action("?action=generate"),
          ],
          [
            h.div([a.class("space-y-4")], [
              h.div([a.class("flex items-center justify-between")], [
                h.label([a.class("text-sm font-medium text-neutral-800")], [
                  element.text("How many codes"),
                ]),
                h.span([a.class("text-xs text-neutral-500")], [
                  element.text("Range: 1-" <> int.to_string(max_gift_codes)),
                ]),
              ]),
              h.div(
                [a.class("space-y-4")],
                list.append(
                  slider_control.range_slider_section(
                    "gift-count-slider",
                    "gift-count-value",
                    1,
                    max_gift_codes,
                    selected_count,
                  ),
                  [
                    h.p([a.class("text-xs text-neutral-500")], [
                      element.text(
                        "Select the number of gift codes to generate.",
                      ),
                    ]),
                    h.div([a.class("space-y-1")], [
                      h.label(
                        [a.class("text-sm font-medium text-neutral-800")],
                        [
                          element.text("Product"),
                        ],
                      ),
                      h.select(
                        [
                          a.name("product_type"),
                          a.class(
                            "w-full rounded-lg border border-neutral-200 px-3 py-2 text-sm text-neutral-900 focus:outline-none focus:ring-2 focus:ring-neutral-900",
                          ),
                        ],
                        list.map(gift_product_options, fn(option) {
                          let value = option.0
                          let label = option.1
                          h.option([a.value(value)], label)
                        }),
                      ),
                      h.p([a.class("text-xs text-neutral-500")], [
                        element.text(
                          "Generated codes are rendered as https://fluxer.gift/<code>.",
                        ),
                      ]),
                    ]),
                    h.button(
                      [
                        a.type_("submit"),
                        a.class(
                          "px-4 py-2 bg-neutral-900 text-white rounded-lg text-sm font-medium hover:bg-neutral-800 transition-colors",
                        ),
                      ],
                      [element.text("Generate Gift Codes")],
                    ),
                  ],
                ),
              ),
            ]),
            h.div([a.class("space-y-2")], [
              h.label([a.class("text-sm font-medium text-neutral-800")], [
                element.text("Generated URLs"),
              ]),
              h.textarea(
                [
                  a.readonly(True),
                  a.attribute("rows", "10"),
                  a.class(
                    "w-full border border-neutral-200 rounded-lg px-4 py-3 text-sm text-neutral-900 bg-neutral-50 focus:outline-none focus:ring-2 focus:ring-neutral-900",
                  ),
                  a.placeholder(
                    "Full gift URLs will appear here after generation.",
                  ),
                ],
                codes_value,
              ),
              h.p([a.class("text-xs text-neutral-500")], [
                element.text("Copy one URL per line when sharing codes."),
              ]),
            ]),
          ],
        ),
        slider_control.slider_sync_script(
          "gift-count-slider",
          "gift-count-value",
        ),
      ]),
    ]),
  ])
}

fn render_access_denied() -> element.Element(a) {
  ui.card(ui.PaddingMedium, [
    h.h1([a.class("text-2xl font-semibold text-neutral-900")], [
      element.text("Gift Codes"),
    ]),
    h.p([a.class("text-sm text-neutral-600")], [
      element.text("You do not have permission to generate gift codes."),
    ]),
  ])
}

pub fn handle_action(
  req: Request,
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  admin_acls: List(String),
  action: option.Option(String),
) -> Response {
  use form_data <- wisp.require_form(req)

  case action {
    option.Some("generate") ->
      handle_generate(ctx, session, current_admin, admin_acls, form_data)
    _ ->
      render_page(
        ctx,
        session,
        current_admin,
        option.None,
        admin_acls,
        default_gift_count,
        option.Some(flash.Flash("Unknown action", flash.Error)),
        option.None,
      )
  }
}

fn handle_generate(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  admin_acls: List(String),
  form_data: wisp.FormData,
) -> Response {
  case acl.has_permission(admin_acls, constants.acl_gift_codes_generate) {
    False ->
      render_page(
        ctx,
        session,
        current_admin,
        option.None,
        admin_acls,
        default_gift_count,
        option.Some(flash.Flash("Permission denied", flash.Error)),
        option.None,
      )
    True ->
      case parse_count(form_data), parse_product_type(form_data) {
        option.Some(value), option.Some(product) ->
          case value < 1 || value > max_gift_codes {
            True ->
              render_page(
                ctx,
                session,
                current_admin,
                option.None,
                admin_acls,
                value,
                option.Some(flash.Flash(
                  "Count must be between 1 and "
                    <> int.to_string(max_gift_codes),
                  flash.Error,
                )),
                option.None,
              )
            False ->
              case codes.generate_gift_codes(ctx, session, value, product) {
                Ok(generated) ->
                  render_page(
                    ctx,
                    session,
                    current_admin,
                    option.None,
                    admin_acls,
                    value,
                    option.Some(flash.Flash(
                      "Generated "
                        <> int.to_string(list.length(generated))
                        <> " gift codes.",
                      flash.Success,
                    )),
                    option.Some(generated),
                  )
                Error(err) ->
                  render_page(
                    ctx,
                    session,
                    current_admin,
                    option.None,
                    admin_acls,
                    value,
                    option.Some(flash.Flash(api_error_message(err), flash.Error)),
                    option.None,
                  )
              }
          }
        option.None, _ ->
          render_page(
            ctx,
            session,
            current_admin,
            option.None,
            admin_acls,
            default_gift_count,
            option.Some(flash.Flash("Count is required", flash.Error)),
            option.None,
          )
        option.Some(value), option.None ->
          render_page(
            ctx,
            session,
            current_admin,
            option.None,
            admin_acls,
            value,
            option.Some(flash.Flash("Product type is required", flash.Error)),
            option.None,
          )
      }
  }
}

fn parse_count(form_data: wisp.FormData) -> option.Option(Int) {
  let value =
    list.key_find(form_data.values, "count")
    |> option.from_result

  case value {
    option.Some(str) ->
      case int.parse(str) {
        Ok(num) -> option.Some(num)
        Error(_) -> option.None
      }
    option.None -> option.None
  }
}

fn parse_product_type(form_data: wisp.FormData) -> option.Option(String) {
  let raw =
    list.key_find(form_data.values, "product_type")
    |> option.from_result

  case raw {
    option.Some(value) ->
      case list.any(gift_product_options, fn(option) { option.0 == value }) {
        True -> option.Some(value)
        False -> option.None
      }
    option.None -> option.None
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
