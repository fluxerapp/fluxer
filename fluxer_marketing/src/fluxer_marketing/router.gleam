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

import fluxer_marketing/badge_proxy
import fluxer_marketing/geoip
import fluxer_marketing/help_center
import fluxer_marketing/locale
import fluxer_marketing/pages/careers_page
import fluxer_marketing/pages/company_page
import fluxer_marketing/pages/donate_page
import fluxer_marketing/pages/download_page
import fluxer_marketing/pages/guidelines_page
import fluxer_marketing/pages/help_article_page
import fluxer_marketing/pages/help_category_page
import fluxer_marketing/pages/help_center_page
import fluxer_marketing/pages/help_search_page
import fluxer_marketing/pages/home_page
import fluxer_marketing/pages/not_found_page
import fluxer_marketing/pages/partners_page
import fluxer_marketing/pages/plutonium_page
import fluxer_marketing/pages/press_page
import fluxer_marketing/pages/privacy_page
import fluxer_marketing/pages/terms_page
import fluxer_marketing/sitemap
import fluxer_marketing/web.{type Context, prepend_base_path}
import gleam/http
import gleam/list
import gleam/option
import gleam/string
import gleam/uri
import wisp.{type Request, type Response}

pub fn handle_request(req: Request, ctx: Context) -> Response {
  case wisp.path_segments(req) {
    [] -> home_page.render(req, ctx)
    ["_locale"] -> handle_locale_change(req, ctx)
    ["_debug", "geoip"] -> handle_geoip_debug(req, ctx)

    ["api", "badges", "product-hunt"] ->
      badge_proxy.product_hunt(ctx.badge_cache)

    ["robots.txt"] -> handle_robots_txt()
    ["sitemap.xml"] -> handle_sitemap(ctx)
    ["terms"] -> terms_page.render(req, ctx)
    ["privacy"] -> privacy_page.render(req, ctx)
    ["guidelines"] -> guidelines_page.render(req, ctx)
    ["company-information"] -> company_page.render(req, ctx)
    ["careers"] -> careers_page.render(req, ctx)
    ["download"] -> download_page.render(req, ctx)
    ["donate"] -> donate_page.render(req, ctx)
    ["plutonium"] -> plutonium_page.render(req, ctx)
    ["partners"] -> partners_page.render(req, ctx)
    ["press"] -> press_page.render(req, ctx)
    ["help"] -> handle_help_root(ctx)
    ["help", "search"] -> handle_help_search(req, ctx)
    ["help", "articles", article_id] ->
      handle_help_article_without_locale(ctx, article_id)
    ["help", locale_code_or_category, "articles", article_id] ->
      handle_help_article_with_locale(
        req,
        ctx,
        locale_code_or_category,
        article_id,
      )
    ["help", locale_code_or_category] ->
      handle_help_locale_or_category(req, ctx, locale_code_or_category)
    ["help", locale_code, category] ->
      handle_help_locale_and_category(req, ctx, locale_code, category)
    _ -> not_found_page.render(req, ctx)
  }
}

fn handle_help_search(req: Request, ctx: Context) -> Response {
  case get_query_param(req, "q") {
    option.Some(query) ->
      case query {
        "" -> wisp.redirect(prepend_base_path(ctx, "/help"))
        _ -> help_search_page.render(req, ctx, query)
      }
    option.None -> wisp.redirect(prepend_base_path(ctx, "/help"))
  }
}

fn get_query_param(req: Request, param: String) -> option.Option(String) {
  case uri.parse_query(req.query |> option.unwrap("")) {
    Ok(params) -> {
      params
      |> list.find_map(fn(pair) {
        case pair {
          #(key, value) if key == param -> Ok(value)
          _ -> Error(Nil)
        }
      })
      |> option.from_result
    }
    Error(_) -> option.None
  }
}

fn handle_help_root(ctx: Context) -> Response {
  let locale_code = locale.get_code_from_locale(ctx.locale) |> string.lowercase
  wisp.redirect(prepend_base_path(ctx, "/help/" <> locale_code))
}

fn handle_help_locale_or_category(
  req: Request,
  ctx: Context,
  locale_code_or_category: String,
) -> Response {
  case locale.get_locale_from_code(locale_code_or_category) {
    Ok(detected_locale) -> {
      let updated_ctx = web.Context(..ctx, locale: detected_locale)
      help_center_page.render(req, updated_ctx)
    }
    Error(_) -> {
      let locale_code =
        locale.get_code_from_locale(ctx.locale) |> string.lowercase
      let redirect_path =
        "/help/" <> locale_code <> "/" <> locale_code_or_category
      let redirect_path = case req.query {
        option.Some(query) -> redirect_path <> "?" <> query
        option.None -> redirect_path
      }
      wisp.redirect(prepend_base_path(ctx, redirect_path))
    }
  }
}

fn handle_help_locale_and_category(
  req: Request,
  ctx: Context,
  locale_code: String,
  category: String,
) -> Response {
  case locale.get_locale_from_code(locale_code) {
    Ok(detected_locale) -> {
      let updated_ctx = web.Context(..ctx, locale: detected_locale)
      let search_query = get_query_param(req, "q")
      help_category_page.render(req, updated_ctx, category, search_query)
    }
    Error(_) -> not_found_page.render(req, ctx)
  }
}

fn handle_help_article_without_locale(
  ctx: Context,
  article_id: String,
) -> Response {
  let locale_code = locale.get_code_from_locale(ctx.locale) |> string.lowercase
  let redirect_path = "/help/" <> locale_code <> "/articles/" <> article_id
  wisp.redirect(prepend_base_path(ctx, redirect_path))
}

fn handle_help_article_with_locale(
  req: Request,
  ctx: Context,
  locale_code: String,
  article_id: String,
) -> Response {
  case locale.get_locale_from_code(locale_code) {
    Ok(article_locale) -> {
      let article_ctx = web.Context(..ctx, locale: article_locale)

      let help_data = help_center.load_help_articles(article_locale)

      case string.split(article_id, "-") {
        [snowflake_id, ..rest] -> {
          case help_center.get_article_by_snowflake(help_data, snowflake_id) {
            Ok(article) -> {
              let expected_slug = help_center.create_slug(article.title)
              let actual_slug = string.join(rest, "-")

              case rest {
                [] -> {
                  let correct_url =
                    "/help/"
                    <> string.lowercase(locale_code)
                    <> "/articles/"
                    <> snowflake_id
                    <> "-"
                    <> expected_slug
                  wisp.redirect(prepend_base_path(ctx, correct_url))
                }
                _ -> {
                  case actual_slug == expected_slug {
                    True ->
                      help_article_page.render_with_locale(
                        req,
                        article_ctx,
                        article,
                        help_data,
                      )
                    False -> {
                      case string.is_empty(actual_slug) {
                        True -> {
                          let correct_url =
                            "/help/"
                            <> string.lowercase(locale_code)
                            <> "/articles/"
                            <> snowflake_id
                            <> "-"
                            <> expected_slug
                          wisp.redirect(prepend_base_path(ctx, correct_url))
                        }
                        False ->
                          help_article_page.render_with_locale(
                            req,
                            article_ctx,
                            article,
                            help_data,
                          )
                      }
                    }
                  }
                }
              }
            }
            Error(_) -> not_found_page.render(req, ctx)
          }
        }
        [] -> not_found_page.render(req, ctx)
      }
    }
    Error(_) -> not_found_page.render(req, ctx)
  }
}

fn handle_robots_txt() -> Response {
  wisp.response(200)
  |> wisp.set_header("content-type", "text/plain; charset=utf-8")
  |> wisp.string_body("User-agent: *\nAllow: /\n")
}

fn handle_sitemap(ctx: Context) -> Response {
  let xml = sitemap.generate(ctx.base_url)

  wisp.response(200)
  |> wisp.set_header("content-type", "application/xml; charset=utf-8")
  |> wisp.string_body(xml)
}

fn handle_locale_change(req: Request, ctx: Context) -> Response {
  case req.method {
    http.Post -> {
      use form <- wisp.require_form(req)
      case form.values {
        [#("locale", locale_code), #("redirect", redirect_path), ..] -> {
          case locale.get_locale_from_code(locale_code) {
            Ok(new_locale) -> {
              let updated_redirect_path =
                update_locale_in_path(redirect_path, new_locale)

              wisp.redirect(prepend_base_path(ctx, updated_redirect_path))
              |> wisp.set_cookie(
                req,
                "locale",
                locale.get_code_from_locale(new_locale),
                wisp.PlainText,
                60 * 60 * 24 * 365,
              )
            }
            Error(_) -> wisp.bad_request()
          }
        }
        _ -> wisp.bad_request()
      }
    }
    _ -> wisp.method_not_allowed([http.Post])
  }
}

pub fn update_locale_in_path(path: String, new_locale: locale.Locale) -> String {
  let new_locale_code =
    locale.get_code_from_locale(new_locale) |> string.lowercase
  let path_parts = string.split(path, "/") |> list.filter(fn(p) { p != "" })

  case path_parts {
    ["help", old_locale, "articles", article_id_and_slug, ..extra] -> {
      case locale.get_locale_from_code(old_locale) {
        Ok(_) -> {
          let snowflake_id =
            string.split(article_id_and_slug, "-") |> list.first
          let article_slug = case snowflake_id {
            Ok(id) -> {
              let help_data = help_center.load_help_articles(new_locale)
              case help_center.get_article_by_snowflake(help_data, id) {
                Ok(article) -> help_center.create_slug(article.title)
                Error(_) ->
                  article_id_and_slug
                  |> string.split("-")
                  |> list.drop(1)
                  |> string.join("-")
              }
            }
            Error(_) -> ""
          }

          let base_id = case snowflake_id {
            Ok(id) -> id
            Error(_) -> article_id_and_slug
          }

          let rebuilt =
            "/help/"
            <> new_locale_code
            <> "/articles/"
            <> base_id
            <> case string.is_empty(article_slug) {
              True -> ""
              False -> "-" <> article_slug
            }

          case extra {
            [] -> rebuilt
            _ -> rebuilt <> "/" <> string.join(extra, "/")
          }
        }
        Error(_) -> path
      }
    }
    ["help", old_locale, ..rest] -> {
      case locale.get_locale_from_code(old_locale) {
        Ok(_) -> "/help/" <> new_locale_code <> "/" <> string.join(rest, "/")
        Error(_) -> path
      }
    }
    _ -> path
  }
}

fn handle_geoip_debug(req: Request, ctx: Context) -> Response {
  let json_body = geoip.debug_info(req, ctx.geoip_host)
  wisp.response(200)
  |> wisp.set_header("content-type", "application/json; charset=utf-8")
  |> wisp.string_body(json_body)
}
