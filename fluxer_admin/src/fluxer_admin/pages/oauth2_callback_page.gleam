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

import fluxer_admin/constants
import fluxer_admin/log
import fluxer_admin/oauth2
import fluxer_admin/session
import fluxer_admin/web.{type Context, prepend_base_path}
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option
import gleam/string
import gleam/uri
import wisp.{type Request, type Response}

pub fn handle(req: Request, ctx: Context) -> Response {
  let query = wisp.get_query(req)
  let code = list.key_find(query, "code") |> option.from_result
  let state = list.key_find(query, "state") |> option.from_result
  let error = list.key_find(query, "error") |> option.from_result
  let stored_state = case wisp.get_cookie(req, "oauth_state", wisp.Signed) {
    Ok(cookie) -> option.Some(cookie)
    Error(_) -> option.None
  }

  case error {
    option.Some(err) -> {
      log.error("OAuth2 callback error: " <> err)
      wisp.redirect(prepend_base_path(ctx, "/login?error=oauth_failed"))
      |> wisp.set_cookie(req, "session", "", wisp.Signed, 0)
      |> wisp.set_cookie(req, "oauth_state", "", wisp.Signed, 0)
    }
    option.None ->
      case code, state, stored_state {
        option.Some(code), option.Some(s), option.Some(stored) if s == stored -> {
          let token_url = ctx.api_endpoint <> "/oauth2/token"
          log.debug("OAuth2 callback: code received; building token request")
          let base_params = [
            #("grant_type", "authorization_code"),
            #("code", code),
            #("redirect_uri", ctx.oauth_redirect_uri),
          ]
          let body =
            base_params
            |> list.map(fn(p) { p.0 <> "=" <> uri.percent_encode(p.1) })
            |> string.join("&")

          let assert Ok(req1) = request.to(token_url)
          let basic_auth =
            "Basic "
            <> oauth2.base64_encode_string(
              ctx.oauth_client_id <> ":" <> ctx.oauth_client_secret,
            )
          log.debug("Posting /oauth2/token with Basic auth and form body")
          let req1 =
            req1
            |> request.set_method(http.Post)
            |> request.set_header(
              "content-type",
              "application/x-www-form-urlencoded",
            )
            |> request.set_header("authorization", basic_auth)
            |> request.set_body(body)

          case httpc.send(req1) {
            Ok(token_resp) if token_resp.status == 200 -> {
              log.debug("[oauth2_callback] Token response 200 OK")
              let token_decoder = {
                use access_token <- decode.field("access_token", decode.string)
                use _refresh_token <- decode.optional_field(
                  "refresh_token",
                  option.None,
                  decode.optional(decode.string),
                )
                decode.success(access_token)
              }

              case json.parse(token_resp.body, token_decoder) {
                Ok(access_token) -> {
                  log.debug(
                    "[oauth2_callback] Parsed access_token successfully",
                  )
                  let info_url = ctx.api_endpoint <> "/users/@me"
                  let assert Ok(req2) = request.to(info_url)
                  let req2 =
                    req2
                    |> request.set_method(http.Get)
                    |> request.set_header(
                      "authorization",
                      "Bearer " <> access_token,
                    )

                  case httpc.send(req2) {
                    Ok(info_resp) if info_resp.status == 200 -> {
                      log.debug("[oauth2_callback] Userinfo response 200 OK")
                      let info_decoder = {
                        use id <- decode.field("id", decode.string)
                        use acls <- decode.optional_field(
                          "acls",
                          [],
                          decode.list(decode.string),
                        )
                        decode.success(#(id, acls))
                      }

                      case json.parse(info_resp.body, info_decoder) {
                        Ok(#(user_id, acls)) -> {
                          log.debug(
                            "[oauth2_callback] Parsed user_id: "
                            <> user_id
                            <> " acls=["
                            <> string.join(acls, ",")
                            <> "]",
                          )
                          let has_admin_acl =
                            list.contains(acls, constants.acl_authenticate)
                            || list.contains(acls, constants.acl_wildcard)

                          case has_admin_acl {
                            True ->
                              case session.create(ctx, user_id, access_token) {
                                Ok(cookie) -> {
                                  let redirect_url =
                                    prepend_base_path(ctx, "/dashboard")
                                  log.debug(
                                    "[oauth2_callback] Session created, redirecting to: "
                                    <> redirect_url,
                                  )
                                  wisp.redirect(redirect_url)
                                  |> wisp.set_cookie(
                                    req,
                                    "session",
                                    cookie,
                                    wisp.Signed,
                                    60 * 60 * 24 * 7,
                                  )
                                  |> wisp.set_cookie(
                                    req,
                                    "oauth_state",
                                    "",
                                    wisp.Signed,
                                    0,
                                  )
                                }
                                Error(_) -> {
                                  log.error(
                                    "[oauth2_callback] Failed to create session!",
                                  )
                                  wisp.redirect(prepend_base_path(ctx, "/login"))
                                }
                              }
                            False -> {
                              log.error(
                                "[oauth2_callback] User missing admin ACLs",
                              )
                              wisp.redirect(prepend_base_path(
                                ctx,
                                "/login?error=missing_admin_acl",
                              ))
                              |> wisp.set_cookie(
                                req,
                                "oauth_state",
                                "",
                                wisp.Signed,
                                0,
                              )
                            }
                          }
                        }
                        Error(_) -> {
                          log.error(
                            "[oauth2_callback] Failed to parse users/@me response",
                          )
                          wisp.redirect(prepend_base_path(ctx, "/login"))
                          |> wisp.set_cookie(
                            req,
                            "oauth_state",
                            "",
                            wisp.Signed,
                            0,
                          )
                        }
                      }
                    }
                    _ -> {
                      log.error(
                        "[oauth2_callback] Userinfo request failed or non-200",
                      )
                      wisp.redirect(prepend_base_path(ctx, "/login"))
                      |> wisp.set_cookie(req, "oauth_state", "", wisp.Signed, 0)
                    }
                  }
                }
                Error(_) -> {
                  log.error("[oauth2_callback] Failed to parse token response")
                  wisp.redirect(prepend_base_path(ctx, "/login"))
                  |> wisp.set_cookie(req, "oauth_state", "", wisp.Signed, 0)
                }
              }
            }
            _ -> {
              log.error("[oauth2_callback] Token request failed or non-200")
              wisp.redirect(prepend_base_path(ctx, "/login"))
              |> wisp.set_cookie(req, "session", "", wisp.Signed, 0)
              |> wisp.set_cookie(req, "oauth_state", "", wisp.Signed, 0)
            }
          }
        }
        _, _, _ -> {
          log.error("[oauth2_callback] State mismatch or missing code/state")
          wisp.redirect(prepend_base_path(ctx, "/login?error=oauth_failed"))
          |> wisp.set_cookie(req, "session", "", wisp.Signed, 0)
          |> wisp.set_cookie(req, "oauth_state", "", wisp.Signed, 0)
        }
      }
  }
}
