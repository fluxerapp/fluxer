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
import fluxer_admin/api/common
import fluxer_admin/api/verifications
import fluxer_admin/avatar
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/ui
import fluxer_admin/constants
import fluxer_admin/user
import fluxer_admin/web.{type Context, type Session, action, href}
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

fn selection_toolbar() -> element.Element(a) {
  h.div(
    [
      a.class(
        "mt-4 flex items-center justify-between gap-3 bg-neutral-50 border border-neutral-200 rounded-lg px-3 py-2",
      ),
      a.attribute("data-selection-toolbar", "true"),
    ],
    [
      h.div([a.class("flex items-center gap-3")], [
        h.input([
          a.type_("checkbox"),
          a.attribute("data-select-all", "true"),
          a.class("h-4 w-4 rounded border-neutral-300"),
        ]),
        h.span([a.class("text-sm text-neutral-700")], [
          element.text("Select all visible"),
        ]),
      ]),
      h.div([a.class("flex items-center gap-3 flex-wrap justify-end")], [
        h.span(
          [
            a.attribute("data-selected-count", "true"),
            a.class("text-sm text-neutral-600"),
          ],
          [element.text("0 selected")],
        ),
        h.div([a.class("flex items-center gap-2")], [
          h.button(
            [
              a.attribute("data-bulk-action", "reject"),
              a.class(
                "px-3 py-1.5 bg-red-600 text-white rounded-lg text-sm disabled:opacity-40 disabled:cursor-not-allowed",
              ),
              a.disabled(True),
            ],
            [element.text("Reject selected")],
          ),
          h.button(
            [
              a.attribute("data-bulk-action", "approve"),
              a.class(
                "px-3 py-1.5 bg-green-600 text-white rounded-lg text-sm disabled:opacity-40 disabled:cursor-not-allowed",
              ),
              a.disabled(True),
            ],
            [element.text("Approve selected")],
          ),
        ]),
      ]),
    ],
  )
}

pub fn view(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
) -> Response {
  let limit = 50
  let result = verifications.list_pending_verifications(ctx, session, limit)
  let admin_acls = case current_admin {
    option.Some(admin) -> admin.acls
    option.None -> []
  }
  let can_review =
    acl.has_permission(admin_acls, constants.acl_pending_verification_review)

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
                      a.attribute("data-remaining-total", "true"),
                    ],
                    [element.text(int.to_string(count) <> " remaining")],
                  )
              },
            ]),
          ]),
          case can_review {
            True -> selection_toolbar()
            False -> element.none()
          },
          case list.is_empty(response.pending_verifications) {
            True -> empty_state()
            False ->
              h.div([a.class("mt-4")], [
                h.div(
                  [
                    a.class("grid gap-4 md:grid-cols-2 xl:grid-cols-3"),
                    a.attribute("data-select-grid", "true"),
                  ],
                  list.map(response.pending_verifications, fn(pv) {
                    render_pending_verification_card(ctx, pv, can_review)
                  }),
                ),
              ])
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
      h.div([], [content, pending_verifications_script()]),
    )
  wisp.html_response(element.to_document_string(html), 200)
}

pub fn view_fragment(
  ctx: Context,
  session: Session,
  page: Int,
  can_review: Bool,
) -> Response {
  let limit = 50
  let _offset = page * limit
  let result = verifications.list_pending_verifications(ctx, session, limit)

  case result {
    Ok(response) -> {
      let fragment =
        h.div(
          [a.class("grid gap-4 md:grid-cols-2 xl:grid-cols-3")],
          list.map(response.pending_verifications, fn(pv) {
            render_pending_verification_card(ctx, pv, can_review)
          }),
        )

      wisp.html_response(element.to_document_string(fragment), 200)
    }
    Error(_) -> {
      let empty =
        h.div([a.class("grid gap-4 md:grid-cols-2 xl:grid-cols-3")], [])

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
  let admin_acls = case current_admin {
    option.Some(admin) -> admin.acls
    option.None -> []
  }
  let can_review =
    acl.has_permission(admin_acls, constants.acl_pending_verification_review)

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
                [
                  a.class("mt-4 max-w-3xl"),
                  a.attribute("data-select-grid", "true"),
                ],
                [
                  render_pending_verification_card(ctx, pv, can_review),
                ],
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
      h.div([], [content, pending_verifications_script()]),
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
  can_review: Bool,
) -> element.Element(a) {
  let metadata_warning = user_agent_warning(pv.metadata)

  h.div(
    [
      a.class(
        "bg-white border border-neutral-200 rounded-xl shadow-sm p-6 focus:outline-none focus:ring-2 focus:ring-neutral-900",
      ),
      a.tabindex(0),
      a.attribute("data-select-card", pv.user_id),
    ],
    [
      h.div([a.class("flex items-start gap-4 mb-6")], [
        case can_review {
          True ->
            h.input([
              a.type_("checkbox"),
              a.class("h-4 w-4 mt-1.5 rounded border-neutral-300"),
              a.attribute("data-select-checkbox", pv.user_id),
            ])
          False -> element.none()
        },
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
        h.div([a.class("flex flex-col items-end gap-2 ml-auto")], [
          case metadata_warning {
            option.Some(msg) -> ui.pill(msg, ui.PillWarning)
            option.None -> element.none()
          },
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
      case can_review {
        True ->
          h.div([a.class("pt-4 border-t border-neutral-200")], [
            h.div([a.class("flex items-center justify-end gap-2 flex-wrap")], [
              h.form(
                [
                  a.method("post"),
                  action(ctx, "/pending-verifications?action=reject"),
                  a.attribute("data-async", "true"),
                  a.attribute("data-confirm", "Reject this registration?"),
                ],
                [
                  h.input([
                    a.type_("hidden"),
                    a.name("user_id"),
                    a.value(pv.user_id),
                  ]),
                  h.button(
                    [
                      a.type_("submit"),
                      a.attribute("data-action-type", "reject"),
                      a.attribute("accesskey", "r"),
                      a.class(
                        "px-4 py-2 bg-red-600 text-white rounded-lg label hover:bg-red-700 transition-colors",
                      ),
                    ],
                    [element.text("Reject")],
                  ),
                ],
              ),
              h.form(
                [
                  a.method("post"),
                  action(ctx, "/pending-verifications?action=approve"),
                  a.attribute("data-async", "true"),
                ],
                [
                  h.input([
                    a.type_("hidden"),
                    a.name("user_id"),
                    a.value(pv.user_id),
                  ]),
                  h.button(
                    [
                      a.type_("submit"),
                      a.attribute("data-action-type", "approve"),
                      a.attribute("accesskey", "a"),
                      a.class(
                        "px-4 py-2 bg-green-600 text-white rounded-lg label hover:bg-green-700 transition-colors",
                      ),
                    ],
                    [element.text("Approve")],
                  ),
                ],
              ),
            ]),
          ])
        False ->
          h.div([a.class("text-sm text-neutral-500 pt-2")], [
            element.text("You need pending_verification:review to act"),
          ])
      },
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

  h.div([a.class("flex flex-col gap-0.5 text-xs text-neutral-600")], [
    h.div([], [element.text("Display Name: " <> display_name)]),
    h.div([], [element.text("IP: " <> ip_display)]),
    h.div([], [element.text("Location: " <> location)]),
    case ip_reverse {
      option.Some(reverse) ->
        h.div([], [element.text("Reverse DNS: " <> reverse)])
      option.None -> element.none()
    },
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

fn pending_verifications_script() -> element.Element(a) {
  let js =
    "
(function () {
  const grid = document.querySelector('[data-select-grid]');
  if (!grid) return;
  const toolbar = document.querySelector('[data-selection-toolbar]');
  const remainingEl = document.querySelector('[data-remaining-total]');

  const countEl = toolbar?.querySelector('[data-selected-count]') || null;
  const selectAll = toolbar?.querySelector('[data-select-all]') || null;
  const bulkButtons = toolbar
    ? Array.from(toolbar.querySelectorAll('[data-bulk-action]'))
    : [];

  function showToast(message, variant) {
    const box = document.createElement('div');
    box.className = 'fixed left-4 right-4 bottom-4 z-50';
    box.innerHTML =
      '<div class=\"max-w-xl mx-auto\">' +
      '<div class=\"px-4 py-3 rounded-lg shadow border ' +
      (variant === 'success'
        ? 'bg-green-50 border-green-200 text-green-800'
        : 'bg-red-50 border-red-200 text-red-800') +
      '\">' +
      '<div class=\"text-sm font-semibold\">' +
      (variant === 'success' ? 'Saved' : 'Action failed') +
      '</div>' +
      '<div class=\"text-sm mt-1 break-words\">' + (message || 'OK') + '</div>' +
      '</div></div>';
    document.body.appendChild(box);
    setTimeout(() => box.remove(), 4200);
  }

  function updateRemaining() {
    if (!remainingEl) return;
    const total = grid.querySelectorAll('[data-select-card]').length;
    remainingEl.textContent = total.toString() + ' remaining';
  }

  function updateSelection() {
    const boxes = Array.from(grid.querySelectorAll('[data-select-checkbox]'));
    const selected = boxes.filter((b) => b.checked);
    if (countEl) countEl.textContent = selected.length + ' selected';
    bulkButtons.forEach((btn) => (btn.disabled = selected.length === 0));
    if (selectAll) {
      selectAll.checked = selected.length > 0 && selected.length === boxes.length;
      selectAll.indeterminate =
        selected.length > 0 && selected.length < boxes.length;
    }
  }

  function cardFor(id) {
    return grid.querySelector('[data-select-card=\"' + id + '\"]');
  }

  function removeCard(id) {
    const card = cardFor(id);
    if (card) card.remove();
    updateRemaining();
    updateSelection();
    if (grid.querySelectorAll('[data-select-card]').length === 0) {
      grid.innerHTML =
        '<div class=\"col-span-full border border-dashed border-neutral-200 rounded-lg p-8 text-center text-neutral-500\">All registration requests have been processed</div>';
    }
  }

  function setButtonLoading(btn, loading) {
    if (!btn) return;
    btn.disabled = loading;
    if (loading) {
      btn.dataset.originalText = btn.textContent;
      btn.textContent = 'Working…';
    } else if (btn.dataset.originalText) {
      btn.textContent = btn.dataset.originalText;
    }
  }

  async function submitForm(form) {
    const actionUrl = new URL(form.action, window.location.origin);
    actionUrl.searchParams.set('background', '1');
    const fd = new FormData(form);
    const body = new URLSearchParams();
    fd.forEach((v, k) => body.append(k, v));
    const resp = await fetch(actionUrl.toString(), {
      method: 'POST',
      headers: { 'content-type': 'application/x-www-form-urlencoded;charset=UTF-8' },
      body: body.toString(),
      credentials: 'same-origin',
    });
    if (!resp.ok && resp.status !== 204) {
      let txt = '';
      try { txt = await resp.text(); } catch (_) {}
      throw new Error(txt || 'Request failed (' + resp.status + ')');
    }
  }

  async function actOn(id, action) {
    const form = grid.querySelector(
      '[data-select-card=\"' + id + '\"] form[data-action-type=\"' + action + '\"]'
    );
    if (!form) throw new Error('Missing form for ' + action);
    await submitForm(form);
    removeCard(id);
  }

  async function handleBulk(action) {
    const boxes = Array.from(grid.querySelectorAll('[data-select-checkbox]:checked'));
    if (boxes.length === 0) return;
    const confirmMsg =
      action === 'reject'
        ? 'Reject ' + boxes.length + ' registration(s)?'
        : 'Approve ' + boxes.length + ' registration(s)?';
    if (!window.confirm(confirmMsg)) return;

    const button = toolbar.querySelector('[data-bulk-action=\"' + action + '\"]');
    setButtonLoading(button, true);
    try {
      for (const box of boxes) {
        const id = box.getAttribute('data-select-checkbox');
        if (!id) continue;
        await actOn(id, action);
      }
      showToast('Completed ' + action + ' for ' + boxes.length + ' item(s)', 'success');
    } catch (err) {
      showToast(err && err.message ? err.message : String(err), 'error');
    } finally {
      setButtonLoading(button, false);
    }
  }

  bulkButtons.forEach((btn) => {
    btn.addEventListener('click', (e) => {
      e.preventDefault();
      const action = btn.getAttribute('data-bulk-action');
      if (action) handleBulk(action);
    });
  });

  if (selectAll) {
    selectAll.addEventListener('change', (e) => {
      const boxes = grid.querySelectorAll('[data-select-checkbox]');
      boxes.forEach((b) => (b.checked = e.target.checked));
      updateSelection();
    });
  }

  grid.addEventListener('change', (e) => {
    const target = e.target;
    if (target && target.matches('[data-select-checkbox]')) {
      updateSelection();
    }
  });

  document.querySelectorAll('form[data-async]').forEach((form) => {
    form.addEventListener('submit', (e) => {
      e.preventDefault();
      const confirmMsg = form.getAttribute('data-confirm');
      if (confirmMsg && !window.confirm(confirmMsg)) return;
      const btn = form.querySelector('button[type=\"submit\"]');
      const id = form.querySelector('[name=\"user_id\"]')?.value;
      const action = btn?.getAttribute('data-action-type') || 'action';
      setButtonLoading(btn, true);
      submitForm(form)
        .then(() => {
          if (id) removeCard(id);
          showToast(
            (action === 'approve' ? 'Approved ' : 'Rejected ') + (id || 'item'),
            'success'
          );
        })
        .catch((err) => showToast(err && err.message ? err.message : String(err), 'error'))
        .finally(() => setButtonLoading(btn, false));
    });
  });

  updateRemaining();
  updateSelection();
})();
"

  h.script([a.attribute("defer", "defer")], js)
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
