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

import fluxer_admin/api/archives
import fluxer_admin/api/common
import fluxer_admin/components/date_time
import fluxer_admin/components/errors
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/ui
import fluxer_admin/web.{type Context, type Session, href}
import gleam/int
import gleam/list
import gleam/option
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import wisp.{type Response}

pub fn view(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
  subject_type: String,
  subject_id: option.Option(String),
) -> Response {
  let result =
    archives.list_archives(ctx, session, subject_type, subject_id, False)

  let content = case result {
    Ok(response) ->
      render_archives(ctx, response.archives, subject_type, subject_id)
    Error(err) -> errors.api_error_view(ctx, err, option.None, option.None)
  }

  let html =
    layout.page(
      "Archives",
      "archives",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

fn render_archives(
  ctx: Context,
  archives: List(archives.Archive),
  subject_type: String,
  subject_id: option.Option(String),
) {
  let filter_hint = case subject_id {
    option.Some(id) -> " for " <> subject_type <> " " <> id
    option.None -> ""
  }

  h.div([a.class("max-w-7xl mx-auto")], [
    ui.flex_row_between([
      ui.heading_page("Archives" <> filter_hint),
      h.div([], []),
    ]),
    case list.is_empty(archives) {
      True ->
        h.div(
          [
            a.class(
              "bg-white border border-dashed border-neutral-300 rounded-lg p-8 text-center",
            ),
          ],
          [
            h.p([a.class("text-neutral-600")], [
              element.text("No archives found" <> filter_hint <> "."),
            ]),
          ],
        )
      False -> render_table(ctx, archives)
    },
  ])
}

fn render_table(ctx: Context, archives: List(archives.Archive)) {
  h.div(
    [a.class("bg-white border border-neutral-200 rounded-lg overflow-hidden")],
    [
      h.table([a.class("min-w-full divide-y divide-neutral-200")], [
        h.thead([a.class("bg-neutral-50")], [
          h.tr([], [
            header_cell("Subject"),
            header_cell("Requested By"),
            header_cell("Requested At"),
            header_cell("Status"),
            header_cell("Actions"),
          ]),
        ]),
        h.tbody(
          [a.class("divide-y divide-neutral-200")],
          list.map(archives, fn(archive) {
            h.tr([], [
              h.td(
                [
                  a.class(
                    "px-6 py-4 whitespace-nowrap text-sm text-neutral-900",
                  ),
                ],
                [
                  h.div([], [
                    h.div([a.class("font-semibold")], [
                      element.text(
                        archive.subject_type <> " " <> archive.subject_id,
                      ),
                    ]),
                    h.div([a.class("text-neutral-500 text-xs")], [
                      element.text("Archive ID: " <> archive.archive_id),
                    ]),
                  ]),
                ],
              ),
              h.td(
                [
                  a.class(
                    "px-6 py-4 whitespace-nowrap text-sm text-neutral-900",
                  ),
                ],
                [
                  element.text(archive.requested_by),
                ],
              ),
              h.td(
                [
                  a.class(
                    "px-6 py-4 whitespace-nowrap text-sm text-neutral-900",
                  ),
                ],
                [
                  element.text(date_time.format_timestamp(archive.requested_at)),
                ],
              ),
              h.td([a.class("px-6 py-4 text-sm")], [
                h.div([a.class("flex items-center gap-2")], [
                  h.span(
                    [
                      a.class(
                        "inline-flex items-center px-2 py-1 rounded-full bg-neutral-100 text-neutral-800 text-xs",
                      ),
                    ],
                    [element.text(status_label(archive))],
                  ),
                  h.span([a.class("text-neutral-600 text-xs")], [
                    element.text(int.to_string(archive.progress_percent) <> "%"),
                  ]),
                ]),
                case archive.progress_step {
                  option.Some(step) ->
                    h.div([a.class("text-xs text-neutral-500 mt-1")], [
                      element.text(step),
                    ])
                  option.None -> element.none()
                },
              ]),
              h.td([a.class("px-6 py-4 whitespace-nowrap text-sm")], [
                case archive.completed_at {
                  option.Some(_) ->
                    h.a(
                      [
                        href(
                          ctx,
                          "/archives/download?subject_type="
                            <> archive.subject_type
                            <> "&subject_id="
                            <> archive.subject_id
                            <> "&archive_id="
                            <> archive.archive_id,
                        ),
                        a.class(
                          "inline-flex items-center px-3 py-1.5 text-sm font-medium text-white bg-neutral-900 rounded-md hover:bg-neutral-800 transition-colors",
                        ),
                      ],
                      [element.text("Download")],
                    )
                  option.None ->
                    h.span([a.class("text-neutral-500")], [
                      element.text("Not ready"),
                    ])
                },
              ]),
            ])
          }),
        ),
      ]),
    ],
  )
}

fn header_cell(label: String) {
  h.th(
    [
      a.class(
        "px-6 py-3 text-left text-xs font-medium text-neutral-700 uppercase tracking-wider",
      ),
    ],
    [element.text(label)],
  )
}

fn status_label(archive: archives.Archive) -> String {
  case archive.failed_at {
    option.Some(_) -> "Failed"
    option.None -> {
      case archive.completed_at {
        option.Some(_) -> "Completed"
        option.None -> "In Progress"
      }
    }
  }
}
