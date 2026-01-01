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

import fluxer_marketing/help_center
import fluxer_marketing/i18n
import fluxer_marketing/markdown_utils
import fluxer_marketing/pages/layout
import fluxer_marketing/web.{type Context}
import kielet.{gettext as g_}
import lustre/element
import wisp

pub fn render(req: wisp.Request, ctx: Context) -> wisp.Response {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let help_data = help_center.load_help_articles(ctx.locale)

  let markdown_element =
    markdown_utils.load_markdown_with_fallback("priv/privacy", ctx.locale)
    |> markdown_utils.render_markdown_to_element(ctx, help_data)

  let content = [markdown_element]

  layout.docs_layout(
    req,
    ctx,
    layout.article_page_meta(
      g_(i18n_ctx, "Privacy Policy"),
      g_(i18n_ctx, "Read the Fluxer Privacy Policy"),
    ),
    g_(i18n_ctx, "Fluxer Privacy Policy"),
    content,
  )
  |> element.to_document_string_tree
  |> wisp.html_response(200)
}
