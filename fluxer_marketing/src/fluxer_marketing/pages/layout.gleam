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

import fluxer_marketing/components/footer
import fluxer_marketing/components/locale_selector
import fluxer_marketing/components/navigation
import fluxer_marketing/components/plutonium_banner
import fluxer_marketing/locale
import fluxer_marketing/pages/layout/icons
import fluxer_marketing/pages/layout/meta
import fluxer_marketing/pages/layout/scripts
import fluxer_marketing/path_utils
import fluxer_marketing/web.{type Context, cache_busted_asset}
import gleam/list
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import wisp

pub type PageMeta =
  meta.PageMeta

pub fn default_page_meta() -> PageMeta {
  meta.default_page_meta()
}

pub fn article_page_meta(title: String, description: String) -> PageMeta {
  meta.article_page_meta(title, description)
}

pub fn format_page_title(base_title: String) -> String {
  meta.format_page_title(base_title)
}

fn build_common_head_elements(
  ctx: Context,
  page_meta: PageMeta,
) -> List(Element(a)) {
  [
    html.meta([attribute.attribute("charset", "UTF-8")]),
    html.meta([
      attribute.name("viewport"),
      attribute.attribute("content", "width=device-width, initial-scale=1.0"),
    ]),
  ]
  |> list.append(meta.build_meta_tags(ctx, page_meta))
  |> list.append([
    html.title([], page_meta.title),
    html.link([
      attribute.rel("preconnect"),
      attribute.href("https://fluxerstatic.com"),
    ]),
    html.link([
      attribute.rel("stylesheet"),
      attribute.href("https://fluxerstatic.com/fonts/bricolage.css"),
    ]),
    html.link([
      attribute.rel("stylesheet"),
      attribute.href(cache_busted_asset(ctx, "/static/app.css")),
    ]),
  ])
  |> list.append(icons.build_icon_links(ctx.cdn_endpoint))
}

pub fn render(
  req: wisp.Request,
  ctx: Context,
  page_meta: PageMeta,
  content: List(Element(a)),
) -> Element(a) {
  let current_path = path_utils.get_current_path(req)

  html.html(
    [attribute.attribute("lang", locale.get_code_from_locale(ctx.locale))],
    [
      html.head(
        [],
        build_common_head_elements(ctx, page_meta)
          |> list.append([scripts.main_page_script(), scripts.download_script()]),
      ),
      html.body(
        [
          attribute.class(
            "flex min-h-screen flex-col bg-[#4641D9] font-sans text-white",
          ),
        ],
        [
          plutonium_banner.render(ctx),
          navigation.render(ctx, req),
          html.div([attribute.class("flex grow flex-col")], content),
          footer.render(ctx),
          locale_selector.render_modal(ctx, current_path),
        ],
      ),
    ],
  )
}

pub fn docs_layout(
  req: wisp.Request,
  ctx: Context,
  page_meta: PageMeta,
  page_title: String,
  content: List(Element(a)),
) -> Element(a) {
  let current_path = path_utils.get_current_path(req)

  html.html(
    [attribute.attribute("lang", locale.get_code_from_locale(ctx.locale))],
    [
      html.head(
        [],
        build_common_head_elements(ctx, page_meta)
          |> list.append([scripts.docs_page_script()]),
      ),
      html.body([attribute.class("bg-white")], [
        plutonium_banner.render(ctx),
        navigation.render(ctx, req),
        html.main(
          [attribute.class("min-h-screen bg-white px-6 pb-16 pt-48 md:pt-60")],
          [
            html.article(
              [attribute.class("prose prose-lg prose-gray mx-auto max-w-4xl")],
              [
                html.h1([attribute.class("mb-2 text-4xl font-bold")], [
                  html.text(page_title),
                ]),
                ..content
              ],
            ),
          ],
        ),
        footer.render(ctx),
        locale_selector.render_modal(ctx, current_path),
      ]),
    ],
  )
}
