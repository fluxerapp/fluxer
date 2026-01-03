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

import fluxer_marketing/web.{type Context}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub type PageMeta {
  PageMeta(title: String, description: String, og_type: String)
}

pub fn default_page_meta() -> PageMeta {
  PageMeta(
    title: "Fluxer: A chat app that puts you first",
    description: "Fluxer is an open-source, independent instant messaging and VoIP platform. Built for friends, groups, and communities.",
    og_type: "website",
  )
}

pub fn article_page_meta(title: String, description: String) -> PageMeta {
  PageMeta(
    title: "Fluxer | " <> title,
    description: description,
    og_type: "article",
  )
}

pub fn format_page_title(base_title: String) -> String {
  case base_title {
    "Fluxer" -> "Fluxer"
    _ -> "Fluxer |" <> base_title
  }
}

pub fn build_meta_tags(ctx: Context, page_meta: PageMeta) -> List(Element(a)) {
  [
    html.meta([
      attribute.name("description"),
      attribute.content(page_meta.description),
    ]),
    html.meta([
      attribute.attribute("property", "og:title"),
      attribute.content(page_meta.title),
    ]),
    html.meta([
      attribute.attribute("property", "og:description"),
      attribute.content(page_meta.description),
    ]),
    html.meta([
      attribute.attribute("property", "og:image"),
      attribute.content(ctx.cdn_endpoint <> "/web/og-image-default.png"),
    ]),
    html.meta([
      attribute.attribute("property", "og:url"),
      attribute.content(ctx.base_url),
    ]),
    html.meta([
      attribute.attribute("property", "og:type"),
      attribute.content(page_meta.og_type),
    ]),
    html.meta([
      attribute.name("twitter:card"),
      attribute.content("summary_large_image"),
    ]),
    html.meta([
      attribute.name("twitter:title"),
      attribute.content(page_meta.title),
    ]),
    html.meta([
      attribute.name("twitter:description"),
      attribute.content(page_meta.description),
    ]),
    html.meta([
      attribute.name("twitter:image"),
      attribute.content(ctx.cdn_endpoint <> "/web/og-image-default.png"),
    ]),
    html.meta([attribute.name("robots"), attribute.content("index,follow")]),
    html.meta([attribute.name("theme-color"), attribute.content("#4641D9")]),
    html.meta([attribute.name("author"), attribute.content("Fluxer Team")]),
    html.link([attribute.rel("canonical"), attribute.href(ctx.base_url)]),
  ]
}
