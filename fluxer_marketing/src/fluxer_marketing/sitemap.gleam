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
import fluxer_marketing/locale
import gleam/list
import gleam/string

pub type UrlEntry {
  UrlEntry(loc: String, changefreq: String, priority: String)
}

pub fn generate(base_url: String) -> String {
  let urls = generate_urls(base_url)

  let url_entries =
    urls
    |> list.map(fn(entry) { "  <url>
    <loc>" <> entry.loc <> "</loc>
    <changefreq>" <> entry.changefreq <> "</changefreq>
    <priority>" <> entry.priority <> "</priority>
  </url>" })
    |> string.join("\n")

  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">
" <> url_entries <> "
</urlset>"
}

fn generate_urls(base_url: String) -> List(UrlEntry) {
  let static_pages = [
    UrlEntry(base_url, "weekly", "1.0"),
    UrlEntry(base_url <> "/terms", "monthly", "0.5"),
    UrlEntry(base_url <> "/privacy", "monthly", "0.5"),
    UrlEntry(base_url <> "/security", "monthly", "0.5"),
    UrlEntry(base_url <> "/guidelines", "monthly", "0.7"),
    UrlEntry(base_url <> "/company-information", "monthly", "0.4"),
    UrlEntry(base_url <> "/careers", "weekly", "0.6"),
    UrlEntry(base_url <> "/download", "weekly", "0.9"),
    UrlEntry(base_url <> "/plutonium", "weekly", "0.8"),
    UrlEntry(base_url <> "/partners", "monthly", "0.6"),
    UrlEntry(base_url <> "/press", "monthly", "0.5"),
    UrlEntry(base_url <> "/help", "daily", "0.9"),
  ]

  let help_urls = generate_help_urls(base_url)

  list.append(static_pages, help_urls)
}

fn generate_help_urls(base_url: String) -> List(UrlEntry) {
  let help_data = help_center.load_help_articles(locale.EnUS)

  let category_urls =
    help_data.categories
    |> list.map(fn(category) {
      UrlEntry(base_url <> "/help/" <> category.name, "weekly", "0.8")
    })

  let article_urls =
    help_data.all_articles
    |> list.map(fn(article) {
      UrlEntry(
        base_url <> "/help/" <> article.category <> "/" <> article.slug,
        "weekly",
        "0.7",
      )
    })

  list.append(category_urls, article_urls)
}
