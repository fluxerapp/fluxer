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

import fluxer_marketing/frontmatter
import fluxer_marketing/locale.{type Locale}
import gleam/dict
import gleam/int
import gleam/list
import gleam/regexp
import gleam/result
import gleam/string
import simplifile

const regex_options = regexp.Options(case_insensitive: False, multi_line: True)

pub type HelpArticle {
  HelpArticle(
    title: String,
    description: String,
    category: String,
    category_title: String,
    category_icon: String,
    order: Int,
    slug: String,
    snowflake_id: String,
    content: String,
  )
}

pub type HelpCategory {
  HelpCategory(
    name: String,
    title: String,
    icon: String,
    article_count: Int,
    articles: List(HelpArticle),
  )
}

pub type HelpCenterData {
  HelpCenterData(
    categories: List(HelpCategory),
    all_articles: List(HelpArticle),
  )
}

pub fn load_help_articles(locale: Locale) -> HelpCenterData {
  let locale_code = locale.get_code_from_locale(locale)
  let help_root = "priv/help"

  case scan_help_directory(help_root, locale_code) {
    Ok(articles) -> {
      let categories = group_by_category(articles)
      HelpCenterData(categories: categories, all_articles: articles)
    }
    Error(_) -> HelpCenterData(categories: [], all_articles: [])
  }
}

fn scan_help_directory(
  root: String,
  locale_code: String,
) -> Result(List(HelpArticle), Nil) {
  case list_directory_recursive(root) {
    Ok(files) -> {
      let locale_suffix = "/" <> locale_code <> ".md"
      let fallback_suffix = "/en-US.md"

      let locale_files =
        files
        |> list.filter(fn(path) { string.ends_with(path, locale_suffix) })

      let fallback_files = case locale_code == "en-US" {
        True -> []
        False ->
          files
          |> list.filter(fn(path) { string.ends_with(path, fallback_suffix) })
          |> list.filter(fn(fallback_path) {
            let base_dir = get_article_base_dir(fallback_path)
            let has_locale_version =
              list.any(locale_files, fn(locale_path) {
                get_article_base_dir(locale_path) == base_dir
              })
            !has_locale_version
          })
      }

      let articles =
        list.append(locale_files, fallback_files)
        |> list.filter_map(fn(path) { load_article(path) })

      Ok(articles)
    }
    Error(_) -> Error(Nil)
  }
}

fn get_article_base_dir(path: String) -> String {
  path
  |> string.split("/")
  |> list.reverse
  |> list.drop(1)
  |> list.reverse
  |> string.join("/")
}

fn list_directory_recursive(dir: String) -> Result(List(String), Nil) {
  case simplifile.read_directory(dir) {
    Ok(entries) -> {
      let results =
        entries
        |> list.map(fn(entry) {
          let path = dir <> "/" <> entry
          case simplifile.is_directory(path) {
            Ok(True) -> list_directory_recursive(path)
            Ok(False) -> Ok([path])
            Error(_) -> Ok([])
          }
        })

      let all_files =
        results
        |> list.filter_map(fn(r) { result.unwrap(r, []) |> Ok })
        |> list.flatten

      Ok(all_files)
    }
    Error(_) -> Error(Nil)
  }
}

fn load_article(path: String) -> Result(HelpArticle, Nil) {
  case simplifile.read(path) {
    Ok(content) -> {
      let fm = frontmatter.parse(content)

      let slug = extract_slug_from_path(path)

      let title = frontmatter.get_string_or(fm, "title", "Untitled")
      let description =
        frontmatter.get_string_or(fm, "description", "No description")
      let category = frontmatter.get_string_or(fm, "category", "general")
      let category_title =
        frontmatter.get_string_or(fm, "category_title", "General")
      let category_icon =
        frontmatter.get_string_or(fm, "category_icon", "sparkle")
      let order = frontmatter.get_int_or(fm, "order", 999)
      let snowflake_id = frontmatter.get_string_or(fm, "snowflake_id", "")

      Ok(HelpArticle(
        title: title,
        description: description,
        category: category,
        category_title: category_title,
        category_icon: category_icon,
        order: order,
        slug: slug,
        snowflake_id: snowflake_id,
        content: frontmatter.get_content(fm),
      ))
    }
    Error(_) -> Error(Nil)
  }
}

fn extract_slug_from_path(path: String) -> String {
  path
  |> string.split("/")
  |> list.reverse
  |> list.drop(1)
  |> list.first
  |> result.unwrap("unknown")
}

fn group_by_category(articles: List(HelpArticle)) -> List(HelpCategory) {
  let grouped =
    articles
    |> list.group(fn(article) { article.category })

  grouped
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(category_name, category_articles) = entry

    let sorted_articles =
      category_articles
      |> list.sort(fn(a: HelpArticle, b: HelpArticle) {
        int.compare(a.order, b.order)
      })

    let first = list.first(sorted_articles)
    let category_title = case first {
      Ok(article) -> article.category_title
      Error(_) -> category_name
    }
    let category_icon = case first {
      Ok(article) -> article.category_icon
      Error(_) -> "sparkle"
    }

    HelpCategory(
      name: category_name,
      title: category_title,
      icon: category_icon,
      article_count: list.length(category_articles),
      articles: sorted_articles,
    )
  })
  |> list.sort(fn(a, b) { string.compare(a.title, b.title) })
}

pub fn get_category(
  data: HelpCenterData,
  category_name: String,
) -> Result(HelpCategory, Nil) {
  data.categories
  |> list.find(fn(cat) { cat.name == category_name })
}

pub fn get_article(
  data: HelpCenterData,
  category_name: String,
  article_slug: String,
) -> Result(HelpArticle, Nil) {
  data.all_articles
  |> list.find(fn(article) {
    article.category == category_name && article.slug == article_slug
  })
}

pub fn get_article_by_snowflake(
  data: HelpCenterData,
  snowflake_id: String,
) -> Result(HelpArticle, Nil) {
  data.all_articles
  |> list.find(fn(article) { article.snowflake_id == snowflake_id })
}

pub fn create_slug(title: String) -> String {
  let lower = string.lowercase(title)

  let hyphened = case regexp.compile("\\s+", regex_options) {
    Ok(regex) -> regexp.replace(regex, lower, "-")
    Error(_) -> lower
  }

  let cleaned = case regexp.compile("[^\\p{L}\\p{N}\\-._~]+", regex_options) {
    Ok(regex) -> regexp.replace(regex, hyphened, "-")
    Error(_) -> hyphened
  }

  let collapsed = case regexp.compile("-+", regex_options) {
    Ok(regex) -> regexp.replace(regex, cleaned, "-")
    Error(_) -> cleaned
  }

  let trimmed = trim_hyphens(collapsed)

  case string.is_empty(trimmed) {
    True -> "article"
    False -> trimmed
  }
}

fn trim_hyphens(text: String) -> String {
  let chars = string.to_graphemes(text)

  let trimmed_start =
    list.drop_while(chars, fn(c) { c == "-" })
    |> list.reverse
    |> list.drop_while(fn(c) { c == "-" })
    |> list.reverse

  case trimmed_start {
    [] -> ""
    _ -> string.join(trimmed_start, "")
  }
}

pub fn search_articles(data: HelpCenterData, query: String) -> List(HelpArticle) {
  let lower_query = string.lowercase(query)

  data.all_articles
  |> list.filter(fn(article) {
    let title_match =
      string.lowercase(article.title) |> string.contains(lower_query)
    let desc_match =
      string.lowercase(article.description) |> string.contains(lower_query)
    title_match || desc_match
  })
}

pub fn filter_by_category(
  articles: List(HelpArticle),
  category: String,
) -> List(HelpArticle) {
  articles
  |> list.filter(fn(article) { article.category == category })
}

pub fn article_href(
  locale: Locale,
  data: HelpCenterData,
  snowflake_id: String,
) -> String {
  let locale_code = locale.get_code_from_locale(locale) |> string.lowercase

  case get_article_by_snowflake(data, snowflake_id) {
    Ok(article) ->
      "/help/"
      <> locale_code
      <> "/articles/"
      <> article.snowflake_id
      <> "-"
      <> create_slug(article.title)
    Error(_) -> "/help/articles/" <> snowflake_id
  }
}
