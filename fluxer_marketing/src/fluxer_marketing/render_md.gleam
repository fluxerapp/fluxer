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
import fluxer_marketing/locale.{type Locale}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/regexp
import gleam/string

const regex_options = regexp.Options(case_insensitive: False, multi_line: True)

pub fn render(
  markdown: String,
  app_endpoint: String,
  locale: Locale,
  help_data: help_center.HelpCenterData,
) {
  render_with_base(markdown, app_endpoint, app_endpoint, locale, help_data)
}

pub fn render_with_base(
  markdown: String,
  base_url: String,
  app_endpoint: String,
  locale: Locale,
  help_data: help_center.HelpCenterData,
) {
  markdown
  |> expand_article_shortcuts(base_url, locale, help_data)
  |> expand_app_shortcuts(app_endpoint)
  |> replace_app_variables(app_endpoint)
  |> resolve_relative_links(base_url)
  |> remove_html_comments
  |> normalize_block_boundaries
  |> string.trim
  |> string.split("\n\n")
  |> list.map(string.trim)
  |> list.filter(fn(block) { !string.is_empty(block) })
  |> list.map(parse_block)
  |> string.join("\n\n")
}

fn expand_article_shortcuts(
  text: String,
  base_url: String,
  locale: Locale,
  help_data: help_center.HelpCenterData,
) -> String {
  case regexp.compile("<% *article +([0-9]+) *%>", regex_options) {
    Ok(regex) -> {
      regexp.scan(regex, text)
      |> list.fold(text, fn(acc, match) {
        case match.submatches {
          [Some(snowflake_id)] -> {
            replace_article_reference(
              acc,
              match.content,
              snowflake_id,
              base_url,
              locale,
              help_data,
            )
          }
          _ -> acc
        }
      })
    }
    Error(_) -> text
  }
}

fn replace_article_reference(
  text: String,
  original_match: String,
  snowflake_id: String,
  base_url: String,
  locale: Locale,
  help_data: help_center.HelpCenterData,
) -> String {
  case help_center.get_article_by_snowflake(help_data, snowflake_id) {
    Ok(article) -> {
      let locale_code = locale.get_code_from_locale(locale) |> string.lowercase
      let slug = help_center.create_slug(article.title)
      let url = build_article_url(base_url, locale_code, snowflake_id, slug)
      let markdown_link = "[" <> article.title <> "](" <> url <> ")"
      string.replace(text, original_match, markdown_link)
    }
    Error(_) -> {
      text
    }
  }
}

fn build_article_url(
  base_url: String,
  locale_code: String,
  snowflake_id: String,
  slug: String,
) -> String {
  let base = normalize_base(base_url)
  base <> "/help/" <> locale_code <> "/articles/" <> snowflake_id <> "-" <> slug
}

fn expand_app_shortcuts(text: String, app_endpoint: String) -> String {
  case regexp.compile("<% *app +([^%]+) *%>", regex_options) {
    Ok(regex) -> {
      regexp.scan(regex, text)
      |> list.fold(text, fn(acc, match) {
        case match.submatches {
          [Some(path)] -> {
            let clean_path = string.trim(path)
            let display = case string.split(app_endpoint, "://") {
              [_, host] -> host <> "/" <> clean_path
              _ -> app_endpoint <> "/" <> clean_path
            }
            let url = app_endpoint <> "/" <> clean_path
            let replacement = "[" <> display <> "](" <> url <> ")"
            string.replace(acc, match.content, replacement)
          }
          _ -> acc
        }
      })
    }
    Error(_) -> text
  }
}

fn replace_app_variables(text: String, app_endpoint: String) -> String {
  let #(app_protocol, app_host) = case string.split(app_endpoint, "://") {
    [protocol, host] -> #(protocol, host)
    _ -> #("https", app_endpoint)
  }
  text
  |> string.replace("%app.proto%", app_protocol)
  |> string.replace("%app.host%", app_host)
}

fn resolve_relative_links(text: String, base_url: String) -> String {
  let base = normalize_base(base_url)

  case regexp.compile("\\[([^\\]]+)\\]\\((/[^)]*)\\)", regex_options) {
    Ok(regex) -> {
      regexp.scan(regex, text)
      |> list.fold(text, fn(acc, match) {
        case match.submatches {
          [Some(link_text), Some(path)] -> {
            let url = base <> path
            let replacement = "[" <> link_text <> "](" <> url <> ")"
            string.replace(acc, match.content, replacement)
          }
          _ -> acc
        }
      })
    }
    Error(_) -> text
  }
}

fn remove_html_comments(text: String) -> String {
  let step1 = case
    regexp.compile("^[ \\t]*<!--[\\s\\S]*?-->[ \\t]*\\n", regex_options)
  {
    Ok(regex) -> regexp.replace(regex, text, "")
    Error(_) -> text
  }

  let step2 = case
    regexp.compile("\\n[ \\t]*<!--.*?-->[ \\t]*(?=\\n|$)", regex_options)
  {
    Ok(regex) -> regexp.replace(regex, step1, "")
    Error(_) -> step1
  }

  case regexp.compile("<!--[\\s\\S]*?-->", regex_options) {
    Ok(regex) -> regexp.replace(regex, step2, "")
    Error(_) -> step2
  }
}

fn normalize_block_boundaries(text: String) -> String {
  text
  |> string.split("\n")
  |> insert_header_separators
  |> string.join("\n")
}

fn insert_header_separators(lines: List(String)) -> List(String) {
  case lines {
    [] -> []
    [line] -> [line]
    [line, next, ..rest] -> {
      let trimmed_next = string.trim(next)

      case is_atx_header_line(line) {
        True -> {
          let processed_rest = insert_header_separators(rest)
          case string.is_empty(trimmed_next) {
            True -> [line, next, ..processed_rest]
            False -> [line, "", next, ..processed_rest]
          }
        }
        False ->
          case is_setext_underline_line(next) {
            True -> {
              case rest {
                [] -> [line, next]
                [third, ..tail] -> {
                  let processed_tail = insert_header_separators(tail)
                  case string.is_empty(string.trim(third)) {
                    True -> [line, next, third, ..processed_tail]
                    False -> [line, next, "", third, ..processed_tail]
                  }
                }
              }
            }
            False -> [line, ..insert_header_separators([next, ..rest])]
          }
      }
    }
  }
}

fn is_atx_header_line(line: String) -> Bool {
  let trimmed = string.trim(line)
  case trimmed {
    "" -> False
    _ -> string.starts_with(trimmed, "#")
  }
}

fn is_setext_underline_line(line: String) -> Bool {
  let trimmed = string.trim(line)
  !string.is_empty(trimmed) && is_all_underline_chars(trimmed)
}

fn parse_block(text: String) -> String {
  case { string.starts_with(text, "#") || is_setext_header(text) } {
    True -> parse_header(text)
    False ->
      case is_horizontal_rule(text) {
        True -> parse_horizontal_rule()
        False ->
          case is_table(text) {
            True -> parse_table(text)
            False ->
              case is_list(text) {
                True -> parse_list(text)
                False -> parse_paragraph(text)
              }
          }
      }
  }
}

fn is_horizontal_rule(text: String) -> Bool {
  let trimmed = string.trim(text)
  case string.split(trimmed, "\n") {
    [single_line] -> {
      case string.trim(single_line) {
        "---" -> True
        _ -> False
      }
    }
    _ -> False
  }
}

fn parse_horizontal_rule() -> String {
  "<hr class=\"my-8\">"
}

fn is_table(text: String) -> Bool {
  let lines = string.split(text, "\n")
  case lines {
    [_header, separator, ..] -> {
      case
        regexp.compile(
          "^\\|?\\s*:?-+:?\\s*(?:\\|\\s*:?-+:?\\s*)*\\|?$",
          regex_options,
        )
      {
        Ok(regex) -> regexp.check(regex, string.trim(separator))
        Error(_) -> False
      }
    }
    _ -> False
  }
}

fn parse_table(text: String) -> String {
  let lines =
    string.split(text, "\n")
    |> list.filter(fn(l) { !string.is_empty(string.trim(l)) })

  case lines {
    [header_line, _separator_line, ..body_lines] -> {
      let header_cells = parse_table_row(header_line)
      let body_rows = list.map(body_lines, parse_table_row)

      let header_html =
        "<thead>\n<tr>\n"
        <> string.join(
          list.map(header_cells, fn(cell) {
            "<th class=\"border border-gray-300 px-4 py-2 bg-gray-50 font-semibold\">"
            <> parse_inline(cell)
            <> "</th>"
          }),
          "\n",
        )
        <> "\n</tr>\n</thead>"

      let body_html = case body_rows {
        [] -> ""
        _ ->
          "<tbody>\n"
          <> string.join(
            list.map(body_rows, fn(row) {
              "<tr>\n"
              <> string.join(
                list.map(row, fn(cell) {
                  "<td class=\"border border-gray-300 px-4 py-2\">"
                  <> parse_inline(cell)
                  <> "</td>"
                }),
                "\n",
              )
              <> "\n</tr>"
            }),
            "\n",
          )
          <> "\n</tbody>"
      }

      "<table class=\"border-collapse border border-gray-300 w-full my-4\">\n"
      <> header_html
      <> "\n"
      <> body_html
      <> "\n</table>"
    }
    _ -> parse_paragraph(text)
  }
}

fn parse_table_row(line: String) -> List(String) {
  let trimmed = string.trim(line)
  let without_edges = case
    { string.starts_with(trimmed, "|") && string.ends_with(trimmed, "|") }
  {
    True ->
      trimmed
      |> string.drop_start(1)
      |> string.drop_end(1)
    False -> trimmed
  }

  string.split(without_edges, "|")
  |> list.map(string.trim)
}

fn is_setext_header(text: String) -> Bool {
  let lines = string.split(text, "\n")
  case lines {
    [_, second] -> {
      let trimmed = string.trim(second)
      is_all_underline_chars(trimmed) && !string.is_empty(trimmed)
    }
    _ -> False
  }
}

fn is_all_underline_chars(text: String) -> Bool {
  text
  |> string.to_graphemes
  |> list.all(fn(c) { c == "=" || c == "-" })
}

fn is_list(text: String) -> Bool {
  text
  |> string.split("\n")
  |> list.any(fn(line) {
    let trimmed = string.trim(line)
    string.starts_with(trimmed, "- ")
    || string.starts_with(trimmed, "* ")
    || string.starts_with(trimmed, "+ ")
    || case regexp.compile("^\\d+\\.\\s", regex_options) {
      Ok(regex) -> regexp.check(regex, trimmed)
      Error(_) -> False
    }
  })
}

fn parse_header(text: String) -> String {
  case string.starts_with(text, "#") {
    True -> parse_atx_header(text)
    False -> parse_setext_header(text)
  }
}

fn parse_atx_header(text: String) -> String {
  case regexp.compile("^(#{1,6})\\s*(.+?)(?:\\s*#+\\s*)?$", regex_options) {
    Ok(regex) -> {
      case regexp.scan(regex, text) {
        [match] -> {
          case match.submatches {
            [Some(hashes), Some(content)] -> {
              let level = string.length(hashes)
              let tag = "h" <> int.to_string(level)
              let raw = string.trim(content)
              let #(clean, custom_id) = extract_header_id(raw)
              let processed_content = parse_inline(clean)
              let slug = case custom_id {
                "" -> create_heading_slug(clean)
                _ -> custom_id
              }
              form_element_with_id(tag, processed_content, slug)
            }
            _ -> parse_paragraph(text)
          }
        }
        _ -> parse_paragraph(text)
      }
    }
    Error(_) -> parse_paragraph(text)
  }
}

fn parse_setext_header(text: String) -> String {
  let lines = string.split(text, "\n")
  case lines {
    [content, underline] -> {
      let level = case string.starts_with(string.trim(underline), "=") {
        True -> 1
        False -> 2
      }
      let tag = "h" <> int.to_string(level)
      let raw = string.trim(content)
      let #(clean, custom_id) = extract_header_id(raw)
      let processed_content = parse_inline(clean)
      let slug = case custom_id {
        "" -> create_heading_slug(clean)
        _ -> custom_id
      }
      form_element_with_id(tag, processed_content, slug)
    }
    _ -> parse_paragraph(text)
  }
}

fn parse_list(text: String) -> String {
  let lines = string.split(text, "\n")
  let #(prefix_lines, list_lines) = separate_prefix_from_list(lines)

  case prefix_lines {
    [] -> {
      let items = parse_list_items(list_lines, [])
      case is_ordered_list_from_lines(list_lines) {
        True -> form_element("ol", "\n" <> string.join(items, "\n") <> "\n")
        False -> form_element("ul", "\n" <> string.join(items, "\n") <> "\n")
      }
    }
    _ -> {
      let prefix_content = parse_paragraph(string.join(prefix_lines, "\n"))
      let items = parse_list_items(list_lines, [])
      let list_content = case is_ordered_list_from_lines(list_lines) {
        True -> form_element("ol", "\n" <> string.join(items, "\n") <> "\n")
        False -> form_element("ul", "\n" <> string.join(items, "\n") <> "\n")
      }
      prefix_content <> "\n\n" <> list_content
    }
  }
}

fn separate_prefix_from_list(
  lines: List(String),
) -> #(List(String), List(String)) {
  case lines {
    [] -> #([], [])
    [line, ..rest] -> {
      case is_list_item_start(string.trim(line)) {
        True -> #([], lines)
        False -> {
          let #(prefix, list_part) = separate_prefix_from_list(rest)
          #([line, ..prefix], list_part)
        }
      }
    }
  }
}

fn is_ordered_list_from_lines(lines: List(String)) -> Bool {
  case regexp.compile("^\\d+\\.\\s", regex_options) {
    Ok(regex) -> {
      lines
      |> list.filter(fn(line) {
        let trimmed = string.trim(line)
        let indent = get_indent_level(line)
        indent == 0 && !string.is_empty(trimmed)
      })
      |> list.any(fn(line) { regexp.check(regex, string.trim(line)) })
    }
    Error(_) -> False
  }
}

fn parse_list_items(lines: List(String), acc: List(String)) -> List(String) {
  case lines {
    [] -> list.reverse(acc)
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      let indent = get_indent_level(line)

      case is_list_item_start(trimmed) {
        True -> {
          case indent > 0 {
            True -> {
              let #(nested_lines, remaining) =
                collect_nested_list(lines, indent)
              let nested_html = parse_nested_list(nested_lines, indent)

              case acc {
                [last_item, ..rest_acc] -> {
                  let updated_item =
                    string.replace(
                      last_item,
                      "</li>",
                      "\n" <> nested_html <> "\n</li>",
                    )
                  parse_list_items(remaining, [updated_item, ..rest_acc])
                }
                [] -> parse_list_items(remaining, acc)
              }
            }
            False -> {
              let content = extract_list_content(trimmed)
              let item_html = form_element("li", parse_inline(content))
              parse_list_items(rest, [item_html, ..acc])
            }
          }
        }
        False -> {
          case acc {
            [last_item, ..rest_acc] -> {
              let updated_item =
                string.replace(
                  last_item,
                  "</li>",
                  " " <> parse_inline(string.trim(line)) <> "</li>",
                )
              parse_list_items(rest, [updated_item, ..rest_acc])
            }
            [] -> parse_list_items(rest, acc)
          }
        }
      }
    }
  }
}

fn get_indent_level(line: String) -> Int {
  let chars = string.to_graphemes(line)
  count_leading_spaces(chars, 0)
}

fn count_leading_spaces(chars: List(String), count: Int) -> Int {
  case chars {
    [" ", ..rest] -> count_leading_spaces(rest, count + 1)
    _ -> count
  }
}

fn collect_nested_list(
  lines: List(String),
  parent_indent: Int,
) -> #(List(String), List(String)) {
  case lines {
    [] -> #([], [])
    [line, ..rest] -> {
      let indent = get_indent_level(line)
      let trimmed = string.trim(line)

      case indent >= parent_indent && !string.is_empty(trimmed) {
        True -> {
          let #(nested, remaining) = collect_nested_list(rest, parent_indent)
          #([line, ..nested], remaining)
        }
        False -> #([], lines)
      }
    }
  }
}

fn parse_nested_list(lines: List(String), base_indent: Int) -> String {
  case lines {
    [] -> ""
    _ -> {
      let normalized_lines =
        list.map(lines, fn(line) {
          let indent = get_indent_level(line)
          case indent >= base_indent {
            True ->
              string.repeat(" ", indent - base_indent) <> string.trim(line)
            False -> line
          }
        })

      let items = parse_list_items(normalized_lines, [])
      let is_ordered = is_ordered_list_from_lines(normalized_lines)

      case is_ordered {
        True -> form_element("ol", "\n" <> string.join(items, "\n") <> "\n")
        False -> form_element("ul", "\n" <> string.join(items, "\n") <> "\n")
      }
    }
  }
}

fn is_list_item_start(line: String) -> Bool {
  string.starts_with(line, "- ")
  || string.starts_with(line, "* ")
  || string.starts_with(line, "+ ")
  || case regexp.compile("^\\d+\\.\\s", regex_options) {
    Ok(regex) -> regexp.check(regex, line)
    Error(_) -> False
  }
}

fn extract_list_content(line: String) -> String {
  case
    {
      string.starts_with(line, "- ")
      || string.starts_with(line, "* ")
      || string.starts_with(line, "+ ")
    }
  {
    True -> string.drop_start(line, 2)
    False -> {
      case regexp.compile("^\\d+\\.\\s(.*)$", regex_options) {
        Ok(regex) -> {
          case regexp.scan(regex, line) {
            [match] -> {
              case match.submatches {
                [Some(content)] -> content
                _ -> line
              }
            }
            _ -> line
          }
        }
        Error(_) -> line
      }
    }
  }
}

fn parse_paragraph(text: String) -> String {
  let content =
    text
    |> string.split("\n")
    |> list.map(string.trim)
    |> string.join("<br>")
    |> parse_inline

  form_element("p", content)
}

fn parse_inline(text: String) -> String {
  text
  |> escape_html
  |> unescape_markdown_patterns
  |> parse_links
  |> parse_email_links
  |> parse_bold
  |> parse_inline_code
}

fn unescape_markdown_patterns(text: String) -> String {
  text
  |> string.replace("&lt;strong&gt;", "<strong>")
  |> string.replace("&lt;/strong&gt;", "</strong>")
  |> string.replace("&lt;a ", "<a ")
  |> string.replace("&lt;/a&gt;", "</a>")
  |> string.replace("&lt;br&gt;", "<br>")
}

fn parse_links(text: String) -> String {
  case regexp.compile("\\[([^\\]]+)\\]\\(([^\\)]+)\\)", regex_options) {
    Ok(regex) -> {
      regexp.scan(regex, text)
      |> list.fold(text, fn(acc, match) {
        case match.submatches {
          [Some(link_text), Some(url)] -> {
            let replacement =
              "<a class=\"text-[#4641D9] hover:underline\" href=\""
              <> url
              <> "\">"
              <> link_text
              <> "</a>"
            string.replace(acc, match.content, replacement)
          }
          _ -> acc
        }
      })
    }
    Error(_) -> text
  }
}

fn parse_email_links(text: String) -> String {
  case
    regexp.compile(
      "([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,})",
      regex_options,
    )
  {
    Ok(regex) -> {
      regexp.scan(regex, text)
      |> list.fold(text, fn(acc, match) {
        case match.submatches {
          [Some(email)] -> {
            let is_in_href =
              string.contains(acc, "href=&quot;" <> email)
              || string.contains(acc, "href=\"mailto:" <> email)

            case is_in_href {
              True -> acc
              False -> {
                let replacement =
                  "<a class=\"text-[#4641D9] hover:underline\" href=\"mailto:"
                  <> email
                  <> "\">"
                  <> email
                  <> "</a>"
                string.replace(acc, email, replacement)
              }
            }
          }
          _ -> acc
        }
      })
    }
    Error(_) -> text
  }
}

fn parse_bold(text: String) -> String {
  case regexp.compile("\\*\\*([^*]+)\\*\\*", regex_options) {
    Ok(regex) -> {
      regexp.scan(regex, text)
      |> list.fold(text, fn(acc, match) {
        case match.submatches {
          [Some(content)] -> {
            let replacement = form_element("strong", content)
            string.replace(acc, match.content, replacement)
          }
          _ -> acc
        }
      })
    }
    Error(_) -> text
  }
}

fn parse_inline_code(text: String) -> String {
  case regexp.compile("`([^`]+)`", regex_options) {
    Ok(regex) -> {
      regexp.scan(regex, text)
      |> list.fold(text, fn(acc, match) {
        case match.submatches {
          [Some(content)] -> {
            let replacement = form_element("strong", content)
            string.replace(acc, match.content, replacement)
          }
          _ -> acc
        }
      })
    }
    Error(_) -> text
  }
}

fn form_element(tag: String, content: String) -> String {
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
}

fn form_element_with_id(tag: String, content: String, id: String) -> String {
  "<"
  <> tag
  <> " id=\""
  <> id
  <> "\" style=\"scroll-margin-top: var(--anchor-offset, 200px)\""
  <> ">"
  <> content
  <> "</"
  <> tag
  <> ">"
}

fn escape_html(text: String) -> String {
  text
  |> string.replace("&", "&amp;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
  |> string.replace("\"", "&quot;")
  |> string.replace("'", "&#39;")
}

fn create_heading_slug(text: String) -> String {
  let lower = string.lowercase(text)

  let hyphenated = case regexp.compile("[^\\p{L}\\p{N}]+", regex_options) {
    Ok(regex) -> regexp.replace(regex, lower, "-")
    Error(_) -> lower
  }

  let collapsed = case regexp.compile("-+", regex_options) {
    Ok(regex) -> regexp.replace(regex, hyphenated, "-")
    Error(_) -> hyphenated
  }

  case string.trim(trim_hyphens(collapsed)) {
    "" -> "section"
    slug -> slug
  }
}

fn trim_hyphens(text: String) -> String {
  let trimmed_start =
    text
    |> string.to_graphemes
    |> list.drop_while(fn(c) { c == "-" })
    |> list.reverse
    |> list.drop_while(fn(c) { c == "-" })
    |> list.reverse

  string.join(trimmed_start, "")
}

fn normalize_base(base_url: String) -> String {
  case string.ends_with(base_url, "/") {
    True -> string.drop_end(base_url, 1)
    False -> base_url
  }
}

fn extract_header_id(raw: String) -> #(String, String) {
  case regexp.compile("\\s*\\{#([^}]+)\\}\\s*$", regex_options) {
    Ok(regex) -> {
      case regexp.scan(regex, raw) {
        [match] -> {
          case match.submatches {
            [Some(custom)] -> {
              let clean = string.trim(string.replace(raw, match.content, ""))
              #(clean, string.trim(custom))
            }
            _ -> #(raw, "")
          }
        }
        _ -> #(raw, "")
      }
    }
    Error(_) -> #(raw, "")
  }
}
