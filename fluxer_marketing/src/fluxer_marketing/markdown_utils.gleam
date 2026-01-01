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
import fluxer_marketing/render_md
import fluxer_marketing/web.{type Context}
import lustre/element
import simplifile

pub fn load_markdown_with_fallback(
  base_path: String,
  locale: locale.Locale,
) -> String {
  let locale_code = locale.get_code_from_locale(locale)
  let file_path = base_path <> "/" <> locale_code <> ".md"

  case simplifile.read(file_path) {
    Ok(content) -> content
    Error(_) -> {
      let fallback_path = base_path <> "/en-US.md"
      case simplifile.read(fallback_path) {
        Ok(content) -> content
        Error(_) -> {
          "# Content not available\n\nThe requested content could not be loaded."
        }
      }
    }
  }
}

pub fn render_markdown_to_element(
  content: String,
  ctx: Context,
  help_data: help_center.HelpCenterData,
) -> element.Element(msg) {
  let rendered =
    render_md.render_with_base(
      content,
      ctx.base_url,
      ctx.app_endpoint,
      ctx.locale,
      help_data,
    )
  element.unsafe_raw_html("", "div", [], rendered)
}
