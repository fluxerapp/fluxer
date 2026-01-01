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

import fluxer_marketing/locale.{type Locale}
import kielet/context.{type Context, Context}
import kielet/database
import kielet/language
import simplifile

pub fn setup_database() -> database.Database {
  let db = database.new()

  db
}

pub fn get_context(db: database.Database, locale: Locale) -> Context {
  let locale_code = locale.get_code_from_locale(locale)

  let db_with_fallback = case load_locale(db, locale_code) {
    Ok(updated_db) -> updated_db
    Error(_) -> db
  }

  Context(db_with_fallback, locale_code)
}

fn load_locale(
  db: database.Database,
  locale_code: String,
) -> Result(database.Database, Nil) {
  let mo_path = "priv/locales/" <> locale_code <> "/LC_MESSAGES/messages.mo"

  case simplifile.read_bits(mo_path) {
    Ok(mo_data) -> {
      case language.load(locale_code, mo_data) {
        Ok(lang) -> Ok(database.add_language(db, lang))
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}
