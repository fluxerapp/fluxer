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

import fluxer_marketing/locale
import fluxer_marketing/router
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn update_locale_in_article_path_rebuilds_slug_test() {
  let path = "/help/en-us/articles/1445730947679911936-requesting-data-deletion"

  router.update_locale_in_path(path, locale.Ar)
  |> should.equal("/help/ar/articles/1445730947679911936-طلب-حذف-البيانات")
}

pub fn update_locale_in_article_path_keeps_extra_segments_test() {
  let path =
    "/help/en-us/articles/1445730947679911936-requesting-data-deletion/extra"

  router.update_locale_in_path(path, locale.Ar)
  |> should.equal(
    "/help/ar/articles/1445730947679911936-طلب-حذف-البيانات/extra",
  )
}
