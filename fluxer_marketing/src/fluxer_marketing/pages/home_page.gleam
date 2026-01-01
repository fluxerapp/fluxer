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

import fluxer_marketing/components/coming_features
import fluxer_marketing/components/communities_section
import fluxer_marketing/components/current_features
import fluxer_marketing/components/final_cta
import fluxer_marketing/components/get_involved_section
import fluxer_marketing/components/hero
import fluxer_marketing/components/partner_section
import fluxer_marketing/components/plutonium_section
import fluxer_marketing/pages/layout
import fluxer_marketing/web.{type Context}
import lustre/element
import wisp

pub fn render(req: wisp.Request, ctx: Context) -> wisp.Response {
  let content = [
    hero.render(ctx),
    communities_section.render(ctx),
    current_features.render(ctx),
    coming_features.render(ctx),
    get_involved_section.render(ctx),
    plutonium_section.render(ctx),
    partner_section.render(ctx),
    final_cta.render(ctx),
  ]

  layout.render(req, ctx, layout.default_page_meta(), content)
  |> element.to_document_string_tree
  |> wisp.html_response(200)
}
