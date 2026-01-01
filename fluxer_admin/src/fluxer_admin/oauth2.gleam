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

import fluxer_admin/web.{type Context}
import gleam/bit_array
import gleam/crypto

pub fn authorize_url(ctx: Context, state: String) -> String {
  ctx.web_app_endpoint
  <> "/oauth2/authorize?response_type=code&client_id="
  <> ctx.oauth_client_id
  <> "&redirect_uri="
  <> ctx.oauth_redirect_uri
  <> "&scope=identify%20email"
  <> "&state="
  <> state
}

pub fn base64_encode_string(value: String) -> String {
  value
  |> bit_array.from_string
  |> bit_array.base64_encode(True)
}

pub fn generate_state() -> String {
  crypto.strong_random_bytes(32)
  |> bit_array.base64_url_encode(False)
}
