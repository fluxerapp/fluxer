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

import gleam/int
import gleam/list
import gleam/string

pub type PricingTier {
  Monthly
  Yearly
  Visionary
  SelfHost
}

pub type Currency {
  USD
  EUR
}

const eea_countries = [
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU",
  "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES",
  "SE", "IS", "LI", "NO",
]

pub fn get_currency(country_code: String) -> Currency {
  case is_eea_country(country_code) {
    True -> EUR
    False -> USD
  }
}

pub fn is_eea_country(country_code: String) -> Bool {
  let upper_code = string.uppercase(country_code)
  list.contains(eea_countries, upper_code)
}

pub fn get_price_cents(tier: PricingTier, currency: Currency) -> Int {
  case tier, currency {
    Monthly, USD -> 499
    Monthly, EUR -> 499
    Yearly, USD -> 4999
    Yearly, EUR -> 4999
    Visionary, USD -> 29_900
    Visionary, EUR -> 29_900
    SelfHost, USD -> 1999
    SelfHost, EUR -> 1999
  }
}

pub fn format_price_cents(price_cents: Int, currency: Currency) -> String {
  case currency {
    USD -> "$" <> format_amount_cents(price_cents)
    EUR -> "€" <> format_amount_cents(price_cents)
  }
}

fn format_amount_cents(price_cents: Int) -> String {
  let dollars = price_cents / 100
  let cents = price_cents % 100
  let dollars_str = int.to_string(dollars)
  let cents_str = case cents < 10 {
    True -> "0" <> int.to_string(cents)
    False -> int.to_string(cents)
  }
  dollars_str <> "." <> cents_str
}

pub fn get_formatted_price(tier: PricingTier, country_code: String) -> String {
  let currency = get_currency(country_code)
  let price_cents = get_price_cents(tier, currency)
  format_price_cents(price_cents, currency)
}
