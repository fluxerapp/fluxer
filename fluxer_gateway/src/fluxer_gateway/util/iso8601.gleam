// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/list
import gleam/result

const gregorian_seconds_to_unix_epoch = 62_167_219_200

pub fn parse_ms(text: String) -> Result(Int, Nil) {
  case <<text:utf8>> {
    <<
      y1,
      y2,
      y3,
      y4,
      "-",
      m1,
      m2,
      "-",
      d1,
      d2,
      "T",
      h1,
      h2,
      ":",
      i1,
      i2,
      ":",
      s1,
      s2,
      rest:bytes,
    >> -> {
      use year <- result.try(digits([y1, y2, y3, y4]))
      use month <- result.try(digits([m1, m2]))
      use day <- result.try(digits([d1, d2]))
      use hour <- result.try(digits([h1, h2]))
      use minute <- result.try(digits([i1, i2]))
      use second <- result.try(digits([s1, s2]))
      use fraction_ms <- result.try(fraction(rest))
      use days <- result.try(gregorian_days(year, month, day))
      let seconds = days * 86_400 + hour * 3600 + minute * 60 + second
      Ok({ seconds - gregorian_seconds_to_unix_epoch } * 1000 + fraction_ms)
    }
    _ -> Error(Nil)
  }
}

fn digits(codes: List(Int)) -> Result(Int, Nil) {
  digits_from(codes, 0)
}

fn digits_from(codes: List(Int), acc: Int) -> Result(Int, Nil) {
  case codes {
    [] -> Ok(acc)
    [code, ..rest] if code >= 48 && code <= 57 ->
      digits_from(rest, acc * 10 + code - 48)
    _ -> Error(Nil)
  }
}

fn fraction(rest: BitArray) -> Result(Int, Nil) {
  case rest {
    <<".", tail:bytes>> -> fraction_digits(tail, [])
    _ -> utc_designator(rest) |> result.map(fn(_) { 0 })
  }
}

fn utc_designator(rest: BitArray) -> Result(Nil, Nil) {
  case rest {
    <<"Z">>
    | <<"Z\n">>
    | <<"+00:00">>
    | <<"+00:00\n">>
    | <<"-00:00">>
    | <<"-00:00\n">>
    | <<"+0000">>
    | <<"+0000\n">>
    | <<"-0000">>
    | <<"-0000\n">> -> Ok(Nil)
    _ -> Error(Nil)
  }
}

fn fraction_digits(bytes: BitArray, acc: List(Int)) -> Result(Int, Nil) {
  case bytes {
    <<code, rest:bytes>> if code >= 48 && code <= 57 ->
      fraction_digits(rest, [code, ..acc])
    _ ->
      utc_designator(bytes)
      |> result.try(fn(_) { fraction_ms(list.reverse(acc)) })
  }
}

fn fraction_ms(codes: List(Int)) -> Result(Int, Nil) {
  case list.length(codes) {
    length if length >= 1 && length <= 9 ->
      list.append(codes, [48, 48, 48]) |> list.take(3) |> digits
    _ -> Error(Nil)
  }
}

fn gregorian_days(year: Int, month: Int, day: Int) -> Result(Int, Nil) {
  use before_month <- result.try(days_before_month(month))
  case day > 0 {
    True ->
      Ok(
        days_before_year(year)
        + before_month
        + leap_day_before(year, month)
        + day
        - 1,
      )
    False -> Error(Nil)
  }
}

fn days_before_year(year: Int) -> Int {
  case year <= 0 {
    True -> 0
    False -> {
      let x = year - 1
      x / 4 - x / 100 + x / 400 + x * 365 + 366
    }
  }
}

fn days_before_month(month: Int) -> Result(Int, Nil) {
  case month {
    1 -> Ok(0)
    2 -> Ok(31)
    3 -> Ok(59)
    4 -> Ok(90)
    5 -> Ok(120)
    6 -> Ok(151)
    7 -> Ok(181)
    8 -> Ok(212)
    9 -> Ok(243)
    10 -> Ok(273)
    11 -> Ok(304)
    12 -> Ok(334)
    _ -> Error(Nil)
  }
}

fn leap_day_before(year: Int, month: Int) -> Int {
  case month >= 3 && is_leap_year(year) {
    True -> 1
    False -> 0
  }
}

fn is_leap_year(year: Int) -> Bool {
  year % 4 == 0 && { year % 100 != 0 || year % 400 == 0 }
}
