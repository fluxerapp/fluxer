// SPDX-License-Identifier: AGPL-3.0-or-later

const FLUXER_EPOCH: u64 = 1_420_070_400_000;

pub fn snowflake_to_timestamp_ms(snowflake: &str) -> Option<u64> {
    let id: u64 = snowflake.parse().ok()?;
    let timestamp_ms = (id >> 22) + FLUXER_EPOCH;
    Some(timestamp_ms)
}

fn parse_admin_timestamp(iso: &str) -> Option<time::OffsetDateTime> {
    time::OffsetDateTime::parse(iso, &time::format_description::well_known::Rfc3339)
        .or_else(|_| {
            time::OffsetDateTime::parse(
                iso,
                &time::format_description::well_known::Iso8601::DEFAULT,
            )
        })
        .ok()
}

pub fn format_admin_timestamp(iso: &str) -> String {
    parse_admin_timestamp(iso).map_or_else(|| iso.to_owned(), format_admin_datetime)
}

pub fn format_admin_date(iso: &str) -> String {
    parse_admin_timestamp(iso).map_or_else(
        || iso.to_owned(),
        |dt| {
            let dt = dt.to_offset(time::UtcOffset::UTC);
            format!(
                "{} {}, {}",
                month_abbreviation(dt.month()),
                dt.day(),
                dt.year()
            )
        },
    )
}

fn month_abbreviation(month: time::Month) -> &'static str {
    match month {
        time::Month::January => "Jan",
        time::Month::February => "Feb",
        time::Month::March => "Mar",
        time::Month::April => "Apr",
        time::Month::May => "May",
        time::Month::June => "Jun",
        time::Month::July => "Jul",
        time::Month::August => "Aug",
        time::Month::September => "Sep",
        time::Month::October => "Oct",
        time::Month::November => "Nov",
        time::Month::December => "Dec",
    }
}

fn format_admin_datetime(dt: time::OffsetDateTime) -> String {
    let dt = dt.to_offset(time::UtcOffset::UTC);
    let month = month_abbreviation(dt.month());
    let day = dt.day();
    let year = dt.year();
    let hour_12 = match dt.hour() {
        0 => 12,
        h if h > 12 => h - 12,
        h => h,
    };
    let minute = dt.minute();
    let ampm = if dt.hour() < 12 { "AM" } else { "PM" };
    format!("{month} {day}, {year}, {hour_12}:{minute:02} {ampm} UTC")
}

pub fn snowflake_creation_date(snowflake: &str) -> String {
    snowflake_to_timestamp_ms(snowflake)
        .and_then(|ms| {
            time::OffsetDateTime::from_unix_timestamp_nanos(i128::from(ms) * 1_000_000).ok()
        })
        .map_or_else(|| "Unknown".to_owned(), format_admin_datetime)
}
