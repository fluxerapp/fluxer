// SPDX-License-Identifier: AGPL-3.0-or-later

const FLUXER_EPOCH: u64 = 1_420_070_400_000;

pub fn snowflake_to_timestamp_ms(snowflake: &str) -> Option<u64> {
    let id: u64 = snowflake.parse().ok()?;
    let timestamp_ms = (id >> 22) + FLUXER_EPOCH;
    Some(timestamp_ms)
}

pub fn format_admin_timestamp(iso: &str) -> String {
    time::OffsetDateTime::parse(iso, &time::format_description::well_known::Rfc3339)
        .or_else(|_| {
            time::OffsetDateTime::parse(
                iso,
                &time::format_description::well_known::Iso8601::DEFAULT,
            )
        })
        .map_or_else(|_| iso.to_owned(), format_admin_datetime)
}

fn format_admin_datetime(dt: time::OffsetDateTime) -> String {
    let dt = dt.to_offset(time::UtcOffset::UTC);
    let month = match dt.month() {
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
    };
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snowflake_to_timestamp_ms_known_value() {
        assert_eq!(snowflake_to_timestamp_ms("0"), Some(FLUXER_EPOCH));
    }

    #[test]
    fn snowflake_to_timestamp_ms_real_id() {
        let snowflake = (1u64 << 22).to_string();
        assert_eq!(
            snowflake_to_timestamp_ms(&snowflake),
            Some(FLUXER_EPOCH + 1)
        );
    }

    #[test]
    fn snowflake_to_timestamp_ms_invalid() {
        assert_eq!(snowflake_to_timestamp_ms(""), None);
        assert_eq!(snowflake_to_timestamp_ms("abc"), None);
        assert_eq!(snowflake_to_timestamp_ms("-1"), None);
    }

    #[test]
    fn snowflake_creation_date_invalid() {
        assert_eq!(snowflake_creation_date("abc"), "Unknown");
    }

    #[test]
    fn snowflake_creation_date_valid() {
        assert_eq!(snowflake_creation_date("0"), "Jan 1, 2015, 12:00 AM UTC");
    }

    #[test]
    fn admin_timestamps_render_in_the_panel_format() {
        assert_eq!(
            format_admin_timestamp("2026-10-06T14:05:09.123Z"),
            "Oct 6, 2026, 2:05 PM UTC"
        );
        assert_eq!(
            format_admin_timestamp("2026-10-06T00:30:00+02:00"),
            "Oct 5, 2026, 10:30 PM UTC"
        );
        assert_eq!(format_admin_timestamp("not a date"), "not a date");
    }
}
