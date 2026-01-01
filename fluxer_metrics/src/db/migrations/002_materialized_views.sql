CREATE MATERIALIZED VIEW IF NOT EXISTS counters_hourly_mv
TO counters_hourly
AS SELECT
    metric_name,
    toStartOfHour(timestamp_bucket) AS period_start,
    dimensions_hash,
    anyLast(dimensions) AS dimensions,
    sum(value) AS total_value,
    count() AS sample_count
FROM counters
GROUP BY metric_name, period_start, dimensions_hash;

CREATE MATERIALIZED VIEW IF NOT EXISTS counters_daily_mv
TO counters_daily
AS SELECT
    metric_name,
    toStartOfDay(period_start) AS period_start,
    dimensions_hash,
    anyLast(dimensions) AS dimensions,
    sum(total_value) AS total_value,
    sum(sample_count) AS sample_count
FROM counters_hourly
GROUP BY metric_name, period_start, dimensions_hash
