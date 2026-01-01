CREATE TABLE IF NOT EXISTS counters (
    metric_name LowCardinality(String),
    timestamp DateTime64(3, 'UTC') CODEC(DoubleDelta, ZSTD(1)),
    timestamp_bucket DateTime64(3, 'UTC') MATERIALIZED toStartOfInterval(timestamp, INTERVAL 1 minute),
    dimensions_hash String,
    dimensions Map(String, String) CODEC(ZSTD(1)),
    value Int64 CODEC(Delta, ZSTD(1)),
    INDEX idx_dimensions_hash dimensions_hash TYPE bloom_filter GRANULARITY 4
)
ENGINE = SummingMergeTree(value)
PARTITION BY toDate(timestamp_bucket)
ORDER BY (metric_name, timestamp_bucket, dimensions_hash)
TTL toDateTime(timestamp) + toIntervalDay(7)
SETTINGS index_granularity = 8192, ttl_only_drop_parts = 1;

CREATE TABLE IF NOT EXISTS gauges (
    id String,
    metric_name LowCardinality(String),
    timestamp DateTime64(3, 'UTC') CODEC(DoubleDelta, ZSTD(1)),
    dimensions_hash String,
    dimensions Map(String, String) CODEC(ZSTD(1)),
    value Float64 CODEC(Gorilla, ZSTD(1)),
    INDEX idx_dimensions_hash dimensions_hash TYPE bloom_filter GRANULARITY 4
)
ENGINE = MergeTree()
PARTITION BY toDate(timestamp)
ORDER BY (metric_name, dimensions_hash, timestamp, id)
TTL toDateTime(timestamp) + toIntervalDay(7)
SETTINGS index_granularity = 8192, ttl_only_drop_parts = 1;

CREATE TABLE IF NOT EXISTS histogram_raw (
    id String,
    metric_name LowCardinality(String),
    timestamp DateTime64(3, 'UTC') CODEC(DoubleDelta, ZSTD(1)),
    timestamp_bucket DateTime64(3, 'UTC') MATERIALIZED toStartOfInterval(timestamp, INTERVAL 1 minute),
    dimensions_hash String,
    dimensions Map(String, String) CODEC(ZSTD(1)),
    value_ms Float64 CODEC(Gorilla, ZSTD(1)),
    INDEX idx_dimensions_hash dimensions_hash TYPE bloom_filter GRANULARITY 4
)
ENGINE = MergeTree()
PARTITION BY toDate(timestamp_bucket)
ORDER BY (metric_name, timestamp_bucket, dimensions_hash, id)
TTL toDateTime(timestamp) + toIntervalDay(7)
SETTINGS index_granularity = 8192, ttl_only_drop_parts = 1;

CREATE TABLE IF NOT EXISTS crashes (
    id String,
    timestamp DateTime64(3, 'UTC') CODEC(DoubleDelta, ZSTD(1)),
    guild_id LowCardinality(String),
    stacktrace String,
    notified UInt8 DEFAULT 0,
    updated_at DateTime64(3, 'UTC') DEFAULT now64(3),
    INDEX idx_guild_id guild_id TYPE bloom_filter GRANULARITY 4
)
ENGINE = ReplacingMergeTree(updated_at)
PARTITION BY toDate(timestamp)
ORDER BY (timestamp, id)
TTL toDateTime(timestamp) + toIntervalDay(90)
SETTINGS index_granularity = 8192, ttl_only_drop_parts = 1;

CREATE TABLE IF NOT EXISTS counters_hourly (
    metric_name LowCardinality(String),
    period_start DateTime64(3, 'UTC') CODEC(DoubleDelta, ZSTD(1)),
    dimensions_hash String,
    dimensions Map(String, String) CODEC(ZSTD(1)),
    total_value Int64 CODEC(Delta, ZSTD(1)),
    sample_count UInt64 CODEC(Delta, ZSTD(1)),
    INDEX idx_dimensions_hash dimensions_hash TYPE bloom_filter GRANULARITY 4
)
ENGINE = SummingMergeTree((total_value, sample_count))
PARTITION BY toYYYYMM(period_start)
ORDER BY (metric_name, period_start, dimensions_hash)
TTL toDateTime(period_start) + toIntervalDay(90)
SETTINGS index_granularity = 8192, ttl_only_drop_parts = 1;

CREATE TABLE IF NOT EXISTS counters_daily (
    metric_name LowCardinality(String),
    period_start DateTime64(3, 'UTC') CODEC(DoubleDelta, ZSTD(1)),
    dimensions_hash String,
    dimensions Map(String, String) CODEC(ZSTD(1)),
    total_value Int64 CODEC(Delta, ZSTD(1)),
    sample_count UInt64 CODEC(Delta, ZSTD(1)),
    INDEX idx_dimensions_hash dimensions_hash TYPE bloom_filter GRANULARITY 4
)
ENGINE = SummingMergeTree((total_value, sample_count))
PARTITION BY toYYYYMM(period_start)
ORDER BY (metric_name, period_start, dimensions_hash)
TTL toDateTime(period_start) + toIntervalDay(365)
SETTINGS index_granularity = 8192, ttl_only_drop_parts = 1
