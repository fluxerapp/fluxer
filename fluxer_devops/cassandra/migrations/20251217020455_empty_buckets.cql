CREATE TABLE IF NOT EXISTS fluxer.channel_empty_buckets (
    channel_id bigint,
    bucket int,
    updated_at timestamp,
    PRIMARY KEY ((channel_id), bucket)
) WITH CLUSTERING ORDER BY (bucket DESC);
