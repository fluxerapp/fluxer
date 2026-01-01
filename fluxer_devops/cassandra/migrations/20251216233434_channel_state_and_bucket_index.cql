CREATE TABLE IF NOT EXISTS fluxer.channel_state (
    channel_id bigint PRIMARY KEY,
    created_bucket int,
    has_messages boolean,
    last_message_id bigint,
    last_message_bucket int,
    updated_at timestamp
);

CREATE TABLE IF NOT EXISTS fluxer.channel_message_buckets (
    channel_id bigint,
    bucket int,
    updated_at timestamp,
    PRIMARY KEY (channel_id, bucket)
) WITH CLUSTERING ORDER BY (bucket DESC);
