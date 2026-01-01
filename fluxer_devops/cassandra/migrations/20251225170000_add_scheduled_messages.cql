CREATE TABLE IF NOT EXISTS fluxer.scheduled_messages (
    user_id bigint,
    scheduled_message_id bigint,
    channel_id bigint,
    payload text,
    scheduled_at timestamp,
    scheduled_local_at text,
    timezone text,
    status text,
    status_reason text,
    created_at timestamp,
    invalidated_at timestamp,
    PRIMARY KEY (user_id, scheduled_message_id)
) WITH CLUSTERING ORDER BY (scheduled_message_id DESC);
