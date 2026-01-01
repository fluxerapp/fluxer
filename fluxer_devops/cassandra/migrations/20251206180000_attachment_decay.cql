CREATE TABLE IF NOT EXISTS fluxer.attachment_decay_by_id (
    attachment_id bigint PRIMARY KEY,
    channel_id bigint,
    message_id bigint,
    filename text,
    size_bytes varint,
    uploaded_at timestamp,
    expires_at timestamp,
    last_accessed_at timestamp,
    cost double,
    lifetime_days int,
    status text
);

CREATE TABLE IF NOT EXISTS fluxer.attachment_decay_by_expiry (
    expiry_bucket int,
    expires_at timestamp,
    attachment_id bigint,
    channel_id bigint,
    message_id bigint,
    PRIMARY KEY ((expiry_bucket), expires_at, attachment_id)
);
