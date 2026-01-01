CREATE TABLE IF NOT EXISTS fluxer.push_devices (
    user_id bigint,
    device_id text,
    fcm_token text,
    platform text,
    device_name text,
    created_at timestamp,
    updated_at timestamp,
    PRIMARY KEY (user_id, device_id)
);
