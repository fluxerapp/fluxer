CREATE TABLE IF NOT EXISTS fluxer.push_devices_by_fcm_token (
    fcm_token text,
    user_id bigint,
    device_id text,
    PRIMARY KEY ((fcm_token), user_id, device_id)
);
