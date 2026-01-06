CREATE TABLE IF NOT EXISTS fluxer.user_dm_history (
    user_id bigint,
    channel_id bigint,
    PRIMARY KEY ((user_id), channel_id)
);
