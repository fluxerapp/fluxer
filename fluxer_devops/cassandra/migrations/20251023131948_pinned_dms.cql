CREATE TABLE IF NOT EXISTS fluxer.pinned_dms (
    user_id bigint,
    channel_id bigint,
    sort_order int,
    PRIMARY KEY ((user_id), channel_id)
);
