CREATE TABLE IF NOT EXISTS fluxer.user_contact_change_logs (
    user_id bigint,
    event_id timeuuid,
    field text,
    old_value text,
    new_value text,
    reason text,
    actor_user_id bigint,
    event_at timestamp,
    PRIMARY KEY ((user_id), event_id)
) WITH CLUSTERING ORDER BY (event_id DESC);
