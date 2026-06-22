-- CQL schema for guild scheduled events (ScyllaDB / Cassandra)
-- Run these statements against the cluster keyspace to bring up the new tables.

CREATE TABLE IF NOT EXISTS guild_scheduled_events (
    guild_id    bigint,
    event_id    bigint,
    channel_id  bigint,
    creator_id  bigint,
    name        text,
    description text,
    image_hash  text,
    scheduled_start_time timestamp,
    scheduled_end_time   timestamp,
    status      text,
    entity_type text,
    entity_id   bigint,
    entity_location text,
    version     int,
    PRIMARY KEY (guild_id, event_id)
) WITH CLUSTERING ORDER BY (event_id ASC);

CREATE TABLE IF NOT EXISTS guild_scheduled_event_users (
    guild_id   bigint,
    event_id   bigint,
    user_id    bigint,
    created_at timestamp,
    PRIMARY KEY ((guild_id, event_id), user_id)
);
