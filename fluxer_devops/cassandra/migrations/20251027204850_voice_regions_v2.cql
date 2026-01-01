CREATE TABLE IF NOT EXISTS fluxer.voice_regions (
    id text PRIMARY KEY,
    name text,
    emoji text,
    latitude double,
    longitude double,
    priority int,
    is_default boolean,
    vip_only boolean,
    required_guild_features set<text>,
    allowed_guild_ids set<bigint>,
    allowed_user_ids set<bigint>,
    created_at timestamp,
    updated_at timestamp
);

CREATE TABLE IF NOT EXISTS fluxer.voice_servers (
    region_id text,
    server_id text,
    endpoint text,
    api_key text,
    api_secret text,
    priority int,
    is_active boolean,
    vip_only boolean,
    required_guild_features set<text>,
    allowed_guild_ids set<bigint>,
    allowed_user_ids set<bigint>,
    created_at timestamp,
    updated_at timestamp,
    PRIMARY KEY (region_id, server_id)
);
