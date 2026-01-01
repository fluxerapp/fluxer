CREATE TABLE IF NOT EXISTS fluxer.expression_packs (
    pack_id bigint PRIMARY KEY,
    pack_type text,
    creator_id bigint,
    name text,
    description text,
    created_at timestamp,
    updated_at timestamp,
    version int
);

CREATE TABLE IF NOT EXISTS fluxer.pack_installations (
    user_id bigint,
    pack_id bigint,
    pack_type text,
    installed_at timestamp,
    PRIMARY KEY (user_id, pack_id)
);
