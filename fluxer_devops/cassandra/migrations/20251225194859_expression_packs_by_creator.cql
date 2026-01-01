CREATE TABLE IF NOT EXISTS fluxer.expression_packs_by_creator (
    creator_id bigint,
    pack_id bigint,
    pack_type text,
    name text,
    description text,
    created_at timestamp,
    updated_at timestamp,
    version int,
    PRIMARY KEY (creator_id, pack_id)
);
