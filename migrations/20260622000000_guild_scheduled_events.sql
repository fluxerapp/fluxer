-- Migration: Add guild_scheduled_events support
-- Addresses: fluxerapp/fluxer-meta#19 (Community Events) and #20 (Instance Controls / Safety)

CREATE TABLE IF NOT EXISTS guild_scheduled_events (
    event_id        BIGINT          NOT NULL,
    guild_id        BIGINT          NOT NULL REFERENCES guilds(guild_id) ON DELETE CASCADE,
    channel_id      BIGINT          REFERENCES channels(channel_id) ON DELETE SET NULL,
    creator_id      BIGINT          NOT NULL REFERENCES users(user_id),
    name            VARCHAR(100)    NOT NULL,
    description     VARCHAR(1000),
    image_hash      VARCHAR(255),
    scheduled_start_time TIMESTAMPTZ NOT NULL,
    scheduled_end_time   TIMESTAMPTZ,
    status          VARCHAR(16)     NOT NULL DEFAULT 'SCHEDULED'
                        CHECK (status IN ('SCHEDULED','ACTIVE','COMPLETED','CANCELLED')),
    entity_type     VARCHAR(16)     NOT NULL
                        CHECK (entity_type IN ('STAGE_INSTANCE','VOICE','EXTERNAL')),
    entity_id       BIGINT,
    entity_metadata JSONB,
    created_at      TIMESTAMPTZ     NOT NULL DEFAULT NOW(),
    updated_at      TIMESTAMPTZ     NOT NULL DEFAULT NOW(),

    PRIMARY KEY (guild_id, event_id)
);

CREATE INDEX IF NOT EXISTS idx_guild_scheduled_events_guild_status
    ON guild_scheduled_events(guild_id, status);

CREATE INDEX IF NOT EXISTS idx_guild_scheduled_events_start_time
    ON guild_scheduled_events(scheduled_start_time);

-- RSVP / subscriber tracking
CREATE TABLE IF NOT EXISTS guild_scheduled_event_users (
    event_id    BIGINT      NOT NULL,
    guild_id    BIGINT      NOT NULL,
    user_id     BIGINT      NOT NULL REFERENCES users(user_id) ON DELETE CASCADE,
    created_at  TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (guild_id, event_id, user_id),
    FOREIGN KEY (guild_id, event_id)
        REFERENCES guild_scheduled_events(guild_id, event_id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_guild_scheduled_event_users_event
    ON guild_scheduled_event_users(guild_id, event_id);
