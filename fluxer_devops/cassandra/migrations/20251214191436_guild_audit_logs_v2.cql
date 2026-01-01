CREATE TABLE IF NOT EXISTS fluxer.guild_audit_logs_v2 (
    guild_id bigint,
    log_id bigint,
    user_id bigint,
    target_id text,
    action_type int,
    reason text,
    options map<text, text>,
    changes text,
    PRIMARY KEY ((guild_id), log_id)
) WITH CLUSTERING ORDER BY (log_id DESC)
AND default_time_to_live = 3888000;

CREATE TABLE IF NOT EXISTS fluxer.guild_audit_logs_v2_by_user (
    guild_id bigint,
    user_id bigint,
    log_id bigint,
    target_id text,
    action_type int,
    reason text,
    options map<text, text>,
    changes text,
    PRIMARY KEY ((guild_id, user_id), log_id)
) WITH CLUSTERING ORDER BY (log_id DESC)
AND default_time_to_live = 3888000;

CREATE TABLE IF NOT EXISTS fluxer.guild_audit_logs_v2_by_action (
    guild_id bigint,
    action_type int,
    log_id bigint,
    user_id bigint,
    target_id text,
    reason text,
    options map<text, text>,
    changes text,
    PRIMARY KEY ((guild_id, action_type), log_id)
) WITH CLUSTERING ORDER BY (log_id DESC)
AND default_time_to_live = 3888000;

CREATE TABLE IF NOT EXISTS fluxer.guild_audit_logs_v2_by_user_action (
    guild_id bigint,
    user_id bigint,
    action_type int,
    log_id bigint,
    target_id text,
    reason text,
    options map<text, text>,
    changes text,
    PRIMARY KEY ((guild_id, user_id, action_type), log_id)
) WITH CLUSTERING ORDER BY (log_id DESC)
AND default_time_to_live = 3888000;
