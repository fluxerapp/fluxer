CREATE TABLE IF NOT EXISTS fluxer.applications (
    application_id bigint PRIMARY KEY,
    owner_user_id bigint,
    name text,
    bot_user_id bigint,
    is_confidential boolean,
    oauth2_redirect_uris set<text>,
    oauth2_scopes set<text>,
    client_secret_hash text,
    bot_token_hash text,
    bot_token_preview text,
    bot_token_created_at timestamp,
    client_secret_created_at timestamp
);

CREATE TABLE IF NOT EXISTS fluxer.applications_by_owner (
    owner_user_id bigint,
    application_id bigint,
    PRIMARY KEY ((owner_user_id), application_id)
) WITH CLUSTERING ORDER BY (application_id DESC);

CREATE TABLE IF NOT EXISTS fluxer.oauth2_authorization_codes (
    code text PRIMARY KEY,
    application_id bigint,
    user_id bigint,
    redirect_uri text,
    scope set<text>,
    code_challenge text,
    code_challenge_method text,
    nonce text,
    created_at timestamp,
    expires_at timestamp
) WITH default_time_to_live = 600;

CREATE TABLE IF NOT EXISTS fluxer.oauth2_access_tokens (
    token_ text PRIMARY KEY,
    application_id bigint,
    user_id bigint,
    scope set<text>,
    created_at timestamp,
    expires_at timestamp
) WITH default_time_to_live = 3600;

CREATE TABLE IF NOT EXISTS fluxer.oauth2_access_tokens_by_user (
    user_id bigint,
    token_ text,
    PRIMARY KEY ((user_id), token_)
) WITH CLUSTERING ORDER BY (token_ DESC);

CREATE TABLE IF NOT EXISTS fluxer.oauth2_refresh_tokens (
    token_ text PRIMARY KEY,
    application_id bigint,
    user_id bigint,
    scope set<text>,
    created_at timestamp,
    expires_at timestamp
) WITH default_time_to_live = 2592000;

CREATE TABLE IF NOT EXISTS fluxer.oauth2_refresh_tokens_by_user (
    user_id bigint,
    token_ text,
    PRIMARY KEY ((user_id), token_)
) WITH CLUSTERING ORDER BY (token_ DESC);
