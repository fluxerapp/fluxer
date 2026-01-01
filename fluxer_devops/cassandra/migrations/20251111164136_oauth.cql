CREATE TABLE IF NOT EXISTS fluxer.oauth_clients (
    client_id bigint PRIMARY KEY,
    client_secret_hash text,
    name text,
    description text,
    icon_url text,
    owner_user_id bigint,
    team_id bigint,
    type text,
    redirect_uris set<text>,
    scopes set<text>,
    grant_types set<text>,
    homepage_url text,
    created_at timestamp,
    updated_at timestamp
);

CREATE TABLE IF NOT EXISTS fluxer.oauth_clients_by_owner (
    owner_user_id bigint,
    client_id bigint,
    PRIMARY KEY ((owner_user_id), client_id)
);

CREATE TABLE IF NOT EXISTS fluxer.oauth_authorization_requests (
    request_id text PRIMARY KEY,
    client_id bigint,
    redirect_uri text,
    scope set<text>,
    state text,
    code_challenge text,
    code_challenge_method text,
    nonce text,
    created_at timestamp,
    expires_at timestamp
) WITH default_time_to_live = 900;

CREATE TABLE IF NOT EXISTS fluxer.oauth_authorization_codes (
    code text PRIMARY KEY,
    client_id bigint,
    user_id bigint,
    redirect_uri text,
    scope set<text>,
    code_challenge text,
    code_challenge_method text,
    nonce text,
    created_at timestamp,
    expires_at timestamp
) WITH default_time_to_live = 900;

CREATE TABLE IF NOT EXISTS fluxer.oauth_access_tokens (
    token_ text PRIMARY KEY,
    client_id bigint,
    user_id bigint,
    scope set<text>,
    created_at timestamp,
    expires_at timestamp
) WITH default_time_to_live = 86400;

CREATE TABLE IF NOT EXISTS fluxer.oauth_access_tokens_by_client (
    client_id bigint,
    token_ text,
    PRIMARY KEY ((client_id), token_)
);

CREATE TABLE IF NOT EXISTS fluxer.oauth_access_tokens_by_user (
    user_id bigint,
    token_ text,
    PRIMARY KEY ((user_id), token_)
);

CREATE TABLE IF NOT EXISTS fluxer.oauth_refresh_tokens (
    token_ text PRIMARY KEY,
    client_id bigint,
    user_id bigint,
    scope set<text>,
    created_at timestamp,
    expires_at timestamp
) WITH default_time_to_live = 2592000;

CREATE TABLE IF NOT EXISTS fluxer.oauth_refresh_tokens_by_client (
    client_id bigint,
    token_ text,
    PRIMARY KEY ((client_id), token_)
);

CREATE TABLE IF NOT EXISTS fluxer.oauth_refresh_tokens_by_user (
    user_id bigint,
    token_ text,
    PRIMARY KEY ((user_id), token_)
);

CREATE TABLE IF NOT EXISTS fluxer.oauth_teams (
    team_id bigint PRIMARY KEY,
    name text,
    owner_user_id bigint,
    created_at timestamp
);

CREATE TABLE IF NOT EXISTS fluxer.oauth_teams_by_owner (
    owner_user_id bigint,
    team_id bigint,
    PRIMARY KEY ((owner_user_id), team_id)
);

CREATE TABLE IF NOT EXISTS fluxer.oauth_team_members (
    team_id bigint,
    user_id bigint,
    role text,
    added_at timestamp,
    PRIMARY KEY ((team_id), user_id)
);

CREATE TABLE IF NOT EXISTS fluxer.oauth_team_members_by_user (
    user_id bigint,
    team_id bigint,
    PRIMARY KEY ((user_id), team_id)
);

CREATE TABLE IF NOT EXISTS fluxer.oauth_bot_tokens (
    token_ text PRIMARY KEY,
    client_id bigint,
    user_id bigint,
    scopes set<text>,
    created_at timestamp,
    revoked boolean
);

CREATE TABLE IF NOT EXISTS fluxer.oidc_keys (
    kid text PRIMARY KEY,
    alg text,
    public_jwk text,
    private_jwk text,
    created_at timestamp,
    active boolean
);
