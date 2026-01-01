CREATE TABLE IF NOT EXISTS fluxer.oauth_bot_tokens_by_client (
    client_id bigint,
    token_ text,
    user_id bigint,
    scopes set<text>,
    created_at timestamp,
    PRIMARY KEY (client_id, token_)
);
