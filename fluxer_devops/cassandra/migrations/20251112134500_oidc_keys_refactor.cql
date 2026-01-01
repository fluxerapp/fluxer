CREATE TABLE IF NOT EXISTS fluxer.oidc_keys_by_kid (
    kid text PRIMARY KEY,
    alg text,
    public_jwk text,
    private_jwk text,
    created_at timestamp
);

CREATE TABLE IF NOT EXISTS fluxer.oidc_key_status (
    name text PRIMARY KEY,
    active_kid text,
    published_kids set<text>,
    updated_at timestamp
);
