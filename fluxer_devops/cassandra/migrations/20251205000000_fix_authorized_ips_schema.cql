CREATE TABLE IF NOT EXISTS fluxer.authorized_ips_v2 (
    user_id bigint,
    ip text,
    PRIMARY KEY (user_id, ip)
);
