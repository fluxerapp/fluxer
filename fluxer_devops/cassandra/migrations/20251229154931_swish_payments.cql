CREATE TABLE IF NOT EXISTS fluxer.swish_payments (
    payment_id text,
    user_id bigint,
    product_type text,
    status text,
    is_gift boolean,
    amount int,
    currency text,
    message text,
    payer_alias text,
    payment_reference text,
    gift_code text,
    created_at timestamp,
    completed_at timestamp,
    PRIMARY KEY ((payment_id))
);

CREATE TABLE IF NOT EXISTS fluxer.swish_payments_by_user (
    user_id bigint,
    payment_id text,
    product_type text,
    status text,
    is_gift boolean,
    amount int,
    currency text,
    created_at timestamp,
    PRIMARY KEY ((user_id), created_at)
) WITH CLUSTERING ORDER BY (created_at DESC);
