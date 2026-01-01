CREATE TABLE IF NOT EXISTS fluxer.payments (
    checkout_session_id text,
    user_id bigint,
    stripe_customer_id text,
    payment_intent_id text,
    subscription_id text,
    invoice_id text,
    price_id text,
    product_type text,
    amount_cents int,
    currency text,
    status text,
    is_gift boolean,
    gift_code text,
    created_at timestamp,
    completed_at timestamp,
    PRIMARY KEY ((checkout_session_id))
);

CREATE TABLE IF NOT EXISTS fluxer.payments_by_payment_intent (
    payment_intent_id text,
    checkout_session_id text,
    PRIMARY KEY ((payment_intent_id))
);

CREATE TABLE IF NOT EXISTS fluxer.payments_by_subscription (
    subscription_id text,
    checkout_session_id text,
    user_id bigint,
    price_id text,
    product_type text,
    PRIMARY KEY ((subscription_id))
);

CREATE TABLE IF NOT EXISTS fluxer.payments_by_user (
    user_id bigint,
    created_at timestamp,
    checkout_session_id text,
    PRIMARY KEY ((user_id), created_at)
) WITH CLUSTERING ORDER BY (created_at DESC);

ALTER TABLE fluxer.gift_codes ADD checkout_session_id text;

CREATE TABLE IF NOT EXISTS fluxer.visionary_users (
    sequence_number int,
    user_id bigint,
    checkout_session_id text,
    gift_code text,
    granted_at timestamp,
    PRIMARY KEY ((sequence_number))
);

CREATE TABLE IF NOT EXISTS fluxer.visionary_users_by_user (
    user_id bigint,
    sequence_number int,
    granted_at timestamp,
    PRIMARY KEY ((user_id))
);

CREATE TABLE IF NOT EXISTS fluxer.visionary_counter (
    id text,
    next_sequence_number counter,
    PRIMARY KEY ((id))
);
