CREATE TABLE IF NOT EXISTS fluxer.users (
    user_id bigint,
    username text,
    discriminator int,
    bot boolean,
    system boolean,
    email text,
    email_verified boolean,
    email_bounced boolean,
    phone text,
    password_hash text,
    totp_secret text,
    authenticator_types set<int>,
    avatar_hash text,
    banner_hash text,
    bio text,
    accent_color int,
    date_of_birth date,
    locale text,
    flags bigint,
    premium_type int,
    premium_since timestamp,
    premium_until timestamp,
    premium_lifetime_sequence int,
    stripe_subscription_id text,
    stripe_customer_id text,
    suspicious_activity_flags int,
    terms_agreed_at timestamp,
    privacy_agreed_at timestamp,
    last_active_at timestamp,
    last_active_ip text,
    temp_banned_until timestamp,
    pending_deletion_at timestamp,
    password_last_changed_at timestamp,
    pronouns text,
    acls set<text>,
    deletion_reason_code int,
    deletion_public_reason text,
    deletion_audit_log_reason text,
    first_refund_at timestamp,
    PRIMARY KEY ((user_id))
);

CREATE TABLE IF NOT EXISTS fluxer.users_by_email (
    email_lower text,
    user_id bigint,
    PRIMARY KEY ((email_lower), user_id)
);

CREATE TABLE IF NOT EXISTS fluxer.users_by_phone (
    phone text,
    user_id bigint,
    PRIMARY KEY ((phone), user_id)
);

CREATE TABLE IF NOT EXISTS fluxer.users_by_username (
    username text,
    discriminator int,
    user_id bigint,
    PRIMARY KEY ((username), discriminator, user_id)
);

CREATE TABLE IF NOT EXISTS fluxer.users_by_stripe_subscription_id (
    stripe_subscription_id text,
    user_id bigint,
    PRIMARY KEY ((stripe_subscription_id), user_id)
);

CREATE TABLE IF NOT EXISTS fluxer.users_by_stripe_customer_id (
    stripe_customer_id text,
    user_id bigint,
    PRIMARY KEY ((stripe_customer_id), user_id)
);

CREATE TABLE IF NOT EXISTS fluxer.user_activity_tracking (
    activity_month text,
    last_active_at timestamp,
    user_id bigint,
    PRIMARY KEY ((activity_month), last_active_at, user_id)
) WITH CLUSTERING ORDER BY (last_active_at ASC, user_id ASC);

CREATE TABLE IF NOT EXISTS fluxer.users_pending_deletion (
    deletion_date date,
    pending_deletion_at timestamp,
    user_id bigint,
    deletion_reason_code int,
    PRIMARY KEY ((deletion_date), pending_deletion_at, user_id)
) WITH CLUSTERING ORDER BY (pending_deletion_at ASC, user_id ASC);

CREATE TYPE IF NOT EXISTS fluxer.custom_status (
    text text,
    emoji_id bigint,
    emoji_name text,
    emoji_animated boolean,
    expires_at timestamp
);

CREATE TYPE IF NOT EXISTS fluxer.guild_folder (
    folder_id int,
    name text,
    color int,
    guild_ids list<bigint>
);

CREATE TABLE IF NOT EXISTS fluxer.user_settings (
    user_id bigint,
    locale text,
    theme text,
    status text,
    custom_status frozen<custom_status>,
    developer_mode boolean,
    message_display_compact boolean,
    animate_emoji boolean,
    animate_stickers int,
    gif_auto_play boolean,
    render_embeds boolean,
    render_reactions boolean,
    render_spoilers int,
    inline_attachment_media boolean,
    inline_embed_media boolean,
    explicit_content_filter int,
    friend_source_flags int,
    default_guilds_restricted boolean,
    restricted_guilds set<bigint>,
    guild_positions list<bigint>,
    guild_folders frozen<list<guild_folder>>,
    afk_timeout int,
    time_format int,
    PRIMARY KEY ((user_id))
);

CREATE TABLE IF NOT EXISTS fluxer.relationships (
    source_user_id bigint,
    target_user_id bigint,
    type int,
    nickname text,
    since timestamp,
    PRIMARY KEY ((source_user_id), target_user_id, type)
);

CREATE TABLE IF NOT EXISTS fluxer.notes (
    source_user_id bigint,
    target_user_id bigint,
    note text,
    PRIMARY KEY ((source_user_id), target_user_id)
);

CREATE TYPE IF NOT EXISTS fluxer.mute_config (
    end_time timestamp,
    selected_time_window int
);

CREATE TYPE IF NOT EXISTS fluxer.channel_override (
    collapsed boolean,
    message_notifications int,
    muted boolean,
    mute_config frozen<mute_config>
);

CREATE TABLE IF NOT EXISTS fluxer.user_guild_settings (
    user_id bigint,
    guild_id bigint,
    message_notifications int,
    muted boolean,
    mute_config frozen<mute_config>,
    mobile_push boolean,
    suppress_everyone boolean,
    suppress_roles boolean,
    hide_muted_channels boolean,
    channel_overrides frozen<map<bigint, channel_override>>,
    version int,
    PRIMARY KEY ((user_id), guild_id)
);

CREATE TABLE IF NOT EXISTS fluxer.private_channels (
    user_id bigint,
    channel_id bigint,
    is_gdm boolean,
    PRIMARY KEY ((user_id), channel_id)
);

CREATE TABLE IF NOT EXISTS fluxer.dm_states (
    hi_user_id bigint,
    lo_user_id bigint,
    channel_id bigint,
    PRIMARY KEY ((hi_user_id, lo_user_id), channel_id)
);

CREATE TABLE IF NOT EXISTS fluxer.read_states (
    user_id bigint,
    channel_id bigint,
    message_id bigint,
    mention_count int,
    last_pin_timestamp timestamp,
    PRIMARY KEY ((user_id), channel_id)
);

CREATE TABLE IF NOT EXISTS fluxer.recent_mentions (
    user_id bigint,
    message_id bigint,
    channel_id bigint,
    guild_id bigint,
    is_everyone boolean,
    is_role boolean,
    PRIMARY KEY ((user_id), message_id)
) WITH CLUSTERING ORDER BY (message_id DESC)
AND default_time_to_live = 604800;

CREATE TABLE IF NOT EXISTS fluxer.recent_mentions_by_guild (
    user_id bigint,
    guild_id bigint,
    message_id bigint,
    channel_id bigint,
    is_everyone boolean,
    is_role boolean,
    PRIMARY KEY ((user_id, guild_id), message_id)
) WITH CLUSTERING ORDER BY (message_id DESC)
AND default_time_to_live = 604800;

CREATE TABLE IF NOT EXISTS fluxer.saved_messages (
    user_id bigint,
    channel_id bigint,
    message_id bigint,
    saved_at timestamp,
    PRIMARY KEY ((user_id), message_id)
) WITH CLUSTERING ORDER BY (message_id DESC);

CREATE TABLE IF NOT EXISTS fluxer.auth_sessions (
    session_id_hash blob,
    user_id bigint,
    created_at timestamp,
    approx_last_used_at timestamp,
    client_ip text,
    client_os text,
    client_platform text,
    client_country text,
    PRIMARY KEY ((session_id_hash))
);

CREATE TABLE IF NOT EXISTS fluxer.auth_sessions_by_user_id (
    user_id bigint,
    session_id_hash blob,
    PRIMARY KEY ((user_id), session_id_hash)
);

CREATE TABLE IF NOT EXISTS fluxer.mfa_backup_codes (
    user_id bigint,
    code text,
    consumed boolean,
    PRIMARY KEY ((user_id), code)
);

CREATE TABLE IF NOT EXISTS fluxer.webauthn_credentials (
    user_id bigint,
    credential_id text,
    public_key blob,
    counter bigint,
    transports set<text>,
    name text,
    created_at timestamp,
    last_used_at timestamp,
    PRIMARY KEY (user_id, credential_id)
);

CREATE TABLE IF NOT EXISTS fluxer.webauthn_credential_lookup (
    credential_id text PRIMARY KEY,
    user_id bigint
);

CREATE TABLE IF NOT EXISTS fluxer.email_verification_tokens (
    token_ text,
    user_id bigint,
    email text,
    PRIMARY KEY ((token_), user_id)
) WITH default_time_to_live = 86400;

CREATE TABLE IF NOT EXISTS fluxer.password_reset_tokens (
    token_ text,
    user_id bigint,
    email text,
    PRIMARY KEY ((token_), user_id)
) WITH default_time_to_live = 3600;

CREATE TABLE IF NOT EXISTS fluxer.ip_authorization_tokens (
    token_ text,
    user_id bigint,
    email text,
    PRIMARY KEY ((token_), user_id)
) WITH default_time_to_live = 1800;

CREATE TABLE IF NOT EXISTS fluxer.email_revert_tokens (
    token_ text,
    user_id bigint,
    email text,
    PRIMARY KEY ((token_), user_id)
) WITH default_time_to_live = 172800;

CREATE TABLE IF NOT EXISTS fluxer.mfa_tickets (
    ticket text,
    user_id bigint,
    PRIMARY KEY ((ticket), user_id)
) WITH default_time_to_live = 300;

CREATE TABLE IF NOT EXISTS fluxer.phone_tokens (
    token_ text PRIMARY KEY,
    phone text,
    user_id bigint
);

CREATE TABLE IF NOT EXISTS fluxer.authorized_ips (
    user_id bigint,
    ip text,
    PRIMARY KEY ((user_id, ip))
);

CREATE TABLE IF NOT EXISTS fluxer.banned_emails (
    email_lower text PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS fluxer.banned_phones (
    phone text PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS fluxer.banned_ips (
    ip text PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS fluxer.beta_codes (
    code text,
    creator_id bigint,
    created_at timestamp,
    redeemer_id bigint,
    redeemed_at timestamp,
    PRIMARY KEY ((creator_id), code)
);

CREATE TABLE IF NOT EXISTS fluxer.beta_codes_by_code (
    code text,
    creator_id bigint,
    PRIMARY KEY ((code), creator_id)
);

CREATE TABLE IF NOT EXISTS fluxer.gift_codes (
    code text,
    amount_cents int,
    duration_months int,
    created_at timestamp,
    created_by_user_id bigint,
    redeemed_at timestamp,
    redeemed_by_user_id bigint,
    stripe_payment_intent_id text,
    PRIMARY KEY ((code))
);

CREATE TABLE IF NOT EXISTS fluxer.gift_codes_by_creator (
    created_by_user_id bigint,
    code text,
    PRIMARY KEY ((created_by_user_id), code)
);

CREATE TABLE IF NOT EXISTS fluxer.gift_codes_by_redeemer (
    redeemed_by_user_id bigint,
    code text,
    PRIMARY KEY ((redeemed_by_user_id), code)
);

CREATE TABLE IF NOT EXISTS fluxer.gift_codes_by_payment_intent (
    stripe_payment_intent_id text,
    code text,
    PRIMARY KEY ((stripe_payment_intent_id), code)
);

CREATE TABLE IF NOT EXISTS fluxer.guilds (
    guild_id bigint,
    owner_id bigint,
    name text,
    vanity_url_code text,
    icon_hash text,
    banner_hash text,
    splash_hash text,
    features set<text>,
    verification_level int,
    mfa_level int,
    nsfw_level int,
    explicit_content_filter int,
    default_message_notifications int,
    system_channel_id bigint,
    system_channel_flags int,
    rules_channel_id bigint,
    afk_channel_id bigint,
    afk_timeout int,
    disabled_operations int,
    max_presences int,
    member_count int,
    audit_logs_indexed_at timestamp,
    PRIMARY KEY ((guild_id))
);

CREATE TABLE IF NOT EXISTS fluxer.guilds_by_owner_id (
    owner_id bigint,
    guild_id bigint,
    PRIMARY KEY ((guild_id), owner_id)
);

CREATE TABLE IF NOT EXISTS fluxer.guild_members (
    guild_id bigint,
    user_id bigint,
    joined_at timestamp,
    nick text,
    avatar_hash text,
    banner_hash text,
    join_source_type int,
    source_invite_code text,
    inviter_id bigint,
    deaf boolean,
    mute boolean,
    communication_disabled_until timestamp,
    role_ids set<bigint>,
    is_premium_sanitized boolean,
    bio text,
    accent_color int,
    pronouns text,
    temporary boolean,
    PRIMARY KEY ((guild_id), user_id)
);

CREATE TABLE IF NOT EXISTS fluxer.guild_members_by_user_id (
    user_id bigint,
    guild_id bigint,
    PRIMARY KEY ((user_id), guild_id)
);

CREATE TABLE IF NOT EXISTS fluxer.guild_roles (
    guild_id bigint,
    role_id bigint,
    name text,
    permissions bigint,
    position int,
    color int,
    icon_hash text,
    unicode_emoji text,
    hoist boolean,
    mentionable boolean,
    PRIMARY KEY ((guild_id), role_id)
);

CREATE TABLE IF NOT EXISTS fluxer.guild_bans (
    guild_id bigint,
    user_id bigint,
    moderator_id bigint,
    banned_at timestamp,
    expires_at timestamp,
    reason text,
    ip text,
    PRIMARY KEY ((guild_id), user_id)
);

CREATE TABLE IF NOT EXISTS fluxer.guild_bans_by_ip (
    guild_id bigint,
    ip text,
    user_id bigint,
    PRIMARY KEY ((guild_id, ip), user_id)
);

CREATE TABLE IF NOT EXISTS fluxer.guild_emojis (
    guild_id bigint,
    emoji_id bigint,
    name text,
    creator_id bigint,
    animated boolean,
    PRIMARY KEY ((guild_id), emoji_id)
);

CREATE TABLE IF NOT EXISTS fluxer.guild_emojis_by_emoji_id (
    emoji_id bigint,
    guild_id bigint,
    name text,
    creator_id bigint,
    animated boolean,
    PRIMARY KEY ((emoji_id))
);

CREATE TABLE IF NOT EXISTS fluxer.guild_stickers (
    guild_id bigint,
    sticker_id bigint,
    name text,
    description text,
    format_type int,
    tags list<text>,
    creator_id bigint,
    PRIMARY KEY ((guild_id), sticker_id)
);

CREATE TABLE IF NOT EXISTS fluxer.guild_stickers_by_sticker_id (
    sticker_id bigint,
    guild_id bigint,
    name text,
    description text,
    format_type int,
    tags list<text>,
    creator_id bigint,
    PRIMARY KEY ((sticker_id))
);

CREATE TABLE IF NOT EXISTS fluxer.guild_audit_logs (
    guild_id bigint,
    log_id bigint,
    user_id bigint,
    target_type text,
    target_id text,
    action text,
    audit_log_reason text,
    metadata map<text, text>,
    changes text,
    created_at timestamp,
    PRIMARY KEY ((guild_id), log_id)
) WITH CLUSTERING ORDER BY (log_id DESC);

CREATE TYPE IF NOT EXISTS fluxer.permission_overwrite (
    type int,
    allow_ bigint,
    deny_ bigint
);

CREATE TABLE IF NOT EXISTS fluxer.channels (
    channel_id bigint,
    guild_id bigint,
    type int,
    name text,
    topic text,
    icon_hash text,
    url text,
    parent_id bigint,
    position int,
    owner_id bigint,
    recipient_ids set<bigint>,
    nsfw boolean,
    rate_limit_per_user int,
    bitrate int,
    user_limit int,
    rtc_region text,
    last_message_id bigint,
    last_pin_timestamp timestamp,
    permission_overwrites frozen<map<bigint, permission_overwrite>>,
    nicks map<text, text>,
    soft_deleted boolean,
    indexed_at timestamp,
    PRIMARY KEY ((channel_id, soft_deleted))
);

CREATE TABLE IF NOT EXISTS fluxer.channels_by_guild_id (
    guild_id bigint,
    channel_id bigint,
    PRIMARY KEY ((guild_id), channel_id)
);

CREATE TABLE IF NOT EXISTS fluxer.invites (
    code text,
    type int,
    guild_id bigint,
    channel_id bigint,
    inviter_id bigint,
    created_at timestamp,
    uses int,
    max_uses int,
    max_age int,
    temporary boolean,
    PRIMARY KEY ((code))
);

CREATE TABLE IF NOT EXISTS fluxer.invites_by_guild_id (
    guild_id bigint,
    code text,
    PRIMARY KEY ((guild_id), code)
);

CREATE TABLE IF NOT EXISTS fluxer.invites_by_channel_id (
    channel_id bigint,
    code text,
    PRIMARY KEY ((channel_id), code)
);

CREATE TABLE IF NOT EXISTS fluxer.webhooks (
    webhook_id bigint,
    webhook_token text,
    type int,
    guild_id bigint,
    channel_id bigint,
    creator_id bigint,
    name text,
    avatar_hash text,
    PRIMARY KEY ((webhook_id), webhook_token)
);

CREATE TABLE IF NOT EXISTS fluxer.webhooks_by_guild_id (
    guild_id bigint,
    webhook_id bigint,
    PRIMARY KEY ((guild_id), webhook_id)
);

CREATE TABLE IF NOT EXISTS fluxer.webhooks_by_channel_id (
    channel_id bigint,
    webhook_id bigint,
    PRIMARY KEY ((channel_id), webhook_id)
);

CREATE TYPE IF NOT EXISTS fluxer.message_attachment (
    attachment_id bigint,
    filename text,
    size bigint,
    title text,
    description text,
    width int,
    height int,
    duration int,
    content_type text,
    content_hash text,
    placeholder text,
    flags int,
    nsfw boolean
);

CREATE TYPE IF NOT EXISTS fluxer.message_embed_author (
    name text,
    url text,
    icon_url text
);

CREATE TYPE IF NOT EXISTS fluxer.message_embed_provider (
    name text,
    url text
);

CREATE TYPE IF NOT EXISTS fluxer.message_embed_footer (
    text text,
    icon_url text
);

CREATE TYPE IF NOT EXISTS fluxer.message_embed_media (
    url text,
    width int,
    height int,
    duration int,
    description text,
    content_type text,
    content_hash text,
    placeholder text,
    flags int
);

CREATE TYPE IF NOT EXISTS fluxer.message_embed_field (
    name text,
    value text,
    inline boolean
);

CREATE TYPE IF NOT EXISTS fluxer.message_embed (
    type text,
    title text,
    description text,
    url text,
    timestamp timestamp,
    color int,
    author frozen<message_embed_author>,
    provider frozen<message_embed_provider>,
    thumbnail frozen<message_embed_media>,
    image frozen<message_embed_media>,
    video frozen<message_embed_media>,
    footer frozen<message_embed_footer>,
    fields frozen<list<message_embed_field>>,
    nsfw boolean
);

CREATE TYPE IF NOT EXISTS fluxer.message_sticker_item (
    sticker_id bigint,
    name text,
    format_type int
);

CREATE TYPE IF NOT EXISTS fluxer.message_reference (
    channel_id bigint,
    message_id bigint,
    guild_id bigint,
    type int
);

CREATE TYPE IF NOT EXISTS fluxer.message_call (
    participant_ids set<bigint>,
    ended_timestamp timestamp
);

CREATE TYPE IF NOT EXISTS fluxer.message_snapshot (
    content text,
    timestamp timestamp,
    edited_timestmap timestamp,
    mention_users set<bigint>,
    mention_roles set<bigint>,
    mention_channels set<bigint>,
    attachments frozen<list<message_attachment>>,
    embeds frozen<list<message_embed>>,
    sticker_items frozen<list<message_sticker_item>>,
    type int,
    flags int
);

CREATE TABLE IF NOT EXISTS fluxer.messages (
    channel_id bigint,
    bucket int,
    message_id bigint,
    author_id bigint,
    type int,
    webhook_id bigint,
    webhook_name text,
    webhook_avatar_hash text,
    content text,
    edited_timestamp timestamp,
    pinned_timestamp timestamp,
    flags int,
    mention_everyone boolean,
    mention_users set<bigint>,
    mention_roles set<bigint>,
    mention_channels set<bigint>,
    attachments frozen<list<message_attachment>>,
    embeds frozen<list<message_embed>>,
    sticker_items frozen<list<message_sticker_item>>,
    message_reference frozen<message_reference>,
    call frozen<message_call>,
    message_snapshots frozen<list<message_snapshot>>,
    PRIMARY KEY ((channel_id, bucket), message_id)
) WITH CLUSTERING ORDER BY (message_id DESC);

CREATE TABLE IF NOT EXISTS fluxer.channel_pins (
    channel_id bigint,
    message_id bigint,
    pinned_timestamp timestamp,
    PRIMARY KEY ((channel_id), pinned_timestamp, message_id)
) WITH CLUSTERING ORDER BY (pinned_timestamp DESC, message_id DESC);

CREATE TABLE IF NOT EXISTS fluxer.messages_by_author_id (
    author_id bigint,
    channel_id bigint,
    message_id bigint,
    PRIMARY KEY ((author_id), channel_id, message_id)
);

CREATE TABLE IF NOT EXISTS fluxer.message_reactions (
    channel_id bigint,
    bucket int,
    message_id bigint,
    emoji_id bigint,
    emoji_name text,
    user_id bigint,
    emoji_animated boolean,
    PRIMARY KEY ((channel_id, bucket), message_id, emoji_id, emoji_name, user_id)
);

CREATE TABLE IF NOT EXISTS fluxer.attachment_lookup (
    channel_id bigint,
    attachment_id bigint,
    filename text,
    message_id bigint,
    PRIMARY KEY ((channel_id), attachment_id, filename)
);

CREATE TABLE IF NOT EXISTS fluxer.favorite_memes (
    user_id bigint,
    meme_id bigint,
    name text,
    alt_text text,
    tags list<text>,
    attachment_id bigint,
    filename text,
    content_type text,
    content_hash text,
    size bigint,
    width int,
    height int,
    duration int,
    storage_key text,
    created_at timestamp,
    PRIMARY KEY ((user_id), meme_id)
) WITH CLUSTERING ORDER BY (meme_id DESC);

CREATE TABLE IF NOT EXISTS fluxer.favorite_memes_by_meme_id (
    meme_id bigint,
    user_id bigint,
    PRIMARY KEY ((meme_id), user_id)
);

CREATE TABLE IF NOT EXISTS fluxer.push_subscriptions (
    user_id bigint,
    subscription_id text,
    endpoint text,
    p256dh_key text,
    auth_key text,
    user_agent text,
    PRIMARY KEY ((user_id), subscription_id)
);

CREATE TABLE IF NOT EXISTS fluxer.processed_payments (
    payment_intent_id text,
    user_id bigint,
    checkout_session_id text,
    price_id text,
    product_type text,
    amount_cents int,
    currency text,
    is_gift boolean,
    processed_at timestamp,
    PRIMARY KEY ((payment_intent_id))
);

CREATE TABLE IF NOT EXISTS fluxer.processed_payments_by_user (
    user_id bigint,
    payment_intent_id text,
    processed_at timestamp,
    PRIMARY KEY ((user_id), processed_at, payment_intent_id)
) WITH CLUSTERING ORDER BY (processed_at DESC, payment_intent_id ASC);

CREATE TABLE IF NOT EXISTS fluxer.visionary_purchases (
    purchase_id bigint,
    user_id bigint,
    purchased_at timestamp,
    sequence_number int,
    stripe_payment_intent_id text,
    PRIMARY KEY ((purchase_id))
);

CREATE TABLE IF NOT EXISTS fluxer.visionary_purchases_by_user (
    user_id bigint,
    purchase_id bigint,
    PRIMARY KEY ((user_id), purchase_id)
);

CREATE TABLE IF NOT EXISTS fluxer.user_harvests (
    user_id bigint,
    harvest_id bigint,
    requested_at timestamp,
    started_at timestamp,
    completed_at timestamp,
    failed_at timestamp,
    storage_key text,
    file_size bigint,
    progress_percent int,
    progress_step text,
    error_message text,
    download_url_expires_at timestamp,
    PRIMARY KEY (user_id, harvest_id)
) WITH CLUSTERING ORDER BY (harvest_id DESC);

CREATE TABLE IF NOT EXISTS fluxer.admin_audit_logs (
    log_id bigint PRIMARY KEY,
    admin_user_id bigint,
    target_type text,
    target_id bigint,
    action text,
    audit_log_reason text,
    metadata map<text, text>,
    created_at timestamp
);

CREATE TYPE IF NOT EXISTS fluxer.iar_message_context (
    message_id bigint,
    author_id bigint,
    author_username text,
    author_discriminator int,
    author_avatar_hash text,
    content text,
    timestamp timestamp,
    edited_timestamp timestamp,
    type int,
    flags int,
    mention_everyone boolean,
    mention_users set<bigint>,
    mention_roles set<bigint>,
    mention_channels set<bigint>,
    attachments frozen<list<message_attachment>>,
    embeds frozen<list<message_embed>>,
    sticker_items frozen<list<message_sticker_item>>
);

CREATE TABLE IF NOT EXISTS fluxer.iar_submissions (
    report_id bigint,
    reporter_id bigint,
    reported_at timestamp,
    status int,
    report_type int,
    category text,
    additional_info text,
    reported_user_id bigint,
    reported_user_username text,
    reported_user_discriminator int,
    reported_user_avatar_hash text,
    reported_guild_id bigint,
    reported_guild_name text,
    reported_guild_icon_hash text,
    reported_message_id bigint,
    reported_channel_id bigint,
    reported_channel_name text,
    message_context frozen<list<iar_message_context>>,
    guild_context_id bigint,
    resolved_at timestamp,
    resolved_by_admin_id bigint,
    public_comment text,
    audit_log_reason text,
    PRIMARY KEY ((report_id))
);

CREATE TABLE IF NOT EXISTS fluxer.pending_verifications (
    user_id bigint,
    created_at timestamp,
    PRIMARY KEY ((user_id))
);

CREATE TABLE IF NOT EXISTS fluxer.pending_verifications_by_time (
    created_at timestamp,
    user_id bigint,
    PRIMARY KEY ((created_at), user_id)
) WITH CLUSTERING ORDER BY (user_id ASC);
