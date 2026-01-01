ALTER TABLE fluxer.iar_submissions ADD reporter_email text;
ALTER TABLE fluxer.iar_submissions ADD reporter_full_legal_name text;
ALTER TABLE fluxer.iar_submissions ADD reporter_country_of_residence text;
ALTER TABLE fluxer.iar_submissions ADD reported_guild_invite_code text;

CREATE TABLE IF NOT EXISTS fluxer.dsa_report_email_verifications (
    email_lower text,
    code_hash text,
    expires_at timestamp,
    last_sent_at timestamp,
    PRIMARY KEY ((email_lower))
);

CREATE TABLE IF NOT EXISTS fluxer.dsa_report_tickets (
    ticket text,
    email_lower text,
    expires_at timestamp,
    created_at timestamp,
    PRIMARY KEY ((ticket))
);
