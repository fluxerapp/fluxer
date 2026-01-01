CREATE TABLE IF NOT EXISTS fluxer.email_change_tickets (
	ticket text PRIMARY KEY,
	user_id bigint,
	require_original boolean,
	original_email text,
	original_verified boolean,
	original_code text,
	original_code_sent_at timestamp,
	original_code_expires_at timestamp,
	new_email text,
	new_code text,
	new_code_sent_at timestamp,
	new_code_expires_at timestamp,
	status text,
	created_at timestamp,
	updated_at timestamp
);

CREATE TABLE IF NOT EXISTS fluxer.email_change_tokens (
	token_ text PRIMARY KEY,
	user_id bigint,
	new_email text,
	expires_at timestamp,
	created_at timestamp
);
