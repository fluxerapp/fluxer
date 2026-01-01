ALTER TABLE fluxer.oauth2_access_tokens
WITH default_time_to_live = 604800;

ALTER TABLE fluxer.oauth2_access_tokens_by_user
WITH default_time_to_live = 604800;
