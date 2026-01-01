ALTER TABLE fluxer.push_devices WITH default_time_to_live = 7776000;

ALTER TABLE fluxer.push_devices_by_fcm_token WITH default_time_to_live = 7776000;

ALTER TABLE fluxer.push_subscriptions WITH default_time_to_live = 7776000;

ALTER TABLE fluxer.phone_tokens WITH default_time_to_live = 2592000;
