// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DerivedEndpoints} from './EndpointDerivation';

export type RuntimeEnv = 'development' | 'production' | 'test';
export type DatabaseBackend = 'postgres' | 'cassandra';
export type PublicScheme = 'http' | 'https';

export interface InstanceBrandingConfig {
	product_name: string;
	icon_url?: string;
	symbol_url?: string;
	logo_url?: string;
	wordmark_url?: string;
	favicon_url?: string;
	theme_color?: string;
}

export interface MasterConfig {
	env: RuntimeEnv;
	domain: {
		base_domain: string;
		public_scheme: PublicScheme;
		internal_scheme: PublicScheme;
		public_port: number;
		internal_port: number;
		static_cdn_domain: string;
		invite_domain: string;
		gift_domain: string;
	};
	endpoint_overrides?: Partial<DerivedEndpoints>;
	endpoints: DerivedEndpoints;
	internal: {
		kv: string;
		kv_provider: 'redis';
		kv_mode: 'standalone' | 'cluster';
		kv_cluster_nodes: Array<{host: string; port: number}>;
		kv_cluster_nat_map: Record<string, {host: string; port: number}>;
		api: string;
		gateway?: string;
		media_proxy: string;
	};
	database: {
		backend: DatabaseBackend;
		cassandra: {
			hosts: Array<string>;
			port: number;
			keyspace: string;
			local_dc: string;
			username: string;
			password: string;
		};
		postgres: {
			url: string;
			host: string;
			port: number;
			database: string;
			username: string;
			password: string;
			ssl: boolean;
			ssl_ca: string;
			max_connections: number;
			kv_table: string;
		};
	};
	s3?: {
		endpoint: string;
		presigned_url_base?: string;
		force_path_style: boolean;
		region: string;
		access_key_id: string;
		secret_access_key: string;
		buckets: {
			cdn: string;
			uploads: string;
			downloads: string;
			reports: string;
			harvests: string;
			static: string;
		};
	};
	services: {
		api: {
			port: number;
			presigned_attachment_uploads_enabled: boolean;
			unfurl_ignored_hosts: Array<string>;
			embeds: {
				oembed_html_enabled: boolean;
				oembed_html_allow_untrusted_on_self_hosted: boolean;
				oembed_html_allowed_hosts: Array<string>;
				cache_default_ttl_seconds: number;
				cache_max_ttl_seconds: number;
				cache_min_ttl_seconds: number;
				cache_respect_remote_ttl: boolean;
			};
			content_moderation?: {
				nsfw_threshold?: number;
			};
			worker?: {
				mode?: 'all_lanes' | 'single_lane' | 'single_task';
				lane?: 'realtime' | 'unfurl' | 'lifecycle' | 'batch';
				task?: string;
				enable_cron_scheduler?: boolean;
				enable_voice_reconciliation?: boolean;
				lane_concurrency_overrides?: {
					realtime?: number;
					unfurl?: number;
					lifecycle?: number;
					batch?: number;
				};
			};
		};
		nats?: {
			core_url?: string;
			jetstream_url?: string;
			auth_token?: string;
		};
		media_proxy: {
			host: string;
			port: number;
			secret_key: string;
			mode: string;
			upload_relay: {
				endpoint: string;
				max_body_bytes: number;
				token_ttl_secs: number;
				keep_direct_countries: Array<string>;
			};
		};
		gateway: {
			port: number;
			rpc_auth_token?: string;
			media_proxy_endpoint?: string;
			api_rpc_endpoint?: string;
			push_enabled: boolean;
		};
		admin: {
			port: number;
			base_path: string;
			secret_key_base: string;
			oauth_client_secret: string;
		};
		marketing: {
			port: number;
			host: string;
			base_path: string;
			secret_key_base: string;
		};
		app_proxy: {
			port: number;
			assets_dir: string;
		};
	};
	auth: {
		sudo_mode_secret: string;
		connection_initiation_secret: string;
		passkeys: {
			rp_name: string;
			rp_id: string;
			additional_allowed_origins: Array<string>;
		};
		vapid: {
			public_key: string;
			private_key: string;
			email: string;
		};
		bluesky: {
			enabled: boolean;
			client_name: string;
			client_uri: string;
			logo_uri: string;
			tos_uri: string;
			policy_uri: string;
			keys: Array<{
				kid: string;
				private_key?: string;
				private_key_path?: string;
			}>;
		};
	};
	cookie: {
		domain: string;
		secure: boolean;
	};
	integrations: {
		email: {
			enabled: boolean;
			provider: 'smtp' | 'none';
			from_email: string;
			from_name: string;
			webhook_secret?: string;
			smtp?: {
				host: string;
				port: number;
				username: string;
				password: string;
				secure: boolean;
			};
		};
		sms: {
			enabled: boolean;
			account_sid?: string;
			auth_token?: string;
			verify_service_sid?: string;
			inbound_challenge_number?: string;
			inbound_webhook_auth_token?: string;
			inbound_webhook_public_url?: string;
		};
		captcha: {
			enabled: boolean;
			provider: 'hcaptcha' | 'turnstile' | 'none';
			hcaptcha?: {
				site_key: string;
				secret_key: string;
			};
			turnstile?: {
				site_key: string;
				secret_key: string;
			};
		};
		voice: {
			enabled: boolean;
			api_key: string;
			api_secret: string;
			url: string;
			webhook_url: string;
			default_region?: {
				id: string;
				name: string;
				emoji: string;
				latitude: number;
				longitude: number;
			};
		};
		search: {
			engine: 'elasticsearch' | 'meilisearch';
			url: string;
			api_key: string;
			username: string;
			password: string;
			tls_reject_unauthorized: boolean;
		};
		stripe: {
			enabled: boolean;
			secret_key: string;
			webhook_secret: string;
			prices?: Record<string, string | undefined>;
		};
		ncmec: {
			enabled: boolean;
			base_url: string;
			username: string;
			password: string;
			reporter_email?: string;
		};
		clamav: {
			enabled: boolean;
			host: string;
			port: number;
			fail_open: boolean;
		};
		klipy: {
			api_key: string;
		};
		youtube: {
			api_key: string;
		};
		bunny: {
			purge_enabled: boolean;
			api_key: string;
			pull_zone_id: number;
		};
		risk_integration: {
			enabled: boolean;
			ipinfo_api_key: string;
			account_policy_dsl?: unknown;
			tor: {
				block_all_relays: boolean;
				reverse_dns_heuristic: boolean;
				reverse_dns_timeout_ms: number;
			};
		};
		push: {
			apns: {
				enabled: boolean;
				team_id?: string;
				key_id?: string;
				private_key?: string;
				private_key_path?: string;
				default_environment?: 'production' | 'development';
				apps?: Array<{
					app_id?: string;
					topic?: string;
					environment?: 'production' | 'development';
					project_id?: string;
				}>;
			};
			fcm: {
				enabled: boolean;
				project_id?: string;
				client_email?: string;
				private_key?: string;
				private_key_path?: string;
				service_account_json_path?: string;
				token_uri?: string;
				apps?: Array<{
					app_id?: string;
					topic?: string;
					environment?: 'production' | 'development';
					project_id?: string;
				}>;
			};
		};
	};
	instance: {
		self_hosted: boolean;
		auto_join_invite_code?: string;
		visionaries_guild_id?: string;
		visionaries_guild_visionary_role_id?: string;
		branding: InstanceBrandingConfig;
		setup: {
			configured: boolean;
		};
		abuse_policy: {
			inbound_phone_country_codes: Array<string>;
			phone_verification: {
				inbound_required_prefixes: Array<string>;
			};
			direct_contact_spam: {
				enabled: boolean;
				country_codes: Array<string>;
				distinct_target_threshold: number;
				target_window_ms: number;
				action: 'flag_spammer' | 'suppress_delivery';
			};
		};
	};
	dev: {
		relax_registration_rate_limits: boolean;
		disable_rate_limits: boolean;
		test_mode_enabled: boolean;
		test_harness_token?: string;
	};
	geoip: {
		maxmind_db_path: string;
	};
	proxy: {
		trust_client_ip_header: boolean;
		client_ip_header: string;
	};
	discovery: {
		enabled: boolean;
		min_member_count: number;
	};
	attachment_decay_enabled: boolean;
	deletion_grace_period_hours: number;
	inactivity_deletion_threshold_days: number;
}
