// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DerivedEndpoints} from '@fluxer/config/src/EndpointDerivation';

export type RuntimeEnv = 'development' | 'production' | 'test';
export type DatabaseBackend = 'postgres' | 'cassandra';
export type PublicScheme = 'http' | 'https';
export const CACHE_PURGE_ADAPTER_NAMES = ['none', 'http'] as const;
export type CachePurgeAdapterName = (typeof CACHE_PURGE_ADAPTER_NAMES)[number];
export const STORE_PRODUCT_SLOT_NAMES = ['monthly', 'yearly', 'gift_1_month', 'gift_1_year'] as const;
export type StoreProductSlotName = (typeof STORE_PRODUCT_SLOT_NAMES)[number];
export const ACCOUNT_IDENTITY_MODE_NAMES = ['email', 'username'] as const;
export type AccountIdentityModeName = (typeof ACCOUNT_IDENTITY_MODE_NAMES)[number];
export const TAG_STYLE_NAMES = ['none', 'random'] as const;
export type TagStyleName = (typeof TAG_STYLE_NAMES)[number];

export interface InstanceBrandingConfig {
	product_name: string;
	icon_url?: string;
	symbol_url?: string;
	logo_url?: string;
	wordmark_url?: string;
	favicon_url?: string;
	theme_color?: string;
	status_page_url?: string;
	status_page_incident_history_url?: string;
}

export interface MasterConfig {
	env: RuntimeEnv;
	domain: {
		base_domain: string;
		public_origin: string;
		public_scheme: PublicScheme;
		public_port: number;
		static_cdn_domain: string;
		invite_domain: string;
		gift_domain: string;
	};
	endpoint_overrides?: Partial<DerivedEndpoints>;
	endpoints: DerivedEndpoints;
	internal: {
		kv: string;
		kv_mode: 'standalone' | 'cluster';
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
			prepared_statements: boolean;
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
			reports: string;
			harvests: string;
		};
	};
	services: {
		api: {
			port: number;
			headers_timeout_ms: number;
			request_timeout_ms: number;
			max_inflight_requests: number;
			ip_ban_exempt_ips: Array<string>;
			donation_proxy_key: string;
			trusted_callers: Array<{
				name: string;
				key: string;
				buckets: Array<string>;
			}>;
			presigned_attachment_uploads_enabled: boolean;
			presigned_harvest_downloads_enabled: boolean;
			unfurl_ignored_hosts: Array<string>;
			app_origin_aliases: Array<string>;
			worker?: {
				mode?: 'all_lanes' | 'single_lane' | 'single_task';
				lane?: 'realtime' | 'unfurl' | 'lifecycle' | 'batch' | 'crosspost';
				task?: string;
				enable_cron_scheduler?: boolean;
				lane_concurrency_overrides?: {
					realtime?: number;
					unfurl?: number;
					lifecycle?: number;
					batch?: number;
					crosspost?: number;
				};
			};
			storage_change_feed?: {
				enabled?: boolean;
				stream?: string;
				skip_buckets?: Array<string>;
			};
		};
		nats?: {
			core_url?: string;
			jetstream_url?: string;
			auth_token?: string;
		};
		media_proxy: {
			secret_key: string;
			upload_relay: {
				endpoint: string;
				secret_base64: string;
				max_body_bytes: number;
				token_ttl_secs: number;
				keep_direct_countries: Array<string>;
			};
			attachment_urls: {
				secrets_base64: Array<string>;
			};
		};
		gateway: {
			rpc_auth_token?: string;
		};
		admin: {
			secret_key_base: string;
			oauth_client_secret: string;
		};
	};
	auth: {
		sudo_mode_secret: string;
		connection_initiation_secret: string;
		profile_pseudonym_secret: string;
		sso_allow_private_addresses: boolean;
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
	integrations: {
		email: {
			enabled: boolean;
			provider: 'smtp' | 'none';
			from_email: string;
			from_name: string;
			reply_to_email: string;
			app_base_url: string;
			webhook_secret?: string;
			smtp?: {
				host: string;
				port: number;
				username: string;
				password: string;
				secure: boolean;
			};
		};
		voice: {
			enabled: boolean;
			api_key: string;
			api_secret: string;
			url: string;
			internal_url: string;
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
			legacy_prices?: Record<string, Array<string> | undefined>;
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
		cache_purge: {
			adapter: CachePurgeAdapterName;
			http: {
				endpoint: string;
				token: string;
				timeout_ms: number;
			};
		};
		blocklist_feeds: {
			enabled?: boolean;
		};
		breached_password_check: {
			enabled?: boolean;
		};
		push: {
			apns: {
				enabled: boolean;
				team_id?: string;
				key_id?: string;
				private_key?: string;
				private_key_path?: string;
				apps?: Array<{
					app_id?: string;
					topic?: string;
					environment?: 'production' | 'development';
				}>;
			};
		};
		app_store: {
			enabled: boolean;
			issuer_id?: string;
			key_id?: string;
			private_key?: string;
			private_key_path?: string;
			apps?: Array<{
				bundle_id: string;
				app_apple_id: number;
			}>;
			products?: Record<string, StoreProductSlotName>;
		};
		google_play: {
			enabled: boolean;
			packages?: Array<string>;
			client_email?: string;
			private_key?: string;
			private_key_path?: string;
			service_account_json_path?: string;
			token_uri?: string;
			products?: Record<string, StoreProductSlotName>;
			push_audience?: string;
			push_service_account_email?: string;
		};
		store_billing: {
			sandbox_user_ids?: Array<string>;
			sandbox_entitles_all: boolean;
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
		account_identity: AccountIdentityModeName | null;
		tag_style: TagStyleName | null;
	};
	dev: {
		relax_registration_rate_limits: boolean;
		disable_rate_limits: boolean;
		test_mode_enabled: boolean;
		test_harness_token?: string;
		validate_responses?: boolean;
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
