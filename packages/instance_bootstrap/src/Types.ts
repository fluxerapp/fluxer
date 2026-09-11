// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';

export interface InstanceEndpoints {
	api: string;
	api_client: string;
	api_public: string;
	gateway: string;
	media: string;
	static_cdn: string;
	marketing: string;
	admin: string;
	invite: string;
	gift: string;
	webapp: string;
}

export interface InstanceCaptcha {
	provider: 'hcaptcha' | 'turnstile' | 'none';
	hcaptcha_site_key: string | null;
	turnstile_site_key: string | null;
}

export interface InstanceFeatures {
	voice_enabled: boolean;
	stripe_enabled: boolean;
	self_hosted: boolean;
	presigned_attachment_uploads: boolean;
	emails_enabled: boolean;
}

export interface InstanceGif {
	provider: string;
	display_name: string;
	attribution_required: boolean;
}

export interface InstanceSso {
	enabled: boolean;
	enforced: boolean;
	display_name: string | null;
	redirect_uri: string;
}

export interface InstanceRegistration {
	mode: 'open' | 'approval' | 'closed';
	admin_registration_urls_enabled: boolean;
}

export interface InstanceCommunity {
	single_community: boolean;
	single_community_guild_id: string | null;
	direct_messages_disabled: boolean;
}

export interface InstanceServices {
	gif_enabled: boolean;
	youtube_enabled: boolean;
	bluesky_enabled: boolean;
}

export interface InstancePush {
	public_vapid_key: string | null;
}

export interface InstanceBranding {
	product_name: string;
	icon_url: string | null;
	symbol_url: string | null;
	logo_url: string | null;
	wordmark_url: string | null;
	favicon_url: string | null;
	theme_color: string | null;
	status_page_url: string | null;
	status_page_incident_history_url: string | null;
}

export interface InstanceSetup {
	configured: boolean;
	admin_url: string | null;
}

export interface InstanceLegal {
	terms_url: string | null;
	privacy_url: string | null;
}

export interface InstanceAppRegistration {
	collect_date_of_birth: boolean;
}

export interface InstanceAppPublic {
	branding: InstanceBranding;
	setup: InstanceSetup;
	legal: InstanceLegal;
	registration: InstanceAppRegistration;
}

export interface InstanceDiscoveryResponse {
	api_code_version: number;
	endpoints: InstanceEndpoints;
	captcha: InstanceCaptcha;
	features: InstanceFeatures;
	gif: InstanceGif;
	sso: InstanceSso;
	registration: InstanceRegistration;
	community: InstanceCommunity;
	services: InstanceServices;
	limits: LimitConfigSnapshot | LimitConfigWireFormat;
	push: InstancePush;
	app_public: InstanceAppPublic;
}

export interface GeoEntry {
	countryCode: string;
	regionCode: string | null;
}

export interface GeolocationResponse {
	countryCode: string | null;
	regionCode: string | null;
	latitude: string | null;
	longitude: string | null;
	ageRestrictedGeos: ReadonlyArray<GeoEntry>;
	ageBlockedGeos: ReadonlyArray<GeoEntry>;
}
