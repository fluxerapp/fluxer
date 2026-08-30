// SPDX-License-Identifier: AGPL-3.0-or-later

const HARNESS_ENDPOINT = 'https://primary.test/api';

export function installVoiceMenuTestBootstrap(): void {
	const host = globalThis as unknown as {window?: Record<string, unknown>};
	if (typeof host.window === 'undefined') {
		host.window = host as unknown as Record<string, unknown>;
	}
	host.window.__FLUXER_BOOTSTRAP__ = {
		config: {
			releaseChannel: 'stable',
			bootstrapApiEndpoint: HARNESS_ENDPOINT,
			bootstrapApiPublicEndpoint: HARNESS_ENDPOINT,
		},
		instance: {
			api_code_version: Number.MAX_SAFE_INTEGER,
			endpoints: {
				api: HARNESS_ENDPOINT,
				api_client: HARNESS_ENDPOINT,
				api_public: HARNESS_ENDPOINT,
				gateway: 'wss://gateway.primary.test',
				media: 'https://media.primary.test',
				static_cdn: 'https://cdn.primary.test',
				marketing: 'https://primary.test',
				admin: 'https://admin.primary.test',
				invite: 'https://primary.test/invite',
				gift: 'https://primary.test/gift',
				webapp: 'https://app.primary.test',
				upload_relay: 'https://upload.primary.test',
			},
			captcha: {provider: 'none', hcaptcha_site_key: null, turnstile_site_key: null},
			features: {
				voice_enabled: false,
				stripe_enabled: false,
				self_hosted: false,
				presigned_attachment_uploads: false,
				emails_enabled: false,
			},
			gif: {provider: 'klipy', display_name: 'Klipy', attribution_required: false},
			sso: {enabled: false, enforced: false, display_name: null, redirect_uri: ''},
			registration: {mode: 'open', admin_registration_urls_enabled: true},
			community: {single_community: false, single_community_guild_id: null, direct_messages_disabled: false},
			services: {gif_enabled: true, youtube_enabled: false, bluesky_enabled: false},
			limits: undefined,
			push: {public_vapid_key: null},
			app_public: {
				branding: {
					product_name: 'Fluxer',
					icon_url: null,
					symbol_url: null,
					logo_url: null,
					wordmark_url: null,
					favicon_url: null,
					theme_color: null,
				},
				setup: {configured: true, admin_url: null},
				legal: {terms_url: null, privacy_url: null},
				registration: {collect_date_of_birth: true},
			},
		},
		geoip: {
			countryCode: null,
			regionCode: null,
			latitude: null,
			longitude: null,
			ageRestrictedGeos: [],
			ageBlockedGeos: [],
		},
	};
}
