// SPDX-License-Identifier: AGPL-3.0-or-later

import {SsoStatusResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {z} from 'zod';

const LimitFilterResponse = z.object({
	traits: z.array(z.string()).optional().describe('Trait filters for this limit rule'),
	guildFeatures: z.array(z.string()).optional().describe('Guild feature filters for this limit rule'),
});

const LimitRuleResponse = z.object({
	id: z.string().describe('Unique identifier for this limit rule'),
	filters: LimitFilterResponse.optional().describe('Filters that determine when this rule applies'),
	overrides: z
		.record(z.string(), z.number())
		.describe('Map of limit keys to their override values (differences from defaults)'),
});

const LimitConfigResponse = z.object({
	version: z.literal(2).describe('Wire format version'),
	traitDefinitions: z.array(z.string()).describe('Available trait definitions (e.g., "premium")'),
	rules: z.array(LimitRuleResponse).describe('Array of limit rules to evaluate'),
	defaultsHash: z.string().describe('Hash of the default limit values for cache invalidation'),
});

const AppPublicConfigResponse = z.object({
	branding: z
		.object({
			product_name: z.string().describe('Public product name shown by client applications'),
			icon_url: z.string().nullable().describe('Optional image URL for the full application icon'),
			symbol_url: z.string().nullable().describe('Optional image URL for the compact application symbol'),
			logo_url: z.string().nullable().describe('Optional image URL for the application logo'),
			wordmark_url: z.string().nullable().describe('Optional image URL for the application wordmark'),
			favicon_url: z.string().nullable().describe('Optional favicon URL for browser metadata'),
			theme_color: z.string().nullable().describe('Optional browser theme color'),
		})
		.describe('Branding values safe to expose to clients'),
	setup: z
		.object({
			configured: z.boolean().describe('Whether the instance administrator has completed initial setup'),
			admin_url: z.string().nullable().describe('Admin panel URL to continue instance setup'),
		})
		.describe('Initial setup state for self-hosted instances'),
	legal: z
		.object({
			terms_url: z.string().nullable().describe('Optional public terms of service URL for account registration'),
			privacy_url: z.string().nullable().describe('Optional public privacy policy URL for account registration'),
		})
		.describe('Optional legal document URLs shown during public registration'),
	registration: z
		.object({
			collect_date_of_birth: z.boolean().describe('Whether public registration collects and validates date of birth'),
		})
		.describe('Public registration field collection policy'),
});

export const WellKnownFluxerResponse = z.object({
	api_code_version: z.number().int().describe('Version of the API server code'),
	endpoints: z
		.object({
			api: z.string().describe('Base URL for authenticated API requests'),
			api_client: z.string().describe('Base URL for client API requests'),
			api_public: z.string().describe('Base URL for public API requests'),
			gateway: z.string().describe('WebSocket URL for the gateway'),
			media: z.string().describe('Base URL for the media proxy'),
			static_cdn: z.string().describe('Base URL for static assets (avatars, emojis, etc.)'),
			marketing: z.string().describe('Base URL for the marketing website'),
			admin: z.string().describe('Base URL for the admin panel'),
			invite: z.string().describe('Base URL for invite links'),
			gift: z.string().describe('Base URL for gift links'),
			webapp: z.string().describe('Base URL for the web application'),
		})
		.describe('Endpoint URLs for various services'),
	captcha: z
		.object({
			provider: z.string().describe('Captcha provider name (hcaptcha, turnstile, none)'),
			hcaptcha_site_key: z.string().nullable().describe('hCaptcha site key if using hCaptcha'),
			turnstile_site_key: z.string().nullable().describe('Cloudflare Turnstile site key if using Turnstile'),
		})
		.describe('Captcha configuration'),
	features: z
		.object({
			voice_enabled: z.boolean().describe('Whether voice/video calling is enabled'),
			stripe_enabled: z.boolean().describe('Whether Stripe payments are enabled'),
			self_hosted: z.boolean().describe('Whether this is a self-hosted instance'),
			presigned_attachment_uploads: z
				.boolean()
				.describe('Whether clients can request presigned attachment upload URLs'),
			emails_enabled: z.boolean().describe('Whether the instance sends emails (verification, password reset, etc.)'),
		})
		.describe('Feature flags for this instance'),
	gif: z
		.object({
			provider: z.string().describe('Stable machine name of the active GIF provider.'),
			display_name: z.string().describe('Human-readable provider name shown in the UI'),
			attribution_required: z
				.boolean()
				.describe('Whether the client must show a "Powered by …" watermark for this provider'),
		})
		.describe('GIF provider configuration for clients'),
	sso: SsoStatusResponse.describe('Single sign-on configuration'),
	registration: z
		.object({
			mode: z.enum(['open', 'approval', 'closed']).describe('Public registration mode for this instance'),
			admin_registration_urls_enabled: z.boolean().describe('Whether admin-issued registration URLs are accepted'),
		})
		.describe('Registration policy for this instance'),
	community: z
		.object({
			single_community: z
				.boolean()
				.describe('Whether this instance runs as a single community that every user automatically joins'),
			single_community_guild_id: z
				.string()
				.nullable()
				.describe('The stock community guild ID when single-community mode is enabled'),
			direct_messages_disabled: z
				.boolean()
				.describe('Whether direct messages and friend requests are disabled instance-wide'),
		})
		.describe('Community topology and direct-message policy for this instance'),
	services: z
		.object({
			gif_enabled: z.boolean().describe('Whether the GIF picker is enabled for this instance'),
			youtube_enabled: z.boolean().describe('Whether YouTube link enrichment is enabled for this instance'),
			bluesky_enabled: z.boolean().describe('Whether Bluesky profile connections are enabled for this instance'),
		})
		.describe('Optional third-party service integrations enabled for this instance'),
	limits: LimitConfigResponse.describe('Limit configuration with rules and trait definitions'),
	push: z
		.object({
			public_vapid_key: z.string().nullable().describe('VAPID public key for web push notifications'),
		})
		.describe('Push notification configuration'),
	app_public: AppPublicConfigResponse.describe('Public application configuration for client-side features'),
});

export type WellKnownFluxerResponse = z.infer<typeof WellKnownFluxerResponse>;
