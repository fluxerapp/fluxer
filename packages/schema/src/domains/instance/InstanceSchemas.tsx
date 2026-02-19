/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {SsoStatusResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {z} from 'zod';

export const LimitFilterResponse = z.object({
	traits: z.array(z.string()).optional().describe('Trait filters for this limit rule'),
	guildFeatures: z.array(z.string()).optional().describe('Guild feature filters for this limit rule'),
});
export type LimitFilterResponse = z.infer<typeof LimitFilterResponse>;

export const LimitRuleResponse = z.object({
	id: z.string().describe('Unique identifier for this limit rule'),
	filters: LimitFilterResponse.optional().describe('Filters that determine when this rule applies'),
	overrides: z
		.record(z.string(), z.number())
		.describe('Map of limit keys to their override values (differences from defaults)'),
});
export type LimitRuleResponse = z.infer<typeof LimitRuleResponse>;

export const LimitConfigResponse = z.object({
	version: z.literal(2).describe('Wire format version'),
	traitDefinitions: z.array(z.string()).describe('Available trait definitions (e.g., "premium")'),
	rules: z.array(LimitRuleResponse).describe('Array of limit rules to evaluate'),
	defaultsHash: z.string().describe('Hash of the default limit values for cache invalidation'),
});
export type LimitConfigResponse = z.infer<typeof LimitConfigResponse>;

export const AppPublicConfigResponse = z.object({
	sentry_dsn: z.string().describe('Sentry DSN for client-side error reporting'),
});
export type AppPublicConfigResponse = z.infer<typeof AppPublicConfigResponse>;

export const InstanceInfoResponse = z.object({
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
			sms_mfa_enabled: z.boolean().describe('Whether SMS-based MFA is available'),
			voice_enabled: z.boolean().describe('Whether voice/video calling is enabled'),
			stripe_enabled: z.boolean().describe('Whether Stripe payments are enabled'),
			self_hosted: z.boolean().describe('Whether this is a self-hosted instance'),
			manual_review_enabled: z.boolean().describe('Whether manual review mode is enabled for registrations'),
		})
		.describe('Feature flags for this instance'),
	gif: z
		.object({
			provider: z.enum(['klipy', 'tenor']).describe('GIF provider used by the instance GIF picker'),
		})
		.describe('GIF provider configuration for clients'),
	sso: SsoStatusResponse.describe('Single sign-on configuration'),
	limits: LimitConfigResponse.describe('Limit configuration with rules and trait definitions'),
	push: z
		.object({
			public_vapid_key: z.string().nullable().describe('VAPID public key for web push notifications'),
		})
		.describe('Push notification configuration'),
	app_public: AppPublicConfigResponse.describe('Public application configuration for client-side features'),
	federation: z
		.object({
			enabled: z.boolean().describe('Whether federation is enabled on this instance'),
			version: z.number().int().describe('Federation protocol version'),
		})
		.optional()
		.describe('Federation configuration'),
	public_key: z
		.object({
			id: z.string().describe('Key identifier'),
			algorithm: z.literal('x25519').describe('Key algorithm'),
			public_key_base64: z.string().describe('Base64-encoded public key'),
		})
		.optional()
		.describe('Public key for E2E encryption'),
	oauth2: z
		.object({
			authorization_endpoint: z.string().describe('OAuth2 authorization endpoint URL'),
			token_endpoint: z.string().describe('OAuth2 token endpoint URL'),
			userinfo_endpoint: z.string().describe('OAuth2 userinfo endpoint URL'),
			scopes_supported: z.array(z.string()).describe('Supported OAuth2 scopes'),
		})
		.optional()
		.describe('OAuth2 endpoints for federation'),
});
export type InstanceInfoResponse = z.infer<typeof InstanceInfoResponse>;

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
			sms_mfa_enabled: z.boolean().describe('Whether SMS-based MFA is available'),
			voice_enabled: z.boolean().describe('Whether voice/video calling is enabled'),
			stripe_enabled: z.boolean().describe('Whether Stripe payments are enabled'),
			self_hosted: z.boolean().describe('Whether this is a self-hosted instance'),
			manual_review_enabled: z.boolean().describe('Whether manual review mode is enabled for registrations'),
		})
		.describe('Feature flags for this instance'),
	gif: z
		.object({
			provider: z.enum(['klipy', 'tenor']).describe('GIF provider used by the instance GIF picker'),
		})
		.describe('GIF provider configuration for clients'),
	sso: SsoStatusResponse.describe('Single sign-on configuration'),
	limits: LimitConfigResponse.describe('Limit configuration with rules and trait definitions'),
	push: z
		.object({
			public_vapid_key: z.string().nullable().describe('VAPID public key for web push notifications'),
		})
		.describe('Push notification configuration'),
	app_public: AppPublicConfigResponse.describe('Public application configuration for client-side features'),
	federation: z
		.object({
			enabled: z.boolean().describe('Whether federation is enabled on this instance'),
			version: z.number().int().describe('Federation protocol version'),
		})
		.optional()
		.describe('Federation configuration'),
	public_key: z
		.object({
			id: z.string().describe('Key identifier'),
			algorithm: z.literal('x25519').describe('Key algorithm'),
			public_key_base64: z.string().describe('Base64-encoded public key'),
		})
		.optional()
		.describe('Public key for E2E encryption'),
	oauth2: z
		.object({
			authorization_endpoint: z.string().describe('OAuth2 authorization endpoint URL'),
			token_endpoint: z.string().describe('OAuth2 token endpoint URL'),
			userinfo_endpoint: z.string().describe('OAuth2 userinfo endpoint URL'),
			scopes_supported: z.array(z.string()).describe('Supported OAuth2 scopes'),
		})
		.optional()
		.describe('OAuth2 endpoints for federation'),
});
export type WellKnownFluxerResponse = z.infer<typeof WellKnownFluxerResponse>;
