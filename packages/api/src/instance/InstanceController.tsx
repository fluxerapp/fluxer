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

import {Config} from '@fluxer/api/src/Config';
import {getKeyManager} from '@fluxer/api/src/federation/KeyManager';
import type {InstanceConfigRepository} from '@fluxer/api/src/instance/InstanceConfigRepository';
import type {LimitConfigService} from '@fluxer/api/src/limits/LimitConfigService';
import {RateLimitMiddleware} from '@fluxer/api/src/middleware/RateLimitMiddleware';
import {OpenAPI} from '@fluxer/api/src/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@fluxer/api/src/RateLimitConfig';
import type {HonoEnv} from '@fluxer/api/src/types/HonoEnv';
import {API_CODE_VERSION} from '@fluxer/constants/src/AppConstants';
import {FEDERATION_PROTOCOL_VERSION} from '@fluxer/constants/src/Federation';
import {WellKnownFluxerResponse} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import type {Hono} from 'hono';

export function InstanceController(app: Hono<HonoEnv>) {
	app.get(
		'/.well-known/fluxer',
		RateLimitMiddleware(RateLimitConfigs.INSTANCE_INFO),
		OpenAPI({
			operationId: 'get_well_known_fluxer',
			summary: 'Get instance discovery document',
			responseSchema: WellKnownFluxerResponse,
			statusCode: 200,
			security: [],
			tags: ['Instance'],
			description:
				'Returns the instance discovery document including API endpoints, feature flags, limits, and federation capabilities. This is the canonical discovery endpoint for all Fluxer clients.',
		}),
		async (ctx) => {
			ctx.header('Access-Control-Allow-Origin', '*');

			const limitConfigService = ctx.get('limitConfigService') as LimitConfigService | undefined;
			const limits = limitConfigService?.getConfigWireFormat();

			const instanceConfigRepository = ctx.get('instanceConfigRepository') as InstanceConfigRepository | undefined;
			const instanceConfig = await instanceConfigRepository?.getInstanceConfig();

			const apiClientEndpoint = Config.endpoints.apiClient;
			const apiPublicEndpoint = Config.endpoints.apiPublic;

			const sso = await ctx.get('ssoService').getPublicStatus();

			const response: Record<string, unknown> = {
				api_code_version: API_CODE_VERSION,
				endpoints: {
					api: apiClientEndpoint,
					api_client: apiClientEndpoint,
					api_public: apiPublicEndpoint,
					gateway: Config.endpoints.gateway,
					media: Config.endpoints.media,
					static_cdn: Config.endpoints.staticCdn,
					marketing: Config.endpoints.marketing,
					admin: Config.endpoints.admin,
					invite: Config.endpoints.invite,
					gift: Config.endpoints.gift,
					webapp: Config.endpoints.webApp,
				},
				captcha: {
					provider: Config.captcha.provider,
					hcaptcha_site_key: Config.captcha.hcaptcha?.siteKey ?? null,
					turnstile_site_key: Config.captcha.turnstile?.siteKey ?? null,
				},
				features: {
					sms_mfa_enabled: Config.dev.testModeEnabled || Config.sms.enabled,
					voice_enabled: Config.voice.enabled,
					stripe_enabled: Config.stripe.enabled,
					self_hosted: Config.instance.selfHosted,
					manual_review_enabled: instanceConfig?.manualReviewEnabled ?? false,
				},
				gif: {
					provider: Config.gif.provider,
				},
				sso,
				limits,
				push: {
					public_vapid_key: Config.push.publicVapidKey ?? null,
				},
				app_public: {
					sentry_dsn: Config.appPublic.sentryDsn,
				},
			};

			if (Config.federation?.enabled) {
				const keyManager = getKeyManager();
				const instanceDomain = Config.domain.baseDomain;

				response.federation = {
					enabled: true,
					version: FEDERATION_PROTOCOL_VERSION,
				};

				response.public_key = {
					id: `https://${instanceDomain}/.well-known/fluxer#main-key`,
					algorithm: 'x25519' as const,
					public_key_base64: keyManager.getPublicKeyBase64(),
				};

				response.oauth2 = {
					authorization_endpoint: `${apiClientEndpoint}/oauth2/authorize`,
					token_endpoint: `${apiClientEndpoint}/oauth2/token`,
					userinfo_endpoint: `${apiClientEndpoint}/oauth2/userinfo`,
					scopes_supported: ['identify', 'guilds', 'guilds.join', 'messages.read', 'messages.write', 'voice'],
				};
			}

			return ctx.json(response);
		},
	);
}
