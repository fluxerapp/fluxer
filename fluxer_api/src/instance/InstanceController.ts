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

import type {Hono} from 'hono';
import type {HonoEnv} from '~/App';
import {Config} from '~/Config';
import {API_CODE_VERSION} from '~/Constants';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';

export function InstanceController(app: Hono<HonoEnv>) {
	app.get('/instance', RateLimitMiddleware(RateLimitConfigs.INSTANCE_INFO), async (ctx) => {
		ctx.header('Access-Control-Allow-Origin', '*');

		const apiClientEndpoint = Config.endpoints.apiClient;
		const apiPublicEndpoint = Config.endpoints.apiPublic;

		const response: Record<string, unknown> = {
			api_code_version: API_CODE_VERSION,
			endpoints: {
				api: apiClientEndpoint,
				api_client: apiClientEndpoint,
				api_public: apiPublicEndpoint,
				gateway: Config.endpoints.gateway,
				media: Config.endpoints.media,
				cdn: Config.endpoints.cdn,
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
				sms_mfa_enabled: Config.sms.enabled,
				voice_enabled: Config.voice.enabled,
				stripe_enabled: Config.stripe.enabled,
				self_hosted: Config.instance.selfHosted,
			},
			push: {
				public_vapid_key: Config.push.publicVapidKey ?? null,
			},
		};

		return ctx.json(response);
	});
}
