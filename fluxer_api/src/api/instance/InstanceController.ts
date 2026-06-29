// SPDX-License-Identifier: AGPL-3.0-or-later

import {API_CODE_VERSION} from '@fluxer/constants/src/AppConstants';
import {buildDiscoveryResponse, type DiscoveryStaticInput} from '@fluxer/instance_bootstrap/src/BuildDiscovery';
import type {InstanceAppPublic} from '@fluxer/instance_bootstrap/src/Types';
import {WellKnownFluxerResponse} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import type {Hono} from 'hono';
import {Config} from '../Config';
import type {GifService} from '../gif/GifService';
import type {LimitConfigService} from '../limits/LimitConfigService';
import {RateLimitMiddleware} from '../middleware/RateLimitMiddleware';
import {OpenAPI} from '../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../RateLimitConfig';
import type {HonoEnv} from '../types/HonoEnv';
import type {InstanceCaptchaEffectiveConfig} from './InstanceConfigRepository';

function buildDiscoveryStaticInput(
	gifService: GifService | undefined,
	appPublic: InstanceAppPublic,
	runtime: {
		captcha: InstanceCaptchaEffectiveConfig;
		emailEnabled: boolean;
	},
): DiscoveryStaticInput {
	const apiClientEndpoint = Config.endpoints.apiClient;
	const apiPublicEndpoint = Config.endpoints.apiPublic;
	const gifProvider = gifService?.getProvider();
	return {
		apiCodeVersion: API_CODE_VERSION,
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
			provider: runtime.captcha.provider,
			hcaptcha_site_key: runtime.captcha.hcaptcha_site_key,
			turnstile_site_key: runtime.captcha.turnstile_site_key,
		},
		features: {
			voice_enabled: Config.voice.enabled,
			stripe_enabled: Config.stripe.enabled,
			self_hosted: Config.instance.selfHosted,
			presigned_attachment_uploads: Config.presignedAttachmentUploadsEnabled,
			emails_enabled: runtime.emailEnabled,
		},
		gif: {
			provider: gifProvider?.meta.name ?? 'klipy',
			display_name: gifProvider?.meta.displayName ?? 'KLIPY',
			attribution_required: gifProvider?.meta.attributionRequired ?? true,
		},
		push: {
			public_vapid_key: Config.push.publicVapidKey ?? null,
		},
		appPublic,
	};
}

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
				'Returns the instance discovery document including API endpoints, feature flags, and limits. This is the canonical discovery endpoint for all Fluxer clients.',
		}),
		async (ctx) => {
			ctx.header('Access-Control-Allow-Origin', '*');
			const gifService = ctx.get('gifService') as GifService | undefined;
			const limitConfigService = ctx.get('limitConfigService') as LimitConfigService | undefined;
			const limits = limitConfigService?.getConfigWireFormat();
			const sso = await ctx.get('ssoService').getPublicStatus();
			const instanceConfigRepository = ctx.get('instanceConfigRepository');
			const [registration, community, services, appPublicConfig, captcha, email] = await Promise.all([
				instanceConfigRepository.getRegistrationPublicConfig(),
				instanceConfigRepository.getInstanceCommunityPublicConfig(),
				instanceConfigRepository.getResolvedServicesConfig(),
				instanceConfigRepository.getAppPublicConfig(),
				instanceConfigRepository.getEffectiveCaptchaConfig(),
				instanceConfigRepository.getEffectiveEmailConfig(),
			]);
			if (!limits) {
				throw new Error('limit_config_service is not bound');
			}
			const response = buildDiscoveryResponse(
				buildDiscoveryStaticInput(
					gifService,
					{
						...appPublicConfig,
						setup: {
							...appPublicConfig.setup,
							admin_url: Config.endpoints.admin || null,
						},
					},
					{
						captcha,
						emailEnabled: email.enabled,
					},
				),
				{
					sso,
					registration,
					community,
					services,
					limits,
				},
			);
			return ctx.json(response);
		},
	);
}
