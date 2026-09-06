// SPDX-License-Identifier: AGPL-3.0-or-later

import {API_CODE_VERSION} from '@fluxer/constants/src/AppConstants';
import {buildDiscoveryResponse, type DiscoveryStaticInput} from '@fluxer/instance_bootstrap/src/BuildDiscovery';
import type {InstanceAppPublic} from '@fluxer/instance_bootstrap/src/Types';
import {WellKnownFluxerResponse} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import type {Hono} from 'hono';
import {Config} from '../Config';
import type {GifService} from '../gif/GifService';
import type {IGifProvider} from '../gif/IGifProvider';
import {RateLimitMiddleware} from '../middleware/RateLimitMiddleware';
import {OpenAPI} from '../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../RateLimitConfig';
import type {HonoEnv} from '../types/HonoEnv';
import {type DiscoveryValidators, isDiscoveryNotModified, nextDiscoveryValidators} from './DiscoveryValidators';
import type {InstanceCaptchaEffectiveConfig} from './InstanceConfigRepository';

let discoveryValidators: DiscoveryValidators | null = null;

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
	let gifProvider: IGifProvider | undefined;
	if (gifService !== undefined) {
		gifProvider = gifService.getProvider();
	}
	let gifProviderName = 'klipy';
	let gifDisplayName = 'Klipy';
	let gifAttributionRequired = false;
	if (gifProvider !== undefined) {
		gifProviderName = gifProvider.meta.name;
		gifDisplayName = gifProvider.meta.displayName;
		gifAttributionRequired = gifProvider.meta.attributionRequired;
	}
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
			hcaptcha_site_key: runtime.captcha.provider === 'hcaptcha' ? runtime.captcha.hcaptcha_site_key : null,
			turnstile_site_key: runtime.captcha.provider === 'turnstile' ? runtime.captcha.turnstile_site_key : null,
		},
		features: {
			voice_enabled: Config.voice.enabled,
			stripe_enabled: Config.stripe.enabled,
			self_hosted: Config.instance.selfHosted,
			presigned_attachment_uploads: Config.presignedAttachmentUploadsEnabled,
			emails_enabled: runtime.emailEnabled,
		},
		gif: {
			provider: gifProviderName,
			display_name: gifDisplayName,
			attribution_required: gifAttributionRequired,
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
			const limits = ctx.get('limitConfigService').getConfigWireFormat();
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
			discoveryValidators = nextDiscoveryValidators(response, discoveryValidators);
			ctx.header('ETag', discoveryValidators.etag);
			ctx.header('Last-Modified', discoveryValidators.lastModified.toUTCString());
			if (
				isDiscoveryNotModified(
					discoveryValidators,
					ctx.req.header('If-None-Match'),
					ctx.req.header('If-Modified-Since'),
				)
			) {
				return ctx.body(null, 304);
			}
			return ctx.json(response);
		},
	);
}
