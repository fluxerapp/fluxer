// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {GifService} from '@app/api/gif/GifService';
import type {IGifProvider} from '@app/api/gif/IGifProvider';
import type {AccountIdentity} from '@app/api/instance/AccountIdentityModeCache';
import {withAccountIdentitySetupLock} from '@app/api/instance/AccountIdentitySetupLock';
import {
	type DiscoveryValidators,
	isDiscoveryNotModified,
	nextDiscoveryValidators,
} from '@app/api/instance/DiscoveryValidators';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {isBillingActive, isPremiumTieringActive, isStripeServiceable} from '@app/api/stripe/BillingConfigCache';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AccountIdentityModes, TagStyles} from '@fluxer/constants/src/AccountIdentityConstants';
import {API_CODE_VERSION} from '@fluxer/constants/src/AppConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {AccountIdentityLockedError} from '@fluxer/errors/src/domains/auth/AccountIdentityLockedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {buildDiscoveryResponse, type DiscoveryStaticInput} from '@fluxer/instance_bootstrap/src/BuildDiscovery';
import type {InstanceAppPublic} from '@fluxer/instance_bootstrap/src/Types';
import type {CaptchaConfig} from '@fluxer/schema/src/domains/admin/CaptchaSchemas';
import {toDomainMigrationDiscovery} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import {
	InstanceAccountIdentityResponse,
	InstanceAccountIdentityUpdateRequest,
	WellKnownFluxerResponse,
} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import type {Hono} from 'hono';

let discoveryValidators: DiscoveryValidators | null = null;

function buildDiscoveryStaticInput(
	gifService: GifService | undefined,
	appPublic: InstanceAppPublic,
	runtime: {
		captcha: CaptchaConfig;
		emailEnabled: boolean;
		accountIdentity: AccountIdentity;
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
			upload_relay: Config.mediaProxy.uploadRelay.endpoint,
			static_cdn: Config.endpoints.staticCdn,
			marketing: Config.endpoints.marketing,
			admin: Config.endpoints.admin,
			invite: Config.endpoints.invite,
			gift: Config.endpoints.gift,
			webapp: Config.endpoints.webApp,
		},
		captcha: {
			provider: runtime.captcha.enabled ? 'altcha' : 'none',
		},
		features: {
			voice_enabled: Config.voice.enabled,
			stripe_enabled: isBillingActive(),
			premium_enabled: isPremiumTieringActive(),
			stripe_serviceable: isStripeServiceable(),
			self_hosted: Config.instance.selfHosted,
			presigned_attachment_uploads: Config.presignedAttachmentUploadsEnabled,
			emails_enabled: runtime.emailEnabled,
			phone_verification_enabled: false,
			account_identity: runtime.accountIdentity.mode,
			tag_style: runtime.accountIdentity.tagStyle,
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
			const [registration, community, services, appPublicConfig, captcha, email, domainMigration, accountIdentity] =
				await Promise.all([
					instanceConfigRepository.getRegistrationPublicConfig(),
					instanceConfigRepository.getInstanceCommunityPublicConfig(),
					instanceConfigRepository.getResolvedServicesConfig(),
					instanceConfigRepository.getAppPublicConfig(),
					instanceConfigRepository.getCaptchaConfig(),
					instanceConfigRepository.getEffectiveEmailConfig(),
					instanceConfigRepository.getDomainMigrationConfig(),
					instanceConfigRepository.getAccountIdentity(),
				]);
			const accountIdentityLocked =
				Config.instance.selfHosted && !appPublicConfig.setup.configured
					? await instanceConfigRepository.isAccountIdentityLocked()
					: undefined;
			const discovery = buildDiscoveryResponse(
				buildDiscoveryStaticInput(
					gifService,
					{
						...appPublicConfig,
						setup: {
							...appPublicConfig.setup,
							admin_url: Config.endpoints.admin || null,
							...(accountIdentityLocked === undefined ? {} : {account_identity_locked: accountIdentityLocked}),
						},
					},
					{
						captcha,
						emailEnabled: email.enabled,
						accountIdentity,
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
			const response = {...discovery, domain_migration: toDomainMigrationDiscovery(domainMigration)};
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
	app.put(
		'/instance/setup/account-identity',
		RateLimitMiddleware(RateLimitConfigs.INSTANCE_SETUP_ACCOUNT_IDENTITY),
		Validator('json', InstanceAccountIdentityUpdateRequest),
		OpenAPI({
			operationId: 'set_instance_account_identity',
			summary: 'Choose the sign-in method for a new instance',
			responseSchema: InstanceAccountIdentityResponse,
			statusCode: 200,
			security: [],
			tags: ['Instance'],
			description:
				'Sets how people sign in on a new self-hosted instance, and for email sign-in whether usernames are unique with no tag. Username sign-in always uses unique usernames. It works only before setup is finished and before the first account exists. After that it fails with ACCOUNT_IDENTITY_LOCKED.',
		}),
		async (ctx) => {
			const {mode, tag_style} = ctx.req.valid('json');
			if (mode === AccountIdentityModes.USERNAME && tag_style === TagStyles.RANDOM) {
				throw InputValidationError.fromCode('tag_style', ValidationErrorCodes.TAG_STYLE_REQUIRES_EMAIL_SIGN_IN);
			}
			const instanceConfigRepository = ctx.get('instanceConfigRepository');
			const identity = await withAccountIdentitySetupLock(ctx.get('apiContext').services.cache, async () => {
				if (await instanceConfigRepository.isAccountIdentityLocked()) {
					throw new AccountIdentityLockedError();
				}
				await instanceConfigRepository.setAccountIdentityMode(mode, 'setup', tag_style ?? TagStyles.NONE);
				return await instanceConfigRepository.getAccountIdentity();
			});
			return ctx.json({mode: identity.mode, tag_style: identity.tagStyle});
		},
	);
}
