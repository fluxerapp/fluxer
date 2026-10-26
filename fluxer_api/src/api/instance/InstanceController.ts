// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {NatsGifProvider} from '@app/api/gif/NatsGifProvider';
import {withAccountIdentitySetupLock} from '@app/api/instance/AccountIdentitySetupLock';
import {
	type DiscoveryValidators,
	isDiscoveryNotModified,
	nextDiscoveryValidators,
} from '@app/api/instance/DiscoveryValidators';
import {customBadgeImageUrl} from '@app/api/instance/InstanceAssetReferences';
import type {CustomBadgeDefinition} from '@app/api/instance/InstanceConfigRepository';
import {assertSetupToken} from '@app/api/instance/SetupToken';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {isBillingActive, isPremiumTieringActive, isStripeServiceable} from '@app/api/stripe/BillingConfigCache';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {requireConfiguredClientIp} from '@app/api/utils/RequestClientIp';
import {Validator} from '@app/api/Validator';
import {
	type EmailDeliveryState,
	EmailDeliveryStates,
	InstantAccountAvailability,
} from '@fluxer/constants/src/AccountConfigConstants';
import {
	type AccountIdentity,
	AccountIdentityDecisionSources,
	AccountIdentityModes,
	TagStyles,
} from '@fluxer/constants/src/AccountIdentityConstants';
import {resolveEffectiveVerificationBasis, resolveIdentifierFirst} from '@fluxer/constants/src/AccountTierRules';
import {API_CODE_VERSION} from '@fluxer/constants/src/AppConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {AccountIdentityLockedError} from '@fluxer/errors/src/domains/auth/AccountIdentityLockedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {buildDiscoveryResponse, type DiscoveryStaticInput} from '@fluxer/instance_bootstrap/src/BuildDiscovery';
import type {InstanceAppPublic} from '@fluxer/instance_bootstrap/src/Types';
import type {AccountsConfig, AuthConfig} from '@fluxer/schema/src/domains/admin/AccountsConfigSchemas';
import type {CaptchaConfig} from '@fluxer/schema/src/domains/admin/CaptchaSchemas';
import {toDomainMigrationDiscovery} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import {
	InstanceAccountIdentityResponse,
	InstanceAccountIdentityUpdateRequest,
	type InstanceGuildBadge,
	InstanceSetupTokenCheckRequest,
	WellKnownFluxerResponse,
} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import type {Hono} from 'hono';

let discoveryValidators: DiscoveryValidators | null = null;

function buildDiscoveryStaticInput(
	gifService: NatsGifProvider | undefined,
	appPublic: InstanceAppPublic,
	runtime: {
		captcha: CaptchaConfig;
		emailDeliveryState: EmailDeliveryState;
		accountIdentity: AccountIdentity;
		auth: AuthConfig;
		accounts: AccountsConfig;
		instantAccountAvailability: InstantAccountAvailability;
	},
): DiscoveryStaticInput {
	const signIn = resolveIdentifierFirst({
		mode: runtime.auth.identifier_first,
		identityMode: runtime.accountIdentity.mode,
		deliveryState: runtime.emailDeliveryState,
	});
	const apiClientEndpoint = Config.endpoints.apiClient;
	const apiPublicEndpoint = Config.endpoints.apiPublic;
	let gifProvider: NatsGifProvider | undefined;
	if (gifService !== undefined) {
		gifProvider = gifService;
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
			provider: runtime.captcha.provider,
		},
		features: {
			voice_enabled: Config.voice.enabled,
			stripe_enabled: isBillingActive(),
			premium_enabled: isPremiumTieringActive(),
			stripe_serviceable: isStripeServiceable(),
			self_hosted: Config.instance.selfHosted,
			presigned_attachment_uploads: Config.presignedAttachmentUploadsEnabled,
			emails_enabled: runtime.emailDeliveryState === EmailDeliveryStates.ON,
			phone_verification_enabled: false,
			discovery_enabled: Config.discovery.enabled,
			account_identity: runtime.accountIdentity.mode,
			tag_style: runtime.accountIdentity.tagStyle,
			identifier_first: signIn.identifierFirst,
			instant_accounts: runtime.instantAccountAvailability === InstantAccountAvailability.AVAILABLE,
			email_codes: signIn.emailCodes,
			date_of_birth_step: true,
			instant_account_availability: runtime.instantAccountAvailability,
			interaction_tier: runtime.accounts.interaction_tier,
			guild_join_gate: runtime.accounts.guild_join_gate,
			email_delivery_state: runtime.emailDeliveryState,
			verification_basis: resolveEffectiveVerificationBasis({
				identityMode: runtime.accountIdentity.mode,
				basis: runtime.accounts.verification_basis,
				deliveryState: runtime.emailDeliveryState,
			}),
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

function mapGuildBadgesForDiscovery(badges: ReadonlyArray<CustomBadgeDefinition>): Array<InstanceGuildBadge> {
	return badges
		.filter((badge) => badge.type === 'guild')
		.map((badge) => ({
			id: badge.id,
			name: badge.name,
			description: badge.description,
			asset_url: customBadgeImageUrl(badge.asset, Config.endpoints.media),
			href: badge.href,
		}));
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
			const gifService = ctx.get('gifService') as NatsGifProvider | undefined;
			const limits = ctx.get('limitConfigService').getConfigWireFormat();
			const sso = await ctx.get('ssoService').getPublicStatus();
			const instanceConfigRepository = ctx.get('instanceConfigRepository');
			const [
				registration,
				community,
				services,
				appPublicConfig,
				captcha,
				emailDeliveryState,
				domainMigration,
				accountIdentity,
				auth,
				accounts,
				instantAccountAvailability,
				notice,
				customBadges,
			] = await Promise.all([
				instanceConfigRepository.getRegistrationPublicConfig(),
				instanceConfigRepository.getInstanceCommunityPublicConfig(),
				instanceConfigRepository.getResolvedServicesConfig(),
				instanceConfigRepository.getAppPublicConfig(),
				instanceConfigRepository.getCaptchaConfig(),
				instanceConfigRepository.getEmailDeliveryState(),
				instanceConfigRepository.getDomainMigrationConfig(),
				instanceConfigRepository.getAccountIdentity(),
				instanceConfigRepository.getAuthConfig(),
				instanceConfigRepository.getAccountsConfig(),
				instanceConfigRepository.getInstantAccountAvailability(),
				instanceConfigRepository.getActiveInstanceNotice(),
				instanceConfigRepository.getCustomBadges(),
			]);
			const lockReason =
				Config.instance.selfHosted && !appPublicConfig.setup.configured
					? await instanceConfigRepository.getAccountIdentityLockReason()
					: undefined;
			const openSetup =
				lockReason === undefined
					? {}
					: {
							account_identity_locked: lockReason !== null,
							account_identity_lock_reason: lockReason,
							setup_token_required: await instanceConfigRepository.isSetupTokenRequired(),
						};
			const discovery = buildDiscoveryResponse(
				buildDiscoveryStaticInput(
					gifService,
					{
						...appPublicConfig,
						setup: {
							...appPublicConfig.setup,
							admin_url: Config.endpoints.admin || null,
							...openSetup,
						},
					},
					{
						captcha,
						emailDeliveryState,
						accountIdentity,
						auth,
						accounts,
						instantAccountAvailability,
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
			const response = {
				...discovery,
				notice,
				domain_migration: toDomainMigrationDiscovery(domainMigration),
				guild_badges: mapGuildBadgesForDiscovery(customBadges),
			};
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
	app.post(
		'/instance/setup/token-checks',
		RateLimitMiddleware(RateLimitConfigs.INSTANCE_SETUP_TOKEN_CHECK),
		Validator('json', InstanceSetupTokenCheckRequest),
		OpenAPI({
			operationId: 'check_instance_setup_token',
			summary: 'Check the setup token of a new instance',
			responseSchema: null,
			statusCode: 204,
			security: [],
			tags: ['Instance'],
			description:
				'Checks a setup token before the setup wizard asks for anything else. It returns 204 when the token is correct, and also when the instance no longer needs one. A missing or wrong token fails with the same field codes as registration and counts toward the same allowance of wrong tokens. The check never uses up the token.',
		}),
		async (ctx) => {
			const {setup_token} = ctx.req.valid('json');
			const {config, rateLimit} = ctx.get('apiContext').services;
			await assertSetupToken({
				rateLimit,
				config,
				repository: ctx.get('instanceConfigRepository'),
				presented: setup_token,
				clientIp: requireConfiguredClientIp(ctx.req.raw, config.proxy),
			});
			return ctx.body(null, 204);
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
				'Sets how people sign in on a new self-hosted instance, and for email sign-in whether usernames are unique with no tag. Username sign-in always uses unique usernames. It works only before setup is finished and before the first account exists. After that it fails with ACCOUNT_IDENTITY_LOCKED. While the instance requires a setup token, the request must send it as setup_token.',
		}),
		async (ctx) => {
			const {mode, tag_style, setup_token} = ctx.req.valid('json');
			const instanceConfigRepository = ctx.get('instanceConfigRepository');
			const {config, rateLimit, cache} = ctx.get('apiContext').services;
			await assertSetupToken({
				rateLimit,
				config,
				repository: instanceConfigRepository,
				presented: setup_token,
				clientIp: requireConfiguredClientIp(ctx.req.raw, config.proxy),
			});
			if (mode === AccountIdentityModes.USERNAME && tag_style === TagStyles.RANDOM) {
				throw InputValidationError.fromCode('tag_style', ValidationErrorCodes.TAG_STYLE_REQUIRES_EMAIL_SIGN_IN);
			}
			const identity = await withAccountIdentitySetupLock(cache, async () => {
				if (await instanceConfigRepository.isAccountIdentityLocked()) {
					throw new AccountIdentityLockedError();
				}
				await instanceConfigRepository.setAccountIdentityMode(
					mode,
					AccountIdentityDecisionSources.SETUP,
					tag_style ?? TagStyles.NONE,
				);
				return await instanceConfigRepository.getAccountIdentity();
			});
			return ctx.json({mode: identity.mode, tag_style: identity.tagStyle});
		},
	);
}
