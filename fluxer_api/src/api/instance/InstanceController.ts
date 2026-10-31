// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {Config} from '@app/api/Config';
import type {NatsGifProvider} from '@app/api/gif/NatsGifProvider';
import {withAccountIdentitySetupLock} from '@app/api/instance/AccountIdentitySetupLock';
import {type CustomBadge, compareBigInts} from '@app/api/instance/custom_badges/CustomBadgeRepository';
import {
	type DiscoveryValidators,
	isDiscoveryNotModified,
	nextDiscoveryValidators,
} from '@app/api/instance/DiscoveryValidators';
import {customBadgeImageUrl} from '@app/api/instance/InstanceAssetReferences';
import type {BuiltInBadgeImages} from '@app/api/instance/InstanceConfigRepository';
import {assertSetupToken} from '@app/api/instance/SetupToken';
import {getStatusPageDetector} from '@app/api/instance/StatusPageDetector';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getVoiceTopology} from '@app/api/middleware/ServiceRegistry';
import {getCustomBadgeCatalog} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {isBillingActive, isPremiumTieringActive, isStripeServiceable} from '@app/api/stripe/BillingConfigCache';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {entityTagMatches} from '@app/api/utils/EntityTag';
import {requireConfiguredClientIp} from '@app/api/utils/RequestClientIp';
import {Validator} from '@app/api/Validator';
import {endpointOrigin} from '@app/api/voice/VoiceServerEndpoints';
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
import {BUILT_IN_GUILD_BADGE_KINDS, type HideableBuiltInBadgeKind} from '@fluxer/constants/src/BadgeConstants';
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
	type InstanceBadge,
	InstanceBadgesQuery,
	InstanceBadgesResponse,
	type InstanceBuiltInGuildBadgeImages,
	type InstanceHiddenBuiltInGuildBadges,
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

function listVoiceOrigins(): Array<string> {
	const topology = getVoiceTopology();
	if (!topology) return [];
	const origins = new Set<string>();
	for (const region of topology.getAllRegions()) {
		for (const server of topology.getServersForRegion(region.id)) {
			const origin = endpointOrigin(server.endpoint);
			if (origin) origins.add(origin);
		}
	}
	return Array.from(origins).sort();
}

function mapBuiltInGuildBadgeImagesForDiscovery(images: BuiltInBadgeImages): InstanceBuiltInGuildBadgeImages {
	const url = (asset: string | undefined) => (asset ? customBadgeImageUrl(asset, Config.endpoints.media) : null);
	return {
		verified: url(images.guild_verified),
		partnered: url(images.guild_partnered),
		discoverable: url(images.guild_discoverable),
	};
}

function mapHiddenBuiltInGuildBadgesForDiscovery(
	hidden: ReadonlySet<HideableBuiltInBadgeKind>,
): InstanceHiddenBuiltInGuildBadges {
	return BUILT_IN_GUILD_BADGE_KINDS.filter((kind) => hidden.has(`guild_${kind}`));
}

const INT64_MAX = (1n << 63n) - 1n;
const BADGE_CACHE_CURRENT = 'public, max-age=86400, immutable';
const BADGE_CACHE_REVALIDATE = 'public, max-age=60, must-revalidate';

function mapGuildBadge(badge: CustomBadge): InstanceBadge {
	return {
		id: badge.id,
		name: badge.name,
		description: badge.description,
		asset_url: customBadgeImageUrl(badge.asset, Config.endpoints.media),
		href: badge.href,
		position: badge.position.toString(),
	};
}

function parseBadgeIds(ids: string): Array<bigint> {
	return [...new Set(ids.split(','))].map((id) => BigInt(id)).filter((id) => id <= INT64_MAX);
}

function badgeEntityTag(version: string, query: string): string {
	return `"${createHash('sha256').update(`${version}\n${query}`).digest('hex').slice(0, 32)}"`;
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
				badgeHead,
				builtInBadgeImages,
				hiddenBuiltInBadges,
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
				getCustomBadgeCatalog().readDiscoveryHead(),
				instanceConfigRepository.getBuiltInBadgeImages(),
				instanceConfigRepository.getHiddenBuiltInBadges(),
			]);
			const statusPageInstatus = await getStatusPageDetector().isInstatus(appPublicConfig.branding.status_page_url);
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
						branding: {...appPublicConfig.branding, status_page_instatus: statusPageInstatus},
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
				guild_badges: badgeHead.value.badges.map(mapGuildBadge),
				guild_badges_partial: badgeHead.value.partial,
				custom_badges_version: getCustomBadgeCatalog().clientVersion(badgeHead.version),
				built_in_guild_badge_images: mapBuiltInGuildBadgeImagesForDiscovery(builtInBadgeImages),
				hidden_built_in_guild_badges: mapHiddenBuiltInGuildBadgesForDiscovery(hiddenBuiltInBadges),
				voice_origins: listVoiceOrigins(),
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
	app.get(
		'/instance/badges',
		RateLimitMiddleware(RateLimitConfigs.INSTANCE_BADGES),
		Validator('query', InstanceBadgesQuery),
		OpenAPI({
			operationId: 'list_instance_badges',
			summary: 'List community badges',
			description:
				'Lists the custom badges that communities on this instance can show, in display order. Send ids to look up the badges listed in badge_ids of a community, or page through every badge with after and limit. Send the custom_badges_version from discovery as version. While that is still the current version, the response can be cached for a day.',
			responseSchema: InstanceBadgesResponse,
			statusCode: 200,
			security: [],
			tags: ['Instance'],
		}),
		async (ctx) => {
			ctx.header('Access-Control-Allow-Origin', '*');
			const query = ctx.req.valid('query');
			if (query.ids !== undefined && query.after !== undefined) {
				throw InputValidationError.create('after', 'Send either ids or after');
			}
			const catalog = getCustomBadgeCatalog();
			const read =
				query.ids !== undefined
					? await catalog.readByIds(parseBadgeIds(query.ids)).then((result) => ({
							...result,
							value: {badges: result.value.filter((badge) => badge.type === 'guild'), next: null},
						}))
					: await catalog.readPage({type: 'guild', after: query.after, limit: query.limit});
			const version = catalog.clientVersion(read.version);
			const normalizedQuery =
				query.ids !== undefined
					? `ids=${parseBadgeIds(query.ids).sort(compareBigInts).join(',')}`
					: `after=${query.after ?? ''}&limit=${query.limit}`;
			ctx.header(
				'Cache-Control',
				read.stable && query.version === version ? BADGE_CACHE_CURRENT : BADGE_CACHE_REVALIDATE,
			);
			if (read.stable) {
				const etag = badgeEntityTag(version, normalizedQuery);
				ctx.header('ETag', etag);
				const ifNoneMatch = ctx.req.header('If-None-Match');
				if (ifNoneMatch !== undefined && entityTagMatches(ifNoneMatch, etag)) {
					return ctx.body(null, 304);
				}
			}
			return ctx.json({
				version,
				badges: read.value.badges.map(mapGuildBadge),
				next: read.value.next,
			});
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
