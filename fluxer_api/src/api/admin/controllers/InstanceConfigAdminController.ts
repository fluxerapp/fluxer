// SPDX-License-Identifier: AGPL-3.0-or-later

import {isDeepStrictEqual} from 'node:util';
import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead, recordAdminWrite} from '@app/api/admin/AdminAuditRecorder';
import {startAttachmentDecayReconcile} from '@app/api/attachment/AttachmentDecayReconcileRun';
import {maybeAutoJoinInvite} from '@app/api/auth/AuthRegistration';
import {ssoDiscoveryCacheKey} from '@app/api/auth/services/SsoService';
import {createGuildID, createInviteCode, createUserID} from '@app/api/BrandedTypes';
import {loadSigningKey} from '@app/api/bluesky/BlueskyOAuthService';
import {Config} from '@app/api/Config';
import {enqueueEndUnassignedGuildVisits} from '@app/api/guild/visits/GuildVisitLifecycle';
import {resolveEffectiveSmtpConfig, type StoredEmailSmtpConfig} from '@app/api/instance/EmailDelivery';
import {assetStorageKey, resolveBrandingAsset} from '@app/api/instance/InstanceAssetReferences';
import {
	type InstanceAttachmentDecayEffectiveConfig,
	type InstanceConfigRepository,
	type InstanceIntegrationsConfigPatch,
	type InstancePolicyConfig,
	REGISTRATION_PENDING_APPROVAL_TRAIT,
	REGISTRATION_REJECTED_TRAIT,
	type SsoGroupRoleMapping,
} from '@app/api/instance/InstanceConfigRepository';
import {clearUsedSetupToken} from '@app/api/instance/SetupToken';
import {
	deriveSsoRedirectUri,
	normalizeAndValidateSsoConfig,
	normalizeSsoButtonLabel,
	normalizeTrustedDomains,
} from '@app/api/instance/SsoConfigValidation';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getKVClient, getWorkerService} from '@app/api/middleware/ServiceRegistry';
import {
	getAssetDeletionQueue,
	getGuildRepository,
	getInstanceConfigRepository,
	getKVAccountDeletionQueue,
	getNatsConfigPublisher,
} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {scheduleSystemDeletion} from '@app/api/user/AccountCleanup';
import {noticeEmailAddress} from '@app/api/user/services/SystemDmMessage';
import {Validator} from '@app/api/Validator';
import {startSingleCommunityJoin} from '@app/api/worker/tasks/JoinSingleCommunityMembers';
import {
	EmailDeliveryModes,
	EmailDeliveryStates,
	IdentifierFirstModes,
	VerificationBases,
} from '@fluxer/constants/src/AccountConfigConstants';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {CaptchaProviders} from '@fluxer/constants/src/CaptchaConstants';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {resolveSmtpTlsMode, type SmtpTlsMode, smtpTlsModeFromSecure} from '@fluxer/constants/src/SmtpConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {InstancePolicyTransitionNotAllowedError} from '@fluxer/errors/src/domains/core/InstancePolicyTransitionNotAllowedError';
import {NotFoundError} from '@fluxer/errors/src/domains/core/NotFoundError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {
	BrandingAssetUploadRequest,
	CreateRegistrationUrlRequest,
	CreateRegistrationUrlResponse,
	InstanceConfigResponse,
	InstanceConfigUpdateRequest,
	InstanceEmailSmtpTestRequest,
	InstanceEmailSmtpTestResponse,
	PendingRegistrationActionRequest,
	PendingRegistrationListResponse,
	RegistrationUrlIdParam,
	UpdateRegistrationUrlRequest,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {
	type CaptchaConfigUpdateRequest,
	captchaProviderFromLegacyEnabled,
	isCaptchaActive,
} from '@fluxer/schema/src/domains/admin/CaptchaSchemas';
import {DomainMigrationConfigSchema} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import {GatewayRolloutConfigSchema} from '@fluxer/schema/src/domains/admin/GatewayRolloutSchemas';
import type {PushRelayConfig, PushRelayConfigUpdateRequest} from '@fluxer/schema/src/domains/admin/PushRelaySchemas';
import {VoiceP2pConfigSchema} from '@fluxer/schema/src/domains/admin/VoiceP2pSchemas';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {ExperimentDeliveryConfigSchema} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import type {InstanceBranding, InstanceNotice} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import {SmtpEmailProvider} from '@pkgs/email/src/SmtpEmailProvider';
import type {Context} from 'hono';

const INSTANCE_BRANDING_ENTITY_ID = 0n;
const ATTACHMENT_EXPIRY_RULE_FIELDS = [
	'enabled',
	'min_size_mb',
	'max_size_mb',
	'max_eligible_size_mb',
	'min_lifetime_days',
	'max_lifetime_days',
	'curve',
] as const satisfies ReadonlyArray<keyof InstanceAttachmentDecayEffectiveConfig>;

async function queueReleasedBrandingAssets(references: ReadonlyArray<string>): Promise<void> {
	const queue = getAssetDeletionQueue();
	for (const reference of references) {
		await queue.queueDeletion({
			s3Key: assetStorageKey(reference),
			cdnUrl: resolveBrandingAsset(reference, Config.endpoints.media),
			reason: 'branding_asset_released',
		});
	}
}

function attachmentExpiryRulesChanged(
	previous: InstanceAttachmentDecayEffectiveConfig,
	next: InstanceAttachmentDecayEffectiveConfig,
): boolean {
	return ATTACHMENT_EXPIRY_RULE_FIELDS.some((field) => previous[field] !== next[field]);
}

function readOptionalField<T extends object, K extends keyof T>(value: T, key: K): T[K] | undefined {
	return Object.hasOwn(value, key) ? value[key] : undefined;
}

function readTrustedDomains(branding: {trusted_domains?: Array<string> | null}): Array<string> | undefined {
	const domains = readOptionalField(branding, 'trusted_domains');
	return domains === undefined
		? undefined
		: normalizeTrustedDomains(domains ?? [], 'app_public.branding.trusted_domains');
}

function readSmtpTlsModeUpdate(
	smtp: {
		tls_mode?: SmtpTlsMode | null;
		secure?: boolean | null;
		port?: number | null;
	},
	savedSmtp: StoredEmailSmtpConfig,
): SmtpTlsMode | null | undefined {
	const tlsMode = readOptionalField(smtp, 'tls_mode');
	if (tlsMode !== undefined) return tlsMode;
	const secure = readOptionalField(smtp, 'secure');
	if (secure === undefined) return undefined;
	const port = readOptionalField(smtp, 'port');
	const nextSmtp = {...savedSmtp, port: port === undefined ? savedSmtp.port : port};
	return smtpTlsModeFromSecure(secure, resolveEffectiveSmtpConfig(nextSmtp).port);
}

async function readSavedSmtpConfig(): Promise<StoredEmailSmtpConfig> {
	return (await getInstanceConfigRepository().getInstanceIntegrationsConfig()).email.smtp;
}

async function validateSsoGroupRoleMappings(
	mappings: Array<{group: string; role_id: string}> | undefined,
): Promise<Array<SsoGroupRoleMapping> | undefined> {
	if (mappings === undefined) return undefined;
	const policy = await getInstanceConfigRepository().getInstancePolicyConfig();
	if (policy.single_community_guild_id !== null && mappings.length > 0) {
		const roles = await getGuildRepository().listRoles(createGuildID(BigInt(policy.single_community_guild_id)));
		const roleIds = new Set(
			roles.map((role) => role.id.toString()).filter((roleId) => roleId !== policy.single_community_guild_id),
		);
		const unknown = mappings.find((mapping) => !roleIds.has(mapping.role_id));
		if (unknown) {
			throw InputValidationError.create(
				'sso.group_role_mappings',
				`Role ${unknown.role_id} is the everyone role or not a role of the single community`,
			);
		}
	}
	return mappings.map(({group, role_id: roleId}) => ({group, roleId}));
}

function mergeOptionalField<T>(currentValue: T, nextValue: T | undefined): T {
	return nextValue === undefined ? currentValue : nextValue;
}

function omitUndefinedFields<T extends object>(value: T): Partial<T> {
	return Object.fromEntries(Object.entries(value).filter(([, entry]) => entry !== undefined)) as Partial<T>;
}

function nonEmptyPatch<T extends object>(value: T | null | undefined): Partial<T> | undefined {
	if (!value) return undefined;
	const patch = omitUndefinedFields(value);
	return Object.keys(patch).length > 0 ? patch : undefined;
}

async function buildInstanceConfigResponse(): Promise<InstanceConfigResponse> {
	const instanceConfigRepository = getInstanceConfigRepository();
	const [
		ssoConfig,
		gatewayRollout,
		pushRelay,
		domainMigration,
		voiceP2p,
		captcha,
		experimentDelivery,
		registrationConfig,
		registrationUrls,
		authConfig,
		accountsConfig,
		guildVisitsConfig,
	] = await Promise.all([
		instanceConfigRepository.getSsoConfig(),
		instanceConfigRepository.getGatewayRolloutConfig(),
		instanceConfigRepository.getPushRelayConfig(),
		instanceConfigRepository.getDomainMigrationConfig(),
		instanceConfigRepository.getVoiceP2pConfig(),
		instanceConfigRepository.getCaptchaConfig(),
		instanceConfigRepository.getExperimentDeliveryConfig(),
		instanceConfigRepository.getRegistrationConfig(),
		instanceConfigRepository.getRegistrationUrlsForAdmin(),
		instanceConfigRepository.getAuthConfig(),
		instanceConfigRepository.getAccountsConfig(),
		instanceConfigRepository.getGuildVisitsConfig(),
	]);
	const [
		appPublic,
		policy,
		resolvedServices,
		integrations,
		media,
		billing,
		accountIdentity,
		accountIdentityLockReason,
		instantAccountAvailability,
		notice,
	] = await Promise.all([
		instanceConfigRepository.getAppPublicConfig(),
		instanceConfigRepository.getInstancePolicyConfig(),
		instanceConfigRepository.getResolvedServicesConfig(),
		instanceConfigRepository.getInstanceIntegrationsAdminConfig(),
		instanceConfigRepository.getInstanceMediaAdminConfig(),
		instanceConfigRepository.getInstanceBillingAdminConfig(),
		instanceConfigRepository.getAccountIdentity(),
		instanceConfigRepository.getAccountIdentityLockReason(),
		instanceConfigRepository.getInstantAccountAvailability(),
		instanceConfigRepository.getInstanceNotice(),
	]);
	return {
		sso: {
			enabled: ssoConfig.enabled,
			enforced: ssoConfig.enforced,
			display_name: ssoConfig.displayName,
			button_label: ssoConfig.buttonLabel,
			issuer: ssoConfig.issuer,
			authorization_url: ssoConfig.authorizationUrl,
			token_url: ssoConfig.tokenUrl,
			userinfo_url: ssoConfig.userInfoUrl,
			jwks_url: ssoConfig.jwksUrl,
			client_id: ssoConfig.clientId,
			client_secret_set: ssoConfig.clientSecretSet ?? false,
			scope: ssoConfig.scope,
			allowed_domains: ssoConfig.allowedEmailDomains,
			auto_provision: ssoConfig.autoProvision,
			username_from_provider: ssoConfig.usernameFromProvider,
			redirect_uri: deriveSsoRedirectUri(Config.endpoints.webApp),
			groups_claim: ssoConfig.groupsClaim,
			group_role_mappings: ssoConfig.groupRoleMappings.map(({group, roleId}) => ({group, role_id: roleId})),
		},
		gateway_rollout: gatewayRollout,
		push_relay: pushRelay,
		domain_migration: domainMigration,
		voice_p2p: voiceP2p,
		captcha: {...captcha, enabled: isCaptchaActive(captcha)},
		auth: {...authConfig, instant_account_availability: instantAccountAvailability},
		accounts: accountsConfig,
		guild_visits: guildVisitsConfig,
		experiment_delivery: experimentDelivery,
		registration: {
			...registrationConfig,
			urls: registrationUrls,
		},
		self_hosted: Config.instance.selfHosted,
		account_identity: {
			mode: accountIdentity.mode,
			locked: accountIdentityLockReason !== null,
			tag_style: accountIdentity.tagStyle,
			lock_reason: accountIdentityLockReason,
		},
		app_public: appPublic,
		policy: {
			single_community_enabled: policy.single_community_enabled,
			single_community_guild_id: policy.single_community_guild_id,
			direct_messages_disabled: policy.direct_messages_disabled,
			direct_messages_locked: policy.direct_messages_locked,
			friends_disabled: policy.friends_disabled,
			personal_notes_enabled: policy.personal_notes_enabled,
			guild_create_access: policy.guild_create_access,
			premium_mode: policy.premium_mode,
			services: {
				gif_enabled: policy.gif_enabled,
				youtube_enabled: policy.youtube_enabled,
				bluesky_enabled: policy.bluesky_enabled,
			},
			services_resolved: resolvedServices,
			services_available: {
				gif: integrations.gif.effective_available,
				youtube: integrations.youtube.effective_available,
				bluesky: integrations.bluesky.effective_enabled,
			},
		},
		integrations,
		media,
		billing,
		notice,
	};
}

function buildAdminIssuedRegistrationUrl(code: string): string {
	const baseUrl = Config.endpoints.webApp.replace(/\/$/, '');
	return `${baseUrl}/register?registration_url=${encodeURIComponent(code)}`;
}

function relayConsentStamp(
	current: PushRelayConfig,
	patch: PushRelayConfigUpdateRequest,
	adminUserId: string,
): Partial<PushRelayConfig> {
	const accepted = patch.relay_consent_accepted;
	if (accepted === undefined || accepted === current.relay_consent_accepted) {
		return {};
	}
	return accepted
		? {relay_consent_accepted_at: new Date().toISOString(), relay_consent_accepted_by: adminUserId}
		: {relay_consent_accepted_at: null, relay_consent_accepted_by: null};
}

function assertSelfHostedBillingSections(data: InstanceConfigUpdateRequest): void {
	if (Config.instance.selfHosted) {
		return;
	}
	if (data.billing) {
		throw InputValidationError.create('billing', 'Billing is configured through the environment on this instance');
	}
	const branding = data.app_public?.branding;
	if (!branding) {
		return;
	}
	for (const field of [
		'premium_product_name',
		'premium_info_url',
		'sign_in_backdrop_url',
		'favicon_dark_url',
		'custom_css',
		'upload_limit_message',
		'trusted_domains',
	] as const) {
		if (readOptionalField(branding, field) !== undefined) {
			throw InputValidationError.create(
				`app_public.branding.${field}`,
				'This setting is only available on self-hosted instances',
			);
		}
	}
}

async function assertBillingCompatibleWithStoredPremiumMode(
	billing: NonNullable<InstanceConfigUpdateRequest['billing']>,
): Promise<void> {
	const requestedEnabled = readOptionalField(billing, 'enabled');
	if (requestedEnabled !== true) {
		return;
	}
	const policy = await getInstanceConfigRepository().readStoredInstancePolicyConfig();
	if (policy.premium_mode === 'everyone') {
		throw InputValidationError.create('billing.enabled', 'Billing can only be enabled when the premium mode is mirror');
	}
}

async function assertPremiumModeCompatibleWithStoredBilling(
	requestedBillingEnabled: boolean | null | undefined,
): Promise<void> {
	if (!Config.instance.selfHosted) {
		return;
	}
	const repository = getInstanceConfigRepository();
	const [policy, billing] = await Promise.all([
		repository.readStoredInstancePolicyConfig(),
		repository.readStoredInstanceBillingConfig(),
	]);
	if (policy.premium_mode === 'everyone') {
		return;
	}
	const nextEnabled = requestedBillingEnabled === undefined ? billing.enabled : requestedBillingEnabled;
	if (nextEnabled === true) {
		throw InputValidationError.create(
			'policy.premium_mode',
			'Disable billing before switching the premium mode to everyone',
		);
	}
}

async function assertBillingCompatibleWithPremiumMode(data: InstanceConfigUpdateRequest): Promise<void> {
	if (!Config.instance.selfHosted) {
		return;
	}
	const requestedEnabled = data.billing ? readOptionalField(data.billing, 'enabled') : undefined;
	const requestedPremiumMode = data.policy ? readOptionalField(data.policy, 'premium_mode') : undefined;
	if (requestedEnabled === undefined && requestedPremiumMode === undefined) {
		return;
	}
	const currentPremiumMode = (await getInstanceConfigRepository().getInstancePolicyConfig()).premium_mode;
	const nextPremiumMode = requestedPremiumMode ?? currentPremiumMode;
	if (nextPremiumMode !== 'everyone') {
		return;
	}
	const nextEnabled =
		requestedEnabled === undefined
			? (await getInstanceConfigRepository().readStoredInstanceBillingConfig()).enabled
			: requestedEnabled;
	if (nextEnabled !== true) {
		return;
	}
	if (requestedEnabled !== undefined) {
		throw InputValidationError.create('billing.enabled', 'Billing can only be enabled when the premium mode is mirror');
	}
	if (currentPremiumMode !== 'everyone') {
		throw InputValidationError.create(
			'policy.premium_mode',
			'Disable billing before switching the premium mode to everyone',
		);
	}
}

function nextCaptchaProvider(
	patch: CaptchaConfigUpdateRequest | null | undefined,
): {provider: CaptchaConfigUpdateRequest['provider']; field: string} | null {
	if (patch?.provider !== undefined) return {provider: patch.provider, field: 'captcha.provider'};
	if (patch?.enabled !== undefined) {
		return {provider: captchaProviderFromLegacyEnabled(patch.enabled), field: 'captcha.enabled'};
	}
	return null;
}

function emailDeliveryPatchField(patch: InstanceIntegrationsConfigPatch['email']): string | null {
	if (patch?.delivery === EmailDeliveryModes.SMTP) return 'integrations.email.delivery';
	if (patch?.delivery === undefined && patch?.enabled === true) return 'integrations.email.enabled';
	return null;
}

async function assertValidBlueskyKeys(patch: InstanceIntegrationsConfigPatch | null): Promise<void> {
	for (const [index, key] of (patch?.bluesky?.keys ?? []).entries()) {
		if (!key.kid || !key.private_key) continue;
		try {
			await loadSigningKey({kid: key.kid, private_key: key.private_key}, index);
		} catch {
			throw InputValidationError.create(
				`integrations.bluesky.keys.${index}.private_key`,
				'The private key must be the whole PEM file, from the BEGIN PRIVATE KEY line to the END PRIVATE KEY line',
			);
		}
	}
}

async function assertConsistentInstanceConfig(
	data: InstanceConfigUpdateRequest,
	integrationsPatch: InstanceIntegrationsConfigPatch | null,
): Promise<void> {
	const repository = getInstanceConfigRepository();
	const [auth, captcha, accounts, identityMode] = await Promise.all([
		repository.getAuthConfig(),
		repository.getCaptchaConfig(),
		repository.getAccountsConfig(),
		repository.getAccountIdentityMode(),
	]);
	const instantPatch = data.auth?.instant_accounts?.enabled;
	const captchaPatch = nextCaptchaProvider(data.captcha);
	const instantOn = instantPatch ?? auth.instant_accounts.enabled;
	if (instantOn && (captchaPatch?.provider ?? captcha.provider) === CaptchaProviders.NONE) {
		if (instantPatch === true) {
			throw InputValidationError.fromCode(
				'auth.instant_accounts.enabled',
				ValidationErrorCodes.INSTANT_ACCOUNTS_REQUIRE_CAPTCHA,
			);
		}
		if (captchaPatch !== null) {
			throw InputValidationError.fromCode(
				captchaPatch.field,
				ValidationErrorCodes.CAPTCHA_REQUIRED_BY_INSTANT_ACCOUNTS,
			);
		}
	}
	const usernameMode = identityMode === AccountIdentityModes.USERNAME;
	const identifierFirst = data.auth?.identifier_first;
	if (usernameMode && (identifierFirst === IdentifierFirstModes.ON || identifierFirst === true)) {
		throw InputValidationError.fromCode(
			'auth.identifier_first',
			ValidationErrorCodes.IDENTIFIER_FIRST_REQUIRES_EMAIL_IDENTITY,
		);
	}
	if (!usernameMode && data.sso?.username_from_provider === true) {
		throw InputValidationError.create(
			'sso.username_from_provider',
			'Usernames can only come from your sign-in provider when people sign in with a username',
		);
	}
	const deliveryField = emailDeliveryPatchField(integrationsPatch?.email);
	if (usernameMode && deliveryField !== null) {
		throw InputValidationError.fromCode(deliveryField, ValidationErrorCodes.EMAIL_DELIVERY_REQUIRES_EMAIL_IDENTITY);
	}
	if (deliveryField !== null && !(await repository.previewEffectiveSmtpConfig(integrationsPatch ?? {})).host.trim()) {
		throw InputValidationError.fromCode('integrations.email.smtp.host', ValidationErrorCodes.SMTP_HOST_REQUIRED);
	}
	const basisPatch = data.accounts?.verification_basis;
	const emailPatched = integrationsPatch?.email !== undefined;
	if ((basisPatch ?? accounts.verification_basis) !== VerificationBases.CONFIRMED_EMAIL) return;
	if (basisPatch === undefined && !emailPatched) return;
	const deliveryState = emailPatched
		? await repository.previewEmailDeliveryState(integrationsPatch ?? {})
		: await repository.getEmailDeliveryState();
	if (deliveryState === EmailDeliveryStates.ON) return;
	throw InputValidationError.fromCode(
		basisPatch !== undefined ? 'accounts.verification_basis' : 'integrations.email.delivery',
		ValidationErrorCodes.VERIFICATION_BASIS_REQUIRES_EMAIL_DELIVERY,
	);
}

function toIntegrationsPatch(
	integrations: InstanceConfigUpdateRequest['integrations'],
	savedSmtp: StoredEmailSmtpConfig,
): InstanceIntegrationsConfigPatch | null {
	if (!integrations) return null;
	const patch: InstanceIntegrationsConfigPatch = {
		gif: integrations.gif
			? omitUndefinedFields({
					klipy_api_key: readOptionalField(integrations.gif, 'klipy_api_key'),
				})
			: undefined,
		youtube: integrations.youtube
			? omitUndefinedFields({
					api_key: readOptionalField(integrations.youtube, 'api_key'),
				})
			: undefined,
		email: undefined,
	};
	if (integrations.email) {
		patch.email = {
			...omitUndefinedFields({
				delivery: readOptionalField(integrations.email, 'delivery'),
				enabled: readOptionalField(integrations.email, 'enabled'),
				provider: readOptionalField(integrations.email, 'provider'),
				from_email: readOptionalField(integrations.email, 'from_email'),
				from_name: readOptionalField(integrations.email, 'from_name'),
				disable_new_ip_authorization: readOptionalField(integrations.email, 'disable_new_ip_authorization'),
			}),
			smtp: integrations.email.smtp
				? omitUndefinedFields({
						host: readOptionalField(integrations.email.smtp, 'host'),
						port: readOptionalField(integrations.email.smtp, 'port'),
						username: readOptionalField(integrations.email.smtp, 'username'),
						password: readOptionalField(integrations.email.smtp, 'password'),
						tls_mode: readSmtpTlsModeUpdate(integrations.email.smtp, savedSmtp),
					})
				: undefined,
		};
	}
	patch.bluesky = integrations.bluesky
		? {
				...omitUndefinedFields({
					enabled: readOptionalField(integrations.bluesky, 'enabled'),
					client_name: readOptionalField(integrations.bluesky, 'client_name'),
					client_uri: readOptionalField(integrations.bluesky, 'client_uri'),
					logo_uri: readOptionalField(integrations.bluesky, 'logo_uri'),
					tos_uri: readOptionalField(integrations.bluesky, 'tos_uri'),
					policy_uri: readOptionalField(integrations.bluesky, 'policy_uri'),
				}),
				keys: readOptionalField(integrations.bluesky, 'keys'),
			}
		: undefined;
	return patch;
}

function listSuppliedSections(data: InstanceConfigUpdateRequest): string | undefined {
	const sections = Object.entries(data)
		.filter(([, value]) => value != null)
		.map(([key]) => key)
		.sort();
	return sections.length > 0 ? sections.join(',') : undefined;
}

interface InstanceConfigUpdateContext {
	readonly ctx: Context<HonoEnv>;
	readonly data: InstanceConfigUpdateRequest;
	readonly instanceConfigRepository: InstanceConfigRepository;
	readonly integrationsPatch: InstanceIntegrationsConfigPatch | null;
}

function createInstanceConfigSectionApplier<T extends object>(
	select: (context: InstanceConfigUpdateContext) => T | null | undefined,
	apply: (section: T, context: InstanceConfigUpdateContext) => Promise<void>,
): (context: InstanceConfigUpdateContext) => Promise<void> | undefined {
	return (context) => {
		const section = select(context);
		if (section) return apply(section, context);
		return undefined;
	};
}

const INSTANCE_CONFIG_SECTION_APPLIERS = {
	gateway_rollout: createInstanceConfigSectionApplier(
		({data}) => data.gateway_rollout,
		async (patch, {instanceConfigRepository}) => {
			const landed = await instanceConfigRepository.updateGatewayRolloutConfig((current) =>
				GatewayRolloutConfigSchema.parse({...current, ...patch}),
			);
			await getNatsConfigPublisher().publish('config.gateway.rollout', 'gateway_rollout_config', landed);
		},
	),
	push_relay: createInstanceConfigSectionApplier(
		({data}) => nonEmptyPatch(data.push_relay),
		async (patch, {ctx, instanceConfigRepository}) => {
			const adminUserId = ctx.get('adminUserId').toString();
			const landed = await instanceConfigRepository.updatePushRelayConfig((current) => ({
				...current,
				...patch,
				...relayConsentStamp(current, patch, adminUserId),
			}));
			await getNatsConfigPublisher().publish('config.push.delivery', 'push_service_delivery_config', landed);
		},
	),
	domain_migration: createInstanceConfigSectionApplier(
		({data}) => nonEmptyPatch(data.domain_migration),
		async (patch, {instanceConfigRepository}) => {
			await instanceConfigRepository.updateDomainMigrationConfig((current) =>
				DomainMigrationConfigSchema.parse({
					...current,
					...patch,
					config_version: current.config_version + 1,
				}),
			);
		},
	),
	voice_p2p: createInstanceConfigSectionApplier(
		({data}) => nonEmptyPatch(data.voice_p2p),
		async (patch, {instanceConfigRepository}) => {
			await instanceConfigRepository.updateVoiceP2pConfig((current) => {
				const next = VoiceP2pConfigSchema.parse({...current, ...patch});
				return isDeepStrictEqual(next, current) ? current : {...next, config_version: current.config_version + 1};
			});
		},
	),
	captcha: createInstanceConfigSectionApplier(
		({data}) => nonEmptyPatch(data.captcha),
		async (patch, {instanceConfigRepository}) => {
			await instanceConfigRepository.updateCaptchaConfig(patch);
		},
	),
	auth: createInstanceConfigSectionApplier(
		({data}) => data.auth,
		async (auth, {instanceConfigRepository}) => {
			await instanceConfigRepository.updateAuthConfig(auth);
		},
	),
	accounts: createInstanceConfigSectionApplier(
		({data}) => data.accounts,
		async (accounts, {instanceConfigRepository}) => {
			await instanceConfigRepository.updateAccountsConfig(accounts);
		},
	),
	guild_visits: createInstanceConfigSectionApplier(
		({data}) => data.guild_visits,
		async (guildVisits, {instanceConfigRepository}) => {
			await instanceConfigRepository.updateGuildVisitsConfig(guildVisits);
			await enqueueEndUnassignedGuildVisits();
		},
	),
	experiment_delivery: createInstanceConfigSectionApplier(
		({data}) => data.experiment_delivery,
		async (patch, {instanceConfigRepository}) => {
			await instanceConfigRepository.updateExperimentDeliveryConfig((current) =>
				ExperimentDeliveryConfigSchema.parse({...current, ...patch}),
			);
		},
	),
	sso: createInstanceConfigSectionApplier(
		({data}) => data.sso,
		async (sso, {ctx, instanceConfigRepository}) => {
			const suppliedButtonLabel = readOptionalField(sso, 'button_label');
			const buttonLabel = suppliedButtonLabel === undefined ? undefined : normalizeSsoButtonLabel(suppliedButtonLabel);
			const current = await instanceConfigRepository.getSsoConfig({includeSecret: true});
			const next = {
				enabled: mergeOptionalField(current.enabled, readOptionalField(sso, 'enabled')),
				enforced: mergeOptionalField(current.enforced, readOptionalField(sso, 'enforced')),
				displayName: mergeOptionalField(current.displayName, readOptionalField(sso, 'display_name')),
				issuer: mergeOptionalField(current.issuer, readOptionalField(sso, 'issuer')),
				authorizationUrl: mergeOptionalField(current.authorizationUrl, readOptionalField(sso, 'authorization_url')),
				tokenUrl: mergeOptionalField(current.tokenUrl, readOptionalField(sso, 'token_url')),
				userInfoUrl: mergeOptionalField(current.userInfoUrl, readOptionalField(sso, 'userinfo_url')),
				jwksUrl: mergeOptionalField(current.jwksUrl, readOptionalField(sso, 'jwks_url')),
				clientId: mergeOptionalField(current.clientId, readOptionalField(sso, 'client_id')),
				scope: mergeOptionalField(current.scope, readOptionalField(sso, 'scope')),
				allowedEmailDomains: mergeOptionalField(current.allowedEmailDomains, readOptionalField(sso, 'allowed_domains')),
				autoProvision: mergeOptionalField(current.autoProvision, readOptionalField(sso, 'auto_provision')),
			};
			const validated = await normalizeAndValidateSsoConfig(next, {
				testModeEnabled: Config.dev.testModeEnabled,
			});
			const supplied = <T>(field: keyof typeof sso, value: T): T | undefined =>
				readOptionalField(sso, field) === undefined ? undefined : value;
			await instanceConfigRepository.setSsoConfig({
				enabled: supplied('enabled', validated.enabled),
				enforced: supplied('enforced', validated.enforced),
				displayName: supplied('display_name', next.displayName),
				buttonLabel,
				issuer: supplied('issuer', validated.issuer),
				authorizationUrl: supplied('authorization_url', validated.authorizationUrl),
				tokenUrl: supplied('token_url', validated.tokenUrl),
				userInfoUrl: supplied('userinfo_url', validated.userInfoUrl),
				jwksUrl: supplied('jwks_url', validated.jwksUrl),
				clientId: supplied('client_id', validated.clientId),
				clientSecret: readOptionalField(sso, 'client_secret'),
				scope: supplied('scope', next.scope),
				allowedEmailDomains: supplied('allowed_domains', validated.allowedEmailDomains),
				autoProvision: supplied('auto_provision', next.autoProvision),
				usernameFromProvider: readOptionalField(sso, 'username_from_provider'),
				groupsClaim: readOptionalField(sso, 'groups_claim'),
				groupRoleMappings: await validateSsoGroupRoleMappings(readOptionalField(sso, 'group_role_mappings')),
			});
			for (const issuer of new Set([current.issuer, validated.issuer])) {
				if (issuer) await ctx.get('cacheService').delete(ssoDiscoveryCacheKey(issuer));
			}
		},
	),
	registration: createInstanceConfigSectionApplier(
		({data}) => data.registration,
		async (registration, {instanceConfigRepository}) => {
			await instanceConfigRepository.setRegistrationConfig({
				mode: registration.mode,
				admin_registration_urls_enabled: registration.admin_registration_urls_enabled,
				allowed_email_domains: registration.allowed_email_domains,
			});
		},
	),
	app_public: createInstanceConfigSectionApplier(
		({data}) => data.app_public,
		async (appPublic, {instanceConfigRepository}) => {
			const {releasedBrandingAssets} = await instanceConfigRepository.writeAppPublicConfig({
				branding: appPublic.branding
					? omitUndefinedFields({
							product_name: readOptionalField(appPublic.branding, 'product_name'),
							icon_url: readOptionalField(appPublic.branding, 'icon_url'),
							symbol_url: readOptionalField(appPublic.branding, 'symbol_url'),
							logo_url: readOptionalField(appPublic.branding, 'logo_url'),
							wordmark_url: readOptionalField(appPublic.branding, 'wordmark_url'),
							favicon_url: readOptionalField(appPublic.branding, 'favicon_url'),
							favicon_dark_url: readOptionalField(appPublic.branding, 'favicon_dark_url'),
							theme_color: readOptionalField(appPublic.branding, 'theme_color'),
							status_page_url: readOptionalField(appPublic.branding, 'status_page_url'),
							status_page_incident_history_url: readOptionalField(
								appPublic.branding,
								'status_page_incident_history_url',
							),
							premium_product_name: readOptionalField(appPublic.branding, 'premium_product_name'),
							premium_info_url: readOptionalField(appPublic.branding, 'premium_info_url'),
							sign_in_backdrop_url: readOptionalField(appPublic.branding, 'sign_in_backdrop_url'),
							custom_css: readOptionalField(appPublic.branding, 'custom_css'),
							upload_limit_message: readOptionalField(appPublic.branding, 'upload_limit_message'),
							trusted_domains: readTrustedDomains(appPublic.branding),
						})
					: undefined,
				legal: appPublic.legal
					? omitUndefinedFields({
							terms_url: readOptionalField(appPublic.legal, 'terms_url'),
							privacy_url: readOptionalField(appPublic.legal, 'privacy_url'),
							guidelines_url: readOptionalField(appPublic.legal, 'guidelines_url'),
						})
					: undefined,
			});
			await queueReleasedBrandingAssets(releasedBrandingAssets);
			const collectDateOfBirth = appPublic.registration
				? readOptionalField(appPublic.registration, 'collect_date_of_birth')
				: undefined;
			if (collectDateOfBirth !== undefined) {
				await instanceConfigRepository.setDateOfBirthCollection(collectDateOfBirth);
			}
		},
	),
	integrations: createInstanceConfigSectionApplier(
		({integrationsPatch}) => integrationsPatch,
		async (integrationsPatch, {instanceConfigRepository}) => {
			await instanceConfigRepository.setInstanceIntegrationsConfig(integrationsPatch);
		},
	),
	media: createInstanceConfigSectionApplier(
		({data}) => data.media,
		async (media, {instanceConfigRepository}) => {
			const previousAttachmentDecay = await instanceConfigRepository.getEffectiveAttachmentDecayConfig();
			await instanceConfigRepository.setInstanceMediaConfig({
				attachment_decay: media.attachment_decay
					? omitUndefinedFields({
							enabled: readOptionalField(media.attachment_decay, 'enabled'),
							min_size_mb: readOptionalField(media.attachment_decay, 'min_size_mb'),
							max_size_mb: readOptionalField(media.attachment_decay, 'max_size_mb'),
							max_eligible_size_mb: readOptionalField(media.attachment_decay, 'max_eligible_size_mb'),
							min_lifetime_days: readOptionalField(media.attachment_decay, 'min_lifetime_days'),
							max_lifetime_days: readOptionalField(media.attachment_decay, 'max_lifetime_days'),
							curve: readOptionalField(media.attachment_decay, 'curve'),
							renew_threshold_days: readOptionalField(media.attachment_decay, 'renew_threshold_days'),
							renew_window_days: readOptionalField(media.attachment_decay, 'renew_window_days'),
						})
					: undefined,
			});
			const attachmentDecay = await instanceConfigRepository.getEffectiveAttachmentDecayConfig();
			if (attachmentDecay.enabled && attachmentExpiryRulesChanged(previousAttachmentDecay, attachmentDecay)) {
				await startAttachmentDecayReconcile(getKVClient(), getWorkerService());
			}
		},
	),
	policy: createInstanceConfigSectionApplier(
		({data}) => data.policy,
		async (policy, {ctx, data}) => {
			await applyInstancePolicyUpdate(
				ctx,
				policy,
				data.billing ? readOptionalField(data.billing, 'enabled') : undefined,
			);
		},
	),
	billing: createInstanceConfigSectionApplier(
		({data}) => data.billing,
		async (billing, {instanceConfigRepository}) => {
			await assertBillingCompatibleWithStoredPremiumMode(billing);
			await instanceConfigRepository.setInstanceBillingConfig(
				omitUndefinedFields({
					enabled: readOptionalField(billing, 'enabled'),
					stripe_secret_key: readOptionalField(billing, 'stripe_secret_key'),
					stripe_webhook_secret: readOptionalField(billing, 'stripe_webhook_secret'),
					default_currency: readOptionalField(billing, 'default_currency'),
					prices: readOptionalField(billing, 'prices'),
					country_currencies: readOptionalField(billing, 'country_currencies'),
					legacy_prices: readOptionalField(billing, 'legacy_prices'),
					automatic_tax: readOptionalField(billing, 'automatic_tax'),
					tax_id_collection: readOptionalField(billing, 'tax_id_collection'),
					terms_consent_required: readOptionalField(billing, 'terms_consent_required'),
					billing_emails: readOptionalField(billing, 'billing_emails'),
				}),
			);
		},
	),
	notice: createInstanceConfigSectionApplier(
		({data}) => data.notice,
		async (notice, {instanceConfigRepository}) => {
			const message = notice.message?.trim();
			let updatedNotice: Omit<InstanceNotice, 'id'> | null = null;
			if (message) {
				updatedNotice = {
					message,
					url: notice.url ?? null,
					starts_at: notice.starts_at ? new Date(notice.starts_at).toISOString() : null,
					ends_at: notice.ends_at ? new Date(notice.ends_at).toISOString() : null,
				};
			}
			await instanceConfigRepository.setInstanceNotice(updatedNotice);
		},
	),
	setup: createInstanceConfigSectionApplier(
		({data}) => data.app_public?.setup,
		async (setup, {instanceConfigRepository}) => {
			const configured = readOptionalField(setup, 'configured');
			await instanceConfigRepository.setAppPublicConfig({
				setup: omitUndefinedFields({configured}),
			});
			if (configured === true) {
				await instanceConfigRepository.claimAdminBootstrap();
				await clearUsedSetupToken(instanceConfigRepository);
			}
		},
	),
};

export function InstanceConfigAdminController(app: HonoApp) {
	const instanceConfigRepository = getInstanceConfigRepository();
	app.get(
		'/admin/instance/config',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_VIEW),
		OpenAPI({
			operationId: 'get_admin_instance_config',
			summary: 'Get instance configuration',
			description:
				'Retrieves instance-wide configuration including webhooks and SSO configuration. Requires INSTANCE_CONFIG_VIEW permission.',
			responseSchema: InstanceConfigResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const response = await buildInstanceConfigResponse();
			await recordAdminRead(ctx, {
				targetType: 'instance_config',
				targetId: 0n,
				action: AdminAuditReadActions.GET_INSTANCE_CONFIG,
				metadata: {registration_url_count: response.registration.urls.length},
			});
			return ctx.json(response);
		},
	);
	app.patch(
		'/admin/instance/config',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('json', InstanceConfigUpdateRequest),
		OpenAPI({
			operationId: 'update_admin_instance_config',
			summary: 'Update instance configuration',
			description:
				'Updates instance configuration settings including webhook URLs and SSO parameters. Changes apply immediately. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: InstanceConfigResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const data = ctx.req.valid('json');
			assertSelfHostedBillingSections(data);
			await assertBillingCompatibleWithPremiumMode(data);
			const integrationsPatch = toIntegrationsPatch(data.integrations, await readSavedSmtpConfig());
			await assertConsistentInstanceConfig(data, integrationsPatch);
			await assertValidBlueskyKeys(integrationsPatch);
			const updateContext = {ctx, data, instanceConfigRepository, integrationsPatch};
			for (const applySection of Object.values(INSTANCE_CONFIG_SECTION_APPLIERS)) {
				const pendingUpdate = applySection(updateContext);
				if (pendingUpdate) await pendingUpdate;
			}
			await recordAdminWrite(ctx, {
				targetType: 'instance_config',
				targetId: 0n,
				action: 'update_instance_config',
				metadata: {
					sections: listSuppliedSections(data),
				},
			});
			return ctx.json(await buildInstanceConfigResponse());
		},
	);
	app.post(
		'/admin/instance/config/branding-assets',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('json', BrandingAssetUploadRequest),
		OpenAPI({
			operationId: 'create_admin_instance_branding_asset',
			summary: 'Upload an instance branding asset',
			description:
				'Uploads a branding image served by the media proxy and stores its URL, or clears it when no image is provided. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: InstanceConfigResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {kind, image} = ctx.req.valid('json');
			const isBackdrop = kind === 'sign_in_backdrop';
			if ((isBackdrop || kind === 'favicon_dark') && !Config.instance.selfHosted) {
				throw InputValidationError.create('kind', 'This setting is only available on self-hosted instances');
			}
			const prepared = await ctx.get('entityAssetService').prepareAssetUpload({
				assetType: isBackdrop ? 'branding_backdrop' : 'branding',
				entityType: 'instance',
				entityId: INSTANCE_BRANDING_ENTITY_ID,
				previousHash: null,
				base64Image: image ?? null,
				errorPath: 'image',
			});
			const brandingPatch: Partial<InstanceBranding> = {[`${kind}_url`]: prepared.newCdnUrl};
			const {releasedBrandingAssets} = await instanceConfigRepository.writeAppPublicConfig({branding: brandingPatch});
			await queueReleasedBrandingAssets(releasedBrandingAssets);
			await recordAdminWrite(ctx, {
				targetType: 'instance_config',
				targetId: 0n,
				action: 'upload_branding_asset',
				metadata: {kind, cleared: prepared.newCdnUrl === null},
			});
			return ctx.json(await buildInstanceConfigResponse());
		},
	);
	app.post(
		'/admin/instance/config/smtp-tests',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('json', InstanceEmailSmtpTestRequest),
		OpenAPI({
			operationId: 'create_admin_instance_smtp_test',
			summary: 'Run an SMTP configuration test',
			description:
				'Validates that an SMTP configuration can authenticate and accept a connection. Fields left out of the body come from the saved configuration, and the saved password is reused only for the saved host and username. An empty body tests what outgoing email uses and fails when email is off or incomplete. With send_to, it also sends a short test message from the configured from address and returns what the server answered, or the error it gave. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: InstanceEmailSmtpTestResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {send_to: sendTo, ...draft} = ctx.req.valid('json');
			const {smtp, from, problem} = await instanceConfigRepository.getSmtpTestConfig(draft);
			let result: InstanceEmailSmtpTestResponse;
			try {
				if (problem) throw new Error(problem);
				const provider = new SmtpEmailProvider({
					...smtp,
					connectionTimeoutMs: 10000,
					greetingTimeoutMs: 10000,
					socketTimeoutMs: 10000,
				});
				await provider.verify();
				result = {ok: true, error: null};
				if (sendTo) {
					if (!from.email.trim()) throw new Error('No from address is set');
					const productName = (await instanceConfigRepository.getAppPublicConfig()).branding.product_name;
					result.server_response = await provider.sendTestEmail({
						to: sendTo,
						from: {email: from.email, name: from.name || productName},
						subject: `${productName} email test`,
						text: `This message was sent from the ${productName} admin panel to check the outgoing email settings. You can ignore it.`,
					});
				}
			} catch (error) {
				result = {ok: false, error: error instanceof Error ? error.message : String(error)};
			}
			await recordAdminWrite(ctx, {
				targetType: 'instance_config',
				targetId: 0n,
				action: 'test_smtp_connection',
				metadata: {
					port: smtp.port,
					tls_mode: resolveSmtpTlsMode(smtp.tlsMode, smtp.port),
					ok: result.ok,
					sent_message: Boolean(sendTo),
				},
			});
			return ctx.json(result);
		},
	);
	app.post(
		'/admin/instance/registration-urls',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('json', CreateRegistrationUrlRequest),
		OpenAPI({
			operationId: 'create_admin_registration_url',
			summary: 'Create an admin-issued registration URL',
			description:
				'Creates a one-time-display registration URL that can be sent manually by an administrator. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: CreateRegistrationUrlResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const data = ctx.req.valid('json');
			const inviteCode = data.invite_code ?? null;
			if (inviteCode !== null && (await ctx.get('inviteService').findInvite(createInviteCode(inviteCode))) === null) {
				throw InputValidationError.create('invite_code', 'No invite has this code');
			}
			const created = await instanceConfigRepository.createRegistrationUrl({
				label: data.label?.trim() || null,
				createdByUserId: ctx.get('adminUserId').toString(),
				expiresAt: data.expires_at ? new Date(data.expires_at) : null,
				maxUses: data.max_uses ?? null,
				approvalRequired: data.approval_required,
				inviteCode,
			});
			await recordAdminWrite(ctx, {
				targetType: 'registration_url',
				targetId: 0n,
				action: 'create_registration_url',
				metadata: {
					approval_required: data.approval_required,
					max_uses: data.max_uses,
					invite_code: inviteCode ?? undefined,
				},
			});
			return ctx.json({
				registration_url: created.registrationUrl,
				code: created.code,
				url: buildAdminIssuedRegistrationUrl(created.code),
			});
		},
	);
	app.delete(
		'/admin/instance/registration-urls/:registration_url_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('param', RegistrationUrlIdParam),
		OpenAPI({
			operationId: 'revoke_admin_registration_url',
			summary: 'Revoke or remove an admin-issued registration URL',
			description:
				'Revokes an admin-issued registration URL so it can no longer be used. Calling it again on a revoked URL removes it from the list. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: InstanceConfigResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const outcome = await instanceConfigRepository.revokeOrRemoveRegistrationUrl(
				ctx.req.valid('param').registration_url_id,
			);
			await recordAdminWrite(ctx, {
				targetType: 'registration_url',
				targetId: 0n,
				action: outcome === 'removed' ? 'remove_registration_url' : 'revoke_registration_url',
			});
			return ctx.json(await buildInstanceConfigResponse());
		},
	);
	app.patch(
		'/admin/instance/registration-urls/:registration_url_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('param', RegistrationUrlIdParam),
		Validator('json', UpdateRegistrationUrlRequest),
		OpenAPI({
			operationId: 'update_admin_registration_url',
			summary: 'Rename an admin-issued registration URL',
			description:
				'Changes the label of an admin-issued registration URL. Returns 404 when no URL has this ID. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: InstanceConfigResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const found = await instanceConfigRepository.renameRegistrationUrl(
				ctx.req.valid('param').registration_url_id,
				ctx.req.valid('json').label?.trim() || null,
			);
			if (!found) {
				throw new NotFoundError({code: APIErrorCodes.NOT_FOUND});
			}
			await recordAdminWrite(ctx, {
				targetType: 'registration_url',
				targetId: 0n,
				action: 'rename_registration_url',
			});
			return ctx.json(await buildInstanceConfigResponse());
		},
	);
	app.get(
		'/admin/instance/pending-registrations',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.REGISTRATION_REVIEW),
		OpenAPI({
			operationId: 'list_admin_pending_registrations',
			summary: 'List pending registrations',
			description:
				'Lists every registration waiting for manual review, oldest first. Requires REGISTRATION_REVIEW permission.',
			responseSchema: PendingRegistrationListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const pendingRegistrations = await instanceConfigRepository.getPendingRegistrations();
			await recordAdminRead(ctx, {
				targetType: 'instance_config',
				targetId: 0n,
				action: AdminAuditReadActions.LIST_PENDING_REGISTRATIONS,
				metadata: {pending_registration_count: pendingRegistrations.length},
			});
			return ctx.json({pending_registrations: pendingRegistrations});
		},
	);
	app.patch(
		'/admin/instance/pending-registrations/:user_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.REGISTRATION_REVIEW),
		Validator('param', UserIdParam),
		Validator('json', PendingRegistrationActionRequest),
		OpenAPI({
			operationId: 'update_admin_pending_registration',
			summary: 'Approve or reject a pending registration',
			description:
				'Decides a registration waiting for manual review and returns the registrations still waiting. Approving removes its pending registration trait and joins the account to the community it was invited to and the single community. Rejecting stops the account from signing in and queues it for deletion right away without sending a notice, unless a deletion is already scheduled. Once the account is deleted, its email address can register again. Requires REGISTRATION_REVIEW permission.',
			responseSchema: PendingRegistrationListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const userId = ctx.req.valid('param').user_id.toString();
			const decision = ctx.req.valid('json').status === 'approved' ? 'approve' : 'reject';
			const pendingRegistration = (await instanceConfigRepository.getPendingRegistrations()).find(
				(entry) => entry.user_id === userId,
			);
			await updatePendingRegistrationUser(ctx, userId, decision, pendingRegistration);
			await instanceConfigRepository.removePendingRegistration(userId);
			return ctx.json({pending_registrations: await instanceConfigRepository.getPendingRegistrations()});
		},
	);
}

async function applyInstancePolicyUpdate(
	ctx: Context<HonoEnv>,
	policy: NonNullable<InstanceConfigUpdateRequest['policy']>,
	requestedBillingEnabled: boolean | null | undefined,
): Promise<void> {
	const instanceConfigRepository = getInstanceConfigRepository();
	const singleCommunityService = ctx.get('singleCommunityService');
	const appPublic = await instanceConfigRepository.getAppPublicConfig();
	const requestedGuildId =
		policy.single_community_guild_id === undefined
			? null
			: await singleCommunityService.findCommunity(policy.single_community_guild_id);
	if (policy.single_community_guild_id !== undefined && requestedGuildId === null) {
		throw InputValidationError.create('policy.single_community_guild_id', 'No community has this ID');
	}
	const adminUser =
		policy.single_community_enabled === true && requestedGuildId === null
			? await ctx.get('userRepository').findUnique(ctx.get('adminUserId'))
			: null;
	if (policy.premium_mode === 'everyone') {
		await assertPremiumModeCompatibleWithStoredBilling(requestedBillingEnabled);
	}
	let enablesSingleCommunity = false;
	const updated = await instanceConfigRepository.updateInstancePolicyConfig((current) => {
		const planned = planInstancePolicyPatch(policy, current, {
			setupConfigured: appPublic.setup.configured,
			adminUserFound: adminUser !== null,
			requestedGuildId: requestedGuildId?.toString() ?? null,
		});
		enablesSingleCommunity = planned.enablesSingleCommunity;
		return planned.patch;
	});
	if (enablesSingleCommunity) {
		await singleCommunityService.ensureStockCommunity({
			owner: adminUser,
			name: policy.single_community_name?.trim() || appPublic.branding.product_name,
			guildId: requestedGuildId,
		});
	}
	if (policy.single_community_add_members === true) {
		const guildId = await singleCommunityService.getStockCommunityId();
		if (guildId === null || !(enablesSingleCommunity || updated.single_community_enabled)) {
			throw InputValidationError.create(
				'policy.single_community_add_members',
				'Turn single-community mode on before adding existing accounts',
			);
		}
		await startSingleCommunityJoin(getKVClient(), getWorkerService(), guildId.toString());
	}
	if (policy.premium_mode !== undefined) {
		await ctx.get('limitConfigService').updatePolicyConfig({premium_mode: policy.premium_mode});
	}
}

function planInstancePolicyPatch(
	policy: NonNullable<InstanceConfigUpdateRequest['policy']>,
	current: InstancePolicyConfig,
	context: {setupConfigured: boolean; adminUserFound: boolean; requestedGuildId: string | null},
): {patch: Partial<InstancePolicyConfig>; enablesSingleCommunity: boolean} {
	const patch: Partial<InstancePolicyConfig> = {};
	let enablesSingleCommunity = false;
	const singleCommunityChange =
		policy.single_community_enabled !== undefined &&
		policy.single_community_enabled !== current.single_community_enabled
			? policy.single_community_enabled
			: null;
	switch (singleCommunityChange) {
		case true: {
			const reusable = context.requestedGuildId !== null || current.single_community_guild_id != null;
			if ((context.setupConfigured && !reusable) || (!context.adminUserFound && context.requestedGuildId === null)) {
				throw new InstancePolicyTransitionNotAllowedError();
			}
			enablesSingleCommunity = true;
			break;
		}
		case false:
			patch.single_community_enabled = false;
			break;
		default:
			if (
				context.requestedGuildId !== null &&
				current.single_community_enabled &&
				policy.single_community_enabled !== false &&
				context.requestedGuildId !== current.single_community_guild_id
			) {
				patch.single_community_guild_id = context.requestedGuildId;
			}
	}
	const unlockDirectMessages = policy.direct_messages_locked === false;
	if (unlockDirectMessages && current.direct_messages_locked) {
		patch.direct_messages_locked = false;
	}
	const changesDirectMessages =
		policy.direct_messages_disabled !== undefined &&
		policy.direct_messages_disabled !== current.direct_messages_disabled;
	if (changesDirectMessages && current.direct_messages_locked && !unlockDirectMessages) {
		throw new InstancePolicyTransitionNotAllowedError();
	}
	if (changesDirectMessages) {
		patch.direct_messages_disabled = policy.direct_messages_disabled;
		if (!policy.direct_messages_disabled) patch.direct_messages_locked = true;
	}

	const friendsDisabled = policy.friends_disabled ?? patch.direct_messages_disabled;
	updateChangedPolicyField(patch, current, 'friends_disabled', friendsDisabled);
	updateChangedPolicyField(patch, current, 'guild_create_access', policy.guild_create_access);
	updateChangedPolicyField(patch, current, 'personal_notes_enabled', policy.personal_notes_enabled);

	for (const field of ['gif_enabled', 'youtube_enabled', 'bluesky_enabled'] as const) {
		const value = policy.services?.[field];
		if (value !== undefined) patch[field] = value ?? null;
	}
	return {patch, enablesSingleCommunity};
}

function updateChangedPolicyField(
	patch: Partial<InstancePolicyConfig>,
	current: InstancePolicyConfig,
	key: 'friends_disabled' | 'guild_create_access' | 'personal_notes_enabled',
	value: boolean | undefined,
): void {
	if (value !== undefined && value !== current[key]) patch[key] = value;
}

async function updatePendingRegistrationUser(
	ctx: Context<HonoEnv>,
	userId: string,
	decision: 'approve' | 'reject',
	pendingRegistration: {invite_code?: string | null} | undefined,
): Promise<void> {
	const userRepository = ctx.get('userRepository');
	const user = await userRepository.findUnique(createUserID(BigInt(userId)));
	if (!user) {
		await recordAdminWrite(ctx, {
			targetType: 'user',
			targetId: BigInt(userId),
			action: decision === 'approve' ? 'approve_registration' : 'reject_registration',
			metadata: {account_found: false},
		});
		return;
	}
	const traits = new Set(user.traits);
	if (
		!pendingRegistration &&
		!traits.has(REGISTRATION_PENDING_APPROVAL_TRAIT) &&
		!traits.has(REGISTRATION_REJECTED_TRAIT)
	) {
		throw new UnknownUserError();
	}
	const wasPendingApproval = traits.delete(REGISTRATION_PENDING_APPROVAL_TRAIT);
	if (decision === 'reject') {
		traits.add(REGISTRATION_REJECTED_TRAIT);
	} else {
		traits.delete(REGISTRATION_REJECTED_TRAIT);
	}
	const updated = await userRepository.patchUpsert(user.id, {traits: traits.size > 0 ? traits : null}, user.toRow());
	const pendingDeletionAt =
		decision === 'reject' && !user.pendingDeletionAt ? await deleteRejectedRegistration(ctx, updated) : null;
	if (decision === 'approve' && wasPendingApproval) {
		await maybeAutoJoinInvite(ctx.get('inviteService'), {
			userId: user.id,
			inviteCode: pendingRegistration?.invite_code ?? null,
			requestCache: ctx.get('requestCache'),
		});
		await ctx.get('singleCommunityService').joinStockCommunity(user.id, ctx.get('requestCache'));
		const email = await noticeEmailAddress(user);
		if (email) {
			await ctx.get('emailService').send('registration_approved', email, {}, user.locale);
		}
	}
	await recordAdminWrite(ctx, {
		targetType: 'user',
		targetId: user.id,
		action: decision === 'approve' ? 'approve_registration' : 'reject_registration',
		metadata: {pending_deletion_at: pendingDeletionAt},
	});
}

async function deleteRejectedRegistration(ctx: Context<HonoEnv>, user: User): Promise<string> {
	const scheduled = await scheduleSystemDeletion(
		{users: ctx.get('userRepository'), gateway: ctx.get('gatewayService'), deletionQueue: getKVAccountDeletionQueue()},
		user,
		{reasonCode: DeletionReasons.OTHER, delayMs: 0, cancelledBySignIn: false, endSessions: true},
	);
	const pendingDeletionAt = scheduled.pendingDeletionAt!.toISOString();
	await ctx.get('workerService').addJob('userProcessPendingDeletion', {
		userId: user.id.toString(),
		deletionReasonCode: DeletionReasons.OTHER,
		pendingDeletionAt,
	});
	return pendingDeletionAt;
}
