// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {InstancePolicyTransitionNotAllowedError} from '@fluxer/errors/src/domains/core/InstancePolicyTransitionNotAllowedError';
import {
	BrandingAssetUploadRequest,
	CreateRegistrationUrlRequest,
	CreateRegistrationUrlResponse,
	InstanceConfigResponse,
	InstanceConfigUpdateRequest,
	InstanceEmailSmtpTestRequest,
	InstanceEmailSmtpTestResponse,
	PendingRegistrationActionRequest,
	RegistrationUrlActionRequest,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {GatewayRolloutConfigSchema} from '@fluxer/schema/src/domains/admin/GatewayRolloutSchemas';
import {SmtpEmailProvider} from '@pkgs/email/src/SmtpEmailProvider';
import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';
import {createUserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {
	type InstanceBrandingConfig,
	type InstancePolicyConfig,
	REGISTRATION_PENDING_APPROVAL_TRAIT,
	REGISTRATION_REJECTED_TRAIT,
} from '../../instance/InstanceConfigRepository';
import {deriveSsoRedirectUri, normalizeAndValidateSsoConfig} from '../../instance/SsoConfigValidation';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {getGatewayRolloutConfigPublisher, getInstanceConfigRepository} from '../../middleware/ServiceSingletons';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp, HonoEnv} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

const INSTANCE_BRANDING_ENTITY_ID = 0n;

function readOptionalField<T extends object, K extends keyof T>(value: T, key: K): T[K] | undefined {
	return Object.hasOwn(value, key) ? value[key] : undefined;
}

function mergeOptionalField<T>(currentValue: T, nextValue: T | undefined): T {
	return nextValue === undefined ? currentValue : nextValue;
}

function omitUndefinedFields<T extends object>(value: T): Partial<T> {
	return Object.fromEntries(Object.entries(value).filter(([, entry]) => entry !== undefined)) as Partial<T>;
}

async function buildInstanceConfigResponse(): Promise<InstanceConfigResponse> {
	const instanceConfigRepository = getInstanceConfigRepository();
	const [ssoConfig, gatewayRollout, registrationConfig, registrationUrls, pendingRegistrations] = await Promise.all([
		instanceConfigRepository.getSsoConfig(),
		instanceConfigRepository.getGatewayRolloutConfig(),
		instanceConfigRepository.getRegistrationConfig(),
		instanceConfigRepository.getRegistrationUrlsForAdmin(),
		instanceConfigRepository.getPendingRegistrations(),
	]);
	const [appPublic, policy, resolvedServices, integrations, media] = await Promise.all([
		instanceConfigRepository.getAppPublicConfig(),
		instanceConfigRepository.getInstancePolicyConfig(),
		instanceConfigRepository.getResolvedServicesConfig(),
		instanceConfigRepository.getInstanceIntegrationsAdminConfig(),
		instanceConfigRepository.getInstanceMediaAdminConfig(),
	]);
	return {
		sso: {
			enabled: ssoConfig.enabled,
			enforced: ssoConfig.enforced,
			display_name: ssoConfig.displayName,
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
			redirect_uri: deriveSsoRedirectUri(Config.endpoints.webApp),
		},
		gateway_rollout: gatewayRollout,
		registration: {
			...registrationConfig,
			urls: registrationUrls,
			pending_registrations: pendingRegistrations,
		},
		self_hosted: Config.instance.selfHosted,
		app_public: appPublic,
		policy: {
			single_community_enabled: policy.single_community_enabled,
			single_community_locked: policy.single_community_locked,
			single_community_guild_id: policy.single_community_guild_id,
			direct_messages_disabled: policy.direct_messages_disabled,
			direct_messages_locked: policy.direct_messages_locked,
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
	};
}

function buildAdminIssuedRegistrationUrl(code: string): string {
	const baseUrl = Config.endpoints.webApp.replace(/\/$/, '');
	return `${baseUrl}/register?registration_url=${encodeURIComponent(code)}`;
}

function requireSetupSessionOrAdminACL(requiredACL: string) {
	const requireAcl = requireAdminACL(requiredACL);
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		const user = ctx.get('user');
		const tokenType = ctx.get('authTokenType');
		if (user && tokenType === 'session') {
			const appPublic = await getInstanceConfigRepository().getAppPublicConfig();
			if (!appPublic.setup.configured) {
				ctx.set('adminUserId', user.id);
				ctx.set('adminUserAcls', user.acls);
				await next();
				return;
			}
		}
		return requireAcl(ctx, next);
	});
}

function hasAdminAuthenticationACL(acls: ReadonlySet<string>): boolean {
	return acls.has(AdminACLs.AUTHENTICATE) || acls.has(AdminACLs.WILDCARD);
}

function completesInitialSetup(data: InstanceConfigUpdateRequest, setupConfigured: boolean): boolean {
	return (
		!setupConfigured &&
		data.app_public?.setup != null &&
		readOptionalField(data.app_public.setup, 'configured') === true
	);
}

async function grantSetupCompleterAdminACL(ctx: Context<HonoEnv>): Promise<void> {
	const user = ctx.get('user');
	if (!user || ctx.get('authTokenType') !== 'session' || hasAdminAuthenticationACL(user.acls)) {
		return;
	}
	const nextACLs = new Set(user.acls);
	nextACLs.add(AdminACLs.WILDCARD);
	const updatedUser = await ctx.get('userRepository').patchUpsert(user.id, {acls: nextACLs}, user.toRow());
	ctx.set('user', updatedUser);
	ctx.set('adminUserAcls', updatedUser.acls);
}

export function InstanceConfigAdminController(app: HonoApp) {
	const instanceConfigRepository = getInstanceConfigRepository();
	app.post(
		'/admin/instance-config/get',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireSetupSessionOrAdminACL(AdminACLs.INSTANCE_CONFIG_VIEW),
		OpenAPI({
			operationId: 'get_instance_config',
			summary: 'Get instance configuration',
			description:
				'Retrieves instance-wide configuration including webhooks and SSO configuration. Requires INSTANCE_CONFIG_VIEW permission.',
			responseSchema: InstanceConfigResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			return ctx.json(await buildInstanceConfigResponse());
		},
	);
	app.post(
		'/admin/instance-config/update',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireSetupSessionOrAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('json', InstanceConfigUpdateRequest),
		OpenAPI({
			operationId: 'update_instance_config',
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
			const appPublicBeforeUpdate = completesInitialSetup(data, false)
				? await instanceConfigRepository.getAppPublicConfig()
				: null;
			const shouldGrantSetupCompleterAdmin =
				appPublicBeforeUpdate !== null && completesInitialSetup(data, appPublicBeforeUpdate.setup.configured);
			if (data.gateway_rollout) {
				const currentRollout = await instanceConfigRepository.getGatewayRolloutConfig();
				const merged = {...currentRollout, ...data.gateway_rollout};
				const validated = GatewayRolloutConfigSchema.parse(merged);
				await instanceConfigRepository.setGatewayRolloutConfig(validated);
				await getGatewayRolloutConfigPublisher().publish(validated);
			}
			if (data.sso) {
				const sso = data.sso;
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
					allowedEmailDomains: mergeOptionalField(
						current.allowedEmailDomains,
						readOptionalField(sso, 'allowed_domains'),
					),
					autoProvision: mergeOptionalField(current.autoProvision, readOptionalField(sso, 'auto_provision')),
				};
				const validated = await normalizeAndValidateSsoConfig(next, {
					testModeEnabled: Config.dev.testModeEnabled,
				});
				await instanceConfigRepository.setSsoConfig({
					enabled: validated.enabled,
					enforced: validated.enforced,
					displayName: next.displayName,
					issuer: validated.issuer,
					authorizationUrl: validated.authorizationUrl,
					tokenUrl: validated.tokenUrl,
					userInfoUrl: validated.userInfoUrl,
					jwksUrl: validated.jwksUrl,
					clientId: validated.clientId,
					clientSecret: readOptionalField(sso, 'client_secret'),
					scope: next.scope,
					allowedEmailDomains: validated.allowedEmailDomains,
					autoProvision: next.autoProvision,
					redirectUri: null,
				});
			}
			if (data.registration) {
				await instanceConfigRepository.setRegistrationConfig({
					mode: data.registration.mode,
					admin_registration_urls_enabled: data.registration.admin_registration_urls_enabled,
				});
			}
			if (data.app_public) {
				await instanceConfigRepository.setAppPublicConfig({
					branding: data.app_public.branding
						? omitUndefinedFields({
								product_name: readOptionalField(data.app_public.branding, 'product_name'),
								icon_url: readOptionalField(data.app_public.branding, 'icon_url'),
								symbol_url: readOptionalField(data.app_public.branding, 'symbol_url'),
								logo_url: readOptionalField(data.app_public.branding, 'logo_url'),
								wordmark_url: readOptionalField(data.app_public.branding, 'wordmark_url'),
								favicon_url: readOptionalField(data.app_public.branding, 'favicon_url'),
								theme_color: readOptionalField(data.app_public.branding, 'theme_color'),
							})
						: undefined,
					setup: data.app_public.setup
						? omitUndefinedFields({
								configured: readOptionalField(data.app_public.setup, 'configured'),
							})
						: undefined,
					legal: data.app_public.legal
						? omitUndefinedFields({
								terms_url: readOptionalField(data.app_public.legal, 'terms_url'),
								privacy_url: readOptionalField(data.app_public.legal, 'privacy_url'),
							})
						: undefined,
					registration: data.app_public.registration
						? omitUndefinedFields({
								collect_date_of_birth: readOptionalField(data.app_public.registration, 'collect_date_of_birth'),
							})
						: undefined,
				});
			}
			if (data.integrations) {
				await instanceConfigRepository.setInstanceIntegrationsConfig({
					gif: data.integrations.gif
						? omitUndefinedFields({
								klipy_api_key: readOptionalField(data.integrations.gif, 'klipy_api_key'),
							})
						: undefined,
					youtube: data.integrations.youtube
						? omitUndefinedFields({
								api_key: readOptionalField(data.integrations.youtube, 'api_key'),
							})
						: undefined,
					captcha: data.integrations.captcha
						? omitUndefinedFields({
								provider: readOptionalField(data.integrations.captcha, 'provider'),
								hcaptcha_site_key: readOptionalField(data.integrations.captcha, 'hcaptcha_site_key'),
								hcaptcha_secret_key: readOptionalField(data.integrations.captcha, 'hcaptcha_secret_key'),
								turnstile_site_key: readOptionalField(data.integrations.captcha, 'turnstile_site_key'),
								turnstile_secret_key: readOptionalField(data.integrations.captcha, 'turnstile_secret_key'),
							})
						: undefined,
					email: data.integrations.email
						? {
								...omitUndefinedFields({
									enabled: readOptionalField(data.integrations.email, 'enabled'),
									provider: readOptionalField(data.integrations.email, 'provider'),
									from_email: readOptionalField(data.integrations.email, 'from_email'),
									from_name: readOptionalField(data.integrations.email, 'from_name'),
								}),
								smtp: data.integrations.email.smtp
									? omitUndefinedFields({
											host: readOptionalField(data.integrations.email.smtp, 'host'),
											port: readOptionalField(data.integrations.email.smtp, 'port'),
											username: readOptionalField(data.integrations.email.smtp, 'username'),
											password: readOptionalField(data.integrations.email.smtp, 'password'),
											secure: readOptionalField(data.integrations.email.smtp, 'secure'),
										})
									: undefined,
							}
						: undefined,
					bluesky: data.integrations.bluesky
						? {
								...omitUndefinedFields({
									enabled: readOptionalField(data.integrations.bluesky, 'enabled'),
									client_name: readOptionalField(data.integrations.bluesky, 'client_name'),
									client_uri: readOptionalField(data.integrations.bluesky, 'client_uri'),
									logo_uri: readOptionalField(data.integrations.bluesky, 'logo_uri'),
									tos_uri: readOptionalField(data.integrations.bluesky, 'tos_uri'),
									policy_uri: readOptionalField(data.integrations.bluesky, 'policy_uri'),
								}),
								keys: readOptionalField(data.integrations.bluesky, 'keys'),
							}
						: undefined,
				});
			}
			if (data.media) {
				await instanceConfigRepository.setInstanceMediaConfig({
					attachment_decay: data.media.attachment_decay
						? omitUndefinedFields({
								enabled: readOptionalField(data.media.attachment_decay, 'enabled'),
								min_size_mb: readOptionalField(data.media.attachment_decay, 'min_size_mb'),
								max_size_mb: readOptionalField(data.media.attachment_decay, 'max_size_mb'),
								max_eligible_size_mb: readOptionalField(data.media.attachment_decay, 'max_eligible_size_mb'),
								min_lifetime_days: readOptionalField(data.media.attachment_decay, 'min_lifetime_days'),
								max_lifetime_days: readOptionalField(data.media.attachment_decay, 'max_lifetime_days'),
								curve: readOptionalField(data.media.attachment_decay, 'curve'),
								renew_threshold_days: readOptionalField(data.media.attachment_decay, 'renew_threshold_days'),
								renew_window_days: readOptionalField(data.media.attachment_decay, 'renew_window_days'),
							})
						: undefined,
				});
			}
			if (data.policy) {
				await applyInstancePolicyUpdate(ctx, data.policy);
			}
			if (shouldGrantSetupCompleterAdmin) {
				await grantSetupCompleterAdminACL(ctx);
				await instanceConfigRepository.markAdminBootstrapped();
			}
			return ctx.json(await buildInstanceConfigResponse());
		},
	);
	app.post(
		'/admin/instance-config/branding-asset',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireSetupSessionOrAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('json', BrandingAssetUploadRequest),
		OpenAPI({
			operationId: 'upload_instance_branding_asset',
			summary: 'Upload or clear an instance branding asset',
			description:
				'Uploads a branding image served by the media proxy and stores its URL, or clears it when no image is provided. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: InstanceConfigResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {kind, image} = ctx.req.valid('json');
			const prepared = await ctx.get('entityAssetService').prepareAssetUpload({
				assetType: 'branding',
				entityType: 'instance',
				entityId: INSTANCE_BRANDING_ENTITY_ID,
				previousHash: null,
				base64Image: image ?? null,
				errorPath: 'image',
			});
			const brandingPatch: Partial<InstanceBrandingConfig> = {[`${kind}_url`]: prepared.newCdnUrl};
			await instanceConfigRepository.setAppPublicConfig({branding: brandingPatch});
			return ctx.json(await buildInstanceConfigResponse());
		},
	);
	app.post(
		'/admin/instance-config/integrations/smtp/test',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireSetupSessionOrAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('json', InstanceEmailSmtpTestRequest),
		OpenAPI({
			operationId: 'test_instance_smtp_config',
			summary: 'Validate SMTP configuration',
			description:
				'Validates that an SMTP configuration can authenticate and accept a connection. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: InstanceEmailSmtpTestResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const data = ctx.req.valid('json');
			try {
				const provider = new SmtpEmailProvider({
					host: data.host,
					port: data.port,
					username: data.username,
					password: data.password,
					secure: data.secure,
					connectionTimeoutMs: 10000,
					greetingTimeoutMs: 10000,
					socketTimeoutMs: 10000,
				});
				await provider.verify();
				return ctx.json({ok: true, error: null});
			} catch (error) {
				return ctx.json({ok: false, error: error instanceof Error ? error.message : String(error)});
			}
		},
	);
	app.post(
		'/admin/instance-config/registration-urls/create',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('json', CreateRegistrationUrlRequest),
		OpenAPI({
			operationId: 'create_registration_url',
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
			const created = await instanceConfigRepository.createRegistrationUrl({
				label: data.label?.trim() || null,
				createdByUserId: ctx.get('adminUserId').toString(),
				expiresAt: data.expires_at ? new Date(data.expires_at) : null,
				maxUses: data.max_uses ?? null,
				approvalRequired: data.approval_required,
			});
			return ctx.json({
				registration_url: created.registrationUrl,
				code: created.code,
				url: buildAdminIssuedRegistrationUrl(created.code),
			});
		},
	);
	app.post(
		'/admin/instance-config/registration-urls/revoke',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('json', RegistrationUrlActionRequest),
		OpenAPI({
			operationId: 'revoke_registration_url',
			summary: 'Revoke an admin-issued registration URL',
			description:
				'Revokes an admin-issued registration URL so it can no longer be used. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: InstanceConfigResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			await instanceConfigRepository.revokeRegistrationUrl(ctx.req.valid('json').id);
			return ctx.json(await buildInstanceConfigResponse());
		},
	);
	app.post(
		'/admin/instance-config/pending-registrations/approve',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('json', PendingRegistrationActionRequest),
		OpenAPI({
			operationId: 'approve_pending_registration',
			summary: 'Approve a pending registration',
			description:
				'Approves a registration waiting for manual review by removing its pending registration trait. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: InstanceConfigResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const userId = ctx.req.valid('json').user_id;
			await updatePendingRegistrationUser(ctx, userId, 'approve');
			await instanceConfigRepository.removePendingRegistration(userId);
			return ctx.json(await buildInstanceConfigResponse());
		},
	);
	app.post(
		'/admin/instance-config/pending-registrations/reject',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('json', PendingRegistrationActionRequest),
		OpenAPI({
			operationId: 'reject_pending_registration',
			summary: 'Reject a pending registration',
			description:
				'Rejects a registration waiting for manual review and prevents the account from logging in. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: InstanceConfigResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const userId = ctx.req.valid('json').user_id;
			await updatePendingRegistrationUser(ctx, userId, 'reject');
			await instanceConfigRepository.removePendingRegistration(userId);
			return ctx.json(await buildInstanceConfigResponse());
		},
	);
}

async function applyInstancePolicyUpdate(
	ctx: Context<HonoEnv>,
	policy: NonNullable<InstanceConfigUpdateRequest['policy']>,
): Promise<void> {
	const instanceConfigRepository = getInstanceConfigRepository();
	const [current, appPublic] = await Promise.all([
		instanceConfigRepository.getInstancePolicyConfig(),
		instanceConfigRepository.getAppPublicConfig(),
	]);
	const patch: Partial<InstancePolicyConfig> = {};
	if (
		policy.single_community_enabled !== undefined &&
		policy.single_community_enabled !== current.single_community_enabled
	) {
		if (policy.single_community_enabled) {
			if (appPublic.setup.configured || current.single_community_locked) {
				throw new InstancePolicyTransitionNotAllowedError();
			}
			const adminUser = await ctx.get('userRepository').findUnique(ctx.get('adminUserId'));
			if (!adminUser) {
				throw new InstancePolicyTransitionNotAllowedError();
			}
			await ctx.get('singleCommunityService').createStockCommunity({
				owner: adminUser,
				name: policy.single_community_name?.trim() || appPublic.branding.product_name,
			});
		} else {
			patch.single_community_enabled = false;
			patch.single_community_locked = true;
		}
	}
	if (
		policy.direct_messages_disabled !== undefined &&
		policy.direct_messages_disabled !== current.direct_messages_disabled
	) {
		if (current.direct_messages_locked) {
			throw new InstancePolicyTransitionNotAllowedError();
		}
		patch.direct_messages_disabled = policy.direct_messages_disabled;
		if (!policy.direct_messages_disabled) {
			patch.direct_messages_locked = true;
		}
	}
	if (policy.premium_mode !== undefined) {
		patch.premium_mode = policy.premium_mode;
	}
	if (policy.services) {
		if (policy.services.gif_enabled !== undefined) {
			patch.gif_enabled = policy.services.gif_enabled ?? null;
		}
		if (policy.services.youtube_enabled !== undefined) {
			patch.youtube_enabled = policy.services.youtube_enabled ?? null;
		}
		if (policy.services.bluesky_enabled !== undefined) {
			patch.bluesky_enabled = policy.services.bluesky_enabled ?? null;
		}
	}
	if (Object.keys(patch).length > 0) {
		await instanceConfigRepository.setInstancePolicyConfig(patch);
	}
	if (policy.premium_mode !== undefined && policy.premium_mode !== current.premium_mode) {
		await ctx.get('limitConfigService').reloadForPolicyChange();
	}
}

async function updatePendingRegistrationUser(
	ctx: Context<HonoEnv>,
	userId: string,
	decision: 'approve' | 'reject',
): Promise<void> {
	const userRepository = ctx.get('userRepository');
	const user = await userRepository.findUnique(createUserID(BigInt(userId)));
	if (!user) {
		return;
	}
	const traits = new Set(user.traits);
	traits.delete(REGISTRATION_PENDING_APPROVAL_TRAIT);
	if (decision === 'reject') {
		traits.add(REGISTRATION_REJECTED_TRAIT);
	} else {
		traits.delete(REGISTRATION_REJECTED_TRAIT);
	}
	await userRepository.patchUpsert(user.id, {traits: traits.size > 0 ? traits : null}, user.toRow());
}
