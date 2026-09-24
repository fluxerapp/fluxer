// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead, recordAdminWrite} from '@app/api/admin/AdminAuditRecorder';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {
	type InstancePolicyConfig,
	REGISTRATION_PENDING_APPROVAL_TRAIT,
	REGISTRATION_REJECTED_TRAIT,
} from '@app/api/instance/InstanceConfigRepository';
import {deriveSsoRedirectUri, normalizeAndValidateSsoConfig} from '@app/api/instance/SsoConfigValidation';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {
	getGatewayRolloutConfigPublisher,
	getInstanceConfigRepository,
	getPushServiceDeliveryConfigPublisher,
} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
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
	RegistrationUrlIdParam,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {GatewayRolloutConfigSchema} from '@fluxer/schema/src/domains/admin/GatewayRolloutSchemas';
import {PushServiceDeliveryConfigSchema} from '@fluxer/schema/src/domains/admin/PushServiceDeliverySchemas';
import {VoiceNoiseSuppressionConfigSchema} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {ExperimentDeliveryConfigSchema} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import type {InstanceBranding} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import {SmtpEmailProvider} from '@pkgs/email/src/SmtpEmailProvider';
import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';

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
	const [
		ssoConfig,
		gatewayRollout,
		voiceNoiseSuppression,
		pushServiceDelivery,
		experimentDelivery,
		registrationConfig,
		registrationUrls,
		pendingRegistrations,
	] = await Promise.all([
		instanceConfigRepository.getSsoConfig(),
		instanceConfigRepository.getGatewayRolloutConfig(),
		instanceConfigRepository.getVoiceNoiseSuppressionConfig(),
		instanceConfigRepository.getPushServiceDeliveryConfig(),
		instanceConfigRepository.getExperimentDeliveryConfig(),
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
		voice_noise_suppression: voiceNoiseSuppression,
		push_service_delivery: pushServiceDelivery,
		experiment_delivery: experimentDelivery,
		registration: {
			...registrationConfig,
			urls: registrationUrls,
			pending_registrations: pendingRegistrations,
		},
		self_hosted: Config.instance.selfHosted,
		app_public: appPublic,
		policy: {
			single_community_enabled: policy.single_community_enabled,
			single_community_guild_id: policy.single_community_guild_id,
			direct_messages_disabled: policy.direct_messages_disabled,
			direct_messages_locked: policy.direct_messages_locked,
			premium_mode: policy.premium_mode,
			services: {
				gif_enabled: policy.gif_enabled,
				youtube_enabled: policy.youtube_enabled,
				bluesky_enabled: policy.bluesky_enabled,
			},
			deferred_phone_gate: {
				enabled: policy.deferred_phone_gate_enabled,
				window_hours: policy.deferred_phone_gate_window_hours,
				member_threshold: policy.deferred_phone_gate_member_threshold,
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

async function grantSetupCompleterAdminACL(ctx: Context<HonoEnv>): Promise<boolean> {
	const user = ctx.get('user');
	if (!user || ctx.get('authTokenType') !== 'session' || hasAdminAuthenticationACL(user.acls)) {
		return false;
	}
	const nextACLs = new Set(user.acls);
	nextACLs.add(AdminACLs.WILDCARD);
	const updatedUser = await ctx.get('userRepository').patchUpsert(user.id, {acls: nextACLs}, user.toRow());
	ctx.set('user', updatedUser);
	ctx.set('adminUserAcls', updatedUser.acls);
	return true;
}

function listSuppliedSections(data: InstanceConfigUpdateRequest): string | undefined {
	const sections = Object.entries(data)
		.filter(([, value]) => value != null)
		.map(([key]) => key)
		.sort();
	return sections.length > 0 ? sections.join(',') : undefined;
}

export function InstanceConfigAdminController(app: HonoApp) {
	const instanceConfigRepository = getInstanceConfigRepository();
	app.get(
		'/admin/instance/config',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireSetupSessionOrAdminACL(AdminACLs.INSTANCE_CONFIG_VIEW),
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
				metadata: {
					registration_url_count: response.registration.urls.length,
					pending_registration_count: response.registration.pending_registrations.length,
				},
			});
			return ctx.json(response);
		},
	);
	app.patch(
		'/admin/instance/config',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireSetupSessionOrAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
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
			const appPublicBeforeUpdate = completesInitialSetup(data, false)
				? await instanceConfigRepository.getAppPublicConfig()
				: null;
			const shouldGrantSetupCompleterAdmin =
				appPublicBeforeUpdate !== null && completesInitialSetup(data, appPublicBeforeUpdate.setup.configured);
			if (data.gateway_rollout) {
				const patch = data.gateway_rollout;
				const landed = await instanceConfigRepository.updateGatewayRolloutConfig((current) =>
					GatewayRolloutConfigSchema.parse({...current, ...patch}),
				);
				await getGatewayRolloutConfigPublisher().publish(landed);
			}
			if (data.voice_noise_suppression) {
				const patch = omitUndefinedFields(data.voice_noise_suppression);
				if (Object.keys(patch).length > 0) {
					await instanceConfigRepository.updateVoiceNoiseSuppressionConfig((current) =>
						VoiceNoiseSuppressionConfigSchema.parse({
							...current,
							...patch,
							config_version: current.config_version + 1,
						}),
					);
				}
			}
			if (data.push_service_delivery) {
				const patch = omitUndefinedFields(data.push_service_delivery);
				if (Object.keys(patch).length > 0) {
					const landed = await instanceConfigRepository.updatePushServiceDeliveryConfig((current) =>
						PushServiceDeliveryConfigSchema.parse({
							...current,
							...patch,
							config_version: current.config_version + 1,
						}),
					);
					await getPushServiceDeliveryConfigPublisher().publish(landed);
				}
			}
			if (data.experiment_delivery) {
				const patch = data.experiment_delivery;
				await instanceConfigRepository.updateExperimentDeliveryConfig((current) =>
					ExperimentDeliveryConfigSchema.parse({...current, ...patch}),
				);
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
				const supplied = <T>(field: keyof typeof sso, value: T): T | undefined =>
					readOptionalField(sso, field) === undefined ? undefined : value;
				await instanceConfigRepository.setSsoConfig({
					enabled: supplied('enabled', validated.enabled),
					enforced: supplied('enforced', validated.enforced),
					displayName: supplied('display_name', next.displayName),
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
								status_page_url: readOptionalField(data.app_public.branding, 'status_page_url'),
								status_page_incident_history_url: readOptionalField(
									data.app_public.branding,
									'status_page_incident_history_url',
								),
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
									disable_new_ip_authorization: readOptionalField(
										data.integrations.email,
										'disable_new_ip_authorization',
									),
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
			if (data.app_public?.setup) {
				await instanceConfigRepository.setAppPublicConfig({
					setup: omitUndefinedFields({
						configured: readOptionalField(data.app_public.setup, 'configured'),
					}),
				});
			}
			let grantedSetupCompleterAdmin = false;
			if (shouldGrantSetupCompleterAdmin) {
				grantedSetupCompleterAdmin = await grantSetupCompleterAdminACL(ctx);
				await instanceConfigRepository.markAdminBootstrapped();
			}
			await recordAdminWrite(ctx, {
				targetType: 'instance_config',
				targetId: 0n,
				action: 'update_instance_config',
				metadata: {
					sections: listSuppliedSections(data),
					granted_acls: grantedSetupCompleterAdmin ? AdminACLs.WILDCARD : undefined,
				},
			});
			return ctx.json(await buildInstanceConfigResponse());
		},
	);
	app.post(
		'/admin/instance/config/branding-assets',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireSetupSessionOrAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
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
			const prepared = await ctx.get('entityAssetService').prepareAssetUpload({
				assetType: 'branding',
				entityType: 'instance',
				entityId: INSTANCE_BRANDING_ENTITY_ID,
				previousHash: null,
				base64Image: image ?? null,
				errorPath: 'image',
			});
			const brandingPatch: Partial<InstanceBranding> = {[`${kind}_url`]: prepared.newCdnUrl};
			await instanceConfigRepository.setAppPublicConfig({branding: brandingPatch});
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
		requireSetupSessionOrAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('json', InstanceEmailSmtpTestRequest),
		OpenAPI({
			operationId: 'create_admin_instance_smtp_test',
			summary: 'Run an SMTP configuration test',
			description:
				'Validates that an SMTP configuration can authenticate and accept a connection. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: InstanceEmailSmtpTestResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const data = ctx.req.valid('json');
			let result: InstanceEmailSmtpTestResponse;
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
				result = {ok: true, error: null};
			} catch (error) {
				result = {ok: false, error: error instanceof Error ? error.message : String(error)};
			}
			await recordAdminWrite(ctx, {
				targetType: 'instance_config',
				targetId: 0n,
				action: 'test_smtp_connection',
				metadata: {port: data.port, secure: data.secure, ok: result.ok},
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
			const created = await instanceConfigRepository.createRegistrationUrl({
				label: data.label?.trim() || null,
				createdByUserId: ctx.get('adminUserId').toString(),
				expiresAt: data.expires_at ? new Date(data.expires_at) : null,
				maxUses: data.max_uses ?? null,
				approvalRequired: data.approval_required,
			});
			await recordAdminWrite(ctx, {
				targetType: 'registration_url',
				targetId: 0n,
				action: 'create_registration_url',
				metadata: {approval_required: data.approval_required, max_uses: data.max_uses},
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
			summary: 'Revoke an admin-issued registration URL',
			description:
				'Revokes an admin-issued registration URL so it can no longer be used. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: InstanceConfigResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			await instanceConfigRepository.revokeRegistrationUrl(ctx.req.valid('param').registration_url_id);
			await recordAdminWrite(ctx, {
				targetType: 'registration_url',
				targetId: 0n,
				action: 'revoke_registration_url',
			});
			return ctx.json(await buildInstanceConfigResponse());
		},
	);
	app.patch(
		'/admin/instance/pending-registrations/:user_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('param', UserIdParam),
		Validator('json', PendingRegistrationActionRequest),
		OpenAPI({
			operationId: 'update_admin_pending_registration',
			summary: 'Approve or reject a pending registration',
			description:
				'Decides a registration waiting for manual review. Approving removes its pending registration trait, rejecting also prevents the account from logging in. Requires INSTANCE_CONFIG_UPDATE permission.',
			responseSchema: InstanceConfigResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const userId = ctx.req.valid('param').user_id.toString();
			const decision = ctx.req.valid('json').status === 'approved' ? 'approve' : 'reject';
			await updatePendingRegistrationUser(ctx, userId, decision);
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
	const appPublic = await instanceConfigRepository.getAppPublicConfig();
	const adminUser =
		policy.single_community_enabled === true
			? await ctx.get('userRepository').findUnique(ctx.get('adminUserId'))
			: null;
	let enablesSingleCommunity = false;
	await instanceConfigRepository.updateInstancePolicyConfig((current) => {
		const planned = planInstancePolicyPatch(policy, current, {
			setupConfigured: appPublic.setup.configured,
			adminUserFound: adminUser !== null,
		});
		enablesSingleCommunity = planned.enablesSingleCommunity;
		return planned.patch;
	});
	if (enablesSingleCommunity && adminUser) {
		await ctx.get('singleCommunityService').ensureStockCommunity({
			owner: adminUser,
			name: policy.single_community_name?.trim() || appPublic.branding.product_name,
		});
	}
	if (policy.premium_mode !== undefined) {
		await ctx.get('limitConfigService').updatePolicyConfig({premium_mode: policy.premium_mode});
	}
}

function planInstancePolicyPatch(
	policy: NonNullable<InstanceConfigUpdateRequest['policy']>,
	current: InstancePolicyConfig,
	context: {setupConfigured: boolean; adminUserFound: boolean},
): {patch: Partial<InstancePolicyConfig>; enablesSingleCommunity: boolean} {
	const patch: Partial<InstancePolicyConfig> = {};
	let enablesSingleCommunity = false;
	if (
		policy.single_community_enabled !== undefined &&
		policy.single_community_enabled !== current.single_community_enabled
	) {
		if (policy.single_community_enabled) {
			if ((context.setupConfigured && current.single_community_guild_id == null) || !context.adminUserFound) {
				throw new InstancePolicyTransitionNotAllowedError();
			}
			enablesSingleCommunity = true;
		} else {
			patch.single_community_enabled = false;
		}
	}
	const unlockDirectMessages = policy.direct_messages_locked === false;
	if (unlockDirectMessages && current.direct_messages_locked) {
		patch.direct_messages_locked = false;
	}
	if (
		policy.direct_messages_disabled !== undefined &&
		policy.direct_messages_disabled !== current.direct_messages_disabled
	) {
		if (current.direct_messages_locked && !unlockDirectMessages) {
			throw new InstancePolicyTransitionNotAllowedError();
		}
		patch.direct_messages_disabled = policy.direct_messages_disabled;
		if (!policy.direct_messages_disabled) {
			patch.direct_messages_locked = true;
		}
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
	if (policy.deferred_phone_gate) {
		if (policy.deferred_phone_gate.enabled !== undefined) {
			patch.deferred_phone_gate_enabled = policy.deferred_phone_gate.enabled;
		}
		if (policy.deferred_phone_gate.window_hours !== undefined) {
			patch.deferred_phone_gate_window_hours = policy.deferred_phone_gate.window_hours;
		}
		if (policy.deferred_phone_gate.member_threshold !== undefined) {
			patch.deferred_phone_gate_member_threshold = policy.deferred_phone_gate.member_threshold;
		}
	}
	return {patch, enablesSingleCommunity};
}

async function updatePendingRegistrationUser(
	ctx: Context<HonoEnv>,
	userId: string,
	decision: 'approve' | 'reject',
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
	const wasPendingApproval = traits.delete(REGISTRATION_PENDING_APPROVAL_TRAIT);
	if (decision === 'reject') {
		traits.add(REGISTRATION_REJECTED_TRAIT);
	} else {
		traits.delete(REGISTRATION_REJECTED_TRAIT);
	}
	await userRepository.patchUpsert(user.id, {traits: traits.size > 0 ? traits : null}, user.toRow());
	if (decision === 'approve' && wasPendingApproval) {
		await ctx.get('singleCommunityService').joinStockCommunity(user.id, ctx.get('requestCache'));
	}
	await ctx.get('adminService').auditService.createAuditLog({
		adminUserId: ctx.get('adminUserId'),
		targetType: 'user',
		targetId: user.id,
		action: decision === 'approve' ? 'approve_registration' : 'reject_registration',
		auditLogReason: ctx.get('auditLogReason'),
	});
}
