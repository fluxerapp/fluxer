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

import {createEmailVerificationToken, createInviteCode, createUserID, type UserID} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import {FIRST_ADMIN_ACL_CONFIG_KEY} from '@fluxer/api/src/constants/InstanceConfig';
import {deleteOneOrMany, executeConditional} from '@fluxer/api/src/database/Cassandra';
import type {IDiscriminatorService} from '@fluxer/api/src/infrastructure/DiscriminatorService';
import type {IEmailDnsValidationService} from '@fluxer/api/src/infrastructure/IEmailDnsValidationService';
import type {KVActivityTracker} from '@fluxer/api/src/infrastructure/KVActivityTracker';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import {InstanceConfigRepository} from '@fluxer/api/src/instance/InstanceConfigRepository';
import type {SnowflakeReservationService} from '@fluxer/api/src/instance/SnowflakeReservationService';
import type {InviteService} from '@fluxer/api/src/invite/InviteService';
import {Logger} from '@fluxer/api/src/Logger';
import type {RequestCache} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import type {AuthSession} from '@fluxer/api/src/models/AuthSession';
import type {User} from '@fluxer/api/src/models/User';
import {UserSettings} from '@fluxer/api/src/models/UserSettings';
import {getUserSearchService} from '@fluxer/api/src/SearchFactory';
import {InstanceConfiguration, UserByEmail} from '@fluxer/api/src/Tables';
import {withBusinessSpan} from '@fluxer/api/src/telemetry/BusinessSpans';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import * as AgeUtils from '@fluxer/api/src/utils/AgeUtils';
import * as FetchUtils from '@fluxer/api/src/utils/FetchUtils';
import {getIpAddressReverse, lookupGeoip} from '@fluxer/api/src/utils/IpUtils';
import {generateRandomUsername} from '@fluxer/api/src/utils/UsernameGenerator';
import {deriveUsernameFromDisplayName} from '@fluxer/api/src/utils/UsernameSuggestionUtils';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {IEmailService} from '@fluxer/email/src/IEmailService';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {formatGeoipLocation, type GeoipResult, UNKNOWN_LOCATION} from '@fluxer/geoip/src/GeoipLookup';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {parseAcceptLanguage} from '@fluxer/locale/src/LocaleService';
import type {IRateLimitService, RateLimitResult} from '@fluxer/rate_limit/src/IRateLimitService';
import type {RegisterRequest} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {recordCounter} from '@fluxer/telemetry/src/Metrics';
import Bowser from 'bowser';
import {types} from 'cassandra-driver';
import {ms} from 'itty-time';

const MINIMUM_AGE_BY_COUNTRY: Record<string, number> = {
	KR: 14,
	VN: 15,
	AW: 16,
	BQ: 16,
	CW: 16,
	SX: 16,
	AT: 14,
	BG: 14,
	HR: 16,
	CY: 14,
	CZ: 15,
	FR: 15,
	DE: 16,
	GR: 15,
	HU: 16,
	IE: 16,
	IT: 14,
	LT: 14,
	LU: 16,
	NL: 16,
	PL: 16,
	RO: 16,
	SM: 16,
	RS: 15,
	SK: 16,
	SI: 16,
	ES: 14,
	CL: 14,
	CO: 14,
	PE: 14,
	VE: 14,
};

const DEFAULT_MINIMUM_AGE = 13;
const USER_AGENT_TRUNCATE_LENGTH = 512;

interface RegistrationMetadataContext {
	metadata: Map<string, string>;
	clientIp: string;
	countryCode: string;
	location: string;
	city: string | null;
	region: string | null;
	osInfo: string;
	browserInfo: string;
	deviceInfo: string;
	truncatedUserAgent: string;
	fluxerTag: string;
	displayName: string;
	email: string;
	ipAddressReverse: string | null;
}

const AGE_BUCKETS: Array<{label: string; min: number; max: number}> = [
	{label: '0-12', min: 0, max: 12},
	{label: '13-17', min: 13, max: 17},
	{label: '18-24', min: 18, max: 24},
	{label: '25-34', min: 25, max: 34},
	{label: '35-44', min: 35, max: 44},
	{label: '45-54', min: 45, max: 54},
	{label: '55-64', min: 55, max: 64},
];

function determineAgeGroup(age: number | null): string {
	if (age === null || age < 0) return 'unknown';
	for (const bucket of AGE_BUCKETS) {
		if (age >= bucket.min && age <= bucket.max) return bucket.label;
	}
	return '65+';
}

function isIpv6(ip: string): boolean {
	return ip.includes(':');
}

function getRetryAfterSeconds(result: RateLimitResult): number {
	return result.retryAfter ?? Math.max(0, Math.ceil((result.resetTime.getTime() - Date.now()) / 1000));
}

function throwRegistrationRateLimit(result: RateLimitResult): never {
	throw new RateLimitError({
		retryAfter: getRetryAfterSeconds(result),
		limit: result.limit,
		resetTime: result.resetTime,
	});
}

function parseDobLocalDate(dateOfBirth: string): types.LocalDate {
	try {
		return types.LocalDate.fromString(dateOfBirth);
	} catch {
		throw InputValidationError.create('date_of_birth', 'Invalid date of birth format');
	}
}

interface RegisterParams {
	data: RegisterRequest;
	request: Request;
	requestCache: RequestCache;
}

export class AuthRegistrationService {
	private instanceConfigRepository = new InstanceConfigRepository();
	private hasWarnedAboutMissingWebhook = false;

	constructor(
		private repository: IUserRepository,
		private inviteService: InviteService | null,
		private rateLimitService: IRateLimitService,
		private emailService: IEmailService,
		private emailDnsValidationService: IEmailDnsValidationService,
		private snowflakeService: SnowflakeService,
		private snowflakeReservationService: SnowflakeReservationService,
		private discriminatorService: IDiscriminatorService,
		private kvActivityTracker: KVActivityTracker,
		private cacheService: ICacheService,
		private hashPassword: (password: string) => Promise<string>,
		private isPasswordPwned: (password: string) => Promise<boolean>,
		private validateAge: (params: {dateOfBirth: string; minAge: number}) => boolean,
		private generateSecureToken: () => Promise<string>,
		private createAuthSession: (params: {user: User; request: Request}) => Promise<[string, AuthSession]>,
	) {}

	async register({data, request, requestCache}: RegisterParams): Promise<{user_id: string; token: string}> {
		return await withBusinessSpan('fluxer.auth.register', 'fluxer.auth.registrations', {}, () =>
			this.performRegister({data, request, requestCache}),
		);
	}

	private async performRegister({
		data,
		request,
		requestCache,
	}: RegisterParams): Promise<{user_id: string; token: string}> {
		if (!data.consent) {
			throw InputValidationError.create('consent', 'You must agree to the Terms of Service and Privacy Policy');
		}

		const now = new Date();

		const clientIp = requireClientIp(request, {
			trustCfConnectingIp: Config.proxy.trust_cf_connecting_ip,
		});
		const geoipResult = await lookupGeoip(clientIp);
		const countryCode = geoipResult.countryCode;

		const minAge = (countryCode && MINIMUM_AGE_BY_COUNTRY[countryCode]) || DEFAULT_MINIMUM_AGE;
		if (!this.validateAge({dateOfBirth: data.date_of_birth, minAge})) {
			throw InputValidationError.create(
				'date_of_birth',
				`You must be at least ${minAge} years old to create an account`,
			);
		}

		if (data.password && (await this.isPasswordPwned(data.password))) {
			throw InputValidationError.create('password', 'Password is too common');
		}

		const rawEmail = data.email ?? null;
		const emailKey = rawEmail ? rawEmail.toLowerCase() : null;

		const enforceRateLimits = !Config.dev.relaxRegistrationRateLimits;
		await this.enforceRegistrationRateLimits({enforceRateLimits, clientIp, emailKey});

		if (rawEmail) {
			const hasValidDns = await this.emailDnsValidationService.hasValidDnsRecords(rawEmail);
			if (!hasValidDns) {
				throw InputValidationError.fromCode('email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
			}

			const emailTaken = await this.repository.findByEmail(rawEmail);
			if (emailTaken) throw InputValidationError.create('email', 'Email already in use');
		}

		let usernameCandidate: string | undefined = data.username ?? undefined;
		let discriminator: number | null = null;

		if (!usernameCandidate) {
			const derivedUsername = deriveUsernameFromDisplayName(data.global_name ?? '');
			if (derivedUsername) {
				try {
					discriminator = await this.allocateDiscriminator(derivedUsername);
					usernameCandidate = derivedUsername;
				} catch (error) {
					if (!(error instanceof InputValidationError)) {
						throw error;
					}
				}
			}
		}

		if (!usernameCandidate) {
			usernameCandidate = generateRandomUsername();
			discriminator = await this.allocateDiscriminator(usernameCandidate);
		} else if (discriminator === null) {
			discriminator = await this.allocateDiscriminator(usernameCandidate);
		}

		const username = usernameCandidate!;

		const userId = await this.generateUserId(emailKey);

		if (rawEmail) {
			const {applied} = await executeConditional(
				UserByEmail.insertIfNotExists({
					email_lower: rawEmail.toLowerCase(),
					user_id: userId,
				}),
			);
			if (!applied) {
				throw InputValidationError.create('email', 'Email already in use');
			}
		}

		const acceptLanguage = request.headers.get('accept-language');
		const userLocale = parseAcceptLanguage(acceptLanguage);

		const passwordHash = data.password ? await this.hashPassword(data.password) : null;

		const instanceConfig = await this.instanceConfigRepository.getInstanceConfig();

		const flags = Config.nodeEnv === 'development' ? UserFlags.STAFF : 0n;

		let user: User;
		try {
			user = await this.repository.create({
				user_id: userId,
				username,
				discriminator,
				global_name: data.global_name || null,
				bot: false,
				system: false,
				email: rawEmail,
				email_verified: false,
				email_bounced: false,
				phone: null,
				password_hash: passwordHash,
				password_last_changed_at: passwordHash ? now : null,
				totp_secret: null,
				authenticator_types: new Set(),
				avatar_hash: null,
				avatar_color: null,
				banner_hash: null,
				banner_color: null,
				bio: null,
				pronouns: null,
				accent_color: null,
				date_of_birth: parseDobLocalDate(data.date_of_birth),
				locale: userLocale,
				flags,
				premium_type: null,
				premium_since: null,
				premium_until: null,
				premium_will_cancel: null,
				premium_billing_cycle: null,
				premium_lifetime_sequence: null,
				stripe_subscription_id: null,
				stripe_customer_id: null,
				has_ever_purchased: null,
				suspicious_activity_flags: null,
				terms_agreed_at: now,
				privacy_agreed_at: now,
				last_active_at: now,
				last_active_ip: clientIp,
				temp_banned_until: null,
				pending_deletion_at: null,
				pending_bulk_message_deletion_at: null,
				pending_bulk_message_deletion_channel_count: null,
				pending_bulk_message_deletion_message_count: null,
				deletion_reason_code: null,
				deletion_public_reason: null,
				deletion_audit_log_reason: null,
				acls: null,
				traits: null,
				first_refund_at: null,
				gift_inventory_server_seq: null,
				gift_inventory_client_seq: null,
				premium_onboarding_dismissed_at: null,
				version: 1,
			});

			if (!Config.dev.testModeEnabled) {
				const firstAdminAclAssigned = await this.reserveFirstAdminAcl();
				if (firstAdminAclAssigned) {
					user = await this.repository.patchUpsert(user.id, {acls: new Set([AdminACLs.WILDCARD])}, user.toRow());
				}
			}

			await this.kvActivityTracker.updateActivity(user.id, now);

			recordCounter({
				name: 'user.registration',
				dimensions: {
					country: countryCode ?? 'unknown',
					state: geoipResult.region ?? 'unknown',
					ip_version: isIpv6(clientIp) ? 'v6' : 'v4',
				},
			});

			const age = data.date_of_birth ? AgeUtils.calculateAge(data.date_of_birth) : null;
			recordCounter({
				name: 'user.age',
				dimensions: {
					country: countryCode ?? 'unknown',
					state: geoipResult.region ?? 'unknown',
					age: age !== null ? age.toString() : 'unknown',
					age_group: determineAgeGroup(age),
				},
			});

			await this.repository.upsertSettings(
				UserSettings.getDefaultUserSettings({
					userId,
					locale: userLocale,
					isAdult: AgeUtils.isUserAdult(data.date_of_birth),
				}),
			);

			await this.maybeIndexUser(user);

			if (rawEmail) await this.maybeSendVerificationEmail({user, email: rawEmail});

			const registrationMetadata = await this.buildRegistrationMetadataContext({
				user,
				clientIp,
				request,
				geoipResult,
			});

			await this.repository.createAuthorizedIp(userId, clientIp);

			await this.maybeAutoJoinInvite({
				userId,
				inviteCode: data.invite_code || Config.instance.autoJoinInviteCode,
				requestCache,
			});

			const [token] = await this.createAuthSession({user, request});

			this.sendRegistrationWebhook(user, registrationMetadata, instanceConfig.registrationAlertsWebhookUrl)
				.catch((error) => {
					Logger.warn(
						{error, userId: user.id.toString()},
						'[AuthRegistrationService] Failed to send registration webhook',
					);
				})
				.catch((error) => {
					Logger.error({error}, '[AuthRegistrationService] Failed to log webhook error');
				});

			return {
				user_id: user.id.toString(),
				token,
			};
		} catch (error) {
			if (rawEmail) {
				try {
					await deleteOneOrMany(
						UserByEmail.deleteByPk({
							email_lower: rawEmail.toLowerCase(),
							user_id: userId,
						}),
					);
				} catch (cleanupError) {
					Logger.error(
						{email: rawEmail, userId: userId.toString(), error: cleanupError},
						'Failed to clean up email reservation after registration failure',
					);
				}
			}
			throw error;
		}
	}

	private async maybeIndexUser(user: User): Promise<void> {
		const userSearchService = getUserSearchService();
		if (!userSearchService) return;

		if ('indexUser' in userSearchService) {
			try {
				await userSearchService.indexUser(user);
			} catch (error) {
				Logger.error({userId: user.id, error}, 'Failed to index user in search');
			}
		}
	}

	private async maybeSendVerificationEmail(params: {user: User; email: string}): Promise<void> {
		const {user, email} = params;
		const token = createEmailVerificationToken(await this.generateSecureToken());

		await this.repository.createEmailVerificationToken({
			token_: token,
			user_id: user.id,
			email,
		});

		await this.emailService.sendEmailVerification(email, user.username, token, user.locale);
	}

	private async maybeAutoJoinInvite(params: {
		userId: UserID;
		inviteCode: string | null | undefined;
		requestCache: RequestCache;
	}): Promise<void> {
		const {userId, inviteCode, requestCache} = params;
		const normalizedInviteCode = inviteCode?.trim();
		if (!normalizedInviteCode) return;

		if (!this.inviteService) return;

		try {
			await this.inviteService.acceptInvite({
				userId,
				inviteCode: createInviteCode(normalizedInviteCode),
				requestCache,
			});
		} catch (error) {
			Logger.warn({inviteCode: normalizedInviteCode, error}, 'Failed to auto-join invite on registration');
		}
	}

	private async enforceRegistrationRateLimits(params: {
		enforceRateLimits: boolean;
		clientIp: string;
		emailKey: string | null;
	}): Promise<void> {
		const {enforceRateLimits, clientIp, emailKey} = params;
		if (!enforceRateLimits) return;

		if (emailKey) {
			const emailRateLimit = await this.rateLimitService.checkLimit({
				identifier: `registration:email:${emailKey}`,
				maxAttempts: 3,
				windowMs: ms('15 minutes'),
			});

			if (!emailRateLimit.allowed) throwRegistrationRateLimit(emailRateLimit);
		}

		const ipRateLimit = await this.rateLimitService.checkLimit({
			identifier: `registration:ip:${clientIp}`,
			maxAttempts: 5,
			windowMs: ms('30 minutes'),
		});

		if (!ipRateLimit.allowed) throwRegistrationRateLimit(ipRateLimit);
	}

	private async allocateDiscriminator(username: string): Promise<number> {
		const result = await this.discriminatorService.generateDiscriminator({username});
		if (!result.available || result.discriminator === -1) {
			throw InputValidationError.create('username', 'Too many users with this username');
		}
		return result.discriminator;
	}

	private async generateUserId(emailKey: string | null): Promise<UserID> {
		if (emailKey) {
			const reserved = this.snowflakeReservationService.getReservedSnowflake(emailKey);
			if (reserved) {
				return createUserID(reserved);
			}
		}
		return createUserID(await this.snowflakeService.generate());
	}

	private truncateUserAgent(userAgent: string): string {
		if (userAgent.length <= USER_AGENT_TRUNCATE_LENGTH) return userAgent;
		return `${userAgent.slice(0, USER_AGENT_TRUNCATE_LENGTH)}...`;
	}

	private parseUserAgentSafe(userAgent: string): {osInfo: string; browserInfo: string; deviceInfo: string} {
		try {
			const result = Bowser.parse(userAgent);
			return {
				osInfo: this.formatOsInfo(result.os) ?? 'Unknown',
				browserInfo: this.formatNameVersion(result.browser?.name, result.browser?.version) ?? 'Unknown',
				deviceInfo: this.formatDeviceInfo(result.platform),
			};
		} catch (error) {
			Logger.warn({error}, 'Failed to parse user agent with Bowser');
			return {osInfo: 'Unknown', browserInfo: 'Unknown', deviceInfo: 'Desktop/Unknown'};
		}
	}

	private formatNameVersion(name?: string, version?: string): string | null {
		if (!name) return null;
		return version ? `${name} ${version}` : name;
	}

	private formatOsInfo(os?: {name?: string; version?: string; versionName?: string}): string | null {
		if (!os?.name) return null;
		if (os.versionName && os.version) return `${os.name} ${os.versionName} (${os.version})`;
		if (os.versionName) return `${os.name} ${os.versionName}`;
		if (os.version) return `${os.name} ${os.version}`;
		return os.name;
	}

	private formatDeviceInfo(platform?: {type?: string; vendor?: string; model?: string}): string {
		const type = this.formatPlatformType(platform?.type);
		const vendorModel = [platform?.vendor, platform?.model].filter(Boolean).join(' ').trim();

		if (vendorModel && type) return `${vendorModel} (${type})`;
		if (vendorModel) return vendorModel;
		if (type) return type;
		return 'Desktop/Unknown';
	}

	private formatPlatformType(type?: string): string | null {
		switch ((type ?? '').toLowerCase()) {
			case 'mobile':
				return 'Mobile';
			case 'tablet':
				return 'Tablet';
			case 'desktop':
				return 'Desktop';
			default:
				return null;
		}
	}

	private async buildRegistrationMetadataContext(params: {
		user: User;
		clientIp: string;
		request: Request;
		geoipResult: GeoipResult;
	}): Promise<RegistrationMetadataContext> {
		const {user, clientIp, request, geoipResult} = params;

		const userAgentHeader = (request.headers.get('user-agent') ?? '').trim();
		const fluxerTag = `${user.username}#${user.discriminator.toString().padStart(4, '0')}`;
		const displayName = user.globalName || user.username;
		const emailDisplay = user.email || 'Not provided';

		const hasUserAgent = userAgentHeader.length > 0;
		const userAgentForDisplay = hasUserAgent ? userAgentHeader : 'Not provided';
		const truncatedUserAgent = this.truncateUserAgent(userAgentForDisplay);

		const uaInfo = hasUserAgent
			? this.parseUserAgentSafe(userAgentHeader)
			: {osInfo: 'Unknown', browserInfo: 'Unknown', deviceInfo: 'Desktop/Unknown'};

		const normalizedIp = geoipResult.normalizedIp ?? clientIp;
		const locationLabel = formatGeoipLocation(geoipResult) ?? UNKNOWN_LOCATION;
		const safeCountryCode = geoipResult.countryCode ?? 'unknown';
		const ipAddressReverse = await getIpAddressReverse(normalizedIp, this.cacheService);

		const metadataEntries: Array<[string, string]> = [
			['fluxer_tag', fluxerTag],
			['display_name', displayName],
			['email', emailDisplay],
			['ip_address', clientIp],
			['normalized_ip', normalizedIp],
			['country_code', safeCountryCode],
			['location', locationLabel],
			['os', uaInfo.osInfo],
			['browser', uaInfo.browserInfo],
			['device', uaInfo.deviceInfo],
			['user_agent', truncatedUserAgent],
		];

		if (geoipResult.city) metadataEntries.push(['city', geoipResult.city]);
		if (geoipResult.region) metadataEntries.push(['region', geoipResult.region]);
		if (geoipResult.countryName) metadataEntries.push(['country_name', geoipResult.countryName]);
		if (ipAddressReverse) metadataEntries.push(['ip_address_reverse', ipAddressReverse]);

		return {
			metadata: new Map(metadataEntries),
			clientIp,
			countryCode: safeCountryCode,
			location: locationLabel,
			city: geoipResult.city,
			region: geoipResult.region,
			osInfo: uaInfo.osInfo,
			browserInfo: uaInfo.browserInfo,
			deviceInfo: uaInfo.deviceInfo,
			truncatedUserAgent,
			fluxerTag,
			displayName,
			email: emailDisplay,
			ipAddressReverse,
		};
	}

	private async reserveFirstAdminAcl(): Promise<boolean> {
		const now = new Date();
		const {applied} = await executeConditional(
			InstanceConfiguration.insertIfNotExists({
				key: FIRST_ADMIN_ACL_CONFIG_KEY,
				value: 'true',
				updated_at: now,
			}),
		);
		return applied;
	}

	private async sendRegistrationWebhook(
		user: User,
		context: RegistrationMetadataContext,
		webhookUrl: string | null,
	): Promise<void> {
		if (!webhookUrl) {
			if (!this.hasWarnedAboutMissingWebhook) {
				Logger.warn(
					'registrationAlertsWebhookUrl is not configured – registration alerts will be disabled until configured',
				);
				this.hasWarnedAboutMissingWebhook = true;
			}
			return;
		}

		const locationDisplay = context.city ? context.location : context.countryCode;

		const embedFields = [
			{name: 'User ID', value: user.id.toString(), inline: true},
			{name: 'FluxerTag', value: context.fluxerTag, inline: true},
			{name: 'Display Name', value: context.displayName, inline: true},
			{name: 'Email', value: context.email, inline: true},
			{name: 'IP Address', value: `\`${context.clientIp}\``, inline: true},
			...(context.ipAddressReverse ? [{name: 'Reverse DNS', value: context.ipAddressReverse, inline: true}] : []),
			{name: 'Location', value: locationDisplay, inline: true},
			{name: 'OS', value: context.osInfo, inline: true},
			{name: 'Browser', value: context.browserInfo, inline: true},
			{name: 'Device', value: context.deviceInfo, inline: true},
			{name: 'User Agent', value: context.truncatedUserAgent, inline: false},
		];

		const payload = {
			username: 'Registration Monitor',
			embeds: [
				{
					title: 'New Account Registered',
					color: 0x10b981,
					fields: embedFields,
					timestamp: new Date().toISOString(),
				},
			],
		};

		try {
			const response = await FetchUtils.sendRequest({
				url: webhookUrl,
				method: 'POST',
				headers: {'Content-Type': 'application/json'},
				body: JSON.stringify(payload),
				timeout: ms('10 seconds'),
				serviceName: 'registration_webhook',
			});
			if (response.status < 200 || response.status >= 300) {
				const body = await FetchUtils.streamToString(response.stream);
				Logger.warn({status: response.status, body}, 'Failed to send registration webhook');
			}
		} catch (error) {
			Logger.warn({error}, 'Failed to send registration webhook');
		}
	}
}
