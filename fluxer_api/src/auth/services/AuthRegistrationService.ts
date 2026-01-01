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

import crypto from 'node:crypto';
import {types} from 'cassandra-driver';
import {UAParser} from 'ua-parser-js';
import type {RegisterRequest} from '~/auth/AuthModel';
import {createEmailVerificationToken, createInviteCode, createUserID, type UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {APIErrorCodes, UserFlags} from '~/Constants';
import {FluxerAPIError, InputValidationError} from '~/Errors';
import type {IDiscriminatorService} from '~/infrastructure/DiscriminatorService';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import {getMetricsService} from '~/infrastructure/MetricsService';
import type {PendingJoinInviteStore} from '~/infrastructure/PendingJoinInviteStore';
import type {RedisActivityTracker} from '~/infrastructure/RedisActivityTracker';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import {InstanceConfigRepository} from '~/instance/InstanceConfigRepository';
import type {InviteService} from '~/invite/InviteService';
import {Logger} from '~/Logger';
import {getUserSearchService} from '~/Meilisearch';
import type {AuthSession, User} from '~/Models';
import {UserSettings} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import * as AgeUtils from '~/utils/AgeUtils';
import * as IpUtils from '~/utils/IpUtils';
import {parseAcceptLanguage} from '~/utils/LocaleUtils';
import {generateRandomUsername} from '~/utils/UsernameGenerator';

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
	if (age === null || age < 0) {
		return 'unknown';
	}

	for (const bucket of AGE_BUCKETS) {
		if (age >= bucket.min && age <= bucket.max) {
			return bucket.label;
		}
	}

	return '65+';
}

interface RegisterParams {
	data: RegisterRequest;
	request: Request;
	requestCache: RequestCache;
}

export class AuthRegistrationService {
	constructor(
		private repository: IUserRepository,
		private inviteService: InviteService,
		private rateLimitService: IRateLimitService,
		private emailService: IEmailService,
		private snowflakeService: SnowflakeService,
		private discriminatorService: IDiscriminatorService,
		private redisActivityTracker: RedisActivityTracker,
		private pendingJoinInviteStore: PendingJoinInviteStore,
		private cacheService: ICacheService,
		private hashPassword: (password: string) => Promise<string>,
		private isPasswordPwned: (password: string) => Promise<boolean>,
		private validateAge: (params: {dateOfBirth: string; minAge: number}) => boolean,
		private generateSecureToken: () => Promise<string>,
		private createAuthSession: (params: {user: User; request: Request}) => Promise<[string, AuthSession]>,
	) {}

	async register({
		data,
		request,
		requestCache,
	}: RegisterParams): Promise<{user_id: string; token: string; pending_verification?: boolean}> {
		if (!data.consent) {
			throw InputValidationError.create('consent', 'You must agree to the Terms of Service and Privacy Policy');
		}

		const countryCode = await IpUtils.getCountryCodeFromReq(request);
		const clientIp = IpUtils.requireClientIp(request);
		const countryResultDetailed = await IpUtils.getCountryCodeDetailed(clientIp);
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

		const enforceRateLimits = !Config.dev.relaxRegistrationRateLimits;

		if (enforceRateLimits && data.email) {
			const emailRateLimit = await this.rateLimitService.checkLimit({
				identifier: `registration:email:${data.email}`,
				maxAttempts: 3,
				windowMs: 15 * 60 * 1000,
			});

			if (!emailRateLimit.allowed) {
				throw new FluxerAPIError({
					code: APIErrorCodes.RATE_LIMITED,
					message: 'Too many registration attempts. Please try again later.',
					status: 429,
				});
			}
		}

		if (enforceRateLimits) {
			const ipRateLimit = await this.rateLimitService.checkLimit({
				identifier: `registration:ip:${clientIp}`,
				maxAttempts: 5,
				windowMs: 30 * 60 * 1000,
			});

			if (!ipRateLimit.allowed) {
				throw new FluxerAPIError({
					code: APIErrorCodes.RATE_LIMITED,
					message: 'Too many registration attempts from this IP. Please try again later.',
					status: 429,
				});
			}
		}

		let betaCode = null;
		let hasValidBetaCode = false;
		if (data.beta_code) {
			if (Config.nodeEnv === 'development' && data.beta_code === 'NOVERIFY') {
				hasValidBetaCode = false;
			} else {
				betaCode = await this.repository.getBetaCode(data.beta_code);
				if (betaCode && !betaCode.redeemerId) {
					hasValidBetaCode = true;
				}
			}
		}

		const rawEmail = data.email?.trim() || null;
		const normalizedEmail = rawEmail?.toLowerCase() || null;

		if (normalizedEmail) {
			const emailTaken = await this.repository.findByEmail(normalizedEmail);
			if (emailTaken) {
				throw InputValidationError.create('email', 'Email already in use');
			}
		}

		const username = data.username || generateRandomUsername();

		const discriminatorResult = await this.discriminatorService.generateDiscriminator({
			username,
			isPremium: false,
		});

		if (!discriminatorResult.available || discriminatorResult.discriminator === -1) {
			throw InputValidationError.create('username', 'Too many users with this username');
		}

		const discriminator = discriminatorResult.discriminator;

		let userId: UserID;
		if (normalizedEmail && process.env.EARLY_TESTER_EMAIL_HASH_TO_SNOWFLAKE) {
			const mapping = JSON.parse(process.env.EARLY_TESTER_EMAIL_HASH_TO_SNOWFLAKE) as Record<string, string>;
			const emailHash = crypto.createHash('sha256').update(normalizedEmail).digest('hex');
			const mappedUserId = mapping[emailHash];
			userId = mappedUserId ? createUserID(BigInt(mappedUserId)) : createUserID(this.snowflakeService.generate());
		} else {
			userId = createUserID(this.snowflakeService.generate());
		}

		const acceptLanguage = request.headers.get('accept-language');
		const userLocale = parseAcceptLanguage(acceptLanguage);
		const passwordHash = data.password ? await this.hashPassword(data.password) : null;

		const instanceConfigRepository = new InstanceConfigRepository();
		const instanceConfig = await instanceConfigRepository.getInstanceConfig();
		const isManualReviewActive = instanceConfigRepository.isManualReviewActiveNow(instanceConfig);

		const shouldRequireVerification =
			(isManualReviewActive && Config.nodeEnv === 'production') ||
			(Config.nodeEnv === 'development' && data.beta_code === 'NOVERIFY');
		const isPendingVerification = shouldRequireVerification && !hasValidBetaCode;

		let baseFlags = Config.nodeEnv === 'development' ? UserFlags.STAFF : 0n;
		if (isPendingVerification) {
			baseFlags |= UserFlags.PENDING_MANUAL_VERIFICATION;
		}

		const now = new Date();
		const user = await this.repository.create({
			user_id: userId,
			username,
			discriminator: discriminator,
			global_name: data.global_name || null,
			bot: false,
			system: false,
			email: rawEmail,
			email_verified: false,
			email_bounced: false,
			phone: null,
			password_hash: passwordHash,
			password_last_changed_at: passwordHash ? new Date() : null,
			totp_secret: null,
			authenticator_types: new Set(),
			avatar_hash: null,
			avatar_color: null,
			banner_hash: null,
			banner_color: null,
			bio: null,
			pronouns: null,
			accent_color: null,
			date_of_birth: types.LocalDate.fromString(data.date_of_birth),
			locale: userLocale,
			flags: baseFlags,
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
			terms_agreed_at: new Date(),
			privacy_agreed_at: new Date(),
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
			first_refund_at: null,
			beta_code_allowance: 0,
			beta_code_last_reset_at: null,
			gift_inventory_server_seq: null,
			gift_inventory_client_seq: null,
			premium_onboarding_dismissed_at: null,
			version: 1,
		});

		await this.redisActivityTracker.updateActivity(user.id, now);

		getMetricsService().counter({
			name: 'user.registration',
			dimensions: {
				country: countryCode ?? 'unknown',
				state: countryResultDetailed.region ?? 'unknown',
				ip_version: clientIp.includes(':') ? 'v6' : 'v4',
			},
		});

		const age = data.date_of_birth ? AgeUtils.calculateAge(data.date_of_birth) : null;
		getMetricsService().counter({
			name: 'user.age',
			dimensions: {
				country: countryCode ?? 'unknown',
				state: countryResultDetailed.region ?? 'unknown',
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

		const userSearchService = getUserSearchService();
		if (userSearchService) {
			await userSearchService.indexUser(user).catch((error) => {
				Logger.error({userId: user.id, error}, 'Failed to index user in search');
			});
		}

		if (rawEmail) {
			const emailVerifyToken = createEmailVerificationToken(await this.generateSecureToken());
			await this.repository.createEmailVerificationToken({
				token_: emailVerifyToken,
				user_id: userId,
				email: rawEmail,
			});

			await this.emailService.sendEmailVerification(rawEmail, user.username, emailVerifyToken, user.locale);
		}

		if (betaCode) {
			await this.repository.updateBetaCodeRedeemed(betaCode.code, userId, new Date());
		}

		const registrationMetadata = await this.buildRegistrationMetadataContext(user, clientIp, request);

		if (isPendingVerification) {
			await this.repository.createPendingVerification(userId, new Date(), registrationMetadata.metadata);
		}

		await this.repository.createAuthorizedIp(userId, clientIp);

		const inviteCodeToJoin = data.invite_code || Config.instance.autoJoinInviteCode;
		if (inviteCodeToJoin != null) {
			if (isPendingVerification) {
				await this.pendingJoinInviteStore.setPendingInvite(userId, inviteCodeToJoin);
			} else if (this.inviteService) {
				try {
					await this.inviteService.acceptInvite({
						userId,
						inviteCode: createInviteCode(inviteCodeToJoin),
						requestCache,
					});
				} catch (error) {
					Logger.warn({inviteCode: inviteCodeToJoin, error}, 'Failed to auto-join invite on registration');
				}
			}
		}

		const [token] = await this.createAuthSession({user, request});

		this.sendRegistrationWebhook(user, registrationMetadata, instanceConfig.registrationAlertsWebhookUrl).catch(
			(error) => {
				Logger.warn({error, userId: user.id.toString()}, 'Failed to send registration webhook');
			},
		);

		return {
			user_id: user.id.toString(),
			token,
			pending_verification: isPendingVerification ? true : undefined,
		};
	}

	private async buildRegistrationMetadataContext(
		user: User,
		clientIp: string,
		request: Request,
	): Promise<RegistrationMetadataContext> {
		const countryResult = await IpUtils.getCountryCodeDetailed(clientIp);
		const userAgentHeader = request.headers.get('user-agent') ?? '';
		const trimmedUserAgent = userAgentHeader.trim();
		const parsedUserAgent = new UAParser(trimmedUserAgent).getResult();

		const fluxerTag = `${user.username}#${user.discriminator.toString().padStart(4, '0')}`;
		const displayName = user.globalName || user.username;
		const emailDisplay = user.email || 'Not provided';
		const normalizedUserAgent = trimmedUserAgent.length > 0 ? trimmedUserAgent : 'Not provided';
		const truncatedUserAgent = this.truncateUserAgent(normalizedUserAgent);
		const normalizedIp = countryResult.normalizedIp ?? clientIp;
		const geoipReason = countryResult.reason ?? 'none';

		const osInfo = parsedUserAgent.os.name
			? `${parsedUserAgent.os.name}${parsedUserAgent.os.version ? ` ${parsedUserAgent.os.version}` : ''}`
			: 'Unknown';
		const browserInfo = parsedUserAgent.browser.name
			? `${parsedUserAgent.browser.name}${parsedUserAgent.browser.version ? ` ${parsedUserAgent.browser.version}` : ''}`
			: 'Unknown';
		const deviceInfo = parsedUserAgent.device.vendor
			? `${parsedUserAgent.device.vendor} ${parsedUserAgent.device.model || ''}`.trim()
			: 'Desktop/Unknown';

		const ipAddressReverse = await IpUtils.getIpAddressReverse(normalizedIp, this.cacheService);
		const locationLabel = IpUtils.formatGeoipLocation(countryResult);

		const metadataEntries: Array<[string, string]> = [
			['fluxer_tag', fluxerTag],
			['display_name', displayName],
			['email', emailDisplay],
			['ip_address', clientIp],
			['normalized_ip', normalizedIp],
			['country_code', countryResult.countryCode],
			['location', locationLabel],
			['geoip_reason', geoipReason],
			['os', osInfo],
			['browser', browserInfo],
			['device', deviceInfo],
			['user_agent', truncatedUserAgent],
		];

		if (countryResult.city) {
			metadataEntries.push(['city', countryResult.city]);
		}
		if (countryResult.region) {
			metadataEntries.push(['region', countryResult.region]);
		}
		if (countryResult.countryName) {
			metadataEntries.push(['country_name', countryResult.countryName]);
		}
		if (ipAddressReverse) {
			metadataEntries.push(['ip_address_reverse', ipAddressReverse]);
		}

		return {
			metadata: new Map(metadataEntries),
			clientIp,
			countryCode: countryResult.countryCode,
			location: locationLabel,
			city: countryResult.city,
			region: countryResult.region,
			osInfo,
			browserInfo,
			deviceInfo,
			truncatedUserAgent,
			fluxerTag,
			displayName,
			email: emailDisplay,
			ipAddressReverse,
		};
	}

	private truncateUserAgent(userAgent: string): string {
		if (userAgent.length <= USER_AGENT_TRUNCATE_LENGTH) {
			return userAgent;
		}

		return `${userAgent.slice(0, USER_AGENT_TRUNCATE_LENGTH)}...`;
	}

	private async sendRegistrationWebhook(
		user: User,
		context: RegistrationMetadataContext,
		webhookUrl: string | null,
	): Promise<void> {
		if (!webhookUrl) return;

		const {
			clientIp,
			countryCode,
			location,
			city,
			osInfo,
			browserInfo,
			deviceInfo,
			truncatedUserAgent,
			fluxerTag,
			displayName,
			email,
			ipAddressReverse,
		} = context;

		const locationDisplay = city ? location : countryCode;

		const embedFields = [
			{name: 'User ID', value: user.id.toString(), inline: true},
			{name: 'FluxerTag', value: fluxerTag, inline: true},
			{name: 'Display Name', value: displayName, inline: true},
			{name: 'Email', value: email, inline: true},
			{name: 'IP Address', value: clientIp, inline: true},
			{name: 'Location', value: locationDisplay, inline: true},
			{name: 'OS', value: osInfo, inline: true},
			{name: 'Browser', value: browserInfo, inline: true},
			{name: 'Device', value: deviceInfo, inline: true},
			{name: 'User Agent', value: truncatedUserAgent, inline: false},
		];

		if (ipAddressReverse) {
			embedFields.splice(6, 0, {name: 'Reverse DNS', value: ipAddressReverse, inline: true});
		}

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
			const response = await fetch(webhookUrl, {
				method: 'POST',
				headers: {'Content-Type': 'application/json'},
				body: JSON.stringify(payload),
			});

			if (!response.ok) {
				const body = await response.text();
				Logger.warn({status: response.status, body}, 'Failed to send registration webhook');
			}
		} catch (error) {
			Logger.warn({error}, 'Failed to send registration webhook');
		}
	}
}
