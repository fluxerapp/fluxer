// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {PHONE_ADD_CLEARABLE_FLAGS, UserFlags} from '@fluxer/constants/src/UserConstants';
import {BotUserAuthEndpointAccessDeniedError} from '@fluxer/errors/src/domains/auth/BotUserAuthEndpointAccessDeniedError';
import {InvalidPhoneNumberError} from '@fluxer/errors/src/domains/auth/InvalidPhoneNumberError';
import {InvalidPhoneVerificationCodeError} from '@fluxer/errors/src/domains/auth/InvalidPhoneVerificationCodeError';
import {PhoneAlreadyUsedError} from '@fluxer/errors/src/domains/auth/PhoneAlreadyUsedError';
import {PhoneCountryNotSupportedError} from '@fluxer/errors/src/domains/auth/PhoneCountryNotSupportedError';
import {PhoneInboundVerificationRequiredError} from '@fluxer/errors/src/domains/auth/PhoneInboundVerificationRequiredError';
import {PhoneLookupUnavailableError} from '@fluxer/errors/src/domains/auth/PhoneLookupUnavailableError';
import {PhoneNumberNotInServiceError} from '@fluxer/errors/src/domains/auth/PhoneNumberNotInServiceError';
import {PhoneNumberNotMobileError} from '@fluxer/errors/src/domains/auth/PhoneNumberNotMobileError';
import {PhoneVerificationNeedsReviewError} from '@fluxer/errors/src/domains/auth/PhoneVerificationNeedsReviewError';
import {PhoneVerificationRequiredError} from '@fluxer/errors/src/domains/auth/PhoneVerificationRequiredError';
import {SmsVerificationUnavailableError} from '@fluxer/errors/src/domains/auth/SmsVerificationUnavailableError';
import {CaptchaVerificationRequiredError} from '@fluxer/errors/src/domains/core/CaptchaVerificationRequiredError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {FluxerError} from '@fluxer/errors/src/FluxerError';
import {PHONE_E164_REGEX} from '@fluxer/schema/src/primitives/UserValidators';
import type {RateLimitResult, RateLimitScope} from '@pkgs/rate_limit/src/IRateLimitService';
import type {PhoneLookupResult} from '@pkgs/sms/src/PhoneLookupTypes';
import {
	ACCEPTED_PHONE_LINE_TYPES,
	getSmsPumpingRiskThreshold,
	HARD_REJECT_PHONE_LINE_TYPES,
	VOIP_PHONE_LINE_TYPES,
} from '@pkgs/sms/src/PhoneLookupTypes';
import {SmsVerificationStartError, TwilioVerificationRateLimitError} from '@pkgs/sms/src/providers/TwilioSmsProvider';
import type {SmsVerificationStartOptions} from '@pkgs/sms/src/SmsVerificationTypes';
import type {ApiContext} from '../ApiContext';
import type {UserID} from '../BrandedTypes';
import {Logger} from '../Logger';
import type {User} from '../models/User';
import {getUserSearchService} from '../SearchFactory';
import {mapUserToPartialResponse, mapUserToPrivateResponse} from '../user/UserMappers';
import {phonePrefixBanCache} from './PhonePrefixBanCache';
import {requiresInboundPhoneVerification} from './PhoneVerificationPrefixPolicy';
import {PhoneVerificationReuseStore} from './PhoneVerificationReuseStore';
import type {IssuedChallenge} from './services/InboundSmsChallengeService';
import type {PhoneAttemptInboundReason, PhoneAttemptRejectReason} from './services/PhoneLookupRepository';

const MINUTE_MS = 60_000;
const HOUR_MS = 60 * MINUTE_MS;
const DAY_MS = 24 * HOUR_MS;
const ACCOUNT_SMS_SEND_LIMIT = {maxAttempts: 3, windowMs: 6 * HOUR_MS};
const PHONE_SMS_SEND_LIMIT = {maxAttempts: 3, windowMs: 5 * DAY_MS};
const TWILIO_PHONE_PREFIX_RATE_LIMIT_LENGTH = 6;

type PhoneVerificationStartResult =
	| {channel: 'sms'}
	| {
			channel: 'inbound_challenge';
			challengeCode: string;
			ourNumber: string;
			expiresAt: Date;
			reason: PhoneAttemptInboundReason;
	  };

interface SendPhoneVerificationOptions {
	clientIp?: string;
	channel?: 'sms' | 'inbound_challenge';
}

function assertNonBotUser(user: User): void {
	if (user.isBot) {
		throw new BotUserAuthEndpointAccessDeniedError();
	}
}

function reuseStoreFor(ctx: ApiContext): PhoneVerificationReuseStore {
	return new PhoneVerificationReuseStore(ctx.services.cache);
}

export async function startInboundPhoneChallenge(ctx: ApiContext, userId: UserID): Promise<IssuedChallenge> {
	const {inboundSmsChallenge, users, config} = ctx.services;
	const ourNumber = config.sms.inboundChallengeNumber;
	if (!inboundSmsChallenge || !ourNumber) {
		Logger.warn(
			{userId: String(userId)},
			'Inbound SMS challenge requested but FLUXER_SMS_INBOUND_CHALLENGE_NUMBER is unset',
		);
		throw new SmsVerificationUnavailableError();
	}
	const user = await users.findUnique(userId);
	if (!user) throw new UnknownUserError();
	assertNonBotUser(user);
	return inboundSmsChallenge.issueChallenge({userId, ourNumber});
}

class PhoneInboundChallengeRequiredError extends Error {
	constructor(readonly reason: PhoneAttemptInboundReason) {
		super(`Phone verification must be completed via inbound challenge: ${reason}`);
		this.name = 'PhoneInboundChallengeRequiredError';
	}
}

interface ValidatePhoneOptions {
	inboundCapable?: boolean;
}

async function validatePhoneOrThrow(ctx: ApiContext, phone: string, options: ValidatePhoneOptions = {}): Promise<void> {
	assertPhoneFormatOrThrow(phone);
	const {phoneLookup, sms} = ctx.services;
	const cached = (await phoneLookup?.getCachedLookup(phone)) ?? null;
	const lookup = cached ?? (await sms.lookupPhone(phone));
	const cacheHit = cached !== null;
	if (!cacheHit && lookup) {
		await phoneLookup?.setCachedLookup(phone, lookup);
	}
	const verdict = computePhoneVerdict(lookup, phone);
	await phoneLookup?.recordAttempt({
		phone,
		lookup,
		verdict: verdict.verdict,
		rejectReason: verdict.verdict === 'reject' ? verdict.rejectReason : null,
		inboundReason: verdict.verdict === 'require_inbound' ? verdict.inboundReason : null,
		lookupCacheHit: cacheHit,
	});
	if (verdict.verdict === 'reject') {
		if (verdict.rejectReason === 'lookup_unavailable') {
			Logger.error({phone}, 'Phone lookup unavailable — rejecting attachment (fail-closed)');
		} else {
			Logger.info(
				{
					phone,
					rejectReason: verdict.rejectReason,
					lineType: lookup?.lineType ?? null,
					carrier: lookup?.carrierName ?? null,
					country: lookup?.countryCode ?? null,
					smsPumpingRiskScore: lookup?.smsPumpingRiskScore ?? null,
					lookupCacheHit: cacheHit,
				},
				'Phone verification rejected at gate',
			);
		}
		throw errorForPhoneRejectReason(verdict.rejectReason);
	}
	if (verdict.verdict === 'require_inbound') {
		Logger.info(
			{
				phone,
				inboundReason: verdict.inboundReason,
				lineType: lookup?.lineType ?? null,
				country: lookup?.countryCode ?? null,
			},
			'Phone verification routed to inbound challenge',
		);
		if (options.inboundCapable) {
			throw new PhoneInboundChallengeRequiredError(verdict.inboundReason);
		}
		throw new PhoneInboundVerificationRequiredError();
	}
}

function assertPhoneFormatOrThrow(phone: string): void {
	if (!PHONE_E164_REGEX.test(phone)) {
		throw new InvalidPhoneNumberError();
	}
	if (phonePrefixBanCache.isBlocked(phone)) {
		throw new PhoneCountryNotSupportedError();
	}
}

async function issueInboundChallengeOrThrow(
	ctx: ApiContext,
	userId: UserID,
	phone: string,
	reason: PhoneAttemptInboundReason,
): Promise<PhoneVerificationStartResult> {
	try {
		const challenge = await startInboundPhoneChallenge(ctx, userId);
		return {
			channel: 'inbound_challenge',
			challengeCode: challenge.challengeCode,
			ourNumber: challenge.ourNumber,
			expiresAt: challenge.expiresAt,
			reason,
		};
	} catch (error) {
		Logger.error({phone, userId: String(userId), reason, error}, 'Inbound phone challenge required but unavailable');
		throw new SmsVerificationUnavailableError();
	}
}

export async function sendPhoneVerificationCode(
	ctx: ApiContext,
	phone: string,
	userId: UserID | null,
	options: SendPhoneVerificationOptions = {},
): Promise<PhoneVerificationStartResult> {
	assertPhoneFormatOrThrow(phone);
	const {users, sms} = ctx.services;
	let requestingUser: User | null = null;
	if (userId) {
		requestingUser = await users.findUnique(userId);
		if (requestingUser) {
			assertNonBotUser(requestingUser);
		}
	}
	const accountForcedInbound =
		requestingUser !== null && (requestingUser.flags & UserFlags.FORCE_INBOUND_PHONE_VERIFICATION) !== 0n;
	const prefixForcedInbound =
		userId !== null && requestingUser !== null && shouldRequireInboundPhoneChallenge(phone, requestingUser);
	const requireInbound = options.channel === 'inbound_challenge' || accountForcedInbound || prefixForcedInbound;
	if (requireInbound && userId) {
		const inboundReason: PhoneAttemptInboundReason = accountForcedInbound ? 'account_forced' : 'expensive_destination';
		return await issueInboundChallengeOrThrow(ctx, userId, phone, inboundReason);
	}
	const riskInput = {
		userId: userId ? userId.toString() : null,
		clientIp: options.clientIp ?? null,
		phone,
	};
	const preRisk = await ctx.services.phoneAttemptRisk.evaluate(riskInput);
	if (preRisk.decision === 'hard_block') {
		Logger.warn({...riskInput, reason: preRisk.reason}, 'phone_attempt_risk hard_block at send');
		throw createPhoneRateLimitError(
			{
				retryAfter: 24 * 60 * 60,
				retryAfterDecimal: 24 * 60 * 60,
				limit: 1,
				resetTime: new Date(Date.now() + 24 * 60 * 60 * 1000),
				resetAfterDecimal: 24 * 60 * 60,
			},
			'user',
		);
	}
	if (preRisk.decision === 'require_inbound' && userId) {
		return await issueInboundChallengeOrThrow(ctx, userId, phone, 'behavioural_risk');
	}
	if (preRisk.decision === 'require_captcha') {
		throw new CaptchaVerificationRequiredError();
	}
	await throwIfProviderCooldownActive(ctx, phone, userId);
	let rejectedAttempt = false;
	let lookupCountry: string | null = null;
	try {
		try {
			await validatePhoneOrThrow(ctx, phone, {inboundCapable: userId !== null});
		} catch (error) {
			if (error instanceof PhoneInboundChallengeRequiredError && userId !== null) {
				await ctx.services.phoneAttemptRisk.record({...riskInput, rejected: false});
				return await issueInboundChallengeOrThrow(ctx, userId, phone, error.reason);
			}
			rejectedAttempt = !(error instanceof PhoneLookupUnavailableError);
			throw error;
		}
		const cached = await ctx.services.phoneLookup?.getCachedLookup(phone);
		lookupCountry = cached?.countryCode ?? null;
		if (await reuseStoreFor(ctx).hasReachedVerificationLimit(phone)) {
			rejectedAttempt = true;
			throw new PhoneAlreadyUsedError();
		}
		await enforceSmsSendProtections(ctx, phone, requestingUser);
		try {
			const startOptions = buildVerificationStartOptions(phone, userId, options);
			await sms.startVerificationWithResult(phone, startOptions);
			return {channel: 'sms'};
		} catch (error) {
			if (error instanceof TwilioVerificationRateLimitError) {
				rejectedAttempt = true;
				await recordProviderCooldown(ctx, phone, userId, error);
				throw error;
			}
			if (error instanceof SmsVerificationStartError) {
				rejectedAttempt = true;
				Logger.warn(
					{phone, userId: userId ? String(userId) : null, context: error.sentryContext},
					'Phone verification send unavailable',
				);
				throw new SmsVerificationUnavailableError();
			}
			rejectedAttempt = true;
			throw error;
		}
	} finally {
		try {
			await ctx.services.phoneAttemptRisk.record({
				...riskInput,
				countryCode: lookupCountry,
				rejected: rejectedAttempt,
			});
		} catch (error) {
			Logger.warn({error}, 'phone_attempt_risk record failed (non-fatal)');
		}
	}
}

function buildVerificationStartOptions(
	phone: string,
	userId: UserID | null,
	options: SendPhoneVerificationOptions,
): SmsVerificationStartOptions {
	const rateLimits: Record<string, string> = {
		fluxer_phone_prefix: getRateLimitPhonePrefix(phone),
	};
	if (userId) {
		rateLimits.fluxer_user_id = userId.toString();
	}
	if (options.clientIp) {
		rateLimits.fluxer_client_ip = options.clientIp;
	}
	return {
		rateLimits,
	};
}

function shouldRequireInboundPhoneChallenge(phone: string, user: User): boolean {
	if (user.hasVerifiedPhone) return false;
	return requiresInboundPhoneVerification(phone);
}

function getRateLimitPhonePrefix(phone: string): string {
	return phone.slice(0, TWILIO_PHONE_PREFIX_RATE_LIMIT_LENGTH);
}

export async function verifyPhoneCode(ctx: ApiContext, phone: string, code: string, userId: UserID): Promise<void> {
	await validatePhoneOrThrow(ctx, phone);
	const {sms} = ctx.services;
	let isValid: boolean;
	try {
		isValid = await sms.checkVerification(phone, code);
	} catch (error) {
		if (error instanceof TwilioVerificationRateLimitError) {
			await recordProviderCooldown(ctx, phone, userId, error);
		}
		throw error;
	}
	if (!isValid) {
		throw new InvalidPhoneVerificationCodeError();
	}
	await attachVerifiedPhoneToAccount(ctx, userId, phone);
}

async function attachVerifiedPhoneToAccount(ctx: ApiContext, userId: UserID, phone: string): Promise<void> {
	const {users, gateway, contactChangeLog} = ctx.services;
	if (!(await reuseStoreFor(ctx).claimVerificationSlot(phone))) {
		throw new PhoneAlreadyUsedError();
	}
	const user = await users.findUnique(userId);
	if (!user) {
		throw new PhoneVerificationRequiredError();
	}
	assertNonBotUser(user);
	if (user.flags & UserFlags.DELETED) {
		throw new PhoneVerificationRequiredError();
	}
	const updates: {flags?: bigint; has_verified_phone: boolean; suspicious_activity_flags?: number} = {
		has_verified_phone: true,
	};
	const shouldClearSpammerFlag = (user.flags & UserFlags.SPAMMER) === UserFlags.SPAMMER;
	if (shouldClearSpammerFlag) {
		updates.flags = user.flags & ~UserFlags.SPAMMER;
	}
	if (user.suspiciousActivityFlags !== null && user.suspiciousActivityFlags !== 0) {
		const newFlags = user.suspiciousActivityFlags & ~PHONE_ADD_CLEARABLE_FLAGS;
		if (newFlags !== user.suspiciousActivityFlags) {
			updates.suspicious_activity_flags = newFlags;
		}
	}
	const updatedUser = await users.patchUpsert(userId, updates, user.toRow());
	await contactChangeLog.recordDiff({
		oldUser: user,
		newUser: updatedUser,
		reason: 'user_requested',
		actorUserId: userId,
	});
	const userSearchService = getUserSearchService();
	if (userSearchService && 'updateUser' in userSearchService) {
		await userSearchService.updateUser(updatedUser).catch((error) => {
			Logger.error({userId, error}, 'Failed to update user in search index');
		});
	}
	await gateway.dispatchPresence({
		userId,
		event: 'USER_UPDATE',
		data: mapUserToPrivateResponse(updatedUser),
	});
	if (shouldClearSpammerFlag) {
		await dispatchForcedGuildMemberUpdates(ctx, updatedUser);
	}
}

async function dispatchForcedGuildMemberUpdates(ctx: ApiContext, updatedUser: User): Promise<void> {
	const {users, gateway} = ctx.services;
	const userPartial = mapUserToPartialResponse(updatedUser);
	const guildIds = await users.getUserGuildIds(updatedUser.id);
	for (const guildId of guildIds) {
		const guildMemberResult = await gateway.getGuildMember({
			guildId,
			userId: updatedUser.id,
		});
		if (!guildMemberResult.success || !guildMemberResult.memberData) {
			continue;
		}
		await gateway.dispatchGuild({
			guildId,
			event: 'GUILD_MEMBER_UPDATE',
			data: {
				...guildMemberResult.memberData,
				user: userPartial,
			},
		});
	}
}

async function enforceSmsSendProtections(ctx: ApiContext, phone: string, user: User | null): Promise<void> {
	if (user) {
		await checkPhoneSendLimit(
			ctx,
			{
				identifier: `auth:phone:send:any:${user.id.toString()}`,
				...ACCOUNT_SMS_SEND_LIMIT,
			},
			'user',
		);
	}
	await checkPhoneSendLimit(
		ctx,
		{
			identifier: `auth:phone:send:phone:${phone}`,
			...PHONE_SMS_SEND_LIMIT,
		},
		'shared',
	);
}

async function checkPhoneSendLimit(
	ctx: ApiContext,
	config: {identifier: string; maxAttempts: number; windowMs: number},
	scope: RateLimitScope,
): Promise<void> {
	const result = await ctx.services.rateLimit.checkLimit(config);
	if (!result.allowed) {
		throw createPhoneRateLimitError(result, scope);
	}
}

function createPhoneRateLimitError(
	result: Pick<RateLimitResult, 'retryAfter' | 'retryAfterDecimal' | 'limit' | 'resetTime' | 'resetAfterDecimal'>,
	scope: RateLimitScope,
): RateLimitError {
	return new RateLimitError({
		code: APIErrorCodes.PHONE_RATE_LIMIT_EXCEEDED,
		retryAfter: result.retryAfter,
		retryAfterDecimal: result.retryAfterDecimal,
		limit: result.limit,
		resetTime: result.resetTime,
		resetAfterDecimal: result.resetAfterDecimal,
		scope,
	});
}

async function throwIfProviderCooldownActive(ctx: ApiContext, phone: string, userId: UserID | null): Promise<void> {
	const accountCooldown = userId ? await readCooldown(ctx, getProviderAccountCooldownKey(userId)) : null;
	const phoneCooldown = await readCooldown(ctx, getProviderPhoneCooldownKey(phone));
	const cooldown = pickLongerCooldown(accountCooldown, phoneCooldown);
	if (!cooldown) {
		return;
	}
	throw createPhoneRateLimitError(
		{
			retryAfter: cooldown.retryAfter,
			retryAfterDecimal: cooldown.retryAfter,
			limit: 1,
			resetTime: new Date(Date.now() + cooldown.retryAfter * 1000),
			resetAfterDecimal: cooldown.retryAfter,
		},
		cooldown.scope,
	);
}

async function recordProviderCooldown(
	ctx: ApiContext,
	phone: string,
	userId: UserID | null,
	error: TwilioVerificationRateLimitError,
): Promise<void> {
	const {cache} = ctx.services;
	const ttlSeconds = Math.max(1, Math.ceil(error.cooldownMs / 1000));
	const payload = {provider_message: error.message};
	if ((error.cooldownScope === 'account' || error.cooldownScope === 'account_and_phone') && userId) {
		await cache.set(getProviderAccountCooldownKey(userId), payload, ttlSeconds);
	}
	if (error.cooldownScope === 'phone' || error.cooldownScope === 'account_and_phone') {
		await cache.set(getProviderPhoneCooldownKey(phone), payload, ttlSeconds);
	}
}

function getProviderAccountCooldownKey(userId: UserID): string {
	return `auth:phone:provider-cooldown:user:${userId.toString()}`;
}

function getProviderPhoneCooldownKey(phone: string): string {
	return `auth:phone:provider-cooldown:phone:${phone}`;
}

async function readCooldown(ctx: ApiContext, key: string): Promise<{retryAfter: number; scope: RateLimitScope} | null> {
	const {cache} = ctx.services;
	const payload = await cache.get<unknown>(key);
	if (!payload) {
		return null;
	}
	const ttl = await cache.ttl(key);
	const retryAfter = ttl > 0 ? ttl : 60;
	return {
		retryAfter,
		scope: key.includes(':phone:') ? 'shared' : 'user',
	};
}

function pickLongerCooldown(
	a: {retryAfter: number; scope: RateLimitScope} | null,
	b: {retryAfter: number; scope: RateLimitScope} | null,
): {retryAfter: number; scope: RateLimitScope} | null {
	if (!a) return b;
	if (!b) return a;
	return a.retryAfter >= b.retryAfter ? a : b;
}

type PhoneVerdictResult =
	| {verdict: 'accept'; rejectReason: null}
	| {verdict: 'reject'; rejectReason: PhoneAttemptRejectReason}
	| {verdict: 'require_inbound'; inboundReason: PhoneAttemptInboundReason};

export function errorForPhoneRejectReason(reason: PhoneAttemptRejectReason): FluxerError {
	switch (reason) {
		case 'invalid_format':
			return new InvalidPhoneNumberError();
		case 'banned_prefix':
			return new PhoneCountryNotSupportedError();
		case 'lookup_unavailable':
			return new PhoneLookupUnavailableError();
		case 'invalid_number':
			return new PhoneNumberNotInServiceError();
		case 'line_type_not_mobile':
		case 'line_type_hard_rejected':
			return new PhoneNumberNotMobileError();
		case 'sms_pumping_risk_high':
		case 'behavioural_risk_blocked':
			return new PhoneVerificationNeedsReviewError();
	}
}

function computePhoneVerdict(lookup: PhoneLookupResult | null, phone: string): PhoneVerdictResult {
	if (lookup == null) {
		return {verdict: 'reject', rejectReason: 'lookup_unavailable'};
	}
	if (!lookup.valid) {
		return {verdict: 'reject', rejectReason: 'invalid_number'};
	}
	if (lookup.lineType && VOIP_PHONE_LINE_TYPES.has(lookup.lineType)) {
		return {verdict: 'require_inbound', inboundReason: 'voip'};
	}
	if (isCanadianPhoneNumber(phone)) {
		return {verdict: 'require_inbound', inboundReason: 'canadian'};
	}
	if (!lookup.lineType || lookup.lineType === 'unknown') {
		return {verdict: 'require_inbound', inboundReason: 'unknown_line_type'};
	}
	if (HARD_REJECT_PHONE_LINE_TYPES.has(lookup.lineType)) {
		return {verdict: 'reject', rejectReason: 'line_type_hard_rejected'};
	}
	if (!ACCEPTED_PHONE_LINE_TYPES.has(lookup.lineType)) {
		return {verdict: 'reject', rejectReason: 'line_type_not_mobile'};
	}
	const threshold = getSmsPumpingRiskThreshold(lookup.countryCode);
	if (lookup.smsPumpingRiskScore !== null && lookup.smsPumpingRiskScore >= threshold) {
		return {verdict: 'reject', rejectReason: 'sms_pumping_risk_high'};
	}
	return {verdict: 'accept', rejectReason: null};
}

const CANADIAN_NPAS: ReadonlySet<string> = new Set([
	'204',
	'226',
	'236',
	'249',
	'250',
	'263',
	'289',
	'306',
	'343',
	'354',
	'365',
	'367',
	'368',
	'382',
	'387',
	'403',
	'416',
	'418',
	'428',
	'431',
	'437',
	'438',
	'450',
	'468',
	'474',
	'506',
	'514',
	'519',
	'548',
	'579',
	'581',
	'584',
	'587',
	'604',
	'613',
	'639',
	'647',
	'672',
	'683',
	'705',
	'709',
	'742',
	'753',
	'778',
	'780',
	'782',
	'807',
	'819',
	'825',
	'867',
	'873',
	'879',
	'902',
	'905',
	'942',
]);

function isCanadianPhoneNumber(e164: string): boolean {
	if (!e164.startsWith('+1') || e164.length < 5) return false;
	const npa = e164.slice(2, 5);
	return CANADIAN_NPAS.has(npa);
}
