// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {Db, nextVersion} from '@app/api/database/CassandraTypes';
import type {OperatorPassWaitlistRow} from '@app/api/database/types/OperatorPassTypes';
import type {EmailDnsValidationService} from '@app/api/infrastructure/EmailDnsValidationService';
import {Logger} from '@app/api/Logger';
import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import type {OperatorPassRepository, OperatorPassWaitlistPatch} from '@app/api/operator_pass/OperatorPassRepository';
import {
	getOperatorPassPhase,
	getOperatorPassSchedule,
	operatorPassNow,
} from '@app/api/operator_pass/OperatorPassSchedule';
import {
	getOperatorPassWaitlistStats,
	listOperatorPassWaitlistFollowupTexts,
} from '@app/api/operator_pass/OperatorPassWaitlistStats';
import {
	buildOperatorPassWaitlistConfirmUrl,
	createOperatorPassWaitlistEntryId,
	createOperatorPassWaitlistFollowupToken,
	createOperatorPassWaitlistVerifyToken,
	OPERATOR_PASS_WAITLIST_VERIFY_EXPIRY_MS,
	operatorPassWaitlistSecretMatches,
	parseOperatorPassWaitlistVerifyToken,
	verifyOperatorPassUnsubscribeToken,
	verifyOperatorPassWaitlistFollowupToken,
} from '@app/api/operator_pass/OperatorPassWaitlistTokens';
import {OperatorPassRateLimitConfigs} from '@app/api/rate_limit_configs/OperatorPassRateLimitConfig';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {OperatorPassWaitlistClosedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWaitlistClosedError';
import {OperatorPassWaitlistFollowupInvalidError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWaitlistFollowupInvalidError';
import {OperatorPassWaitlistFollowupTokenInvalidError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWaitlistFollowupTokenInvalidError';
import {OperatorPassWaitlistTokenExpiredError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWaitlistTokenExpiredError';
import {OperatorPassWaitlistTokenInvalidError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWaitlistTokenInvalidError';
import type {
	AdminOperatorPassWaitlistFollowupTextEntry,
	AdminOperatorPassWaitlistStatsResponse,
} from '@fluxer/schema/src/domains/admin/AdminOperatorPassSchemas';
import {
	OPERATOR_PASS_WAITLIST_ABOUT_KEYS,
	OPERATOR_PASS_WAITLIST_FOLLOWUP_KEYS,
	OPERATOR_PASS_WAITLIST_FOLLOWUP_OTHER_TEXT_MAX,
	type OperatorPassCurrency,
	type OperatorPassPriceFeedback,
	type OperatorPassWaitlistAboutKey,
	type OperatorPassWaitlistFollowupKey,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type {IEmailService, OperatorPassWaitlistVerificationEmail} from '@pkgs/email/src/IEmailService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import {seconds} from 'itty-time';

const RECONFIRM_INTERVAL_SECONDS = seconds('30 days');
const SUPPRESS_ATTEMPTS = 3;
const FOLLOWUP_ATTEMPTS = 3;
const FOLLOWUP_KEYS: ReadonlySet<string> = new Set(OPERATOR_PASS_WAITLIST_FOLLOWUP_KEYS);
const ABOUT_KEYS: ReadonlySet<string> = new Set(OPERATOR_PASS_WAITLIST_ABOUT_KEYS);
const FOLLOWUP_TEXT_FORBIDDEN = /[\p{Cc}\p{Cs}\p{Co}\u2028\u2029]/u;

export interface OperatorPassBannedEmailChecker {
	isEmailBanned(email: string): Promise<boolean>;
}

export interface OperatorPassWaitlistJoinParams {
	email: string;
	meterRateLimits: boolean;
	countryCode: string | null;
	locale: string | null;
	consentIp: string | null;
	priceFeedback: OperatorPassPriceFeedback | null;
	currency: OperatorPassCurrency | null;
}

export type OperatorPassWaitlistSuppression = 'bounced' | 'unsubscribed';

export interface OperatorPassWaitlistJoinResult {
	followupToken: string;
}

export interface OperatorPassWaitlistFollowupParams {
	token: string;
	picks: ReadonlyArray<string>;
	about: ReadonlyArray<string> | undefined;
	otherText: string | null | undefined;
	meterRateLimits: boolean;
}

interface OperatorPassWaitlistFollowupAnswer {
	picks: Set<OperatorPassWaitlistFollowupKey>;
	about: Set<OperatorPassWaitlistAboutKey> | null;
	otherText: string | null;
}

function parseKeys<Key extends string>(allowed: ReadonlySet<string>, values: ReadonlyArray<string>): Set<Key> {
	const keys = new Set<Key>();
	for (const value of values) {
		if (!allowed.has(value) || keys.has(value as Key)) {
			throw new OperatorPassWaitlistFollowupInvalidError();
		}
		keys.add(value as Key);
	}
	return keys;
}

function parseOperatorPassWaitlistFollowupAnswer(
	picks: ReadonlyArray<string>,
	about: ReadonlyArray<string> | undefined,
	otherText: string | null | undefined,
): OperatorPassWaitlistFollowupAnswer {
	const keys = parseKeys<OperatorPassWaitlistFollowupKey>(FOLLOWUP_KEYS, picks);
	const aboutKeys = about === undefined ? null : parseKeys<OperatorPassWaitlistAboutKey>(ABOUT_KEYS, about);
	if (otherText == null) {
		return {picks: keys, about: aboutKeys, otherText: null};
	}
	const text = otherText.replace(/\r\n?/gu, '\n').trim();
	const length = [...text].length;
	const textForbidden = FOLLOWUP_TEXT_FORBIDDEN.test(text.replace(/[\n\t]/gu, ''));
	if (!keys.has('other') || length < 1 || length > OPERATOR_PASS_WAITLIST_FOLLOWUP_OTHER_TEXT_MAX || textForbidden) {
		throw new OperatorPassWaitlistFollowupInvalidError();
	}
	return {picks: keys, about: aboutKeys, otherText: text};
}

function isSubscribed(entry: OperatorPassWaitlistRow): boolean {
	return entry.status === 'pending' || entry.status === 'confirmed';
}

function isReconfirm(entry: OperatorPassWaitlistRow | null): boolean {
	return entry !== null && (entry.status === 'unsubscribed' || entry.unsubscribed_at !== null);
}

function reconfirmKey(email: string): string {
	return `operator_pass:waitlist:reconfirm:${email}`;
}

export class OperatorPassWaitlistService {
	constructor(
		readonly repository: OperatorPassRepository,
		readonly emailService: IEmailService,
		readonly emailDnsValidationService: Pick<EmailDnsValidationService, 'hasValidDnsRecords'>,
		readonly bannedEmailChecker: OperatorPassBannedEmailChecker,
		readonly kv: Pick<IKVProvider, 'setnx' | 'exists'>,
		readonly rateLimitService: Pick<IRateLimitService, 'checkBucketLimit' | 'peekLimit'>,
	) {}

	private async charge(identifier: string, {bucket, config}: RouteRateLimitConfig): Promise<void> {
		const result = await this.rateLimitService.checkBucketLimit(`${identifier}:${bucket}`, config);
		if (result.allowed) return;
		throw new RateLimitError({
			retryAfter: result.retryAfter,
			retryAfterDecimal: result.retryAfterDecimal,
			limit: result.limit,
			resetTime: result.resetTime,
			resetAfterDecimal: result.resetAfterDecimal,
			scope: 'shared',
		});
	}

	async join(params: OperatorPassWaitlistJoinParams): Promise<OperatorPassWaitlistJoinResult> {
		const scheduleNow = operatorPassNow();
		if (getOperatorPassPhase(scheduleNow) !== 'waitlist') {
			throw new OperatorPassWaitlistClosedError();
		}
		const email = params.email.trim().toLowerCase();
		if (!(await this.emailDnsValidationService.hasValidDnsRecords(email))) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_DOMAIN_CANNOT_RECEIVE_MAIL);
		}
		if (params.meterRateLimits) {
			await this.charge(
				`email:${createHash('sha256').update(email).digest('hex').slice(0, 32)}`,
				OperatorPassRateLimitConfigs.OPERATOR_PASS_WAITLIST_ADDRESS,
			);
		}
		const banned = await this.bannedEmailChecker.isEmailBanned(email);
		const existing = await this.repository.findWaitlistEntryByEmail(email);
		const reconfirmThrottled = (await this.kv.exists(reconfirmKey(email))) > 0;
		const now = new Date();
		const entryId = existing?.status === 'pending' ? existing.entry_id : createOperatorPassWaitlistEntryId();
		const verifyToken = createOperatorPassWaitlistVerifyToken(entryId);
		const followupEntryId = existing ? createOperatorPassWaitlistEntryId() : entryId;
		const followupToken = createOperatorPassWaitlistFollowupToken(followupEntryId, now);
		const writes =
			!banned &&
			existing?.status !== 'confirmed' &&
			existing?.status !== 'bounced' &&
			!reconfirmThrottled &&
			(!isReconfirm(existing) || (await this.kv.setnx(reconfirmKey(email), '1', RECONFIRM_INTERVAL_SECONDS)));
		if (banned) {
			Logger.info('Operator Pass waitlist signup refused for a banned email');
		}
		const expiresAt = new Date(now.getTime() + OPERATOR_PASS_WAITLIST_VERIFY_EXPIRY_MS);
		const entry: OperatorPassWaitlistRow = {
			email,
			entry_id: entryId,
			status: 'pending',
			locale: params.locale,
			country_code: params.countryCode?.toUpperCase() ?? existing?.country_code ?? null,
			created_at: existing?.created_at ?? now,
			updated_at: now,
			confirmed_at: null,
			unsubscribed_at: existing?.unsubscribed_at ?? null,
			consent_ip: params.consentIp,
			verify_token_hash: verifyToken.secretHash,
			verify_expires_at: expiresAt,
			sale_email_sent_at: existing?.sale_email_sent_at ?? null,
			price_feedback: params.priceFeedback,
			currency: params.currency,
			followup_picks: existing?.followup_picks ?? null,
			followup_about: existing?.followup_about ?? null,
			followup_other_text: existing?.followup_other_text ?? null,
			followup_answered_at: existing?.followup_answered_at ?? null,
			version: nextVersion(existing?.version),
		};
		let saved = false;
		if (writes) {
			saved = await this.repository.saveWaitlistEntry(entry, existing);
			if (!saved) {
				Logger.info('Operator Pass waitlist signup lost a race with another write and sent nothing');
			}
		} else {
			await this.repository.probeWaitlistEntry(email, followupEntryId);
		}
		if (params.meterRateLimits && saved) {
			await this.charge('all', OperatorPassRateLimitConfigs.OPERATOR_PASS_WAITLIST_SENDS);
		} else if (params.meterRateLimits) {
			const {bucket, config} = OperatorPassRateLimitConfigs.OPERATOR_PASS_WAITLIST_SENDS;
			await this.rateLimitService.peekLimit({identifier: bucket, maxAttempts: config.limit, windowMs: config.windowMs});
		}
		if (saved) {
			void this.sendVerification(email, entryId, params.locale, {
				confirmUrl: buildOperatorPassWaitlistConfirmUrl(verifyToken.token),
			});
		}
		return {followupToken};
	}

	async answerFollowup(params: OperatorPassWaitlistFollowupParams): Promise<void> {
		const entryId = verifyOperatorPassWaitlistFollowupToken(params.token);
		if (!entryId) {
			throw new OperatorPassWaitlistFollowupTokenInvalidError();
		}
		if (params.meterRateLimits) {
			await this.charge(`entry:${entryId}`, OperatorPassRateLimitConfigs.OPERATOR_PASS_WAITLIST_FOLLOWUP_ENTRY);
		}
		const answer = parseOperatorPassWaitlistFollowupAnswer(params.picks, params.about, params.otherText);
		let current = await this.repository.findWaitlistEntryByEntryId(entryId);
		if (!current) {
			await this.repository.probeWaitlistEntry(entryId, entryId);
			return;
		}
		for (let attempt = 0; current && attempt < FOLLOWUP_ATTEMPTS; attempt += 1) {
			const patch: OperatorPassWaitlistPatch = {
				followup_picks: answer.picks.size > 0 ? Db.set(new Set<string>(answer.picks)) : Db.clear(),
			};
			if (answer.about !== null) {
				patch.followup_about = answer.about.size > 0 ? Db.set(new Set<string>(answer.about)) : Db.clear();
			}
			patch.followup_other_text = answer.otherText === null ? Db.clear() : Db.set(answer.otherText);
			patch.followup_answered_at = Db.set(new Date());
			const updated = await this.repository.updateWaitlistEntry(current, patch);
			if (updated) return;
			current = await this.repository.findWaitlistEntryByEntryId(entryId);
		}
		Logger.warn({entryId}, 'Operator Pass waitlist follow-up answer lost every write race');
	}

	private async sendVerification(
		email: string,
		entryId: string,
		locale: string | null,
		params: OperatorPassWaitlistVerificationEmail,
	): Promise<void> {
		try {
			const sent = await this.emailService.send('operator_pass_waitlist_verification', email, params, locale);
			if (!sent) {
				Logger.warn({entryId}, 'Operator Pass waitlist verification email was not sent');
			}
		} catch (error) {
			Logger.error({entryId, error}, 'Operator Pass waitlist verification email failed');
		}
	}

	async getStats(): Promise<AdminOperatorPassWaitlistStatsResponse> {
		const stats = await getOperatorPassWaitlistStats(this.repository);
		return {...stats, standard_price_set: getOperatorPassSchedule().standardPricesMinor !== null};
	}

	async listFollowupTexts(): Promise<Array<AdminOperatorPassWaitlistFollowupTextEntry>> {
		return listOperatorPassWaitlistFollowupTexts(this.repository);
	}

	async confirm(token: string): Promise<void> {
		const parsed = parseOperatorPassWaitlistVerifyToken(token);
		if (!parsed) {
			throw new OperatorPassWaitlistTokenInvalidError();
		}
		const entry = await this.repository.findWaitlistEntryByEntryId(parsed.entryId);
		if (!entry) {
			throw new OperatorPassWaitlistTokenInvalidError();
		}
		if (entry.status === 'confirmed') {
			return;
		}
		if (entry.status !== 'pending' || !entry.verify_token_hash) {
			throw new OperatorPassWaitlistTokenInvalidError();
		}
		if (!operatorPassWaitlistSecretMatches(parsed.secret, entry.verify_token_hash)) {
			throw new OperatorPassWaitlistTokenInvalidError();
		}
		const now = new Date();
		if (!entry.verify_expires_at || entry.verify_expires_at.getTime() <= now.getTime()) {
			throw new OperatorPassWaitlistTokenExpiredError();
		}
		const confirmed = await this.repository.updateWaitlistEntry(entry, {
			status: Db.set('confirmed'),
			confirmed_at: Db.set(now),
			updated_at: Db.set(now),
			verify_token_hash: Db.clear(),
			verify_expires_at: Db.clear(),
		});
		if (!confirmed && (await this.repository.findWaitlistEntryByEntryId(parsed.entryId))?.status !== 'confirmed') {
			throw new OperatorPassWaitlistTokenInvalidError();
		}
	}

	async unsubscribe(token: string): Promise<void> {
		const entryId = verifyOperatorPassUnsubscribeToken(token);
		if (!entryId) {
			throw new OperatorPassWaitlistTokenInvalidError();
		}
		const entry = await this.repository.findWaitlistEntryByEntryId(entryId);
		if (!entry || !isSubscribed(entry)) {
			return;
		}
		await this.markSuppressed(entry, 'unsubscribed');
	}

	async findEntryForAddress(address: string): Promise<OperatorPassWaitlistRow | null> {
		return this.repository.findWaitlistEntryByEmail(address.trim().toLowerCase());
	}

	async suppress(email: string, reason: OperatorPassWaitlistSuppression): Promise<boolean> {
		const entry = await this.findEntryForAddress(email);
		if (!entry || !isSubscribed(entry)) {
			return false;
		}
		await this.markSuppressed(entry, reason);
		return true;
	}

	private async markSuppressed(entry: OperatorPassWaitlistRow, reason: OperatorPassWaitlistSuppression): Promise<void> {
		let current: OperatorPassWaitlistRow | null = entry;
		for (let attempt = 0; current && isSubscribed(current) && attempt < SUPPRESS_ATTEMPTS; attempt += 1) {
			const now = new Date();
			const updated = await this.repository.updateWaitlistEntry(current, {
				status: Db.set(reason),
				updated_at: Db.set(now),
				verify_token_hash: Db.clear(),
				verify_expires_at: Db.clear(),
				...(reason === 'unsubscribed' ? {unsubscribed_at: Db.set(now)} : {}),
			});
			if (updated) {
				Logger.info({entryId: entry.entry_id, reason}, 'Operator Pass waitlist entry suppressed');
				return;
			}
			current = await this.repository.findWaitlistEntryByEmail(entry.email);
		}
		if (current && isSubscribed(current)) {
			Logger.error({entryId: entry.entry_id, reason}, 'Operator Pass waitlist entry could not be suppressed');
		}
	}
}
