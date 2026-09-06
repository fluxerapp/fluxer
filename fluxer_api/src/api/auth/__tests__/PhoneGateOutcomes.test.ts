// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {FluxerError} from '@fluxer/errors/src/FluxerError';
import type {PhoneLineType, PhoneLookupResult} from '@pkgs/sms/src/PhoneLookupTypes';
import {afterEach, describe, expect, it, vi} from 'vitest';
import type {ApiContext} from '../../ApiContext';
import type {UserID} from '../../BrandedTypes';
import {errorForPhoneRejectReason, sendPhoneVerificationCode, verifyPhoneCode} from '../AuthPhone';
import {phonePrefixBanCache} from '../PhonePrefixBanCache';
import type {PhoneAttemptInboundReason, PhoneAttemptRejectReason} from '../services/PhoneLookupRepository';

const USER_ID = 1n as UserID;
const MOBILE_US = '+15125550123';
const CANADIAN = '+16045550123';

function lookup(overrides: Partial<PhoneLookupResult> = {}): PhoneLookupResult {
	return {
		valid: true,
		lineType: 'mobile',
		countryCode: 'US',
		carrierName: 'Test',
		smsPumpingRiskScore: 0,
		...overrides,
	};
}

interface HarnessOptions {
	lookupResult?: PhoneLookupResult | null;
	userFlags?: bigint;
	riskDecision?: 'allow' | 'require_inbound';
	challengeFails?: boolean;
}

function buildContext(options: HarnessOptions = {}) {
	const issueChallenge = vi.fn(async () => {
		if (options.challengeFails) {
			throw new Error('inbound challenge service down');
		}
		return {challengeCode: 'ABCD', ourNumber: '+15005550006', expiresAt: new Date(0)};
	});
	const record = vi.fn(async (_input: {rejected: boolean}) => undefined);
	const services = {
		sms: {
			lookupPhone: vi.fn(async () => (options.lookupResult === undefined ? lookup() : options.lookupResult)),
			checkVerification: vi.fn(async () => true),
			startVerificationWithResult: vi.fn(async () => undefined),
		},
		phoneLookup: null,
		users: {
			findUnique: vi.fn(async () => ({
				id: USER_ID,
				isBot: false,
				flags: options.userFlags ?? 0n,
				hasVerifiedPhone: false,
			})),
		},
		cache: {get: vi.fn(async () => null), ttl: vi.fn(async () => 0), set: vi.fn(async () => undefined)},
		phoneAttemptRisk: {
			evaluate: vi.fn(async () => ({decision: options.riskDecision ?? 'allow', reason: null, counters: {}})),
			record,
		},
		inboundSmsChallenge: {issueChallenge},
		config: {sms: {inboundChallengeNumber: '+15005550006'}},
	};
	return {ctx: {services} as unknown as ApiContext, issueChallenge, record};
}

async function codeFromVerify(phone: string, options: HarnessOptions = {}): Promise<string> {
	const {ctx} = buildContext(options);
	try {
		await verifyPhoneCode(ctx, phone, '123456', USER_ID);
	} catch (error) {
		return (error as FluxerError).code;
	}
	throw new Error(`verifyPhoneCode did not reject ${phone}`);
}

async function inboundReasonFromSend(phone: string, options: HarnessOptions = {}, channel?: 'inbound_challenge') {
	const {ctx} = buildContext(options);
	const result = await sendPhoneVerificationCode(ctx, phone, USER_ID, channel ? {channel} : {});
	if (result.channel !== 'inbound_challenge') {
		throw new Error(`sendPhoneVerificationCode returned ${result.channel} for ${phone}`);
	}
	return result.reason;
}

afterEach(() => {
	phonePrefixBanCache.unban('+31970');
});

const REJECT_REASON_CODES: Record<PhoneAttemptRejectReason, string> = {
	invalid_format: APIErrorCodes.INVALID_PHONE_NUMBER,
	banned_prefix: APIErrorCodes.PHONE_COUNTRY_NOT_SUPPORTED,
	lookup_unavailable: APIErrorCodes.PHONE_LOOKUP_UNAVAILABLE,
	invalid_number: APIErrorCodes.PHONE_NUMBER_NOT_IN_SERVICE,
	line_type_not_mobile: APIErrorCodes.PHONE_NUMBER_NOT_MOBILE,
	line_type_hard_rejected: APIErrorCodes.PHONE_NUMBER_NOT_MOBILE,
	sms_pumping_risk_high: APIErrorCodes.PHONE_VERIFICATION_NEEDS_REVIEW,
	behavioural_risk_blocked: APIErrorCodes.PHONE_VERIFICATION_NEEDS_REVIEW,
};

const INBOUND_REASONS: Record<PhoneAttemptInboundReason, true> = {
	voip: true,
	canadian: true,
	unknown_line_type: true,
	expensive_destination: true,
	account_forced: true,
	behavioural_risk: true,
};

describe('every reject reason maps to its own error', () => {
	it.each(Object.entries(REJECT_REASON_CODES))('%s produces %s', (reason, code) => {
		expect(errorForPhoneRejectReason(reason as PhoneAttemptRejectReason).code).toBe(code);
	});
	it('covers all eight declared reject reasons', () => {
		expect(Object.keys(REJECT_REASON_CODES)).toHaveLength(8);
	});
});

describe('reject reasons reaching the caller through the real gate', () => {
	it('invalid_format keeps the plain invalid-number error', async () => {
		expect(await codeFromVerify('5551234')).toBe(REJECT_REASON_CODES.invalid_format);
	});
	it('banned_prefix says the country is unsupported', async () => {
		phonePrefixBanCache.ban('+31970');
		expect(await codeFromVerify('+31970123456')).toBe(REJECT_REASON_CODES.banned_prefix);
	});
	it('lookup_unavailable blames our own checker', async () => {
		expect(await codeFromVerify(MOBILE_US, {lookupResult: null})).toBe(REJECT_REASON_CODES.lookup_unavailable);
	});
	it('invalid_number says the carrier rejected it', async () => {
		expect(await codeFromVerify(MOBILE_US, {lookupResult: lookup({valid: false})})).toBe(
			REJECT_REASON_CODES.invalid_number,
		);
	});
	it.each([
		'landline',
		'tollFree',
		'premium',
		'sharedCost',
		'uan',
		'voicemail',
		'pager',
	] as const)('line_type_hard_rejected for %s says it is not a mobile', async (lineType) => {
		expect(await codeFromVerify(MOBILE_US, {lookupResult: lookup({lineType})})).toBe(
			REJECT_REASON_CODES.line_type_hard_rejected,
		);
	});
	it('sms_pumping_risk_high routes to human review', async () => {
		expect(await codeFromVerify(MOBILE_US, {lookupResult: lookup({smsPumpingRiskScore: 100})})).toBe(
			REJECT_REASON_CODES.sms_pumping_risk_high,
		);
	});
	it('line_type_not_mobile is unreachable through the verdict and is covered by the mapper alone', async () => {
		const accepted: Array<PhoneLineType> = ['mobile', 'personal'];
		for (const lineType of accepted) {
			const {ctx} = buildContext({lookupResult: lookup({lineType})});
			const rejection = await verifyPhoneCode(ctx, MOBILE_US, '123456', USER_ID).then(
				() => null,
				(error: FluxerError) => error.code,
			);
			expect(rejection).not.toBe(APIErrorCodes.PHONE_NUMBER_NOT_MOBILE);
			expect(rejection).not.toBe(APIErrorCodes.PHONE_INBOUND_VERIFICATION_REQUIRED);
		}
		expect(errorForPhoneRejectReason('line_type_not_mobile').code).toBe(REJECT_REASON_CODES.line_type_not_mobile);
	});
	it('behavioural_risk_blocked has no producer in the gate and is covered by the mapper alone', () => {
		expect(errorForPhoneRejectReason('behavioural_risk_blocked').code).toBe(
			REJECT_REASON_CODES.behavioural_risk_blocked,
		);
	});
});

describe('every inbound reason', () => {
	it('covers all six declared inbound reasons', () => {
		expect(Object.keys(INBOUND_REASONS)).toHaveLength(6);
	});
	it.each(['fixedVoip', 'nonFixedVoip'] as const)('routes %s to the inbound challenge as voip', async (lineType) => {
		expect(await inboundReasonFromSend(MOBILE_US, {lookupResult: lookup({lineType})})).toBe('voip');
	});
	it('routes a Canadian number to the inbound challenge as canadian', async () => {
		expect(await inboundReasonFromSend(CANADIAN)).toBe('canadian');
	});
	it.each([null, 'unknown'] as const)('routes line type %s to the inbound challenge', async (lineType) => {
		expect(await inboundReasonFromSend(MOBILE_US, {lookupResult: lookup({lineType})})).toBe('unknown_line_type');
	});
	it('routes an explicit inbound channel request as expensive_destination', async () => {
		expect(await inboundReasonFromSend(MOBILE_US, {}, 'inbound_challenge')).toBe('expensive_destination');
	});
	it('routes a force-flagged account as account_forced', async () => {
		expect(await inboundReasonFromSend(MOBILE_US, {userFlags: UserFlags.FORCE_INBOUND_PHONE_VERIFICATION})).toBe(
			'account_forced',
		);
	});
	it('routes a risk-flagged account as behavioural_risk', async () => {
		expect(await inboundReasonFromSend(MOBILE_US, {riskDecision: 'require_inbound'})).toBe('behavioural_risk');
	});
});

describe('inbound reasons a caller cannot answer', () => {
	it.each([
		[lookup({lineType: 'fixedVoip'}), MOBILE_US],
		[lookup({lineType: 'nonFixedVoip'}), MOBILE_US],
		[lookup({lineType: null}), MOBILE_US],
		[lookup({lineType: 'unknown'}), MOBILE_US],
		[lookup(), CANADIAN],
	])('tells the user to restart phone verification instead of calling the number invalid', async (result, phone) => {
		expect(await codeFromVerify(phone, {lookupResult: result})).toBe(APIErrorCodes.PHONE_INBOUND_VERIFICATION_REQUIRED);
	});
	it('never reports the inbound reason to the caller', async () => {
		const voip = await codeFromVerify(MOBILE_US, {lookupResult: lookup({lineType: 'fixedVoip'})});
		const canadian = await codeFromVerify(CANADIAN);
		const unknown = await codeFromVerify(MOBILE_US, {lookupResult: lookup({lineType: 'unknown'})});
		expect(new Set([voip, canadian, unknown]).size).toBe(1);
	});
});

describe('inbound challenge issuance failures', () => {
	it.each([
		['voip', {lookupResult: lookup({lineType: 'fixedVoip'})}, undefined],
		['account_forced', {userFlags: UserFlags.FORCE_INBOUND_PHONE_VERIFICATION}, undefined],
		['behavioural_risk', {riskDecision: 'require_inbound' as const}, undefined],
		['expensive_destination', {}, 'inbound_challenge' as const],
	])('surfaces the send-path outage for %s', async (_reason, options, channel) => {
		const {ctx} = buildContext({...(options as HarnessOptions), challengeFails: true});
		try {
			await sendPhoneVerificationCode(ctx, MOBILE_US, USER_ID, channel ? {channel} : {});
		} catch (error) {
			expect((error as FluxerError).code).toBe(APIErrorCodes.SMS_VERIFICATION_UNAVAILABLE);
			return;
		}
		throw new Error('sendPhoneVerificationCode did not reject');
	});
});

async function recordedRejectionFor(options: HarnessOptions): Promise<boolean> {
	const {ctx, record} = buildContext(options);
	await sendPhoneVerificationCode(ctx, MOBILE_US, USER_ID, {}).catch(() => undefined);
	const call = record.mock.calls.at(-1)?.[0];
	if (!call) {
		throw new Error('phoneAttemptRisk.record was never called');
	}
	return call.rejected;
}

describe('what each rejection costs the user', () => {
	it('does not count our own lookup outage against them', async () => {
		expect(await recordedRejectionFor({lookupResult: null})).toBe(false);
	});
	it.each([
		['invalid_number', lookup({valid: false})],
		['line_type_hard_rejected', lookup({lineType: 'landline'})],
		['sms_pumping_risk_high', lookup({smsPumpingRiskScore: 100})],
	])('still counts %s against them', async (_reason, lookupResult) => {
		expect(await recordedRejectionFor({lookupResult})).toBe(true);
	});
	it('does not count an inbound-challenge routing against them', async () => {
		expect(await recordedRejectionFor({lookupResult: lookup({lineType: 'fixedVoip'})})).toBe(false);
	});
});
