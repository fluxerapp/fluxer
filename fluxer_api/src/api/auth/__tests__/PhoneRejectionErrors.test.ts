// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {describe, expect, it} from 'vitest';
import {errorForPhoneRejectReason} from '../AuthPhone';
import type {PhoneAttemptRejectReason} from '../services/PhoneLookupRepository';

const EXPECTED_CODES: ReadonlyArray<[PhoneAttemptRejectReason, string]> = [
	['invalid_format', APIErrorCodes.INVALID_PHONE_NUMBER],
	['banned_prefix', APIErrorCodes.PHONE_COUNTRY_NOT_SUPPORTED],
	['lookup_unavailable', APIErrorCodes.PHONE_LOOKUP_UNAVAILABLE],
	['invalid_number', APIErrorCodes.PHONE_NUMBER_NOT_IN_SERVICE],
	['line_type_not_mobile', APIErrorCodes.PHONE_NUMBER_NOT_MOBILE],
	['line_type_hard_rejected', APIErrorCodes.PHONE_NUMBER_NOT_MOBILE],
	['sms_pumping_risk_high', APIErrorCodes.PHONE_VERIFICATION_NEEDS_REVIEW],
	['behavioural_risk_blocked', APIErrorCodes.PHONE_VERIFICATION_NEEDS_REVIEW],
];

describe('errorForPhoneRejectReason', () => {
	it.each(EXPECTED_CODES)('maps %s to %s', (reason, code) => {
		expect(errorForPhoneRejectReason(reason).code).toBe(code);
	});
	it.each(EXPECTED_CODES)('returns a 400 for %s', (reason) => {
		expect(errorForPhoneRejectReason(reason).status).toBe(400);
	});
	it('gives both line type rejections the same code', () => {
		expect(errorForPhoneRejectReason('line_type_not_mobile').code).toBe(
			errorForPhoneRejectReason('line_type_hard_rejected').code,
		);
	});
	it('gives both fraud signals the same response body', async () => {
		const pumping = await errorForPhoneRejectReason('sms_pumping_risk_high').getResponse().text();
		const behavioural = await errorForPhoneRejectReason('behavioural_risk_blocked').getResponse().text();
		expect(pumping).toBe(behavioural);
	});
	it('keeps the fraud signals distinct from every benign reason', () => {
		const review = errorForPhoneRejectReason('sms_pumping_risk_high').code;
		const benign = EXPECTED_CODES.filter(([, code]) => code !== review).map(([, code]) => code);
		expect(benign).not.toContain(review);
	});
});
