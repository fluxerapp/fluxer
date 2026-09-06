// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ErrorCodeToI18nKey} from '@fluxer/errors/src/i18n/ErrorCodeMappings';
import {getErrorMessageUnsafe} from '@fluxer/errors/src/i18n/ErrorI18n';
import {describe, expect, it} from 'vitest';
import {errorForPhoneRejectReason} from '../AuthPhone';
import type {PhoneAttemptRejectReason} from '../services/PhoneLookupRepository';

const ALL_REJECT_REASONS: ReadonlyArray<PhoneAttemptRejectReason> = [
	'invalid_format',
	'banned_prefix',
	'lookup_unavailable',
	'invalid_number',
	'line_type_not_mobile',
	'line_type_hard_rejected',
	'sms_pumping_risk_high',
	'behavioural_risk_blocked',
];

const PHONE_GATE_CODES: ReadonlyArray<string> = Array.from(
	new Set([
		...ALL_REJECT_REASONS.map((reason) => errorForPhoneRejectReason(reason).code),
		APIErrorCodes.PHONE_INBOUND_VERIFICATION_REQUIRED,
	]),
);

const REPO_ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../../../../..');
const CLIENT_SWITCH_PATH = path.join(
	REPO_ROOT,
	'fluxer_app/src/features/auth/components/modals/required_action/RequiredActionShared.tsx',
);

describe('phone gate error codes', () => {
	it('produces every code the gate is meant to produce', () => {
		expect(PHONE_GATE_CODES.slice().sort()).toEqual(
			[
				APIErrorCodes.INVALID_PHONE_NUMBER,
				APIErrorCodes.PHONE_COUNTRY_NOT_SUPPORTED,
				APIErrorCodes.PHONE_INBOUND_VERIFICATION_REQUIRED,
				APIErrorCodes.PHONE_LOOKUP_UNAVAILABLE,
				APIErrorCodes.PHONE_NUMBER_NOT_IN_SERVICE,
				APIErrorCodes.PHONE_NUMBER_NOT_MOBILE,
				APIErrorCodes.PHONE_VERIFICATION_NEEDS_REVIEW,
			].sort(),
		);
	});
	it.each(PHONE_GATE_CODES)('%s is a declared APIErrorCode', (code) => {
		expect(Object.values(APIErrorCodes)).toContain(code);
	});
	it.each(PHONE_GATE_CODES)('%s has an ErrorCodeMappings entry', (code) => {
		expect(ErrorCodeToI18nKey[code as keyof typeof ErrorCodeToI18nKey]).toBeTruthy();
	});
	it.each(PHONE_GATE_CODES)('%s resolves to a real English message', (code) => {
		const message = getErrorMessageUnsafe(code, 'en-US');
		expect(message).not.toBe(code);
		expect(message).not.toBe(ErrorCodeToI18nKey[code as keyof typeof ErrorCodeToI18nKey]);
		expect(message.length).toBeGreaterThan(0);
	});
	it.each(PHONE_GATE_CODES)('%s has a case in the required-action client switch', (code) => {
		const source = readFileSync(CLIENT_SWITCH_PATH, 'utf8');
		expect(source).toContain(`case APIErrorCodes.${code}:`);
	});
	it('keeps the two fraud reasons on one shared code', () => {
		expect(errorForPhoneRejectReason('sms_pumping_risk_high').code).toBe(
			errorForPhoneRejectReason('behavioural_risk_blocked').code,
		);
	});
	it('gives every benign reason a code the fraud reasons do not use', () => {
		const review = errorForPhoneRejectReason('sms_pumping_risk_high').code;
		const benign = ALL_REJECT_REASONS.filter(
			(reason) => reason !== 'sms_pumping_risk_high' && reason !== 'behavioural_risk_blocked',
		).map((reason) => errorForPhoneRejectReason(reason).code);
		expect(benign).not.toContain(review);
	});
});
