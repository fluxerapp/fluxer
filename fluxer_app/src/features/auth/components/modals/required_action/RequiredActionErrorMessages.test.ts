// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {setupI18n} from '@lingui/core';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

vi.mock('@app/features/app/config/Config', () => ({
	default: {
		PUBLIC_BUILD_VERSION: 'test',
		PUBLIC_RELEASE_CHANNEL: 'canary',
		PUBLIC_BOOTSTRAP_API_ENDPOINT: 'https://example.invalid',
		PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: 'https://example.invalid',
	},
}));
vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? '', locale: 'en'}}),
}));
vi.mock('@app/features/app/components/alerts/GenericErrorModalCommands', () => ({showGenericErrorModal: vi.fn()}));
vi.mock('@app/features/app/components/dialogs/Modal', () => ({}));
vi.mock('@app/features/app/components/shared/ExternalLink', () => ({ExternalLink: () => null}));
vi.mock('@app/features/ui/button/Button', () => ({Button: () => null}));
vi.mock('@app/features/ui/commands/ToastCommands', () => ({showToast: vi.fn()}));
vi.mock('@app/features/ui/components/form/Form', () => ({Form: () => null}));
vi.mock('@app/features/ui/components/form/FormInput', () => ({Input: () => null}));
vi.mock('@app/features/auth/state/SudoPrompt', () => ({isAbortError: () => false}));

const {APIErrorCodes} = await import('@fluxer/constants/src/ApiErrorCodes');
const {HttpError} = await import('@app/features/platform/types/EndpointError');
const {resolveRequiredActionErrorMessage} = await import('./RequiredActionShared');
const {
	ENTER_VALID_PHONE_DESCRIPTOR,
	PHONE_CANNOT_BE_USED_DESCRIPTOR,
	PHONE_COUNTRY_NOT_SUPPORTED_DESCRIPTOR,
	PHONE_INBOUND_REQUIRED_DESCRIPTOR,
	PHONE_LOOKUP_UNAVAILABLE_DESCRIPTOR,
	PHONE_NEEDS_REVIEW_DESCRIPTOR,
	PHONE_NOT_IN_SERVICE_DESCRIPTOR,
	PHONE_NOT_MOBILE_DESCRIPTOR,
	SOMETHING_WENT_WRONG_TRY_AGAIN_DESCRIPTOR,
	TOO_MANY_ATTEMPTS_DESCRIPTOR,
} = await import('./RequiredActionDescriptors');

const i18n = setupI18n({locale: 'en', messages: {en: {}}});

function apiError(code: string, body: Record<string, unknown> = {}) {
	return new HttpError({
		method: 'POST',
		path: '/auth/verify-phone',
		status: 400,
		body: {code, message: 'server message', ...body},
		responseHeaders: {},
	});
}

const PHONE_REJECTION_CASES = [
	[APIErrorCodes.PHONE_COUNTRY_NOT_SUPPORTED, PHONE_COUNTRY_NOT_SUPPORTED_DESCRIPTOR],
	[APIErrorCodes.PHONE_NUMBER_NOT_IN_SERVICE, PHONE_NOT_IN_SERVICE_DESCRIPTOR],
	[APIErrorCodes.PHONE_NUMBER_NOT_MOBILE, PHONE_NOT_MOBILE_DESCRIPTOR],
	[APIErrorCodes.PHONE_LOOKUP_UNAVAILABLE, PHONE_LOOKUP_UNAVAILABLE_DESCRIPTOR],
	[APIErrorCodes.PHONE_INBOUND_VERIFICATION_REQUIRED, PHONE_INBOUND_REQUIRED_DESCRIPTOR],
	[APIErrorCodes.PHONE_VERIFICATION_NEEDS_REVIEW, PHONE_NEEDS_REVIEW_DESCRIPTOR],
] as const;

describe('resolveRequiredActionErrorMessage', () => {
	for (const [code, descriptor] of PHONE_REJECTION_CASES) {
		for (const context of ['phone-number', 'phone-code'] as const) {
			it(`explains ${code} on the ${context} step`, () => {
				const message = resolveRequiredActionErrorMessage(i18n, apiError(code), context);
				expect(message).toBe(i18n._(descriptor));
				expect(message).not.toBe(i18n._(SOMETHING_WENT_WRONG_TRY_AGAIN_DESCRIPTOR));
			});
		}
	}

	it('never blames the number when our own lookup is down', () => {
		const message = resolveRequiredActionErrorMessage(
			i18n,
			apiError(APIErrorCodes.PHONE_LOOKUP_UNAVAILABLE),
			'phone-number',
		);
		expect(message).not.toContain('invalid');
		expect(message).toContain('try the same number again');
	});

	it('keeps the fraud message free of any distinguishing detail', () => {
		const message = resolveRequiredActionErrorMessage(
			i18n,
			apiError(APIErrorCodes.PHONE_VERIFICATION_NEEDS_REVIEW),
			'phone-number',
		);
		expect(message).toBe(i18n._(PHONE_NEEDS_REVIEW_DESCRIPTOR));
		expect(message).toContain('Contact support');
	});

	it('keeps the existing invalid-number copy per step', () => {
		expect(resolveRequiredActionErrorMessage(i18n, apiError(APIErrorCodes.INVALID_PHONE_NUMBER), 'phone-number')).toBe(
			i18n._(ENTER_VALID_PHONE_DESCRIPTOR),
		);
		expect(resolveRequiredActionErrorMessage(i18n, apiError(APIErrorCodes.INVALID_PHONE_NUMBER), 'phone-code')).toBe(
			i18n._(PHONE_CANNOT_BE_USED_DESCRIPTOR),
		);
	});

	it('still falls back to the generic message for a code nothing maps', () => {
		expect(resolveRequiredActionErrorMessage(i18n, apiError('SOME_UNMAPPED_CODE'), 'phone-number')).toBe(
			i18n._(SOMETHING_WENT_WRONG_TRY_AGAIN_DESCRIPTOR),
		);
	});

	it('lets a rate limit win over a new phone rejection code', () => {
		const message = resolveRequiredActionErrorMessage(
			i18n,
			apiError(APIErrorCodes.PHONE_VERIFICATION_NEEDS_REVIEW, {retry_after: 86400}),
			'phone-number',
		);
		expect(message).toBe(i18n._(TOO_MANY_ATTEMPTS_DESCRIPTOR));
	});
});

const PHONE_GATE_CODES = [
	APIErrorCodes.INVALID_PHONE_NUMBER,
	APIErrorCodes.PHONE_COUNTRY_NOT_SUPPORTED,
	APIErrorCodes.PHONE_INBOUND_VERIFICATION_REQUIRED,
	APIErrorCodes.PHONE_LOOKUP_UNAVAILABLE,
	APIErrorCodes.PHONE_NUMBER_NOT_IN_SERVICE,
	APIErrorCodes.PHONE_NUMBER_NOT_MOBILE,
	APIErrorCodes.PHONE_VERIFICATION_NEEDS_REVIEW,
] as const;

describe('phone gate code coverage', () => {
	for (const context of ['phone-number', 'phone-code'] as const) {
		it(`no phone gate code falls through to the generic message on the ${context} step`, () => {
			const generic = i18n._(SOMETHING_WENT_WRONG_TRY_AGAIN_DESCRIPTOR);
			for (const code of PHONE_GATE_CODES) {
				expect(resolveRequiredActionErrorMessage(i18n, apiError(code), context)).not.toBe(generic);
			}
		});
	}
	it('gives each of the six new codes its own message', () => {
		const messages = PHONE_REJECTION_CASES.map(([code]) =>
			resolveRequiredActionErrorMessage(i18n, apiError(code), 'phone-number'),
		);
		expect(new Set(messages).size).toBe(PHONE_REJECTION_CASES.length);
	});
	it('never tells a user their working number is invalid for a benign rejection', () => {
		const benign = [
			APIErrorCodes.PHONE_COUNTRY_NOT_SUPPORTED,
			APIErrorCodes.PHONE_LOOKUP_UNAVAILABLE,
			APIErrorCodes.PHONE_INBOUND_VERIFICATION_REQUIRED,
		];
		for (const code of benign) {
			const message = resolveRequiredActionErrorMessage(i18n, apiError(code), 'phone-number');
			expect(message).not.toBe(i18n._(ENTER_VALID_PHONE_DESCRIPTOR));
			expect(message).not.toBe(i18n._(PHONE_CANNOT_BE_USED_DESCRIPTOR));
		}
	});
	it('routes every dead-end rejection somewhere a person can act', () => {
		const routed = [
			APIErrorCodes.PHONE_COUNTRY_NOT_SUPPORTED,
			APIErrorCodes.PHONE_NUMBER_NOT_IN_SERVICE,
			APIErrorCodes.PHONE_NUMBER_NOT_MOBILE,
			APIErrorCodes.PHONE_VERIFICATION_NEEDS_REVIEW,
		];
		for (const code of routed) {
			expect(resolveRequiredActionErrorMessage(i18n, apiError(code), 'phone-number')).toContain('support');
		}
	});
});
