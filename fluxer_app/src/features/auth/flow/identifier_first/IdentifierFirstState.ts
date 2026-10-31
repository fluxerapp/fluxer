// SPDX-License-Identifier: AGPL-3.0-or-later

import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import {InstantAccountAvailability} from '@fluxer/constants/src/AccountConfigConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {IdentifyResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import type {InstanceFeatures} from '@fluxer/schema/src/domains/instance/InstanceSchemas';

const EMAIL_CODE_LENGTH = 6;

type EmailCodePurpose = 'sign_in' | 'sign_up';

interface SentCode {
	readonly email: string;
	readonly ticket: string;
	readonly purpose: EmailCodePurpose;
	readonly existingAccount: boolean;
	readonly resendAvailableAt: number;
}

export type IdentifierStep =
	| {readonly kind: 'email'}
	| {readonly kind: 'name'}
	| ({readonly kind: 'profile'} & SentCode)
	| ({readonly kind: 'code'} & SentCode)
	| {readonly kind: 'password'; readonly email: string; readonly ticket: string | null}
	| {readonly kind: 'create_password'; readonly email: string};

export type IdentifyDecision =
	| {readonly kind: 'send_code'}
	| {readonly kind: 'password'}
	| {readonly kind: 'create_password'}
	| {readonly kind: 'registration_closed'};

export function decideAfterIdentify(
	response: IdentifyResponse,
	emailCodes: boolean,
	hasRegistrationLink = false,
): IdentifyDecision {
	if (response.account === 'existing') {
		return emailCodes && response.sign_in_method === 'code' ? {kind: 'send_code'} : {kind: 'password'};
	}
	if (response.registration?.mode === 'closed' && !hasRegistrationLink) {
		return {kind: 'registration_closed'};
	}
	if (!emailCodes || response.registration?.requires_password === true) {
		return {kind: 'create_password'};
	}
	return {kind: 'send_code'};
}

export function needsPasswordStep(error: unknown): boolean {
	return failureCode(error) === APIErrorCodes.PASSWORD_SIGN_IN_REQUIRED;
}

export function normalizeEmailCode(value: string): string {
	return value.replace(/\D/g, '').slice(0, EMAIL_CODE_LENGTH);
}

export function isCompleteEmailCode(value: string): boolean {
	return value.length === EMAIL_CODE_LENGTH;
}

export function secondsUntil(timestamp: number, now: number): number {
	return Math.max(0, Math.ceil((timestamp - now) / 1000));
}

export function parseIsoTimestamp(value: string, fallback: number): number {
	const parsed = Date.parse(value);
	return Number.isNaN(parsed) ? fallback : parsed;
}

export function offersInstantAccount(
	features: Pick<InstanceFeatures, 'instant_accounts' | 'instant_account_availability'>,
	registrationUrlCode: string | null,
): boolean {
	const availability =
		features.instant_account_availability ??
		(features.instant_accounts === true ? InstantAccountAvailability.AVAILABLE : InstantAccountAvailability.DISABLED);
	if (availability === InstantAccountAvailability.AVAILABLE) return true;
	return (
		registrationUrlCode !== null &&
		(availability === InstantAccountAvailability.REGISTRATION_CLOSED ||
			availability === InstantAccountAvailability.APPROVAL_REQUIRED)
	);
}
