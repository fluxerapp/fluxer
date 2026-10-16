// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {getAccountDisplayLabels} from '@app/features/auth/AccountDisplayUtils';
import {FORGOT_YOUR_PASSWORD_DESCRIPTOR} from '@app/features/auth/AuthMessageDescriptors';
import {accountSignInIdentifier} from '@app/features/auth/utils/AccountSignInIdentifier';
import {
	CREATE_ACCOUNT_DESCRIPTOR,
	SIGN_IN_DESCRIPTOR,
	TWO_FACTOR_AUTHENTICATION_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Account} from '@app/features/platform/state/AuthSession';
import type {MessageDescriptor} from '@lingui/core';

export const AccountSwitcherAuthPurposeType = Object.freeze({
	ADD: 'add',
	RELOGIN: 'relogin',
} as const);

export type AccountSwitcherAuthPurposeType =
	(typeof AccountSwitcherAuthPurposeType)[keyof typeof AccountSwitcherAuthPurposeType];

export type AccountSwitcherAuthPurpose =
	| {readonly type: typeof AccountSwitcherAuthPurposeType.ADD}
	| {readonly type: typeof AccountSwitcherAuthPurposeType.RELOGIN; readonly account: Account};

export const AccountSwitcherAuthStep = Object.freeze({
	LOGIN: 'login',
	REGISTER: 'register',
	FORGOT: 'forgot',
	MFA: 'mfa',
} as const);

export type AccountSwitcherAuthStep = (typeof AccountSwitcherAuthStep)[keyof typeof AccountSwitcherAuthStep];

export const ACCOUNT_SWITCHER_AUTH_STEP_ORDER: ReadonlyArray<AccountSwitcherAuthStep> = Object.freeze([
	AccountSwitcherAuthStep.LOGIN,
	AccountSwitcherAuthStep.REGISTER,
	AccountSwitcherAuthStep.FORGOT,
	AccountSwitcherAuthStep.MFA,
]);

export function getAccountSwitcherAuthStepLabel(step: AccountSwitcherAuthStep): MessageDescriptor {
	switch (step) {
		case AccountSwitcherAuthStep.REGISTER:
			return CREATE_ACCOUNT_DESCRIPTOR;
		case AccountSwitcherAuthStep.FORGOT:
			return FORGOT_YOUR_PASSWORD_DESCRIPTOR;
		case AccountSwitcherAuthStep.MFA:
			return TWO_FACTOR_AUTHENTICATION_DESCRIPTOR;
		default:
			return SIGN_IN_DESCRIPTOR;
	}
}

export function getInitialRuntimeSnapshot(purpose: AccountSwitcherAuthPurpose): RuntimeConfigSnapshot | null {
	if (purpose.type !== AccountSwitcherAuthPurposeType.RELOGIN) {
		return null;
	}
	const instance = purpose.account.instance;
	if (instance == null) {
		return null;
	}
	return instance;
}

export function getReloginIdentifier(purpose: AccountSwitcherAuthPurpose): string | null {
	if (purpose.type !== AccountSwitcherAuthPurposeType.RELOGIN) {
		return null;
	}
	const identifier = accountSignInIdentifier(purpose.account);
	if (identifier == null || identifier === '') {
		return null;
	}
	return identifier;
}

export function getReloginAccountLabel(purpose: AccountSwitcherAuthPurpose): string | null {
	if (purpose.type !== AccountSwitcherAuthPurposeType.RELOGIN) {
		return null;
	}
	const labels = getAccountDisplayLabels(purpose.account);
	if (!labels.available) {
		return null;
	}
	if (labels.displayLabel === '') {
		return null;
	}
	return labels.displayLabel;
}
