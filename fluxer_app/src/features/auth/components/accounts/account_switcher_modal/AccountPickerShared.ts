// SPDX-License-Identifier: AGPL-3.0-or-later

import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {ACTIVE_ACCOUNT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Account} from '@app/features/platform/state/AuthSession';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const SAVED_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Saved accounts',
	comment: 'Accessible label for the saved account list.',
});
const SAVED_ACCOUNT_DESCRIPTOR = msg({
	message: 'Saved account',
	comment: 'Short status label for a saved account that is not currently active.',
});
export const SESSION_EXPIRED_DESCRIPTOR = msg({
	message: 'Session expired',
	comment: 'Short status label for a saved account whose session token has expired.',
});

export interface AccountPickerContext {
	readonly accountKey: string;
	readonly isCurrent: boolean;
	readonly isExpired: boolean;
}

export type AccountPickerDisabledPredicate = (account: Account, context: AccountPickerContext) => boolean;

export function getAccountPickerContext(account: Account, currentAccountKey: string | null): AccountPickerContext {
	const accountKey = getAccountKey(account);
	return {
		accountKey,
		isCurrent: accountKey === currentAccountKey,
		isExpired: account.isValid === false,
	};
}

export function resolveAccountPickerDisabled(
	value: boolean | AccountPickerDisabledPredicate,
	account: Account,
	context: AccountPickerContext,
): boolean {
	if (typeof value === 'function') {
		return value(account, context);
	}
	return value;
}

export function getAccountPickerStatusDescriptor(context: AccountPickerContext): MessageDescriptor {
	if (context.isExpired) {
		return SESSION_EXPIRED_DESCRIPTOR;
	}
	if (context.isCurrent) {
		return ACTIVE_ACCOUNT_DESCRIPTOR;
	}
	return SAVED_ACCOUNT_DESCRIPTOR;
}
