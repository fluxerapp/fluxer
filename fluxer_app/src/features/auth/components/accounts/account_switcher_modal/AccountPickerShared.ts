// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const SAVED_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Saved accounts',
	comment: 'Accessible label for the saved account list.',
});
export const SESSION_EXPIRED_DESCRIPTOR = msg({
	message: 'Session expired',
	comment: 'Short status label for a saved account whose session token has expired.',
});
export const USE_ANOTHER_ACCOUNT_DESCRIPTOR = msg({
	message: 'Use another account',
	comment: 'Button under the saved account list that opens a sign-in form for an account that is not in the list.',
});
export const SIGN_IN_AGAIN_DESCRIPTOR = msg({
	message: 'Sign in again',
	comment:
		'Menu item for a saved account whose session has expired, in the account list and in the account switcher. It opens the sign-in form for that account.',
});
export const CONFIRM_SIGN_OUT_DESCRIPTOR = msg({
	message: 'Sign out of {displayName}?',
	comment:
		'Question shown inside an account row, or in the account switcher menu for one account, before signing that account out of this device. displayName is the account name.',
});
export const ACCOUNT_OPTIONS_DESCRIPTOR = msg({
	message: 'Options for {displayName}',
	comment:
		'Accessible label for the menu of sign-in and sign-out actions for one saved account, in the account list and in the account switcher. displayName is the account name.',
});
export const SIGN_OUT_EXPIRED_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Sign out of all expired accounts',
	comment:
		'Button under the saved account list, and in the account switcher while managing accounts, that signs out every saved account whose session has expired, except the account in use. Only shown when two or more of those sessions have expired.',
});
