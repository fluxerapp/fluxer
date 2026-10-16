// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AccountSwitcherAuthPurpose} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthTypes';
import type {Account} from '@app/features/platform/state/AuthSession';

export const AccountSwitcherView = Object.freeze({
	ACCOUNTS: 'accounts',
	AUTH: 'auth',
	CONFIRM: 'confirm',
} as const);

export type AccountSwitcherView = (typeof AccountSwitcherView)[keyof typeof AccountSwitcherView];

export type AccountSwitcherContentView = Exclude<AccountSwitcherView, typeof AccountSwitcherView.CONFIRM>;

export const AccountSwitcherActionType = Object.freeze({
	CURRENT: 'current',
	SAVED: 'saved',
} as const);

export type AccountSwitcherActionType = (typeof AccountSwitcherActionType)[keyof typeof AccountSwitcherActionType];

export interface ConfirmAction {
	readonly type: AccountSwitcherActionType;
	readonly account: Account;
}

export type AccountSwitcherViewState =
	| {readonly view: typeof AccountSwitcherView.ACCOUNTS}
	| {readonly view: typeof AccountSwitcherView.AUTH; readonly purpose: AccountSwitcherAuthPurpose}
	| {readonly view: typeof AccountSwitcherView.CONFIRM; readonly action: ConfirmAction};
