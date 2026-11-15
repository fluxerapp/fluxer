// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import AccountEngine from '@app/features/account_engine/state/AccountEngine';
import type {HandlerHost} from '@app/features/app/keybindings/keybind_manager/handlers/types';
import {pickNewestOtherAccountMention} from '@app/features/app/keybindings/utils/AccountMentionTarget';
import {toggleAccountSwitcherModal} from '@app/features/auth/commands/AccountSwitcherModalCommands';
import {focusNewAccountViewAfterSwitch} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherReturnFocus';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {
	adjacentSwitcherAccount,
	mostRecentOtherAccount,
	switcherAccounts,
} from '@app/features/auth/state/AccountSwitcherAccounts';
import Accounts from '@app/features/auth/state/Accounts';
import {switchToSavedAccount} from '@app/features/auth/utils/AccountSwitcherModalUtils';
import {ACCOUNT_SWITCHER_SLOT_ACTIONS} from '@app/features/input/state/input_keybind/KeybindCommands';
import type {Account} from '@app/features/platform/state/AuthSession';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const NO_NEW_MENTIONS_ON_OTHER_ACCOUNTS_DESCRIPTOR = msg({
	message: 'No new mentions on your other accounts.',
	comment: 'Toast shown when the jump to mention shortcut finds no unread mention on any other signed-in account.',
});

function routeForAccountChannel(guildId: string | null, channelId: string, messageId: string | null): string {
	if (guildId === null) {
		return messageId === null ? Routes.dmChannel(channelId) : Routes.dmChannelMessage(channelId, messageId);
	}
	return messageId === null
		? Routes.guildChannel(guildId, channelId)
		: Routes.channelMessage(guildId, channelId, messageId);
}

async function switchToAccount(account: Account | null): Promise<void> {
	if (account === null) return;
	const previousAccountKey = Accounts.currentAccountKey;
	await switchToSavedAccount({accountKey: getAccountKey(account)});
	if (Accounts.currentAccountKey !== previousAccountKey) focusNewAccountViewAfterSwitch(document);
}

function otherAccountKeys(): Array<string> {
	const currentAccountKey = Accounts.currentAccountKey;
	return Accounts.getAllAccounts()
		.map((account) => getAccountKey(account))
		.filter((accountKey) => accountKey !== currentAccountKey);
}

async function jumpToNewestOtherAccountMention(i18n: I18n): Promise<void> {
	const accountKeys = otherAccountKeys();
	const activities =
		accountKeys.length === 0 ? [] : await AccountEngine.loadActivity({accountKeys, channelLimit: 25, mentionLimit: 10});
	const target = pickNewestOtherAccountMention(activities, Accounts.currentAccountKey);
	if (target === null) {
		ToastCommands.createToast({type: 'info', children: i18n._(NO_NEW_MENTIONS_ON_OTHER_ACCOUNTS_DESCRIPTOR)});
		return;
	}
	await switchToSavedAccount({
		accountKey: target.accountKey,
		redirectAfterSwitch: routeForAccountChannel(target.guildId, target.channelId, target.messageId),
	});
}

export function registerAccountKeybindHandlers(host: HandlerHost, i18n: I18n): void {
	host.register('system_open_account_switcher', ({type}) => {
		if (type !== 'press') return;
		toggleAccountSwitcherModal();
	});
	host.register('system_switch_account_next', ({type}) => {
		if (type !== 'press') return;
		void switchToAccount(adjacentSwitcherAccount(1));
	});
	host.register('system_switch_account_prev', ({type}) => {
		if (type !== 'press') return;
		void switchToAccount(adjacentSwitcherAccount(-1));
	});
	host.register('system_switch_account_recent', ({type}) => {
		if (type !== 'press') return;
		void switchToAccount(mostRecentOtherAccount());
	});
	host.register('system_jump_account_mention', ({type}) => {
		if (type !== 'press') return;
		void jumpToNewestOtherAccountMention(i18n).catch((error: unknown) => {
			host.logger.error('Failed to jump to the newest mention on another account', error);
		});
	});
	host.register('nav_quick_switcher_accounts', ({type}) => {
		if (type !== 'press') return;
		if (QuickSwitcher.getIsOpen()) QuickSwitcher.hide();
		else QuickSwitcher.show({scope: 'other_accounts'});
	});
	for (const [slotIndex, action] of ACCOUNT_SWITCHER_SLOT_ACTIONS.entries()) {
		host.register(action, ({type}) => {
			if (type !== 'press') return;
			void switchToAccount(switcherAccounts()[slotIndex] ?? null);
		});
	}
}
