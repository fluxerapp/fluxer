// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const CHOOSE_AN_ACCOUNT_DESCRIPTOR = msg({
	message: 'Choose an account',
	comment: 'Heading of the full-screen account switcher, above the grid of saved accounts.',
});
export const MANAGE_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Manage accounts',
	comment:
		'Button under the account grid in the account switcher that turns on manage mode, and the heading shown while manage mode is on.',
});
export const SLOT_KEYS_HINT_DESCRIPTOR = msg({
	message: 'Press 1 to 9 to switch directly.',
	comment:
		'Hint under the account switcher heading. Pressing a number key from 1 to 9 switches to the account in that position.',
});
export const OPEN_SHORTCUT_HINT_DESCRIPTOR = msg({
	message: 'Open this anytime with',
	comment:
		'Hint under the account switcher heading, followed by the keyboard shortcut that opens the account switcher.',
});
export const MANAGE_HINT_DESCRIPTOR = msg({
	message: 'Choose an account to manage it, or drag accounts to reorder them.',
	comment:
		'Hint under the account switcher heading while manage mode is on. The order of accounts also sets which number key switches to each one.',
});
export const MOVE_LEFT_DESCRIPTOR = msg({
	message: 'Move left',
	comment:
		'Menu item for one account in the account switcher. It moves the account one place to the left in the grid, which also changes its number key.',
});
export const MOVE_RIGHT_DESCRIPTOR = msg({
	message: 'Move right',
	comment:
		'Menu item for one account in the account switcher. It moves the account one place to the right in the grid, which also changes its number key.',
});
export const ACCOUNT_MOVED_DESCRIPTOR = msg({
	message: 'Moved {displayName} to position {position} of {count}',
	comment:
		'Screen reader announcement after an account is moved in the account switcher, by dragging, by the Move left or Move right menu items, or with Alt and an arrow key. displayName is the account name, position and count are numbers.',
});
export const SEARCH_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Search accounts',
	comment:
		'Placeholder and accessible label of the search field above the account grid in the account switcher. It matches account names, usernames and instance hosts.',
});
export const NO_ACCOUNTS_MATCH_DESCRIPTOR = msg({
	message: 'No accounts match "{query}".',
	comment: 'Shown in the account switcher when the account search finds nothing. query is what the user typed.',
});
export const CURRENT_ACCOUNT_DESCRIPTOR = msg({
	message: 'Current account',
	comment: 'Screen reader status for the account in use, read out on its tile in the account switcher.',
});
export const RECONNECTING_DESCRIPTOR = msg({
	message: 'Reconnecting',
	comment:
		'Status under a saved account in the account switcher while the app tries to reconnect that account in the background.',
});
export const CONNECTION_OFFLINE_DESCRIPTOR = msg({
	message: 'Offline',
	comment:
		'Status under a saved account in the account switcher when the app cannot reach its instance because the device is offline.',
});
export const HAS_UNREAD_MESSAGES_DESCRIPTOR = msg({
	message: 'Unread messages',
	comment:
		'Screen reader status for a saved account that has unread messages but no mentions, in the account switcher.',
});
export const SLOT_POSITION_DESCRIPTOR = msg({
	message: 'Press {slot}',
	comment:
		'Screen reader hint on an account tile in the account switcher. Pressing that number key switches to the account. slot is a number from 1 to 9.',
});
export const ON_INSTANCE_DESCRIPTOR = msg({
	message: 'on {instanceLabel}',
	comment:
		'Screen reader part of an account tile in the account switcher, naming the instance the account is on. instanceLabel is an instance name or host.',
});
