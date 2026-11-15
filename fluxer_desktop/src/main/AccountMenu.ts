// SPDX-License-Identifier: AGPL-3.0-or-later

import {isolateBidiText} from '@electron/common/BidiIsolation';
import type {AccountEngineLabel} from '@electron/main/AccountEngineHost';
import type {AccountEngineAccountSummary} from '@fluxer/desktop_ipc/src/AccountEngineContract';

interface AccountMenuEntry {
	readonly accountKey: string;
	readonly label: string;
	readonly checked: boolean;
}

interface AccountMenuActions {
	readonly switchAccount: (accountKey: string) => void;
	readonly openAccountSwitcher: () => void;
}

type Translate = (key: string, vars?: Record<string, string>) => string;

function accountUserId(accountKey: string): string {
	const index = accountKey.lastIndexOf('::');
	return index === -1 ? accountKey : accountKey.slice(index + 2);
}

function compareSwitcherOrder(a: AccountEngineLabel, b: AccountEngineLabel): number {
	if (a.switcherPosition !== null && b.switcherPosition !== null && a.switcherPosition !== b.switcherPosition) {
		return a.switcherPosition - b.switcherPosition;
	}
	if (a.switcherPosition !== null && b.switcherPosition === null) {
		return -1;
	}
	if (a.switcherPosition === null && b.switcherPosition !== null) {
		return 1;
	}
	return compareAccountAge(a, b);
}

function compareAccountAge(a: AccountEngineLabel, b: AccountEngineLabel): number {
	const aUserId = accountUserId(a.accountKey);
	const bUserId = accountUserId(b.accountKey);
	if (aUserId.length !== bUserId.length) {
		return aUserId.length - bUserId.length;
	}
	if (aUserId !== bUserId) {
		return aUserId < bUserId ? -1 : 1;
	}
	if (a.accountKey === b.accountKey) {
		return 0;
	}
	return a.accountKey < b.accountKey ? -1 : 1;
}

export function buildAccountMenuEntries(
	labels: ReadonlyArray<AccountEngineLabel>,
	summaries: ReadonlyArray<AccountEngineAccountSummary>,
	translate: Translate,
	{hidePersonalInformation = false}: {hidePersonalInformation?: boolean} = {},
): ReadonlyArray<AccountMenuEntry> {
	if (labels.length < 2) {
		return [];
	}
	if (hidePersonalInformation) {
		return [...labels].sort(compareSwitcherOrder).map((entry, index) => ({
			accountKey: entry.accountKey,
			label: translate('desktop.tray.accountNumbered', {number: String(index + 1)}),
			checked: entry.isForeground,
		}));
	}
	const severalInstances = new Set(labels.map((label) => label.instanceHost)).size > 1;
	const mentionsByAccount = new Map(summaries.map((summary) => [summary.accountKey, summary.mentionCount]));
	return [...labels].sort(compareSwitcherOrder).map((entry) => {
		const name = isolateBidiText(entry.displayName);
		const named = severalInstances
			? translate('desktop.tray.accountWithHost', {name, host: isolateBidiText(entry.instanceHost)})
			: name;
		const mentions = entry.isForeground ? 0 : (mentionsByAccount.get(entry.accountKey) ?? 0);
		return {
			accountKey: entry.accountKey,
			label:
				mentions > 0 ? translate('desktop.tray.accountWithMentions', {label: named, count: String(mentions)}) : named,
			checked: entry.isForeground,
		};
	});
}

export function buildAccountMenuItems(
	entries: ReadonlyArray<AccountMenuEntry>,
	actions: AccountMenuActions,
	translate: Translate,
): Array<Electron.MenuItemConstructorOptions> {
	if (entries.length === 0) {
		return [];
	}
	return [
		...entries.map(
			(entry): Electron.MenuItemConstructorOptions => ({
				label: entry.label,
				type: 'radio',
				checked: entry.checked,
				click: () => actions.switchAccount(entry.accountKey),
			}),
		),
		{type: 'separator'},
		{label: translate('desktop.tray.chooseAccount'), click: actions.openAccountSwitcher},
	];
}

export function createRefreshThrottle(
	refresh: () => void,
	intervalMs: number,
): {schedule: () => void; cancel: () => void} {
	let timer: NodeJS.Timeout | null = null;
	let pending = false;
	const arm = (): void => {
		timer = setTimeout(() => {
			timer = null;
			if (!pending) {
				return;
			}
			pending = false;
			refresh();
			arm();
		}, intervalMs);
		timer.unref?.();
	};
	return {
		schedule: () => {
			if (timer !== null) {
				pending = true;
				return;
			}
			refresh();
			arm();
		},
		cancel: () => {
			if (timer !== null) {
				clearTimeout(timer);
			}
			timer = null;
			pending = false;
		},
	};
}
