// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAccountTransitionDismissal} from '@app/features/platform/hooks/useAccountTransitionDismissal';
import {AccountScopedWork, AccountScopedWorkTransitionReason} from '@app/features/platform/state/AccountScopedWork';
import {act, useState} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, expect, test} from 'vitest';

let root: Root | null = null;
let container: HTMLElement | null = null;

function AccountBoundSheet({onOpenChange}: {onOpenChange: (open: boolean) => void}) {
	const [open, setOpen] = useState(true);
	useAccountTransitionDismissal(open, () => {
		setOpen(false);
		onOpenChange(false);
	});
	return open ? (
		<div data-testid="sheet" data-flx="platform.use-account-transition-dismissal-test.account-bound-sheet.sheet" />
	) : null;
}

function switchAccounts(): Promise<void> {
	return AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {});
}

beforeEach(() => {
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	container = document.createElement('div');
	document.body.appendChild(container);
	root = createRoot(container);
});

afterEach(() => {
	act(() => root?.unmount());
	container?.remove();
	root = null;
	container = null;
});

test('a surface opened under one account is dismissed when the account changes', async () => {
	const changes: Array<boolean> = [];
	act(() =>
		root?.render(
			<AccountBoundSheet
				onOpenChange={(open) => changes.push(open)}
				data-flx="platform.use-account-transition-dismissal-test.account-bound-sheet"
			/>,
		),
	);
	expect(container?.querySelector('[data-testid="sheet"]')).not.toBeNull();
	await act(switchAccounts);
	expect(changes).toEqual([false]);
	expect(container?.querySelector('[data-testid="sheet"]')).toBeNull();
});

test('a dismissed surface no longer reacts to later account changes', async () => {
	const changes: Array<boolean> = [];
	act(() =>
		root?.render(
			<AccountBoundSheet
				onOpenChange={(open) => changes.push(open)}
				data-flx="platform.use-account-transition-dismissal-test.account-bound-sheet--2"
			/>,
		),
	);
	await act(switchAccounts);
	await act(switchAccounts);
	expect(changes).toEqual([false]);
});

test('an unmounted surface releases its registration', async () => {
	const changes: Array<boolean> = [];
	act(() =>
		root?.render(
			<AccountBoundSheet
				onOpenChange={(open) => changes.push(open)}
				data-flx="platform.use-account-transition-dismissal-test.account-bound-sheet--3"
			/>,
		),
	);
	act(() => root?.render(null));
	await act(switchAccounts);
	expect(changes).toEqual([]);
});
