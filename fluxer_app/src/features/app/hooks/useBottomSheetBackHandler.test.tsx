// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {createMemoryHistory} from '@app/features/platform/components/router/RouterHistory';
import {AccountScopedWork, AccountScopedWorkTransitionReason} from '@app/features/platform/state/AccountScopedWork';
import {act, useState} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, expect, test, vi} from 'vitest';

const SHEET_URL = 'http://localhost/channels/100/200';
const INCOMING_ACCOUNT_URL = 'http://localhost/channels/300/400';

const router = vi.hoisted(() => ({history: null as ReturnType<typeof createMemoryHistory> | null}));

vi.mock('@app/features/navigation/utils/RouterUtils', () => ({getHistory: () => router.history}));

const {useBottomSheetBackHandler} = await import('@app/features/app/hooks/useBottomSheetBackHandler');
const {useAccountTransitionDismissal} = await import('@app/features/platform/hooks/useAccountTransitionDismissal');

let root: Root | null = null;
let container: HTMLElement | null = null;
let closeSheet: (() => void) | null = null;

function Sheet() {
	const [open, setOpen] = useState(true);
	closeSheet = () => setOpen(false);
	useAccountTransitionDismissal(open, closeSheet);
	useBottomSheetBackHandler(open, closeSheet);
	return null;
}

function requireHistory(): ReturnType<typeof createMemoryHistory> {
	if (router.history === null) {
		throw new Error('The test history is not installed');
	}
	return router.history;
}

function sheetMarker(): unknown {
	const state = requireHistory().getLocation().state;
	return state !== null && typeof state === 'object' && 'bottomSheet' in state ? state.bottomSheet : undefined;
}

beforeEach(() => {
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	(window as unknown as {happyDOM: {setURL(url: string): void}}).happyDOM.setURL(SHEET_URL);
	router.history = createMemoryHistory(SHEET_URL);
	container = document.createElement('div');
	document.body.appendChild(container);
	root = createRoot(container);
});

afterEach(() => {
	act(() => root?.unmount());
	container?.remove();
	root = null;
	container = null;
	closeSheet = null;
	router.history = null;
});

test('closing a sheet by hand steps back over its history entry', () => {
	const history = requireHistory();
	const back = vi.spyOn(history, 'back');
	act(() => root?.render(<Sheet />));
	expect(sheetMarker()).toBeTypeOf('string');
	act(() => closeSheet?.());
	expect(back).toHaveBeenCalledTimes(1);
	expect(history.location.href).toBe(SHEET_URL);
	expect(sheetMarker()).toBeUndefined();
});

test('a sheet dismissed by an account switch leaves navigation to the switch', async () => {
	const history = requireHistory();
	const back = vi.spyOn(history, 'back');
	act(() => root?.render(<Sheet />));
	expect(sheetMarker()).toBeTypeOf('string');
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = false;
	await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {
		await vi.waitFor(() => expect(sheetMarker()).toBeUndefined());
		history.replace(new URL(INCOMING_ACCOUNT_URL));
	});
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	expect(back).not.toHaveBeenCalled();
	expect(history.location.href).toBe(INCOMING_ACCOUNT_URL);
});
