// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {useOptOutSelection} from '@app/features/auth/components/pages/oauth_authorize_page/hooks/useOptOutSelection';
import {useScopeSelection} from '@app/features/auth/components/pages/oauth_authorize_page/hooks/useScopeSelection';
import {act} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

let root: Root;
let node: HTMLDivElement;
let scopes: ReturnType<typeof useScopeSelection>;
let selection: ReturnType<typeof useOptOutSelection>;
const neverLocked = () => false;
const lockedEmail = (key: string) => key === 'email';
const requested = ['bot', 'identify', 'email'];
function Probe({locked = neverLocked}: {locked?: (key: string) => boolean}) {
	scopes = useScopeSelection(requested);
	selection = useOptOutSelection(requested, locked);
	return null;
}
beforeEach(async () => {
	node = document.createElement('div');
	document.body.append(node);
	root = createRoot(node);
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	await act(async () => root.render(<Probe data-flx="auth.oauth-authorize-page.use-opt-out-selection-test.probe" />));
});
afterEach(async () => {
	await act(async () => root.unmount());
	node.remove();
});
describe('OAuth opt-out consent guards', () => {
	it('cannot deselect the required bot scope, while optional scope choices reach the wire string', async () => {
		await act(async () => {
			scopes.toggle('bot');
			scopes.toggle('email');
		});
		expect(scopes.isLocked('bot')).toBe(true);
		expect(scopes.selected.has('bot')).toBe(true);
		expect(scopes.selected.has('email')).toBe(false);
		expect(scopes.toScopeString()).toBe('bot identify');
		expect(scopes.adjusted).toBe(true);
	});
	it('restores a newly locked item even when an older opt-out override exists', async () => {
		await act(async () => selection.toggle('email'));
		expect(selection.selected.has('email')).toBe(false);
		await act(async () =>
			root.render(
				<Probe locked={lockedEmail} data-flx="auth.oauth-authorize-page.use-opt-out-selection-test.probe--2" />,
			),
		);
		expect(selection.selected.has('email')).toBe(true);
		await act(async () => selection.toggle('email'));
		expect(selection.selected.has('email')).toBe(true);
	});
	it('lets optional scopes return to their requested default', async () => {
		await act(async () => scopes.toggle('email'));
		await act(async () => scopes.toggle('email'));
		expect(scopes.selected).toEqual(new Set(requested));
		expect(scopes.adjusted).toBe(false);
	});
});
