// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	applyDefaultAppShellDocumentBranding,
	applyRuntimeDocumentBranding,
} from '@app/features/app/state/RuntimeDocumentBranding';
import {updateDocumentTitleBadge, useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import type {InstanceAppPublic} from '@fluxer/instance_bootstrap/src/Types';
import {act} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, expect, test, vi} from 'vitest';

vi.mock('@app/features/user/utils/LocaleUtils', () => ({getCurrentLocale: () => 'en-US'}));

Object.assign(globalThis, {IS_REACT_ACT_ENVIRONMENT: true});

const ACME_APP_PUBLIC: InstanceAppPublic = {
	branding: {
		product_name: 'Acme Chat',
		icon_url: null,
		symbol_url: null,
		logo_url: null,
		wordmark_url: null,
		favicon_url: 'https://chat.acme.test/favicon.png',
		theme_color: null,
		status_page_url: null,
		status_page_incident_history_url: null,
		premium_product_name: 'Premium',
		premium_info_url: null,
	},
	setup: {configured: true, admin_url: null},
	legal: {terms_url: null, privacy_url: null},
	registration: {collect_date_of_birth: true},
};

let root: Root | null = null;

function TitledPage({parts}: {parts: Array<string>}) {
	useFluxerDocumentTitle(parts);
	return null;
}

function renderTitledPage(parts: Array<string>): void {
	act(() => {
		root ??= createRoot(document.createElement('div'));
		root.render(<TitledPage parts={parts} />);
	});
}

beforeEach(() => {
	document.head.innerHTML = '<link rel="icon" sizes="32x32" href="https://fluxer.app/web/favicon-32x32.png">';
});

afterEach(() => {
	act(() => root?.unmount());
	root = null;
	updateDocumentTitleBadge(0, false);
	applyDefaultAppShellDocumentBranding();
});

test('pages titled after the instance branding loads keep the instance name', () => {
	applyRuntimeDocumentBranding(ACME_APP_PUBLIC);
	renderTitledPage(['Friends']);
	expect(document.title).toBe('Acme Chat | Friends');

	renderTitledPage(['general', 'Acme HQ']);
	updateDocumentTitleBadge(3, true);
	expect(document.title).toBe('(3) Acme Chat | general | Acme HQ');
});

test('a page titled before the branding loads switches to the instance name', () => {
	renderTitledPage(['Sign in']);
	expect(document.title).toBe('Fluxer | Sign in');

	applyRuntimeDocumentBranding(ACME_APP_PUBLIC);
	expect(document.title).toBe('Acme Chat | Sign in');

	applyDefaultAppShellDocumentBranding();
	expect(document.title).toBe('Fluxer | Sign in');
});

test('the instance favicon replaces the bundled one until the default branding returns', () => {
	const icons = (): Array<string> =>
		Array.from(document.head.querySelectorAll<HTMLLinkElement>('link[rel="icon"]'), (link) => link.href);

	applyRuntimeDocumentBranding(ACME_APP_PUBLIC);
	expect(icons()).toEqual(['https://chat.acme.test/favicon.png']);

	applyDefaultAppShellDocumentBranding();
	expect(icons()).toEqual(['https://fluxer.app/web/favicon-32x32.png']);
});
