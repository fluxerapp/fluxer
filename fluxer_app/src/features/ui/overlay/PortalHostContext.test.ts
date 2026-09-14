// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {scopePortalHostToDocument} from '@app/features/ui/overlay/PortalHostContext';
import {describe, expect, it} from 'vitest';

describe('scopePortalHostToDocument', () => {
	it('keeps a host that belongs to the scoped document', () => {
		const host = document.createElement('div');
		expect(scopePortalHostToDocument(host, document)).toBe(host);
	});

	it('drops a host owned by another document', () => {
		const popoutDocument = document.implementation.createHTMLDocument('popout');
		const popoutHost = popoutDocument.createElement('div');
		expect(scopePortalHostToDocument(popoutHost, document)).toBeNull();
		expect(scopePortalHostToDocument(popoutHost, popoutDocument)).toBe(popoutHost);
	});

	it('passes a missing host through', () => {
		expect(scopePortalHostToDocument(null, document)).toBeNull();
	});
});
