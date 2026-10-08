// SPDX-License-Identifier: AGPL-3.0-or-later

import {getConfig} from '@app/api/Config';
import {resolveContactEmails} from '@app/api/instance/ContactEmails';
import {afterEach, describe, expect, it} from 'vitest';

describe('resolveContactEmails', () => {
	const originalSelfHosted = getConfig().instance.selfHosted;

	afterEach(() => {
		getConfig().instance.selfHosted = originalSelfHosted;
	});

	it('gives a hosted instance the hosted mailboxes', () => {
		getConfig().instance.selfHosted = false;
		expect(resolveContactEmails()).toEqual({
			appealsEmail: 'appeals@fluxer.com',
			safetyEmail: 'safety@fluxer.com',
			supportEmail: 'support@fluxer.com',
		});
	});

	it('gives a self-hosted instance no mailbox at all', () => {
		getConfig().instance.selfHosted = true;
		expect(resolveContactEmails()).toEqual({appealsEmail: null, safetyEmail: null, supportEmail: null});
	});

	it('follows the instance switch on every call', () => {
		getConfig().instance.selfHosted = true;
		expect(resolveContactEmails().supportEmail).toBeNull();
		getConfig().instance.selfHosted = false;
		expect(resolveContactEmails().supportEmail).toBe('support@fluxer.com');
	});
});
