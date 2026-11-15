// SPDX-License-Identifier: AGPL-3.0-or-later

import {alertSupport} from '@app/api/billing/BillingSupportAlert';
import {Config} from '@app/api/Config';
import {getEmailService} from '@app/api/middleware/ServiceSingletons';
import type {TestEmailService} from '@pkgs/email/src/TestEmailService';
import {afterEach, beforeEach, expect, test} from 'vitest';

const emails = () => getEmailService() as TestEmailService;

const alert = {
	dedupeKey: 'self-serve-refund:re_alert_test',
	subject: 'Self-serve refund re_alert_test failed',
	summary: 'A self-serve refund failed at Stripe, so the money did not reach the buyer.',
	facts: {
		refundId: 're_alert_test',
		userId: 1001n,
		amountMinor: 499,
		currency: 'usd',
		error: 'expired_or_canceled_card',
	},
};

beforeEach(() => {
	emails().clearSentEmails();
});

afterEach(() => {
	Config.instance.selfHosted = false;
	emails().clearSentEmails();
});

test('a billing alert emails support once in plain text and self-hosted instances send nothing', async () => {
	await alertSupport(alert);
	await alertSupport(alert);

	const sent = emails().listSentEmails();
	expect(sent).toHaveLength(1);
	expect(sent[0]).toMatchObject({to: 'support@fluxer.com', type: 'billing_support_alert'});
	expect(sent[0].metadata.subject).toBe('[Billing alert] Self-serve refund re_alert_test failed');
	expect(sent[0].metadata.text).toContain('Stripe refund: re_alert_test');
	expect(sent[0].metadata.text).toContain('Amount: $4.99 (USD)');
	expect(sent[0].metadata.text).toContain('/users/1001');
	expect(sent[0].metadata.text).not.toMatch(/<[a-z]/i);

	emails().clearSentEmails();
	Config.instance.selfHosted = true;
	await alertSupport({...alert, dedupeKey: 'self-hosted'});
	expect(emails().listSentEmails()).toEqual([]);
});
