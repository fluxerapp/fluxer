// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {resolveContactEmails} from '@app/api/instance/ContactEmails';
import {Logger} from '@app/api/Logger';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getEmailService} from '@app/api/middleware/ServiceSingletons';
import {getEffectiveBillingConfig} from '@app/api/stripe/BillingConfigCache';
import {formatMinorUnitPrice} from '@fluxer/number_utils/src/PriceFormatting';
import {seconds} from 'itty-time';

const DEDUPE_TTL_SECONDS = seconds('24 hours');
const DEDUPE_KEY_PREFIX = 'billing_support_alert:';
const ERROR_MAX_LENGTH = 500;

interface BillingSupportAlertFacts {
	invoiceId?: string | null;
	creditNoteId?: string | null;
	paymentIntentId?: string | null;
	refundId?: string | null;
	disputeId?: string | null;
	checkoutSessionId?: string | null;
	subscriptionId?: string | null;
	googlePlayOrderId?: string | null;
	userId?: string | bigint | null;
	documentNumber?: string | null;
	series?: string | null;
	amountMinor?: number | null;
	currency?: string | null;
	country?: string | null;
	error?: unknown;
}

interface BillingSupportAlert {
	dedupeKey: string;
	subject: string;
	summary: string;
	facts: BillingSupportAlertFacts;
}

function errorMessage(error: unknown): string | null {
	if (error == null) return null;
	const message = error instanceof Error ? error.message : String(error);
	return message.length > ERROR_MAX_LENGTH ? `${message.slice(0, ERROR_MAX_LENGTH)}...` : message;
}

function stripeDashboardUrl(path: string): string {
	const testMode = /^(sk|rk)_test_/.test(getEffectiveBillingConfig().secretKey ?? '');
	return `https://dashboard.stripe.com/${testMode ? 'test/' : ''}${path}`;
}

function billingSupportAlertText({summary, facts}: Pick<BillingSupportAlert, 'summary' | 'facts'>): string {
	const amount = formatMinorUnitPrice(facts.amountMinor, facts.currency, 'en-US');
	const rows: Array<[string, string | null | undefined]> = [
		['Stripe invoice', facts.invoiceId],
		['Stripe credit note', facts.creditNoteId],
		['Stripe payment intent', facts.paymentIntentId],
		['Stripe refund', facts.refundId],
		['Stripe dispute', facts.disputeId],
		['Stripe checkout session', facts.checkoutSessionId],
		['Stripe subscription', facts.subscriptionId],
		['Google Play order', facts.googlePlayOrderId],
		['Fluxer user', facts.userId?.toString()],
		['Document', facts.documentNumber],
		['Series', facts.series],
		['Amount', amount ? `${amount} (${facts.currency?.toUpperCase()})` : null],
		['Country', facts.country],
		['Error', errorMessage(facts.error)],
	];
	const links: Array<[string, string | null]> = [
		['Invoice in Stripe', facts.invoiceId ? stripeDashboardUrl(`invoices/${facts.invoiceId}`) : null],
		['Payment in Stripe', facts.paymentIntentId ? stripeDashboardUrl(`payments/${facts.paymentIntentId}`) : null],
		['User in admin', facts.userId != null ? `${Config.endpoints.admin}/users/${facts.userId}` : null],
	];
	const lines = [summary, ''];
	for (const [label, value] of rows) {
		if (value) lines.push(`${label}: ${value}`);
	}
	const shownLinks = links.filter(([, url]) => url);
	if (shownLinks.length > 0) {
		lines.push('');
		for (const [label, url] of shownLinks) lines.push(`${label}: ${url}`);
	}
	return lines.join('\n');
}

export async function alertSupport(alert: BillingSupportAlert): Promise<void> {
	if (Config.instance.selfHosted) return;
	const recipient = Config.email.supportAlertEmail ?? resolveContactEmails().supportEmail;
	if (!recipient) return;
	const kv = getKVClient();
	const key = `${DEDUPE_KEY_PREFIX}${alert.dedupeKey}`;
	try {
		if (!(await kv.setnx(key, '1', DEDUPE_TTL_SECONDS))) return;
		try {
			const sent = await getEmailService().send('billing_support_alert', recipient, {
				subject: `[Billing alert] ${alert.subject}`,
				text: billingSupportAlertText(alert),
			});
			if (sent) return;
			Logger.warn({dedupeKey: alert.dedupeKey}, 'Could not email a billing alert to support');
		} catch (error) {
			Logger.warn({error, dedupeKey: alert.dedupeKey}, 'Could not email a billing alert to support');
		}
		await kv.del(key);
	} catch (error) {
		Logger.warn({error, dedupeKey: alert.dedupeKey}, 'Could not record a billing alert to support');
	}
}
