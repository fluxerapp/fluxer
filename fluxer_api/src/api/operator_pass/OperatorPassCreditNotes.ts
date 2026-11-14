// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BillingDocumentService, IssueContext} from '@app/api/billing_documents/BillingDocumentService';
import type {BillingDocumentRow} from '@app/api/database/types/BillingDocumentTypes';
import type {OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import {Logger} from '@app/api/Logger';
import type Stripe from 'stripe';

export function operatorPassDocumentContext(
	purchase: OperatorPassPurchaseRow,
	locale: string | null = null,
): IssueContext {
	return {userId: purchase.purchaser_id, locale};
}

export async function creditOperatorPassRefunds(
	stripe: Stripe,
	documents: BillingDocumentService,
	purchase: OperatorPassPurchaseRow,
): Promise<Array<BillingDocumentRow>> {
	if (!purchase.payment_intent_id) return [];
	const refunds = await stripe.refunds.list({payment_intent: purchase.payment_intent_id, limit: 100});
	const issued: Array<BillingDocumentRow> = [];
	const settled = refunds.data
		.filter((refund) => refund.status === 'succeeded')
		.sort((left, right) => left.created - right.created || left.id.localeCompare(right.id));
	for (const refund of settled) {
		const document = await documents.issueForRefund(refund, operatorPassDocumentContext(purchase));
		if (document) {
			issued.push(document);
			Logger.info(
				{sessionId: purchase.checkout_session_id, refundId: refund.id, number: document.display_number},
				'Credit note in place for an Operator Pass refund',
			);
		}
	}
	return issued;
}
