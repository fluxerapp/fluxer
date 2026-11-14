// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import type {PaymentDisputeReviewRow} from '@app/api/database/types/PaymentDisputeTypes';
import {getPaymentDisputeRepository} from '@app/api/middleware/ServiceSingletons';

function auditReview(disputeId: string, purchaserId: UserID, status: PaymentDisputeReviewRow['status']) {
	const now = new Date();
	return {
		dispute_id: disputeId,
		product: 'plutonium',
		order_id: `cs_${disputeId}`,
		invoice_number: null,
		purchaser_id: purchaserId,
		is_gift: false,
		reason: 'general',
		payment_intent_id: `pi_${disputeId}`,
		charge_id: `ch_${disputeId}`,
		amount_minor: 2499,
		currency: 'USD',
		evidence_submitted: false,
		evidence_due_by: new Date(now.getTime() + 14 * 24 * 60 * 60 * 1000),
		disputed_at: now,
		closed_at: null,
		outcome: null,
		purchased_at: now,
		perks_started_at: now,
		withdrawal_ends_at: now,
		withdrawn_at: null,
		refunded_at: null,
		perks_removed_at: null,
		status,
		created_at: now,
		decided_at: status === 'open' ? null : now,
		decided_by: null,
		decision_note: null,
		warning_deadline: null,
		warning_email_sent_at: null,
		warning_dm_sent_at: null,
		warned_at: null,
		enforcement_decision_id: null,
		reversed_at: null,
		reversed_by: null,
		reversal_note: null,
		dispute_purchase_at: null,
		version: 1,
	} satisfies PaymentDisputeReviewRow;
}

export const PaymentDisputeAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/payment-dispute-reviews',
		async prepare() {
			return {
				request: {path: '/admin/payment-dispute-reviews'},
				expected: {
					action: 'list_payment_dispute_reviews',
					targetType: 'payment_dispute_review',
					targetId: '0',
					metadata: {result_count: '0'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/payment-dispute-reviews/:dispute_id/decision',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			await getPaymentDisputeRepository().createReview(
				auditReview('dp_audit_decision', createUserID(BigInt(target.userId)), 'open'),
			);
			return {
				request: {
					path: '/admin/payment-dispute-reviews/dp_audit_decision/decision',
					body: {decision: 'justified', note: null},
				},
				expected: {
					action: 'decide_payment_dispute_review',
					targetType: 'user',
					targetId: target.userId,
					metadata: {
						dispute_id: 'dp_audit_decision',
						product: 'plutonium',
						decision: 'justified',
						status: 'justified',
						warning_email_sent: 'false',
						warning_dm_sent: 'false',
					},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/payment-dispute-reviews/:dispute_id/reversal',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			await getPaymentDisputeRepository().createReview(
				auditReview('dp_audit_reversal', createUserID(BigInt(target.userId)), 'unjustified'),
			);
			return {
				request: {
					path: '/admin/payment-dispute-reviews/dp_audit_reversal/reversal',
					body: {note: 'The bank statement shows a duplicate charge.'},
				},
				expected: {
					action: 'reverse_payment_dispute_review',
					targetType: 'user',
					targetId: target.userId,
					metadata: {
						dispute_id: 'dp_audit_reversal',
						product: 'plutonium',
						decision_ended: 'false',
					},
				},
			};
		},
	},
];
