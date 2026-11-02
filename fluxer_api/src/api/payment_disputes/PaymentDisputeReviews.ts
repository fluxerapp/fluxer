// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import {Db} from '@app/api/database/CassandraTypes';
import type {
	OperatorPassPurchaseRow,
	OperatorPassRemovalHistoryRow,
	OperatorPassUnitRow,
} from '@app/api/database/types/OperatorPassTypes';
import type {PaymentDisputeProduct, PaymentDisputeReviewRow} from '@app/api/database/types/PaymentDisputeTypes';
import {resolveContactEmails} from '@app/api/instance/ContactEmails';
import {Logger} from '@app/api/Logger';
import {getOperatorPassWithdrawalRightEndsAt} from '@app/api/operator_pass/OperatorPassPurchaseRules';
import type {PaymentDisputeRepository} from '@app/api/payment_disputes/PaymentDisputeRepository';
import {extractId} from '@app/api/stripe/StripeUtils';
import {PremiumPurchaseBlockedError} from '@fluxer/errors/src/domains/payment/PremiumPurchaseBlockedError';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import type Stripe from 'stripe';

const PAYMENT_DISPUTE_EXEMPT_REASONS: ReadonlyArray<string> = [
	'fraudulent',
	'unrecognized',
	'debit_not_authorized',
	'duplicate',
	'credit_not_processed',
	'product_not_received',
	'product_unacceptable',
];

const PAYMENT_DISPUTE_REVIEW_BLOCKERS = [
	'exempt_reason',
	'during_withdrawal',
	'withdrawn',
	'refunded_before_dispute',
	'perks_not_provided',
	'perks_removed',
	'before_terms_effective',
] as const;

export type PaymentDisputeReviewBlocker = (typeof PAYMENT_DISPUTE_REVIEW_BLOCKERS)[number];

export interface PaymentDisputeSubject {
	product: PaymentDisputeProduct;
	orderId: string;
	invoiceNumber: string | null;
	purchaserId: UserID;
	isGift: boolean;
	paymentIntentId: string | null;
	amountMinor: number;
	currency: string;
	disputedAt: Date | null;
	purchasedAt: Date | null;
	perksStartedAt: Date | null;
	withdrawalEndsAt: Date | null;
	withdrawnAt: Date | null;
	refundedAt: Date | null;
	perksRemovedAt: Date | null;
}

export function isPaymentDisputeOpen(review: PaymentDisputeReviewRow): boolean {
	return review.closed_at == null;
}

function underClosureTerms(review: PaymentDisputeReviewRow, disputedAt: number): boolean {
	const effectiveAt = Config.paymentDisputes.closureEffectiveAt?.getTime() ?? null;
	if (effectiveAt === null || !review.purchased_at) return false;
	return disputedAt >= effectiveAt && review.purchased_at.getTime() >= effectiveAt;
}

export function getPaymentDisputeReviewBlockers(review: PaymentDisputeReviewRow): Array<PaymentDisputeReviewBlocker> {
	const blockers: Array<PaymentDisputeReviewBlocker> = [];
	if (PAYMENT_DISPUTE_EXEMPT_REASONS.includes(review.reason)) {
		blockers.push('exempt_reason');
	}
	const disputedAt = review.disputed_at?.getTime() ?? review.created_at.getTime();
	if (!review.withdrawal_ends_at || disputedAt < review.withdrawal_ends_at.getTime()) {
		blockers.push('during_withdrawal');
	}
	if (review.withdrawn_at) {
		blockers.push('withdrawn');
	}
	if (review.refunded_at && review.refunded_at.getTime() <= disputedAt) {
		blockers.push('refunded_before_dispute');
	}
	if (!review.perks_started_at) {
		blockers.push('perks_not_provided');
	}
	if (review.perks_removed_at) {
		blockers.push('perks_removed');
	}
	if (review.product === 'plutonium' && !underClosureTerms(review, disputedAt)) {
		blockers.push('before_terms_effective');
	}
	return blockers;
}

function firstPerkRemovalBefore(history: ReadonlyArray<OperatorPassRemovalHistoryRow>, disputedAt: Date): Date | null {
	let first: Date | null = null;
	for (const entry of history) {
		if (entry.removed_at.getTime() <= disputedAt.getTime() && (!first || entry.removed_at < first)) {
			first = entry.removed_at;
		}
	}
	return first;
}

function firstOperatorPassRefundAt(
	purchase: OperatorPassPurchaseRow,
	units: ReadonlyArray<OperatorPassUnitRow>,
): Date | null {
	let first = purchase.refunded_at ?? null;
	for (const unit of units) {
		if (unit.end_reason === 'refund' && unit.ended_at && (!first || unit.ended_at < first)) {
			first = unit.ended_at;
		}
	}
	return first;
}

export function getOperatorPassDisputeSubject(
	purchase: OperatorPassPurchaseRow,
	dispute: Stripe.Dispute,
	holderRemovals: ReadonlyArray<OperatorPassRemovalHistoryRow>,
	units: ReadonlyArray<OperatorPassUnitRow>,
): PaymentDisputeSubject {
	const disputedAt = purchase.disputed_at ?? new Date(dispute.created * 1000);
	return {
		product: 'operator_pass',
		orderId: purchase.checkout_session_id,
		invoiceNumber: purchase.invoice_number ?? null,
		purchaserId: purchase.purchaser_id,
		isGift: false,
		paymentIntentId: purchase.payment_intent_id ?? null,
		amountMinor: purchase.amount_minor,
		currency: purchase.currency,
		disputedAt,
		purchasedAt: purchase.purchased_at ?? null,
		perksStartedAt: purchase.delivered_at ?? purchase.perks_granted_at ?? null,
		withdrawalEndsAt: getOperatorPassWithdrawalRightEndsAt(purchase),
		withdrawnAt: purchase.withdrawn_at ?? null,
		refundedAt: firstOperatorPassRefundAt(purchase, units),
		perksRemovedAt: firstPerkRemovalBefore(holderRemovals, disputedAt),
	};
}

function getEvidenceDueBy(dispute: Stripe.Dispute): Date | null {
	const dueBy = dispute.evidence_details?.due_by;
	return typeof dueBy === 'number' ? new Date(dueBy * 1000) : null;
}

function isClosedDisputeStatus(status: Stripe.Dispute.Status): boolean {
	return status === 'won' || status === 'lost' || status === 'warning_closed' || status === 'prevented';
}

export async function hasOpenPaymentDispute(
	repository: Pick<PaymentDisputeRepository, 'listReviewsByPurchaser'>,
	userId: UserID,
): Promise<boolean> {
	const reviews = await repository.listReviewsByPurchaser(userId);
	return reviews.some(isPaymentDisputeOpen);
}

export async function assertNoOpenPaymentDispute(
	repository: Pick<PaymentDisputeRepository, 'listReviewsByPurchaser'>,
	userId: UserID,
): Promise<void> {
	if (await hasOpenPaymentDispute(repository, userId)) {
		throw new PremiumPurchaseBlockedError('payment_dispute_open');
	}
}

export async function flagPurchaseDuringDispute(
	repository: Pick<PaymentDisputeRepository, 'listReviewsByPurchaser' | 'updateReview'>,
	userId: UserID,
	purchaseKey: string,
): Promise<number> {
	let flagged = 0;
	for (const review of await repository.listReviewsByPurchaser(userId)) {
		if (!isPaymentDisputeOpen(review) || review.dispute_purchase_at) {
			continue;
		}
		if (await repository.updateReview(review, {dispute_purchase_at: Db.set(new Date())})) {
			flagged += 1;
			Logger.warn(
				{disputeId: review.dispute_id, purchaserId: userId.toString(), purchaseKey},
				'A purchase was applied while a payment dispute is open',
			);
		}
	}
	return flagged;
}

export async function listPaymentDisputeReviews(
	repository: Pick<PaymentDisputeRepository, 'listReviews'>,
): Promise<Array<PaymentDisputeReviewRow>> {
	const reviews = await repository.listReviews();
	return reviews.sort((left, right) => right.created_at.getTime() - left.created_at.getTime());
}

export class PaymentDisputeReviewService {
	constructor(
		private readonly repository: PaymentDisputeRepository,
		private readonly auditService: AdminAuditService,
		private readonly emailService: Pick<IEmailService, 'send'>,
	) {}

	async openReview(subject: PaymentDisputeSubject, dispute: Stripe.Dispute): Promise<PaymentDisputeReviewRow> {
		const existing = await this.repository.findReview(dispute.id);
		if (existing) {
			return (await this.refreshReview(dispute)) ?? existing;
		}
		const review = this.buildReview(subject, dispute);
		const created = await this.repository.createReview(review);
		if (!created) {
			return (await this.repository.findReview(dispute.id)) ?? review;
		}
		await this.auditService.createAuditLog({
			adminUserId: SYSTEM_USER_ID,
			targetType: 'user',
			targetId: BigInt(subject.purchaserId),
			action: 'open_payment_dispute_review',
			auditLogReason: 'Payment dispute opened, purchases are blocked until it closes and a person reviews it',
			metadata: new Map([
				['dispute_id', dispute.id],
				['product', review.product],
				['order_id', review.order_id],
				['reason', review.reason],
				['evidence_due_by', review.evidence_due_by?.toISOString() ?? ''],
				['blockers', getPaymentDisputeReviewBlockers(review).join(',')],
			]),
		});
		Logger.info(
			{disputeId: dispute.id, product: review.product, orderId: review.order_id, purchaserId: subject.purchaserId},
			'Opened a payment dispute review',
		);
		await this.alertStaff(review);
		return review;
	}

	private async alertStaff(review: PaymentDisputeReviewRow): Promise<void> {
		const {supportEmail} = resolveContactEmails();
		if (!supportEmail || review.closed_at) {
			return;
		}
		const sent = await this.emailService.send('payment_dispute_review_alert', supportEmail, {
			disputeId: review.dispute_id,
			product: review.product,
			invoiceNumber: review.invoice_number ?? null,
			purchaserId: review.purchaser_id.toString(),
			reason: review.reason,
			amountMinor: review.amount_minor,
			currency: review.currency,
			evidenceDueBy: review.evidence_due_by ?? null,
			blockers: getPaymentDisputeReviewBlockers(review),
			reviewUrl: `${Config.endpoints.admin}/payment-disputes`,
		});
		if (!sent) {
			Logger.warn({disputeId: review.dispute_id}, 'Failed to email staff about a new payment dispute review');
		}
	}

	async refreshReview(dispute: Stripe.Dispute): Promise<PaymentDisputeReviewRow | null> {
		for (let attempt = 0; attempt < 3; attempt++) {
			const review = await this.repository.findReview(dispute.id);
			if (!review || !isPaymentDisputeOpen(review)) {
				return review;
			}
			const updated = await this.repository.updateReview(review, {
				reason: Db.set(dispute.reason ?? review.reason),
				evidence_submitted: Db.set(dispute.evidence_details?.has_evidence === true),
				evidence_due_by: Db.set(getEvidenceDueBy(dispute) ?? review.evidence_due_by),
			});
			if (updated) {
				return updated;
			}
		}
		return this.repository.findReview(dispute.id);
	}

	async closeReview(subject: PaymentDisputeSubject, dispute: Stripe.Dispute): Promise<PaymentDisputeReviewRow> {
		const closedAt = new Date();
		for (let attempt = 0; attempt < 3; attempt++) {
			const review = await this.repository.findReview(dispute.id);
			if (!review) {
				await this.repository.createReview({
					...this.buildReview(subject, dispute),
					closed_at: closedAt,
					outcome: dispute.status,
				});
				continue;
			}
			if (!isPaymentDisputeOpen(review)) {
				return review;
			}
			const updated = await this.repository.updateReview(review, {
				reason: Db.set(dispute.reason ?? review.reason),
				evidence_submitted: Db.set(dispute.evidence_details?.has_evidence === true),
				closed_at: Db.set(closedAt),
				outcome: Db.set(dispute.status),
				withdrawn_at: Db.set(review.withdrawn_at ?? subject.withdrawnAt),
				perks_removed_at: Db.set(review.perks_removed_at ?? subject.perksRemovedAt),
			});
			if (updated) {
				Logger.info(
					{disputeId: dispute.id, outcome: dispute.status, status: updated.status},
					'Closed a payment dispute review',
				);
				return updated;
			}
		}
		const review = await this.repository.findReview(dispute.id);
		if (!review || isPaymentDisputeOpen(review)) {
			throw new Error(`Payment dispute review ${dispute.id} could not be closed`);
		}
		return review;
	}

	private buildReview(subject: PaymentDisputeSubject, dispute: Stripe.Dispute): PaymentDisputeReviewRow {
		const now = new Date();
		return {
			dispute_id: dispute.id,
			product: subject.product,
			order_id: subject.orderId,
			invoice_number: subject.invoiceNumber,
			purchaser_id: subject.purchaserId,
			is_gift: subject.isGift,
			reason: dispute.reason ?? 'general',
			payment_intent_id: subject.paymentIntentId,
			charge_id: extractId(dispute.charge),
			amount_minor: dispute.amount ?? subject.amountMinor,
			currency: (dispute.currency ?? subject.currency).toUpperCase(),
			evidence_submitted: dispute.evidence_details?.has_evidence === true,
			evidence_due_by: getEvidenceDueBy(dispute),
			disputed_at: subject.disputedAt ?? new Date(dispute.created * 1000),
			closed_at: isClosedDisputeStatus(dispute.status) ? now : null,
			outcome: isClosedDisputeStatus(dispute.status) ? dispute.status : null,
			purchased_at: subject.purchasedAt,
			perks_started_at: subject.perksStartedAt,
			withdrawal_ends_at: subject.withdrawalEndsAt,
			withdrawn_at: subject.withdrawnAt,
			refunded_at: subject.refundedAt,
			perks_removed_at: subject.perksRemovedAt,
			status: 'open',
			created_at: now,
			decided_at: null,
			decided_by: null,
			decision_note: null,
			warning_deadline: null,
			warned_at: null,
			warning_email_sent_at: null,
			warning_dm_sent_at: null,
			enforcement_decision_id: null,
			reversed_at: null,
			reversed_by: null,
			reversal_note: null,
			dispute_purchase_at: null,
			version: 1,
		};
	}
}
