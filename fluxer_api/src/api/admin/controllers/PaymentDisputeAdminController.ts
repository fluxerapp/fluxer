// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead, recordAdminWrite} from '@app/api/admin/AdminAuditRecorder';
import type {PaymentDisputeReviewRow} from '@app/api/database/types/PaymentDisputeTypes';
import {enforcementActor} from '@app/api/enforcement/EnforcementActor';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getPaymentDisputeRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {
	getClosuresAwaited,
	getPaymentDisputeWarningDeadline,
} from '@app/api/payment_disputes/PaymentDisputeEnforcement';
import {
	getPaymentDisputeReviewBlockers,
	isPaymentDisputeOpen,
	listPaymentDisputeReviews,
} from '@app/api/payment_disputes/PaymentDisputeReviews';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	AdminPaymentDisputeDecisionRequest,
	AdminPaymentDisputeIdParam,
	AdminPaymentDisputeReversalRequest,
	AdminPaymentDisputeReviewListResponse,
	AdminPaymentDisputeReviewResponse,
} from '@fluxer/schema/src/domains/admin/AdminPaymentDisputeSchemas';

const TAG = 'Admin';

function mapReview(
	review: PaymentDisputeReviewRow,
	now: Date,
	awaitedDecisionId: bigint | null,
): AdminPaymentDisputeReviewResponse {
	const blockers = getPaymentDisputeReviewBlockers(review);
	const unreviewed = review.status === 'open' && isPaymentDisputeOpen(review);
	const warningDeadline =
		review.warning_deadline ?? (unreviewed ? getPaymentDisputeWarningDeadline(review.evidence_due_by, now) : null);
	return {
		dispute_id: review.dispute_id,
		product: review.product,
		order_id: review.order_id,
		invoice_number: review.invoice_number ?? null,
		purchaser_id: review.purchaser_id.toString(),
		is_gift: review.is_gift,
		reason: review.reason,
		amount_minor: review.amount_minor,
		currency: review.currency,
		evidence_submitted: review.evidence_submitted,
		evidence_due_by: review.evidence_due_by?.toISOString() ?? null,
		disputed_at: review.disputed_at?.toISOString() ?? null,
		closed_at: review.closed_at?.toISOString() ?? null,
		outcome: review.outcome ?? null,
		purchased_at: review.purchased_at?.toISOString() ?? null,
		perks_started_at: review.perks_started_at?.toISOString() ?? null,
		withdrawal_ends_at: review.withdrawal_ends_at?.toISOString() ?? null,
		withdrawn_at: review.withdrawn_at?.toISOString() ?? null,
		refunded_at: review.refunded_at?.toISOString() ?? null,
		perks_removed_at: review.perks_removed_at?.toISOString() ?? null,
		status: review.status,
		blockers,
		warnable: unreviewed && blockers.length === 0 && warningDeadline !== null,
		warning_deadline: warningDeadline?.toISOString() ?? null,
		warning_email_sent_at: review.warning_email_sent_at?.toISOString() ?? null,
		warning_dm_sent_at: review.warning_dm_sent_at?.toISOString() ?? null,
		warned_at: review.warned_at?.toISOString() ?? null,
		enforcement_decision_id: review.enforcement_decision_id?.toString() ?? null,
		awaited_decision_id: awaitedDecisionId?.toString() ?? null,
		decided_at: review.decided_at?.toISOString() ?? null,
		decided_by: review.decided_by?.toString() ?? null,
		decision_note: review.decision_note ?? null,
		reversed_at: review.reversed_at?.toISOString() ?? null,
		reversed_by: review.reversed_by?.toString() ?? null,
		reversal_note: review.reversal_note ?? null,
		dispute_purchase_at: review.dispute_purchase_at?.toISOString() ?? null,
	};
}

export function PaymentDisputeAdminController(app: HonoApp) {
	app.get(
		'/admin/payment-dispute-reviews',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		OpenAPI({
			operationId: 'list_admin_payment_dispute_reviews',
			summary: 'List payment dispute reviews',
			responseSchema: AdminPaymentDisputeReviewListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Lists the Operator Pass and Plutonium payment disputes, newest first. A review opens when the dispute opens, and the account that made the payment cannot buy anything while any of its disputes is open. Each review shows the grounds that rule out finding it unjustified, the date a warning would name, and the other closure decision a pending closure waits for. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const reviews = await listPaymentDisputeReviews(getPaymentDisputeRepository());
			await recordAdminRead(ctx, {
				targetType: 'payment_dispute_review',
				targetId: 0n,
				action: AdminAuditReadActions.LIST_PAYMENT_DISPUTE_REVIEWS,
				metadata: {result_count: reviews.length},
			});
			const now = new Date();
			const awaited = await getClosuresAwaited(getUserRepository(), reviews);
			return ctx.json({
				reviews: reviews.map((review) => mapReview(review, now, awaited.get(review.dispute_id) ?? null)),
			});
		},
	);
	app.post(
		'/admin/payment-dispute-reviews/:dispute_id/decision',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_DELETE),
		Validator('param', AdminPaymentDisputeIdParam),
		Validator('json', AdminPaymentDisputeDecisionRequest),
		OpenAPI({
			operationId: 'decide_admin_payment_dispute_review',
			summary: 'Decide payment dispute review',
			responseSchema: AdminPaymentDisputeReviewResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Records whether an open payment dispute is justified. Unjustified is refused when the terms rule it out, after the dispute closed, or when the warning date, 3 days before the evidence deadline, is less than 2 days away. Unjustified emails the account and sends it a system message naming that date: if the dispute is not withdrawn by then and ends with the money returned, an enforcement decision closes the account and sends the enforcement notice for Plutonium, and for the Operator Pass the bonus is removed permanently under section 8, which bars the account from buying or receiving a pass and emails the statement of reasons. The warning counts only once both went out. When the instance has direct messages turned off, no system message is sent and the email alone counts. Deciding unjustified again on a review whose warning is incomplete sends only what is missing, with the same date. A justified dispute never leads to action against the account. A decision is final, only an appeal reverses an unjustified one. Creates audit log entry. Requires USER_DELETE permission.',
		}),
		async (ctx) => {
			const {dispute_id: disputeId} = ctx.req.valid('param');
			const body = ctx.req.valid('json');
			const {review, emailSent, dmSent} = await ctx.get('adminService').paymentDisputeEnforcement.decide({
				disputeId,
				decision: body.decision,
				note: body.note,
				adminUserId: ctx.get('adminUserId'),
			});
			await recordAdminWrite(ctx, {
				targetType: 'user',
				targetId: review.purchaser_id,
				action: 'decide_payment_dispute_review',
				metadata: {
					dispute_id: disputeId,
					product: review.product,
					decision: body.decision,
					status: review.status,
					warning_deadline: review.warning_deadline?.toISOString() ?? undefined,
					warning_email_sent: emailSent,
					warning_dm_sent: dmSent,
				},
			});
			return ctx.json(mapReview(review, new Date(), null));
		},
	);
	app.post(
		'/admin/payment-dispute-reviews/:dispute_id/reversal',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_DELETE),
		Validator('param', AdminPaymentDisputeIdParam),
		Validator('json', AdminPaymentDisputeReversalRequest),
		OpenAPI({
			operationId: 'reverse_admin_payment_dispute_review',
			summary: 'Reverse payment dispute decision',
			responseSchema: AdminPaymentDisputeReviewResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: TAG,
			description:
				'Reverses an unjustified finding when an appeal under section 6.2 of the terms shows the dispute was justified. The review becomes justified, a permanent Operator Pass bonus removal made for this dispute is lifted, and the enforcement decision that closed the account is reversed, which restores the account and sends the enforcement ended notice, unless the deletion already started or another unreversed unjustified dispute is linked to the same decision. Creates audit log entry. Requires USER_DELETE permission, and BAN_IP_REMOVE when the decision blocked networks.',
		}),
		async (ctx) => {
			const {dispute_id: disputeId} = ctx.req.valid('param');
			const {note} = ctx.req.valid('json');
			const {review, decisionEnded} = await ctx.get('adminService').paymentDisputeEnforcement.reverse({
				disputeId,
				note,
				actor: enforcementActor(ctx),
			});
			await recordAdminWrite(ctx, {
				targetType: 'user',
				targetId: review.purchaser_id,
				action: 'reverse_payment_dispute_review',
				metadata: {
					dispute_id: disputeId,
					product: review.product,
					enforcement_decision_id: review.enforcement_decision_id?.toString() ?? undefined,
					decision_ended: decisionEnded,
				},
			});
			return ctx.json(mapReview(review, new Date(), null));
		},
	);
}
