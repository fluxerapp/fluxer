// SPDX-License-Identifier: AGPL-3.0-or-later

import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {PaymentDisputeReviewRow} from '@app/api/database/types/PaymentDisputeTypes';
import type {OperatorPassServices} from '@app/api/middleware/OperatorPassServiceFactory';
import type {User} from '@app/api/models/User';
import {isOperatorPassPermanentlyRemoved} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';

const OPERATOR_PASS_DISPUTE_RULE = 'Section 9 of the Operator Pass terms';

export type OperatorPassDisputeServices = Pick<OperatorPassServices, 'removalService' | 'repository'>;

function disputeMarker(disputeId: string): string {
	return `Payment dispute ${disputeId}`;
}

function isBarFor(entry: {rule: string; facts: string}, disputeId: string): boolean {
	return entry.rule === OPERATOR_PASS_DISPUTE_RULE && entry.facts.startsWith(disputeMarker(disputeId));
}

function describeDispute(review: PaymentDisputeReviewRow): {id: string; deadline: Date} {
	return {id: review.dispute_id, deadline: review.warning_deadline ?? review.closed_at ?? new Date(0)};
}

export interface OperatorPassDisputeBarProgress {
	isAuditPending(disputeId: string): Promise<boolean>;
	markAuditPending(disputeId: string): Promise<void>;
	clearAuditPending(disputeId: string): Promise<void>;
	isNoticePending(disputeId: string): Promise<boolean>;
	markNoticePending(disputeId: string): Promise<void>;
	clearNoticePending(disputeId: string): Promise<void>;
}

function barFacts(review: PaymentDisputeReviewRow): string {
	const dispute = describeDispute(review);
	return `${disputeMarker(review.dispute_id)} was found unjustified, you were told why and given until ${dispute.deadline.toISOString()} to withdraw it, and it ended with the money returned. The passes that payment paid for have ended, and this account can't buy or receive a pass again.`;
}

async function settleNotice(
	progress: OperatorPassDisputeBarProgress,
	user: User,
	disputeId: string,
	notified: boolean,
): Promise<void> {
	if (notified || !user.email) {
		await progress.clearNoticePending(disputeId);
	}
}

async function takeAuditDue(progress: OperatorPassDisputeBarProgress, disputeId: string): Promise<boolean> {
	if (!(await progress.isAuditPending(disputeId))) {
		return false;
	}
	await progress.clearAuditPending(disputeId);
	return true;
}

async function applyBar(
	services: OperatorPassDisputeServices,
	progress: OperatorPassDisputeBarProgress,
	user: User,
	review: PaymentDisputeReviewRow,
): Promise<void> {
	await progress.markAuditPending(review.dispute_id);
	await progress.markNoticePending(review.dispute_id);
	const {notified} = await services.removalService.remove({
		user,
		adminUserId: review.decided_by ?? SYSTEM_USER_ID,
		rule: OPERATOR_PASS_DISPUTE_RULE,
		facts: barFacts(review),
		automated: false,
		serious: true,
		durationDays: null,
		notifyUser: true,
		dispute: describeDispute(review),
	});
	await settleNotice(progress, user, review.dispute_id, notified);
}

async function recordBarBehindRemoval(
	services: OperatorPassDisputeServices,
	progress: OperatorPassDisputeBarProgress,
	user: User,
	review: PaymentDisputeReviewRow,
): Promise<void> {
	await progress.markAuditPending(review.dispute_id);
	await services.repository.saveRemovalHistory({
		user_id: user.id,
		removed_at: operatorPassNow(),
		removed_until: null,
		rule: OPERATOR_PASS_DISPUTE_RULE,
		facts: barFacts(review),
		automated: false,
		serious: true,
		notice_sent_at: null,
		admin_user_id: review.decided_by ?? SYSTEM_USER_ID,
		ended_at: null,
		end_reason: null,
	});
}

export async function barOperatorPassAfterDispute(
	services: OperatorPassDisputeServices,
	progress: OperatorPassDisputeBarProgress,
	user: User,
	review: PaymentDisputeReviewRow,
): Promise<boolean> {
	const disputeId = review.dispute_id;
	const history = await services.repository.listRemovalHistory(user.id);
	const started = history.findLast((entry) => isBarFor(entry, disputeId));
	if (started?.ended_at != null) {
		return false;
	}
	const removal = await services.repository.findRemoval(user.id);
	if (!started) {
		if (isOperatorPassPermanentlyRemoved(removal)) {
			await recordBarBehindRemoval(services, progress, user, review);
		} else {
			await applyBar(services, progress, user, review);
		}
		return takeAuditDue(progress, disputeId);
	}
	const active = removal?.removal_state === 'removed' && removal.removed_at?.getTime() === started.removed_at.getTime();
	const noticePending = await progress.isNoticePending(disputeId);
	if ((active && noticePending) || (!active && !isOperatorPassPermanentlyRemoved(removal))) {
		const {notified} = await services.removalService.resumeRemoval(
			user,
			started,
			noticePending,
			describeDispute(review),
		);
		await settleNotice(progress, user, disputeId, notified);
	}
	return takeAuditDue(progress, disputeId);
}

export async function liftOperatorPassDisputeBar(
	services: OperatorPassDisputeServices,
	progress: OperatorPassDisputeBarProgress,
	user: User,
	disputeId: string,
	otherGrounds: ReadonlyArray<PaymentDisputeReviewRow>,
): Promise<boolean> {
	const removal = await services.repository.findRemoval(user.id);
	const history = await services.repository.listRemovalHistory(user.id);
	if (!removal || !isBarFor(removal, disputeId)) {
		const behind = history.findLast((entry) => entry.ended_at == null && isBarFor(entry, disputeId));
		if (behind) {
			await services.repository.endRemovalHistory(user.id, behind.removed_at, operatorPassNow(), 'restored');
		}
		return false;
	}
	const next = otherGrounds.find((review) => review.dispute_id !== disputeId);
	if (next) {
		const nextStarted = history.findLast((entry) => entry.ended_at == null && isBarFor(entry, next.dispute_id));
		if (nextStarted && removal.removed_at) {
			await services.repository.endRemovalHistory(user.id, removal.removed_at, operatorPassNow(), 'restored');
			await progress.markNoticePending(next.dispute_id);
			const {notified} = await services.removalService.resumeRemoval(
				user,
				nextStarted,
				true,
				describeDispute(next),
				null,
			);
			await settleNotice(progress, user, next.dispute_id, notified);
			return false;
		}
		await applyBar(services, progress, user, next);
		return false;
	}
	return services.removalService.restore(user.id);
}
