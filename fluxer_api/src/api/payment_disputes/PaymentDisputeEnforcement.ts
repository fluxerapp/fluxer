// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import {Db} from '@app/api/database/CassandraTypes';
import type {PaymentDisputeReviewRow} from '@app/api/database/types/PaymentDisputeTypes';
import type {EnforcementActor} from '@app/api/enforcement/EnforcementActor';
import {
	decisionAuditMetadata,
	type EnforcementDecisionService,
	type PaymentDisputeClosureReason,
} from '@app/api/enforcement/EnforcementDecisionService';
import {hasHolderDeletionPending} from '@app/api/enforcement/SuspensionState';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {resolveContactEmails} from '@app/api/instance/ContactEmails';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import {Logger} from '@app/api/Logger';
import {getWorkerService} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import type {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {
	barOperatorPassAfterDispute,
	liftOperatorPassDisputeBar,
	type OperatorPassDisputeBarProgress,
	type OperatorPassDisputeServices,
} from '@app/api/operator_pass/OperatorPassDisputeBar';
import {followOperatorPassWallStanding} from '@app/api/operator_pass/OperatorPassWallStanding';
import type {
	PaymentDisputeRepository,
	PaymentDisputeReviewPatch,
} from '@app/api/payment_disputes/PaymentDisputeRepository';
import {getPaymentDisputeReviewBlockers, isPaymentDisputeOpen} from '@app/api/payment_disputes/PaymentDisputeReviews';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {reschedulePendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import {type SystemDmDeps, systemDmsHidden, trySendSystemDm} from '@app/api/user/services/SystemDmMessage';
import {isAccountClosed} from '@app/api/user/UserHelpers';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';
import {EnforcementDecisionEndedError} from '@fluxer/errors/src/domains/moderation/EnforcementDecisionEndedError';
import {PaymentDisputeExemptError} from '@fluxer/errors/src/domains/payment/PaymentDisputeExemptError';
import {PaymentDisputeReviewClosedError} from '@fluxer/errors/src/domains/payment/PaymentDisputeReviewClosedError';
import {PaymentDisputeWarningTooLateError} from '@fluxer/errors/src/domains/payment/PaymentDisputeWarningTooLateError';
import {UnknownPaymentDisputeReviewError} from '@fluxer/errors/src/domains/payment/UnknownPaymentDisputeReviewError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {
	getPaymentDisputeWarningVariables,
	paymentDisputeWarningSlots,
	renderEmailTemplateText,
} from '@pkgs/email/src/EmailService';
import type {IEmailService, PaymentDisputeWarningEmail} from '@pkgs/email/src/IEmailService';
import {ms, seconds} from 'itty-time';

const WARNING_LEAD_MS = ms('3 days');
const MIN_WARNING_NOTICE_MS = ms('2 days');
const DELETION_DELAY_DAYS = 60;
const UPDATE_ATTEMPTS = 3;
const DECISION_LOCK_TTL_SECONDS = seconds('2 minutes');

export type PaymentDisputeDecision = 'justified' | 'unjustified';

export interface PaymentDisputeEnforcementDependencies {
	repository: PaymentDisputeRepository;
	userRepository: UserRepository;
	userCacheService: UserCacheService;
	emailService: IEmailService;
	cacheService: ICacheService;
	gatewayService: IGatewayService;
	auditService: AdminAuditService;
	kvDeletionQueue: KVAccountDeletionQueueService;
	oauth2Tokens: Pick<OAuth2TokenRepository, 'deleteAllAccessTokensForUser' | 'deleteAllRefreshTokensForUser'>;
	systemDm: SystemDmDeps | null;
	enforcement: EnforcementDecisionService;
	operatorPass: OperatorPassDisputeServices | null;
}

export function getPaymentDisputeWarningDeadline(evidenceDueBy: Date | null, now: Date): Date | null {
	if (!evidenceDueBy) {
		return null;
	}
	const deadline = new Date(evidenceDueBy.getTime() - WARNING_LEAD_MS);
	return deadline.getTime() - now.getTime() >= MIN_WARNING_NOTICE_MS ? deadline : null;
}

export interface PaymentDisputeDecisionResult {
	review: PaymentDisputeReviewRow;
	emailSent: boolean;
	dmSent: boolean;
}

function toDecisionResult(review: PaymentDisputeReviewRow): PaymentDisputeDecisionResult {
	return {review, emailSent: review.warning_email_sent_at != null, dmSent: review.warning_dm_sent_at != null};
}

function isPaymentDisputeWarningIncomplete(review: PaymentDisputeReviewRow): boolean {
	return review.status === 'unjustified' && review.warned_at == null && review.reversed_at == null;
}

function isLostAfterWarning(review: PaymentDisputeReviewRow): boolean {
	return (
		review.outcome === 'lost' &&
		review.status === 'unjustified' &&
		review.warned_at != null &&
		review.warning_deadline != null &&
		review.closed_at != null &&
		review.closed_at.getTime() >= review.warning_deadline.getTime() &&
		review.reversed_at == null &&
		getPaymentDisputeReviewBlockers(review).length === 0
	);
}

export function shouldCloseAfterPaymentDispute(review: PaymentDisputeReviewRow): boolean {
	return review.product !== 'operator_pass' && review.enforcement_decision_id == null && isLostAfterWarning(review);
}

function operatorPassDisputeAuditKey(disputeId: string): string {
	return `payment_dispute:operator_pass_bar:audit_pending:${disputeId}`;
}

function operatorPassDisputeNoticeKey(disputeId: string): string {
	return `payment_dispute:operator_pass_bar:notice_pending:${disputeId}`;
}

export function shouldBarAfterOperatorPassDispute(review: PaymentDisputeReviewRow): boolean {
	return review.product === 'operator_pass' && isLostAfterWarning(review);
}

function getClosingDecisionId(user: User): bigint | null {
	return user.enforcementDecisionId !== null && isAccountClosed(user) ? user.enforcementDecisionId : null;
}

export async function getClosuresAwaited(
	users: Pick<UserRepository, 'findUnique'>,
	reviews: ReadonlyArray<PaymentDisputeReviewRow>,
): Promise<Map<string, bigint>> {
	const awaited = new Map<string, bigint>();
	const holders = new Map<UserID, bigint | null>();
	for (const review of reviews) {
		if (!shouldCloseAfterPaymentDispute(review)) {
			continue;
		}
		if (!holders.has(review.purchaser_id)) {
			const user = await users.findUnique(review.purchaser_id);
			holders.set(review.purchaser_id, user && !user.deletionStartedAt ? getClosingDecisionId(user) : null);
		}
		const decisionId = holders.get(review.purchaser_id);
		if (decisionId != null) {
			awaited.set(review.dispute_id, decisionId);
		}
	}
	return awaited;
}

export async function queuePaymentDisputeClosures(purchaserId: UserID): Promise<void> {
	await getWorkerService().addJob('closePaymentDisputeAccounts', {purchaser_id: purchaserId.toString()});
}

export class PaymentDisputeEnforcementService {
	constructor(private readonly deps: PaymentDisputeEnforcementDependencies) {}

	async decide(params: {
		disputeId: string;
		decision: PaymentDisputeDecision;
		note: string | null;
		adminUserId: UserID;
	}): Promise<PaymentDisputeDecisionResult> {
		return this.withDecisionLock(params.disputeId, () => this.decideLocked(params));
	}

	private async decideLocked(params: {
		disputeId: string;
		decision: PaymentDisputeDecision;
		note: string | null;
		adminUserId: UserID;
	}): Promise<PaymentDisputeDecisionResult> {
		const review = await this.requireReview(params.disputeId);
		if (params.decision === 'unjustified' && isPaymentDisputeWarningIncomplete(review)) {
			return this.completeWarning(review);
		}
		if (review.status !== 'open') {
			return toDecisionResult(review);
		}
		if (!isPaymentDisputeOpen(review)) {
			throw new PaymentDisputeReviewClosedError();
		}
		const now = new Date();
		let warningDeadline: Date | null = null;
		if (params.decision === 'unjustified') {
			const blockers = getPaymentDisputeReviewBlockers(review);
			if (blockers.length > 0) {
				throw new PaymentDisputeExemptError(blockers);
			}
			warningDeadline = getPaymentDisputeWarningDeadline(review.evidence_due_by, now);
			if (!warningDeadline) {
				throw new PaymentDisputeWarningTooLateError();
			}
		}
		const decided = await this.deps.repository.updateReview(review, {
			status: Db.set(params.decision),
			decided_at: Db.set(now),
			decided_by: Db.set(params.adminUserId),
			decision_note: Db.set(params.note),
			warning_deadline: Db.set(warningDeadline),
		});
		if (!decided) {
			throw new ConflictError({code: APIErrorCodes.CONFLICT, message: 'The review changed while it was being decided'});
		}
		if (!warningDeadline) {
			return toDecisionResult(decided);
		}
		return this.completeWarning(decided);
	}

	async reverse(params: {
		disputeId: string;
		note: string;
		actor: EnforcementActor;
	}): Promise<{review: PaymentDisputeReviewRow; decisionEnded: boolean}> {
		return this.withDecisionLock(params.disputeId, async () => {
			const review = await this.requireReview(params.disputeId);
			return this.withAccountLock(review.purchaser_id, () => this.reverseLocked(params));
		});
	}

	private async reverseLocked(params: {
		disputeId: string;
		note: string;
		actor: EnforcementActor;
	}): Promise<{review: PaymentDisputeReviewRow; decisionEnded: boolean}> {
		const review = await this.requireReview(params.disputeId);
		if (review.status !== 'unjustified' || review.reversed_at) {
			throw new ConflictError({
				code: APIErrorCodes.CONFLICT,
				message: 'Only an unjustified decision that has not been reversed can be reversed',
			});
		}
		const decisionEnded = review.enforcement_decision_id
			? await this.releaseClosure(review, review.enforcement_decision_id, params)
			: false;
		const operatorPass = this.deps.operatorPass;
		if (review.product === 'operator_pass' && operatorPass) {
			const user = await this.deps.userRepository.findUnique(review.purchaser_id);
			if (user) {
				const otherGrounds = (await this.deps.repository.listReviewsByPurchaser(review.purchaser_id)).filter(
					(other) => other.dispute_id !== review.dispute_id && shouldBarAfterOperatorPassDispute(other),
				);
				await liftOperatorPassDisputeBar(operatorPass, this.disputeBarProgress, user, review.dispute_id, otherGrounds);
			}
		}
		const reversed = await this.patchReview(review.dispute_id, {
			status: Db.set('justified'),
			reversed_at: Db.set(new Date()),
			reversed_by: Db.set(params.actor.adminUserId),
			reversal_note: Db.set(params.note),
		});
		if (!reversed) {
			throw new ConflictError({
				code: APIErrorCodes.CONFLICT,
				message: 'The review changed while it was being reversed',
			});
		}
		return {review: reversed, decisionEnded};
	}

	private async releaseClosure(
		review: PaymentDisputeReviewRow,
		decisionId: bigint,
		params: {note: string; actor: EnforcementActor},
	): Promise<boolean> {
		const others = await this.deps.repository.listReviewsByPurchaser(review.purchaser_id);
		const remaining = others.find(
			(other) =>
				other.dispute_id !== review.dispute_id &&
				other.enforcement_decision_id === decisionId &&
				other.status === 'unjustified' &&
				other.reversed_at == null,
		);
		if (remaining) {
			await this.deps.enforcement.addNote(
				decisionId,
				`Payment dispute ${review.dispute_id} was reversed after an appeal: ${params.note}\n\nThe closure stays in place for payment dispute ${remaining.dispute_id}`,
				params.actor,
			);
			return false;
		}
		try {
			await this.deps.enforcement.end(decisionId, {outcome: 'reversed'}, {...params.actor, internalNote: params.note});
			return true;
		} catch (error) {
			if (error instanceof EnforcementDecisionEndedError) {
				return false;
			}
			throw error;
		}
	}

	async closeWaitingAccounts(reviews: ReadonlyArray<PaymentDisputeReviewRow>): Promise<number> {
		let failed = 0;
		for (const review of reviews) {
			const close = shouldCloseAfterPaymentDispute(review);
			if (!close && !shouldBarAfterOperatorPassDispute(review)) {
				continue;
			}
			const user = await this.deps.userRepository.findUnique(review.purchaser_id);
			if (!user || user.deletionStartedAt) {
				continue;
			}
			try {
				if (close) {
					await this.closeAccount(review);
				} else {
					await this.barOperatorPass(review);
				}
			} catch (error) {
				failed++;
				Logger.warn(
					{disputeId: review.dispute_id, error},
					'Failed to close the account of a lost unjustified payment dispute, the next run retries it',
				);
			}
		}
		return failed;
	}

	async barOperatorPass(review: PaymentDisputeReviewRow): Promise<boolean> {
		const services = this.deps.operatorPass;
		if (!services) {
			return false;
		}
		return this.withDecisionLock(review.dispute_id, () =>
			this.withAccountLock(review.purchaser_id, () => this.barOperatorPassLocked(services, review.dispute_id)),
		);
	}

	private async barOperatorPassLocked(services: OperatorPassDisputeServices, disputeId: string): Promise<boolean> {
		const current = await this.requireReview(disputeId);
		if (!shouldBarAfterOperatorPassDispute(current)) {
			return false;
		}
		const user = await this.deps.userRepository.findUnique(current.purchaser_id);
		if (!user || user.deletionStartedAt) {
			return false;
		}
		const barred = await barOperatorPassAfterDispute(services, this.disputeBarProgress, user, current);
		if (barred) {
			await this.deps.auditService.createAuditLog({
				adminUserId: SYSTEM_USER_ID,
				targetType: 'user',
				targetId: BigInt(user.id),
				action: 'bar_operator_pass_after_payment_dispute',
				auditLogReason: `Payment dispute ${current.dispute_id} on Operator Pass order ${current.order_id} was found unjustified, the account was warned and the dispute ended with the money returned after ${current.warning_deadline?.toISOString()}`,
				metadata: new Map([
					['dispute_id', current.dispute_id],
					['decided_by', (current.decided_by ?? SYSTEM_USER_ID).toString()],
				]),
			});
		}
		return barred;
	}

	private readonly disputeBarProgress: OperatorPassDisputeBarProgress = {
		isAuditPending: async (disputeId) =>
			(await this.deps.cacheService.get<boolean>(operatorPassDisputeAuditKey(disputeId))) === true,
		markAuditPending: async (disputeId) => {
			await this.deps.cacheService.set(operatorPassDisputeAuditKey(disputeId), true);
		},
		clearAuditPending: async (disputeId) => {
			await this.deps.cacheService.delete(operatorPassDisputeAuditKey(disputeId));
		},
		isNoticePending: async (disputeId) =>
			(await this.deps.cacheService.get<boolean>(operatorPassDisputeNoticeKey(disputeId))) === true,
		markNoticePending: async (disputeId) => {
			await this.deps.cacheService.set(operatorPassDisputeNoticeKey(disputeId), true);
		},
		clearNoticePending: async (disputeId) => {
			await this.deps.cacheService.delete(operatorPassDisputeNoticeKey(disputeId));
		},
	};

	async closeAccount(review: PaymentDisputeReviewRow): Promise<PaymentDisputeReviewRow> {
		return this.withDecisionLock(review.dispute_id, () =>
			this.withAccountLock(review.purchaser_id, () => this.closeAccountLocked(review.dispute_id)),
		);
	}

	private async closeAccountLocked(disputeId: string): Promise<PaymentDisputeReviewRow> {
		const review = await this.requireReview(disputeId);
		if (!shouldCloseAfterPaymentDispute(review)) {
			return review;
		}
		const user = await this.deps.userRepository.findUnique(review.purchaser_id);
		if (!user || user.deletionStartedAt) {
			Logger.warn({disputeId: review.dispute_id}, 'Account of a lost unjustified payment dispute is already gone');
			return review;
		}
		const reasonCode = DeletionReasons.BILLING_DISPUTE_OR_ABUSE;
		const closingDecisionId = getClosingDecisionId(user);
		if (closingDecisionId !== null) {
			return this.joinClosure(review, user, closingDecisionId, reasonCode);
		}
		const decidedBy = review.decided_by ?? SYSTEM_USER_ID;
		const latestDeletionAt = new Date();
		latestDeletionAt.setDate(latestDeletionAt.getDate() + DELETION_DELAY_DAYS);
		const deletionAt =
			user.pendingDeletionAt && user.pendingDeletionAt.getTime() < latestDeletionAt.getTime()
				? user.pendingDeletionAt
				: latestDeletionAt;
		const internalNote = [
			`Payment dispute ${review.dispute_id} on ${review.product} order ${review.order_id} was found unjustified, the account was warned and the dispute ended with the money returned after ${review.warning_deadline?.toISOString()}`,
			...(hasHolderDeletionPending(user) && user.pendingDeletionAt
				? [
						`The account holder had already scheduled the account for deletion on ${user.pendingDeletionAt.toISOString()}, and the closure keeps the earlier date`,
					]
				: []),
		].join('\n\n');
		const decision = await this.deps.enforcement.recordPaymentDisputeClosure(
			{
				subject: user,
				reasonCode,
				decidedBy,
				paymentAt: review.purchased_at ?? review.disputed_at ?? review.created_at,
				deletionAt,
				statement: review.decision_note ?? null,
				internalNote,
			},
			async (decisionId) => {
				await this.applyClosure(user, {reasonCode, deletionAt, decidedBy, decisionId, internalNote});
			},
		);
		const marked = await this.linkDecision(review, decision.decision_id);
		await this.deps.auditService.createAuditLog({
			adminUserId: SYSTEM_USER_ID,
			targetType: 'user',
			targetId: BigInt(user.id),
			action: 'close_account_after_payment_dispute',
			auditLogReason: internalNote,
			metadata: new Map([
				['dispute_id', review.dispute_id],
				['product', review.product],
				['pending_deletion_at', deletionAt.toISOString()],
				['decided_by', decidedBy.toString()],
				...decisionAuditMetadata(decision),
			]),
		});
		Logger.info(
			{userId: user.id, disputeId: review.dispute_id, decisionId: decision.decision_id.toString()},
			'Closed the account after a lost unjustified dispute',
		);
		return marked;
	}

	private async joinClosure(
		review: PaymentDisputeReviewRow,
		user: User,
		decisionId: bigint,
		reasonCode: PaymentDisputeClosureReason,
	): Promise<PaymentDisputeReviewRow> {
		const decision = await this.deps.enforcement.resumePaymentDisputeClosure(user, decisionId, reasonCode);
		if (!decision) {
			Logger.info(
				{disputeId: review.dispute_id, decisionId: decisionId.toString()},
				'Account of a lost unjustified payment dispute is closed by another decision, the closure waits for it to end',
			);
			return review;
		}
		const deletionAt = user.pendingDeletionAt ?? decision.deletion_at;
		if (deletionAt) {
			await this.enforceClosure(user, user, deletionAt, reasonCode);
		}
		const marked = await this.linkDecision(review, decisionId);
		await this.deps.auditService.createAuditLog({
			adminUserId: SYSTEM_USER_ID,
			targetType: 'user',
			targetId: BigInt(user.id),
			action: 'close_account_after_payment_dispute',
			auditLogReason: `Payment dispute ${review.dispute_id} was found unjustified and ended with the money returned while the account was already closed for a payment dispute`,
			metadata: new Map([
				['dispute_id', review.dispute_id],
				['product', review.product],
				['pending_deletion_at', user.pendingDeletionAt?.toISOString() ?? ''],
				['decided_by', (review.decided_by ?? SYSTEM_USER_ID).toString()],
				...decisionAuditMetadata(decision),
			]),
		});
		return marked;
	}

	private async linkDecision(review: PaymentDisputeReviewRow, decisionId: bigint): Promise<PaymentDisputeReviewRow> {
		const marked = await this.patchReview(review.dispute_id, {enforcement_decision_id: Db.set(decisionId)});
		if (!marked) {
			throw new ConflictError({
				code: APIErrorCodes.CONFLICT,
				message: 'The review changed while the closure was being linked to it',
			});
		}
		return marked;
	}

	private async applyClosure(
		user: User,
		params: {
			reasonCode: PaymentDisputeClosureReason;
			deletionAt: Date;
			decidedBy: UserID;
			decisionId: bigint;
			internalNote: string;
		},
	): Promise<void> {
		const flags = (user.flags | UserFlags.DELETED) & ~UserFlags.SELF_DELETED;
		const updated = await this.deps.userRepository.updateDeletionSchedule(user, {
			flags: user.tempBannedUntil ? flags & ~UserFlags.DISABLED : flags,
			pending_deletion_at: params.deletionAt,
			deletion_reason_code: params.reasonCode,
			deletion_public_reason: null,
			deletion_audit_log_reason: params.internalNote,
			deletion_scheduled_by: params.decidedBy,
			deletion_scheduled_at: new Date(),
			enforcement_decision_id: params.decisionId,
			temp_banned_until: null,
		});
		await this.enforceClosure(user, updated, params.deletionAt, params.reasonCode);
		await followOperatorPassWallStanding(user, updated);
	}

	private async enforceClosure(
		user: User,
		updated: User,
		deletionAt: Date,
		reasonCode: PaymentDisputeClosureReason,
	): Promise<void> {
		await reschedulePendingDeletion({
			userId: user.id,
			currentPendingDeletionAt: user.pendingDeletionAt,
			nextPendingDeletionAt: deletionAt,
			deletionReasonCode: reasonCode,
			deletionQueue: this.deps.kvDeletionQueue,
		});
		await revokeAllAuthSessions({users: this.deps.userRepository, gateway: this.deps.gatewayService}, user.id);
		await this.deps.oauth2Tokens.deleteAllAccessTokensForUser(user.id);
		await this.deps.oauth2Tokens.deleteAllRefreshTokensForUser(user.id);
		await this.deps.userCacheService.setUserPartialResponseFromUser(updated);
	}

	private async completeWarning(review: PaymentDisputeReviewRow): Promise<PaymentDisputeDecisionResult> {
		if (!isPaymentDisputeOpen(review)) {
			throw new PaymentDisputeReviewClosedError();
		}
		const warningDate = review.warning_deadline;
		if (!warningDate || warningDate.getTime() - Date.now() < MIN_WARNING_NOTICE_MS) {
			throw new PaymentDisputeWarningTooLateError();
		}
		const user = await this.deps.userRepository.findUnique(review.purchaser_id);
		if (!user) {
			return toDecisionResult(review);
		}
		const params: PaymentDisputeWarningEmail = {
			product: review.product,
			invoiceNumber: review.invoice_number ?? null,
			orderDate: review.purchased_at ?? review.disputed_at ?? review.created_at,
			amountMinor: review.amount_minor,
			currency: review.currency,
			warningDate,
			note: review.decision_note ?? null,
		};
		let current = review;
		if (!current.warning_email_sent_at && user.email) {
			if (await this.deps.emailService.send('payment_dispute_warning', user.email, params, user.locale)) {
				current = await this.markWarningSent(current, {warning_email_sent_at: Db.set(new Date())});
			}
		}
		const dmNeeded = !(await systemDmsHidden());
		if (dmNeeded && !current.warning_dm_sent_at && (await this.sendWarningDm(user, params))) {
			current = await this.markWarningSent(current, {warning_dm_sent_at: Db.set(new Date())});
		}
		if (current.warning_email_sent_at && (current.warning_dm_sent_at || !dmNeeded)) {
			current = await this.markWarningSent(current, {warned_at: Db.set(new Date())});
		} else {
			Logger.warn(
				{
					userId: user.id,
					disputeId: review.dispute_id,
					emailSent: current.warning_email_sent_at != null,
					dmSent: current.warning_dm_sent_at != null,
				},
				'Payment dispute warning is incomplete, deciding unjustified again retries what is missing',
			);
		}
		return toDecisionResult(current);
	}

	private async markWarningSent(
		review: PaymentDisputeReviewRow,
		patch: PaymentDisputeReviewPatch,
	): Promise<PaymentDisputeReviewRow> {
		const updated = await this.patchReview(review.dispute_id, patch);
		if (!updated) {
			throw new ConflictError({code: APIErrorCodes.CONFLICT, message: 'The review changed while it was being warned'});
		}
		return updated;
	}

	private async sendWarningDm(user: User, params: PaymentDisputeWarningEmail): Promise<boolean> {
		const systemDm = this.deps.systemDm;
		if (!systemDm) {
			return false;
		}
		const contactEmails = resolveContactEmails();
		const content = renderEmailTemplateText(
			'payment_dispute_warning',
			user.locale,
			getPaymentDisputeWarningVariables(params, {
				marketingBaseUrl: Config.endpoints.marketing,
				appealsEmail: contactEmails.appealsEmail,
				privacyEmail: contactEmails.privacyEmail,
			}),
			getInstanceProductName(),
			(template) => paymentDisputeWarningSlots(template, params, user.locale, Config.endpoints.marketing),
			{format: 'markdown'},
		);
		if (content === null) {
			Logger.warn({userId: user.id}, 'Payment dispute warning DM template could not be resolved');
			return false;
		}
		return trySendSystemDm(systemDm, this.deps.userRepository, user.id, content);
	}

	private async withDecisionLock<T>(disputeId: string, action: () => Promise<T>): Promise<T> {
		return this.withLock(
			`payment_dispute:decision_lock:${disputeId}`,
			'The review is being decided in another request',
			action,
		);
	}

	private async withAccountLock<T>(purchaserId: UserID, action: () => Promise<T>): Promise<T> {
		return this.withLock(
			`payment_dispute:account_lock:${purchaserId}`,
			'Another payment dispute of the account is being closed or reversed',
			action,
		);
	}

	private async withLock<T>(lockKey: string, message: string, action: () => Promise<T>): Promise<T> {
		const token = await this.deps.cacheService.acquireLock(lockKey, DECISION_LOCK_TTL_SECONDS);
		if (!token) {
			throw new ConflictError({code: APIErrorCodes.CONFLICT, message});
		}
		try {
			return await action();
		} finally {
			await this.deps.cacheService.releaseLock(lockKey, token);
		}
	}

	private async requireReview(disputeId: string): Promise<PaymentDisputeReviewRow> {
		const review = await this.deps.repository.findReview(disputeId);
		if (!review) {
			throw new UnknownPaymentDisputeReviewError();
		}
		return review;
	}

	private async patchReview(
		disputeId: string,
		patch: PaymentDisputeReviewPatch,
	): Promise<PaymentDisputeReviewRow | null> {
		for (let attempt = 0; attempt < UPDATE_ATTEMPTS; attempt++) {
			const current = await this.deps.repository.findReview(disputeId);
			if (!current) {
				return null;
			}
			const updated = await this.deps.repository.updateReview(current, patch);
			if (updated) {
				return updated;
			}
		}
		return null;
	}
}
