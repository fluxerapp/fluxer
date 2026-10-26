// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {mapUserToAdminResponse} from '@app/api/admin/models/UserTypes';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {AdminBanManagementService} from '@app/api/admin/services/AdminBanManagementService';
import {trySendAdminNotification} from '@app/api/admin/services/AdminNotification';
import {collectKnownUserIps} from '@app/api/admin/services/KnownUserIps';
import {type ReporterResolutionNotifier, wasReporterNotified} from '@app/api/admin/services/ReporterResolutionNotifier';
import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import {createReportID, type UserID} from '@app/api/BrandedTypes';
import type {BillingRepository} from '@app/api/billing/repositories/BillingRepository';
import {Config} from '@app/api/Config';
import type {EnforcementDecisionRow} from '@app/api/database/types/EnforcementTypes';
import {cancelClosure} from '@app/api/enforcement/ClosureState';
import type {EnforcementActor} from '@app/api/enforcement/EnforcementActor';
import {
	decisionAuditMetadata,
	type EnforcementDecisionService,
	networkBlockAuditMetadata,
	type RecordedDecision,
} from '@app/api/enforcement/EnforcementDecisionService';
import type {EnforcementNoticeDelivery} from '@app/api/enforcement/EnforcementNoticeDelivery';
import type {SkippedNetwork} from '@app/api/enforcement/NetworkBlocks';
import {formatNoticeDate} from '@app/api/enforcement/NoticeDates';
import {emitAdminAction} from '@app/api/infrastructure/activity/AccountChangeEvents';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import type {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {type IARSubmission, ReportStatus} from '@app/api/report/ReportModels';
import type {ReportService} from '@app/api/report/ReportService';
import {getReportSearchService} from '@app/api/SearchFactory';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {isEnforcementDeletionReason} from '@app/api/user/ProfileVisibility';
import {
	type PartialUserChangePropagationDeps,
	propagateUserUpdate,
} from '@app/api/user/services/PartialUserChangePropagation';
import {reschedulePendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import {isDeletionComplete, isHeldByDecision} from '@app/api/user/UserHelpers';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {
	ACCOUNT_DELETION_MIN_DAYS,
	ENFORCEMENT_CLOSURE_MIN_DAYS,
	ENFORCEMENT_REASONS,
	ENFORCEMENT_RULES,
} from '@fluxer/constants/src/EnforcementReasons';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {NoPendingDeletionError} from '@fluxer/errors/src/domains/core/NoPendingDeletionError';
import {EnforcementDecisionOwnsStateError} from '@fluxer/errors/src/domains/moderation/EnforcementDecisionOwnsStateError';
import {ReportAlreadyResolvedError} from '@fluxer/errors/src/domains/moderation/ReportAlreadyResolvedError';
import type {
	AdminUserDeletionCancelRequest,
	AdminUserDeletionScheduleRequest,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type Stripe from 'stripe';

interface AdminUserDeletionServiceDeps {
	apiContext: ApiContext;
	auditService: AdminAuditService;
	banManagementService: AdminBanManagementService;
	reportService: ReportService;
	userUpdateDeps: PartialUserChangePropagationDeps;
	kvDeletionQueue: KVAccountDeletionQueueService;
	stripe: Stripe | null;
	billingRepository: BillingRepository;
	oauth2Tokens: Pick<OAuth2TokenRepository, 'deleteAllAccessTokensForUser' | 'deleteAllRefreshTokensForUser'>;
	storeEntitlementService: StoreEntitlementService;
	reporterResolutionNotifier: ReporterResolutionNotifier;
	enforcement: EnforcementDecisionService;
	delivery: EnforcementNoticeDelivery;
}

type AutoResolveOutcome = 'completed' | 'search_unavailable' | 'enumeration_failed';

interface AutoResolveSummary {
	outcome: AutoResolveOutcome;
	foundCount: number;
	resolvedCount: number;
	failedCount: number;
	notifiedCount: number;
}

type ClosureBasisType = AdminUserDeletionScheduleRequest['basis']['type'];

const BASIS_DELETION_REASONS = {
	holder_request: DeletionReasons.USER_REQUESTED,
	housekeeping: DeletionReasons.INACTIVITY,
} as const;

const reportResolvingDeletionReasons: ReadonlySet<number> = new Set([
	DeletionReasons.SPAM,
	DeletionReasons.CHEATING_OR_EXPLOITATION,
	DeletionReasons.COORDINATED_RAIDING,
	DeletionReasons.AUTOMATION_OR_SELFBOT,
	DeletionReasons.SCAM_OR_SOCIAL_ENGINEERING,
	DeletionReasons.HARASSMENT_OR_BULLYING,
	DeletionReasons.BAN_EVASION,
	DeletionReasons.TOKEN_OR_CREDENTIAL_SCAM,
	DeletionReasons.HATE_SPEECH_OR_EXTREMIST_CONTENT,
	DeletionReasons.MALICIOUS_LINKS_OR_MALWARE,
	DeletionReasons.IMPERSONATION_OR_FAKE_IDENTITY,
]);

const manuallyResolvedReportCategories: ReadonlySet<string> = new Set(['child_safety', 'underage_user', 'self_harm']);

function describePendingDeletion(user: User, prefix: string): Array<[string, string]> {
	if (!user.pendingDeletionAt) return [];
	return [
		[`${prefix}_pending_deletion_at`, user.pendingDeletionAt.toISOString()],
		[`${prefix}_scheduled_by`, user.deletionScheduledBy?.toString() ?? ''],
		[`${prefix}_scheduled_at`, user.deletionScheduledAt?.toISOString() ?? ''],
		[`${prefix}_reason_code`, user.deletionReasonCode?.toString() ?? ''],
	];
}

function sameInstant(left: Date, right: string): boolean {
	return left.getTime() === new Date(right).getTime();
}

export function resolveDeletionDays(basis: ClosureBasisType, requestedDays: number): number {
	const appealable = basis === 'enforcement' && !Config.instance.selfHosted;
	return Math.max(requestedDays, appealable ? ENFORCEMENT_CLOSURE_MIN_DAYS : ACCOUNT_DELETION_MIN_DAYS);
}

function closedFlags(user: User, enforcement: boolean): bigint {
	if (!enforcement) {
		return (user.flags & ~UserFlags.DELETED) | UserFlags.SELF_DELETED;
	}
	const flags = (user.flags | UserFlags.DELETED) & ~UserFlags.SELF_DELETED;
	return user.tempBannedUntil ? flags & ~UserFlags.DISABLED : flags;
}

export class AdminUserDeletionService {
	constructor(private readonly deps: AdminUserDeletionServiceDeps) {}

	async scheduleAccountDeletion(userId: UserID, request: AdminUserDeletionScheduleRequest, actor: EnforcementActor) {
		const {user, skippedNetworks} = await this.applyScheduledDeletion(userId, request, actor);
		return {
			user: await mapUserToAdminResponse(user, this.deps.apiContext.services.cache, actor.acls),
			skipped_ips: skippedNetworks,
		};
	}

	async applyScheduledDeletion(
		userId: UserID,
		request: AdminUserDeletionScheduleRequest,
		actor: EnforcementActor,
	): Promise<{user: User; skippedNetworks: Array<SkippedNetwork>}> {
		const {basis} = request;
		const user = await this.deps.apiContext.services.users.findUniqueAssert(userId);
		if (isDeletionComplete(user)) {
			throw new ConflictError({
				code: APIErrorCodes.CONFLICT,
				message: 'This account has already been deleted',
			});
		}
		if (
			user.pendingDeletionAt &&
			(!request.replace_pending_deletion_at ||
				!sameInstant(user.pendingDeletionAt, request.replace_pending_deletion_at))
		) {
			throw new ConflictError({
				code: APIErrorCodes.CONFLICT,
				message: 'A deletion is already scheduled for this account',
			});
		}
		if (basis.type !== 'enforcement') {
			if (isHeldByDecision(user)) {
				throw new EnforcementDecisionOwnsStateError(user.enforcementDecisionId!);
			}
			if (request.remove_evidence) {
				throw InputValidationError.fromCode('remove_evidence', ValidationErrorCodes.ENFORCEMENT_EVIDENCE_REQUIRED);
			}
			if (request.block_networks) {
				throw InputValidationError.fromCode(
					'block_networks',
					ValidationErrorCodes.ENFORCEMENT_NETWORK_BLOCK_NOT_ALLOWED,
				);
			}
		}
		const immediate = request.immediate === true;
		if (immediate && !Config.instance.selfHosted) {
			throw InputValidationError.create('immediate', 'Immediate deletion is only available on self-hosted instances');
		}
		const days = immediate ? 0 : resolveDeletionDays(basis.type, request.days_until_deletion);
		const scheduledAt = new Date();
		const pendingDeletionAt = new Date(scheduledAt);
		pendingDeletionAt.setDate(pendingDeletionAt.getDate() + days);
		let closed = user;
		let recorded: RecordedDecision | null = null;
		if (basis.type === 'enforcement') {
			recorded = await this.deps.enforcement.record(
				{
					subject: {type: 'user', user},
					kind: 'closure',
					input: basis.decision,
					endsAt: null,
					deletionAt: pendingDeletionAt,
					removeEvidence: request.remove_evidence ?? false,
					blockNetworks: request.block_networks,
					actor,
				},
				async (decisionId) => {
					closed = await this.writeClosure(user, {
						reasonCode: basis.decision.reason_code,
						pendingDeletionAt,
						scheduledAt,
						actorId: actor.adminUserId,
						decisionId,
					});
				},
			);
		} else {
			closed = await this.writeClosure(user, {
				reasonCode: BASIS_DELETION_REASONS[basis.type],
				pendingDeletionAt,
				scheduledAt,
				actorId: actor.adminUserId,
				decisionId: null,
			});
		}
		await this.cancelSubscription(user, actor.adminUserId);
		await this.deps.storeEntitlementService.revokeForBannedUser(userId);
		const notificationSent =
			basis.type === 'enforcement' || immediate
				? null
				: await this.sendScheduledEmail(user, basis.type, pendingDeletionAt);
		const notificationSentValue = notificationSent ? 'true' : 'false';
		await this.deps.auditService.createAuditLog({
			adminUserId: actor.adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'schedule_deletion',
			auditLogReason: actor.internalNote,
			metadata: new Map([
				['basis', basis.type],
				['days', days.toString()],
				['reason_code', closed.deletionReasonCode?.toString() ?? ''],
				['pending_deletion_at', pendingDeletionAt.toISOString()],
				...(immediate ? ([['immediate', 'true']] as Array<[string, string]>) : []),
				...describePendingDeletion(user, 'replaced'),
				...(recorded
					? [...decisionAuditMetadata(recorded.decision), ...networkBlockAuditMetadata(recorded)]
					: ([['notification_sent', notificationSentValue]] as Array<[string, string]>)),
			]),
		});
		const decision = recorded?.decision ?? null;
		const knownIps = decision ? await this.banIdentifiers(user, decision, actor) : new Set<string>();
		if (decision && reportResolvingDeletionReasons.has(decision.reason_code)) {
			await this.resolvePendingReportsAgainstUser({user, adminUserId: actor.adminUserId});
		}
		await emitAdminAction(actor.adminUserId, userId, 'schedule_deletion', {
			reasonCode: closed.deletionReasonCode,
			ips: knownIps,
		});
		await propagateUserUpdate(this.deps.userUpdateDeps, user, closed);
		if (immediate) {
			await this.deps.apiContext.services.worker.addJob('userProcessPendingDeletion', {
				userId: userId.toString(),
				deletionReasonCode: closed.deletionReasonCode ?? BASIS_DELETION_REASONS.housekeeping,
				pendingDeletionAt: pendingDeletionAt.toISOString(),
			});
		}
		return {user: closed, skippedNetworks: recorded?.skippedNetworks ?? []};
	}

	async cancelAccountDeletion(userId: UserID, data: AdminUserDeletionCancelRequest, actor: EnforcementActor) {
		const {cache} = this.deps.apiContext.services;
		const user = await this.deps.apiContext.services.users.findUniqueAssert(userId);
		if (!user.pendingDeletionAt) {
			throw new NoPendingDeletionError();
		}
		if (user.enforcementDecisionId !== null) {
			throw new EnforcementDecisionOwnsStateError(user.enforcementDecisionId);
		}
		if (!sameInstant(user.pendingDeletionAt, data.expected_pending_deletion_at)) {
			throw new ConflictError({
				code: APIErrorCodes.CONFLICT,
				message: 'The pending deletion does not match expected_pending_deletion_at',
			});
		}
		const cancelled = await cancelClosure(this.deps, user);
		const enforcementClosure = isEnforcementDeletionReason(user.deletionReasonCode);
		if (enforcementClosure) {
			await this.deps.delivery.sendLegacyEnded(user);
		} else {
			await this.sendCancelledEmail(user);
		}
		await this.deps.auditService.createAuditLog({
			adminUserId: actor.adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'cancel_deletion',
			auditLogReason: actor.internalNote,
			metadata: new Map([
				...describePendingDeletion(user, 'cancelled'),
				['notice', enforcementClosure ? 'enforcement_ended' : 'account_deletion_cancelled'],
			]),
		});
		await emitAdminAction(actor.adminUserId, userId, 'cancel_deletion');
		return {user: await mapUserToAdminResponse(cancelled, cache, actor.acls)};
	}

	private async writeClosure(
		user: User,
		params: {
			reasonCode: number;
			pendingDeletionAt: Date;
			scheduledAt: Date;
			actorId: UserID;
			decisionId: bigint | null;
		},
	): Promise<User> {
		const {users} = this.deps.apiContext.services;
		const {decisionId} = params;
		const closed = await users.updateDeletionSchedule(user, {
			flags: closedFlags(user, decisionId !== null),
			pending_deletion_at: params.pendingDeletionAt,
			deletion_reason_code: params.reasonCode,
			deletion_scheduled_by: params.actorId,
			deletion_scheduled_at: params.scheduledAt,
			...(decisionId !== null ? {enforcement_decision_id: decisionId, temp_banned_until: null} : {}),
		});
		await reschedulePendingDeletion({
			userId: user.id,
			currentPendingDeletionAt: user.pendingDeletionAt,
			nextPendingDeletionAt: params.pendingDeletionAt,
			deletionReasonCode: params.reasonCode,
			deletionQueue: this.deps.kvDeletionQueue,
		});
		await revokeAllAuthSessions(this.deps.apiContext.services, user.id);
		await this.deps.oauth2Tokens.deleteAllAccessTokensForUser(user.id);
		await this.deps.oauth2Tokens.deleteAllRefreshTokensForUser(user.id);
		return closed;
	}

	private async sendScheduledEmail(
		user: User,
		basis: keyof typeof BASIS_DELETION_REASONS,
		deletionDate: Date,
	): Promise<boolean> {
		const {email: emailService} = this.deps.apiContext.services;
		const email = user.email;
		if (!email) return false;
		const date = formatNoticeDate(deletionDate, user.locale);
		return trySendAdminNotification(
			() =>
				basis === 'holder_request'
					? emailService.send('account_deletion_scheduled_requested', email, {deletionDate: date}, user.locale)
					: emailService.send('account_deletion_scheduled_inactivity', email, {deletionDate: date}, user.locale),
			{action: 'schedule_deletion', targetId: user.id.toString()},
		);
	}

	private async sendCancelledEmail(user: User): Promise<void> {
		const {email: emailService} = this.deps.apiContext.services;
		const email = user.email;
		if (!email) return;
		await trySendAdminNotification(() => emailService.send('account_deletion_cancelled', email, {}, user.locale), {
			action: 'cancel_deletion',
			targetId: user.id.toString(),
		});
	}

	private async banIdentifiers(
		user: User,
		decision: EnforcementDecisionRow,
		actor: EnforcementActor,
	): Promise<ReadonlySet<string>> {
		const {rule} = ENFORCEMENT_REASONS[decision.reason_code];
		if (!ENFORCEMENT_RULES[rule].barsNewAccounts) {
			return new Set();
		}
		if (user.email) {
			try {
				await this.deps.banManagementService.banEmail({email: user.email}, actor.adminUserId, actor.internalNote);
			} catch (error) {
				Logger.warn({error, userId: user.id.toString()}, 'Failed to ban the email of a closed account');
			}
		}
		return collectKnownUserIps(this.deps.apiContext.services.users, user);
	}

	private async cancelSubscription(user: User, adminUserId: UserID): Promise<void> {
		const {stripe, billingRepository} = this.deps;
		const userId = user.id;
		if (!user.stripeSubscriptionId || !stripe) return;
		try {
			const sub = await billingRepository.subscriptions.findById(user.stripeSubscriptionId);
			const latestInvoiceId = sub?.latest_invoice_id ?? null;
			let chargeIdForRefund: string | null = null;
			if (latestInvoiceId) {
				const payment = await billingRepository.payments.findPrimaryForInvoice(latestInvoiceId);
				chargeIdForRefund = payment?.charge_id ?? null;
			}
			const canceled = await stripe.subscriptions.cancel(user.stripeSubscriptionId, {
				invoice_now: false,
				prorate: false,
			});
			try {
				await billingRepository.subscriptions.upsertFromStripe(canceled, {
					knownUserId: BigInt(userId),
					snapshotCapturedAt: new Date(),
				});
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, subId: canceled.id},
					'Mirror upsert failed after deletion-time subscription cancel; reconciler will heal',
				);
			}
			Logger.info(
				{userId: userId.toString(), subscriptionId: user.stripeSubscriptionId},
				'Stripe subscription cancelled on ban',
			);
			if (chargeIdForRefund) {
				const refund = await stripe.refunds.create({
					charge: chargeIdForRefund,
					reason: 'fraudulent',
					metadata: {
						admin_user_id: String(adminUserId),
						target_user_id: String(userId),
						reason: 'pending_deletion',
					},
				});
				try {
					await billingRepository.refunds.upsertFromStripe(refund, {
						invoiceId: latestInvoiceId ?? undefined,
						customerId: user.stripeCustomerId ?? undefined,
						userId: BigInt(userId),
					});
				} catch (mirrorErr) {
					Logger.error(
						{mirrorErr, refundId: refund.id},
						'Mirror upsert failed after deletion-time refund; reconciler will heal',
					);
				}
				Logger.info({userId: userId.toString(), chargeId: chargeIdForRefund}, 'Stripe refund issued on ban');
			}
		} catch (err) {
			Logger.error(
				{err, userId: userId.toString(), subscriptionId: user.stripeSubscriptionId},
				'Failed to cancel/refund Stripe subscription on ban',
			);
		}
	}

	private async resolvePendingReportsAgainstUser(params: {user: User; adminUserId: UserID}): Promise<void> {
		const {user, adminUserId} = params;
		const {reportService, reporterResolutionNotifier} = this.deps;
		const auditLogReason = 'auto-resolved on scheduled deletion of reported user';
		const summary: AutoResolveSummary = {
			outcome: 'completed',
			foundCount: 0,
			resolvedCount: 0,
			failedCount: 0,
			notifiedCount: 0,
		};
		const reportSearchService = getReportSearchService();
		if (!reportSearchService) {
			Logger.warn(
				{userId: user.id.toString()},
				'Report search is unavailable; pending reports were not auto-resolved on scheduled deletion',
			);
			summary.outcome = 'search_unavailable';
			await this.recordAutoResolveSummary({user, adminUserId, auditLogReason, summary});
			return;
		}
		const pageSize = 100;
		const pendingReportIds = new Set<string>();
		let offset = 0;
		try {
			while (true) {
				const {hits} = await reportSearchService.searchReports(
					'',
					{
						reportedUserId: user.id.toString(),
						status: ReportStatus.PENDING,
					},
					{limit: pageSize, offset},
				);
				if (hits.length === 0) break;
				for (const hit of hits) {
					if (manuallyResolvedReportCategories.has(hit.category)) continue;
					pendingReportIds.add(hit.id);
				}
				offset += hits.length;
				if (hits.length < pageSize) break;
			}
		} catch (error) {
			summary.outcome = 'enumeration_failed';
			Logger.warn(
				{error, userId: user.id.toString()},
				'Failed to enumerate pending reports for auto-resolution on scheduled deletion',
			);
		}
		summary.foundCount = pendingReportIds.size;
		for (const hitId of pendingReportIds) {
			const reportId = createReportID(BigInt(hitId));
			let resolvedReport: IARSubmission;
			try {
				resolvedReport = await reportService.resolveReport(reportId, adminUserId, null, auditLogReason, {
					outcome: 'actioned',
					resolvedBy: 'system',
					reporterOutcome: 'restricted',
				});
			} catch (error) {
				if (error instanceof ReportAlreadyResolvedError) continue;
				summary.failedCount++;
				Logger.warn(
					{error, userId: user.id.toString(), reportId: reportId.toString()},
					'Failed to auto-resolve report on scheduled deletion',
				);
				continue;
			}
			summary.resolvedCount++;
			const notice = await reporterResolutionNotifier.notifyReporterOfResolution(resolvedReport, null, 'restricted');
			if (wasReporterNotified(notice)) {
				summary.notifiedCount++;
			}
		}
		await this.recordAutoResolveSummary({user, adminUserId, auditLogReason, summary});
	}

	private async recordAutoResolveSummary(params: {
		user: User;
		adminUserId: UserID;
		auditLogReason: string;
		summary: AutoResolveSummary;
	}): Promise<void> {
		const {user, adminUserId, auditLogReason, summary} = params;
		await this.deps.auditService
			.createAuditLog({
				adminUserId,
				targetType: 'user',
				targetId: BigInt(user.id),
				action: 'auto_resolve_reports_on_deletion',
				auditLogReason,
				metadata: new Map([
					['outcome', summary.outcome],
					['found_count', summary.foundCount.toString()],
					['resolved_count', summary.resolvedCount.toString()],
					['failed_count', summary.failedCount.toString()],
					['notified_count', summary.notifiedCount.toString()],
				]),
			})
			.catch((error) => {
				Logger.warn(
					{error, userId: user.id.toString(), ...summary},
					'Failed to write audit log for auto-resolved reports on scheduled deletion',
				);
			});
	}
}
