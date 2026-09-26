// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {mapUserToAdminResponse} from '@app/api/admin/models/UserTypes';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {AdminBanManagementService} from '@app/api/admin/services/AdminBanManagementService';
import type {AdminUserUpdatePropagator} from '@app/api/admin/services/AdminUserUpdatePropagator';
import * as AuthSession from '@app/api/auth/AuthSession';
import {createReportID, createUserID, type UserID} from '@app/api/BrandedTypes';
import type {BillingRepository} from '@app/api/billing/repositories/BillingRepository';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {ReportStatus} from '@app/api/report/IReportRepository';
import type {ReportService} from '@app/api/report/ReportService';
import {getReportSearchService} from '@app/api/SearchFactory';
import {clearPendingDeletion, reschedulePendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ReportAlreadyResolvedError} from '@fluxer/errors/src/domains/moderation/ReportAlreadyResolvedError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {ScheduleAccountDeletionRequest} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type Stripe from 'stripe';

interface AdminUserDeletionServiceDeps {
	apiContext: ApiContext;
	auditService: AdminAuditService;
	banManagementService: AdminBanManagementService;
	reportService: ReportService;
	updatePropagator: AdminUserUpdatePropagator;
	kvDeletionQueue: KVAccountDeletionQueueService;
	stripe: Stripe | null;
	billingRepository: BillingRepository;
}

const minUserRequestedDeletionDays = 14;
const minStandardDeletionDays = 60;

export function resolveDeletionDays(reasonCode: number, requestedDays: number): number {
	const minDays =
		reasonCode === DeletionReasons.USER_REQUESTED ? minUserRequestedDeletionDays : minStandardDeletionDays;
	return Math.max(requestedDays, minDays);
}

export class AdminUserDeletionService {
	constructor(private readonly deps: AdminUserDeletionServiceDeps) {}

	async scheduleAccountDeletion(
		data: ScheduleAccountDeletionRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {cache: cacheService} = this.deps.apiContext.services;
		const updatedUser = await this.applyScheduledDeletion(data, adminUserId, auditLogReason);
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	async applyScheduledDeletion(
		data: ScheduleAccountDeletionRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<User> {
		const {users: userRepository, email: emailService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const daysUntilDeletion = resolveDeletionDays(data.reason_code, data.days_until_deletion);
		const pendingDeletionAt = new Date();
		pendingDeletionAt.setDate(pendingDeletionAt.getDate() + daysUntilDeletion);
		const updatedUser = await userRepository.updateDeletionSchedule(user, {
			flags: user.flags | UserFlags.DELETED,
			pending_deletion_at: pendingDeletionAt,
			deletion_reason_code: data.reason_code,
			deletion_public_reason: data.public_reason ?? null,
			deletion_audit_log_reason: auditLogReason,
		});
		await reschedulePendingDeletion({
			userId,
			currentPendingDeletionAt: user.pendingDeletionAt,
			nextPendingDeletionAt: pendingDeletionAt,
			deletionReasonCode: data.reason_code,
			userRepository,
			deletionQueue: this.deps.kvDeletionQueue,
		});
		await AuthSession.terminateAllUserSessions(this.deps.apiContext, userId);
		const {stripe, billingRepository} = this.deps;
		if (user.stripeSubscriptionId && stripe) {
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
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: data.user_id,
			action: 'schedule_deletion',
			auditLogReason,
			metadata: new Map([
				['days', daysUntilDeletion.toString()],
				['reason_code', data.reason_code.toString()],
			]),
		});
		if (data.reason_code !== DeletionReasons.USER_REQUESTED) {
			await this.banIdentifiersForScheduledDeletion({
				user,
				adminUserId,
				auditLogReason,
				deletionReasonCode: data.reason_code,
			});
			await this.resolvePendingReportsAgainstUser({user, adminUserId});
		}
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		if (user.email) {
			try {
				await emailService.sendAccountScheduledForDeletionEmail(
					user.email,
					user.username,
					data.public_reason ?? null,
					pendingDeletionAt,
					user.locale,
				);
			} catch (error) {
				Logger.warn(
					{error, userId: userId.toString()},
					'Failed to send scheduled deletion email after the deletion was scheduled',
				);
			}
		}
		return updatedUser;
	}

	async cancelAccountDeletion(
		data: {
			user_id: bigint;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, email: emailService, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const updatedUser = await userRepository.updateDeletionSchedule(user, {
			flags: user.flags & ~UserFlags.DELETED & ~UserFlags.SELF_DELETED,
			pending_deletion_at: null,
			deletion_reason_code: null,
			deletion_public_reason: null,
			deletion_audit_log_reason: null,
		});
		await clearPendingDeletion({
			userId,
			pendingDeletionAt: user.pendingDeletionAt,
			userRepository,
			deletionQueue: this.deps.kvDeletionQueue,
		});
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		if (user.email) {
			await emailService.sendUnbanNotification(user.email, user.username, auditLogReason || null, user.locale);
		}
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'cancel_deletion',
			auditLogReason,
			metadata: new Map(),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	private async banIdentifiersForScheduledDeletion(params: {
		user: User;
		adminUserId: UserID;
		auditLogReason: string | null;
		deletionReasonCode: number;
	}): Promise<void> {
		const {user, adminUserId, auditLogReason, deletionReasonCode} = params;
		const {users: userRepository} = this.deps.apiContext.services;
		const {banManagementService} = this.deps;
		const reason = auditLogReason ?? 'auto-enforcement on scheduled deletion';
		if (user.email) {
			try {
				await banManagementService.banEmail({email: user.email}, adminUserId, reason);
			} catch (error) {
				Logger.warn({error, userId: user.id.toString()}, 'Failed to auto-ban email on scheduled deletion');
			}
		}
		const ipsToReview = new Set<string>();
		if (user.lastActiveIp) {
			ipsToReview.add(user.lastActiveIp);
		}
		try {
			const authorizedIps = await userRepository.getAuthorizedIps(user.id);
			for (const {ip} of authorizedIps) {
				if (ip) ipsToReview.add(ip);
			}
		} catch (error) {
			Logger.warn({error, userId: user.id.toString()}, 'Failed to list authorized IPs for scheduled deletion review');
		}
		try {
			const sessions = await userRepository.listAuthSessions(user.id);
			for (const session of sessions) {
				if (session.clientIp) ipsToReview.add(session.clientIp);
			}
		} catch (error) {
			Logger.warn({error, userId: user.id.toString()}, 'Failed to list auth sessions for scheduled deletion review');
		}
		try {
			const tombstones = await userRepository.listAuthSessionTombstones(user.id);
			for (const tombstone of tombstones) {
				if (tombstone.clientIp) ipsToReview.add(tombstone.clientIp);
			}
		} catch (error) {
			Logger.warn(
				{error, userId: user.id.toString()},
				'Failed to list auth session tombstones for scheduled deletion review',
			);
		}
		for (const ip of ipsToReview) {
			try {
				await banManagementService.markSuspiciousIpForScheduledDeletion(
					{
						ip,
						sourceUserId: user.id,
						deletionReasonCode,
					},
					adminUserId,
					reason,
				);
			} catch (error) {
				Logger.warn({error, userId: user.id.toString(), ip}, 'Failed to mark suspicious IP on scheduled deletion');
			}
		}
	}

	private async resolvePendingReportsAgainstUser(params: {user: User; adminUserId: UserID}): Promise<void> {
		const {user, adminUserId} = params;
		const {reportService, auditService} = this.deps;
		const reportSearchService = getReportSearchService();
		if (!reportSearchService) {
			Logger.warn(
				{userId: user.id.toString()},
				'Report search is unavailable; pending reports were not auto-resolved on scheduled deletion',
			);
			return;
		}
		const auditLogReason = 'auto-resolved on scheduled deletion of reported user';
		const pageSize = 100;
		const pendingReportIds = new Set<string>();
		let resolvedCount = 0;
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
					pendingReportIds.add(hit.id);
				}
				offset += hits.length;
				if (hits.length < pageSize) break;
			}
		} catch (error) {
			Logger.warn(
				{error, userId: user.id.toString()},
				'Failed to enumerate pending reports for auto-resolution on scheduled deletion',
			);
		}
		for (const hitId of pendingReportIds) {
			const reportId = createReportID(BigInt(hitId));
			try {
				await reportService.resolveReport(reportId, adminUserId, null, auditLogReason);
				resolvedCount++;
			} catch (error) {
				if (error instanceof ReportAlreadyResolvedError) continue;
				Logger.warn(
					{error, userId: user.id.toString(), reportId: reportId.toString()},
					'Failed to auto-resolve report on scheduled deletion',
				);
			}
		}
		if (resolvedCount > 0) {
			await auditService
				.createAuditLog({
					adminUserId,
					targetType: 'user',
					targetId: BigInt(user.id),
					action: 'auto_resolve_reports_on_deletion',
					auditLogReason,
					metadata: new Map([['resolved_count', resolvedCount.toString()]]),
				})
				.catch((error) => {
					Logger.warn(
						{error, userId: user.id.toString(), resolvedCount},
						'Failed to write audit log for auto-resolved reports on scheduled deletion',
					);
				});
		}
	}
}
