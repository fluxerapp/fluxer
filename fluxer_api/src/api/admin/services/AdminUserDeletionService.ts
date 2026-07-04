// SPDX-License-Identifier: AGPL-3.0-or-later

import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ReportAlreadyResolvedError} from '@fluxer/errors/src/domains/moderation/ReportAlreadyResolvedError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	BulkScheduleUserDeletionRequest,
	ScheduleAccountDeletionRequest,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type Stripe from 'stripe';
import type {ApiContext} from '../../ApiContext';
import * as AuthSession from '../../auth/AuthSession';
import {createReportID, createUserID, type UserID} from '../../BrandedTypes';
import type {BillingRepository} from '../../billing/repositories/BillingRepository';
import type {KVAccountDeletionQueueService} from '../../infrastructure/KVAccountDeletionQueueService';
import {Logger} from '../../Logger';
import type {User} from '../../models/User';
import {ReportStatus} from '../../report/IReportRepository';
import type {ReportService} from '../../report/ReportService';
import {getReportSearchService} from '../../SearchFactory';
import {clearPendingDeletion, reschedulePendingDeletion} from '../../user/services/PendingDeletionCoordinator';
import {mapUserToAdminResponse} from '../models/UserTypes';
import type {AdminAuditService} from './AdminAuditService';
import type {AdminBanManagementService} from './AdminBanManagementService';
import type {AdminUserUpdatePropagator} from './AdminUserUpdatePropagator';
import {BulkCancelledError, type BulkProgressHelpers} from './BulkProgressHelpers';

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

export class AdminUserDeletionService {
	constructor(private readonly deps: AdminUserDeletionServiceDeps) {}

	async scheduleAccountDeletion(
		data: ScheduleAccountDeletionRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, email: emailService, cache: cacheService, gateway: gatewayService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const minDays =
			data.reason_code === DeletionReasons.USER_REQUESTED ? minUserRequestedDeletionDays : minStandardDeletionDays;
		const daysUntilDeletion = Math.max(data.days_until_deletion, minDays);
		const pendingDeletionAt = new Date();
		pendingDeletionAt.setDate(pendingDeletionAt.getDate() + daysUntilDeletion);
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				flags: user.flags | UserFlags.DELETED,
				pending_deletion_at: pendingDeletionAt,
				deletion_reason_code: data.reason_code,
				deletion_public_reason: data.public_reason ?? null,
				deletion_audit_log_reason: auditLogReason,
			},
			user.toRow(),
		);
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
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		if (user.email) {
			await emailService.sendAccountScheduledForDeletionEmail(
				user.email,
				user.username,
				data.public_reason ?? null,
				pendingDeletionAt,
				user.locale,
			);
		}
		if (data.reason_code !== DeletionReasons.USER_REQUESTED) {
			await this.banIdentifiersForScheduledDeletion({
				user,
				adminUserId,
				auditLogReason,
				deletionReasonCode: data.reason_code,
			});
			await this.resolvePendingReportsAgainstUser({user, adminUserId});
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
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls, gatewayService),
		};
	}

	async cancelAccountDeletion(
		data: {
			user_id: bigint;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, email: emailService, cache: cacheService, gateway: gatewayService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		await clearPendingDeletion({
			userId,
			pendingDeletionAt: user.pendingDeletionAt,
			userRepository,
			deletionQueue: this.deps.kvDeletionQueue,
		});
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				flags: user.flags & ~UserFlags.DELETED & ~UserFlags.SELF_DELETED,
				pending_deletion_at: null,
				deletion_reason_code: null,
				deletion_public_reason: null,
				deletion_audit_log_reason: null,
			},
			user.toRow(),
		);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		if (user.email) {
			await emailService.sendUnbanNotification(
				user.email,
				user.username,
				auditLogReason || 'deletion canceled',
				user.locale,
			);
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
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls, gatewayService),
		};
	}

	async bulkScheduleUserDeletion(
		data: BulkScheduleUserDeletionRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
		helpers?: BulkProgressHelpers,
	) {
		const {auditService} = this.deps;
		const successful: Array<string> = [];
		const failed: Array<{
			id: string;
			error: string;
		}> = [];
		const total = data.user_ids.length;
		await helpers?.reportProgress(0, total, `Scheduling deletion of ${total} users`);
		let processed = 0;
		for (const userIdBigInt of data.user_ids) {
			if (helpers && (await helpers.shouldCancel())) throw new BulkCancelledError();
			try {
				await this.scheduleAccountDeletion(
					{
						user_id: userIdBigInt,
						reason_code: data.reason_code,
						public_reason: data.public_reason,
						days_until_deletion: data.days_until_deletion,
					},
					adminUserId,
					null,
					acls,
				);
				successful.push(userIdBigInt.toString());
			} catch (error) {
				failed.push({
					id: userIdBigInt.toString(),
					error: error instanceof Error ? error.message : 'Unknown error',
				});
			}
			processed++;
			if (helpers && processed % 10 === 0) {
				await helpers.reportProgress(processed, total, null);
			}
		}
		await helpers?.reportProgress(total, total, `+${successful.length} ok, ${failed.length} failed`);
		const bulkMinDays =
			data.reason_code === DeletionReasons.USER_REQUESTED ? minUserRequestedDeletionDays : minStandardDeletionDays;
		const bulkDaysUntilDeletion = Math.max(data.days_until_deletion, bulkMinDays);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(0),
			action: 'bulk_schedule_deletion',
			auditLogReason,
			metadata: new Map([
				['user_count', data.user_ids.length.toString()],
				['reason_code', data.reason_code.toString()],
				['days', bulkDaysUntilDeletion.toString()],
			]),
		});
		return {
			successful,
			failed,
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
			return;
		}
		const auditLogReason = 'auto-resolved on scheduled deletion of reported user';
		const pageSize = 100;
		const seen = new Set<string>();
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
				let advanced = false;
				for (const hit of hits) {
					if (seen.has(hit.id)) continue;
					seen.add(hit.id);
					advanced = true;
					const reportId = createReportID(BigInt(hit.id));
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
				if (!advanced) {
					offset += hits.length;
				}
				if (hits.length < pageSize) break;
			}
		} catch (error) {
			Logger.warn(
				{error, userId: user.id.toString()},
				'Failed to enumerate pending reports for auto-resolution on scheduled deletion',
			);
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
