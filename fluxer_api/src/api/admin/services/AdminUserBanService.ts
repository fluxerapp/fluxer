// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapUserToAdminResponse} from '@app/api/admin/models/UserTypes';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {UserID} from '@app/api/BrandedTypes';
import type {ClosureStateDeps} from '@app/api/enforcement/ClosureState';
import type {EnforcementActor} from '@app/api/enforcement/EnforcementActor';
import {
	decisionAuditMetadata,
	type EnforcementDecisionService,
	networkBlockAuditMetadata,
} from '@app/api/enforcement/EnforcementDecisionService';
import type {EnforcementNoticeDelivery} from '@app/api/enforcement/EnforcementNoticeDelivery';
import {setFeatureLimit} from '@app/api/enforcement/FeatureLimitState';
import {permanentSuspensionEnd} from '@app/api/enforcement/NoticeDates';
import {applySuspension, liftSuspension} from '@app/api/enforcement/SuspensionState';
import {emitAdminAction} from '@app/api/infrastructure/activity/AccountChangeEvents';
import {isAccountClosed, isTemporarilyBanned} from '@app/api/user/UserHelpers';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {EnforcementAccountClosedError} from '@fluxer/errors/src/domains/moderation/EnforcementAccountClosedError';
import {EnforcementDecisionOwnsStateError} from '@fluxer/errors/src/domains/moderation/EnforcementDecisionOwnsStateError';
import type {
	AdminUserBanRequest,
	AdminUserFeatureLimitRequest,
	AdminUserWarningRequest,
} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';
import {ms} from 'itty-time';

interface AdminUserBanServiceDeps extends ClosureStateDeps {
	auditService: AdminAuditService;
	enforcement: EnforcementDecisionService;
	delivery: EnforcementNoticeDelivery;
}

const REPORTS_LIMIT_DEFAULT_DAYS = 90;
const REPORTS_LIMIT_MAX_DAYS = 365;

function featureLimitEnd(data: AdminUserFeatureLimitRequest, now: Date): Date | null {
	if (data.limit !== 'reports') {
		if (data.ends_at !== undefined) {
			throw InputValidationError.fromCode('ends_at', ValidationErrorCodes.ENFORCEMENT_LIMIT_END_INVALID);
		}
		return null;
	}
	const endsAt = data.ends_at
		? new Date(data.ends_at)
		: new Date(now.getTime() + ms(`${REPORTS_LIMIT_DEFAULT_DAYS} days`));
	if (endsAt <= now || endsAt.getTime() > now.getTime() + ms(`${REPORTS_LIMIT_MAX_DAYS} days`)) {
		throw InputValidationError.fromCode('ends_at', ValidationErrorCodes.ENFORCEMENT_LIMIT_END_INVALID);
	}
	return endsAt;
}

export class AdminUserBanService {
	constructor(private readonly deps: AdminUserBanServiceDeps) {}

	async tempBanUser(userId: UserID, data: AdminUserBanRequest, actor: EnforcementActor) {
		const {auditService, enforcement} = this.deps;
		const user = await this.deps.apiContext.services.users.findUniqueAssert(userId);
		if (isAccountClosed(user)) {
			throw new EnforcementAccountClosedError();
		}
		const now = new Date();
		const permanent = data.duration_hours === 0;
		const bannedUntil = permanent
			? permanentSuspensionEnd(now)
			: new Date(now.getTime() + ms(`${data.duration_hours} hours`));
		let suspended = user;
		const recorded = await enforcement.record(
			{
				subject: {type: 'user', user},
				kind: 'suspension',
				input: data.decision,
				endsAt: permanent ? null : bannedUntil,
				removeEvidence: data.remove_evidence ?? false,
				blockNetworks: data.block_networks,
				actor,
			},
			async (decisionId) => {
				suspended = await applySuspension(this.deps, user, bannedUntil, decisionId);
			},
		);
		await auditService.createAuditLog({
			adminUserId: actor.adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'temp_ban',
			auditLogReason: actor.internalNote,
			metadata: new Map([
				['duration_hours', data.duration_hours.toString()],
				['banned_until', bannedUntil.toISOString()],
				...decisionAuditMetadata(recorded.decision),
				...networkBlockAuditMetadata(recorded),
			]),
		});
		await emitAdminAction(actor.adminUserId, userId, 'temp_ban', {durationHours: data.duration_hours});
		return {
			user: await mapUserToAdminResponse(suspended, this.deps.apiContext.services.cache, actor.acls),
			skipped_ips: recorded.skippedNetworks,
		};
	}

	async warnUser(userId: UserID, data: AdminUserWarningRequest, actor: EnforcementActor) {
		const user = await this.deps.apiContext.services.users.findUniqueAssert(userId);
		const {decision} = await this.deps.enforcement.record(
			{subject: {type: 'user', user}, kind: 'warning', input: data.decision, actor},
			async () => {},
		);
		await this.deps.auditService.createAuditLog({
			adminUserId: actor.adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'warn_user',
			auditLogReason: actor.internalNote,
			metadata: new Map(decisionAuditMetadata(decision)),
		});
		return {decision_id: decision.decision_id.toString()};
	}

	async limitFeatures(userId: UserID, data: AdminUserFeatureLimitRequest, actor: EnforcementActor) {
		const user = await this.deps.apiContext.services.users.findUniqueAssert(userId);
		const endsAt = featureLimitEnd(data, new Date());
		let limited = user;
		const {decision} = await this.deps.enforcement.record(
			{subject: {type: 'user', user}, kind: 'feature_limit', input: data.decision, endsAt, limit: data.limit, actor},
			async () => {
				limited = await setFeatureLimit(this.deps, user, data.limit, true, actor.adminUserId);
			},
		);
		await this.deps.auditService.createAuditLog({
			adminUserId: actor.adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'limit_features',
			auditLogReason: actor.internalNote,
			metadata: new Map([
				['limit', data.limit],
				...(endsAt ? ([['ends_at', endsAt.toISOString()]] as Array<[string, string]>) : []),
				...decisionAuditMetadata(decision),
			]),
		});
		return {user: await mapUserToAdminResponse(limited, this.deps.apiContext.services.cache, actor.acls)};
	}

	async unbanUser(userId: UserID, actor: EnforcementActor) {
		const {auditService, delivery} = this.deps;
		const user = await this.deps.apiContext.services.users.findUniqueAssert(userId);
		if (user.enforcementDecisionId !== null) {
			throw new EnforcementDecisionOwnsStateError(user.enforcementDecisionId);
		}
		const wasSuspended = isTemporarilyBanned(user) && !isAccountClosed(user);
		const updated = await liftSuspension(this.deps, user);
		if (wasSuspended) {
			await delivery.sendLegacyEnded(user);
		}
		await auditService.createAuditLog({
			adminUserId: actor.adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'unban',
			auditLogReason: actor.internalNote,
			metadata: new Map([['ended_notice', wasSuspended ? 'true' : 'false']]),
		});
		await emitAdminAction(actor.adminUserId, userId, 'unban');
		return {user: await mapUserToAdminResponse(updated, this.deps.apiContext.services.cache, actor.acls)};
	}
}
