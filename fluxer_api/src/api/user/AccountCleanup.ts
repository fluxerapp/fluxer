// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import {getCachedAccountsConfig} from '@app/api/instance/AccountsConfigCache';
import {getKVAccountDeletionQueue} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {followOperatorPassWallStanding} from '@app/api/operator_pass/OperatorPassWallStanding';
import {hasWayBackIn} from '@app/api/user/AccountTier';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {reschedulePendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import {AccountCleanupDefaults, LastSessionCleanupModes} from '@fluxer/constants/src/AccountConfigConstants';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';

export interface SystemDeletionDeps {
	users: UserRepository;
	gateway: IGatewayService;
	deletionQueue: Pick<KVAccountDeletionQueueService, 'scheduleDeletion' | 'removeFromQueue'>;
}

export async function scheduleSystemDeletion(
	deps: SystemDeletionDeps,
	user: User,
	params: {reasonCode: number; delayMs: number; cancelledBySignIn: boolean; endSessions: boolean},
): Promise<User> {
	const pendingDeletionAt = new Date(Date.now() + params.delayMs);
	const updated = await deps.users.updateDeletionSchedule(user, {
		flags: params.cancelledBySignIn ? user.flags | UserFlags.SELF_DELETED : user.flags | UserFlags.DELETED,
		pending_deletion_at: pendingDeletionAt,
		deletion_reason_code: params.reasonCode,
		deletion_scheduled_by: SYSTEM_USER_ID,
		deletion_scheduled_at: new Date(),
	});
	await reschedulePendingDeletion({
		userId: user.id,
		currentPendingDeletionAt: user.pendingDeletionAt,
		nextPendingDeletionAt: pendingDeletionAt,
		deletionReasonCode: params.reasonCode,
		deletionQueue: deps.deletionQueue,
	});
	if (params.endSessions) {
		await revokeAllAuthSessions({users: deps.users, gateway: deps.gateway}, user.id);
	}
	await followOperatorPassWallStanding(user, updated);
	return updated;
}

export async function scheduleAbandonedOnLastLogout(ctx: ApiContext, user: User): Promise<void> {
	const cleanup = getCachedAccountsConfig().unclaimed_account_cleanup;
	if (cleanup.on_last_session_end !== LastSessionCleanupModes.AFTER_GRACE) return;
	if (user.isBot || user.pendingDeletionAt !== null) return;
	if (await hasWayBackIn(ctx.services, user)) return;
	if ((await ctx.services.users.listAuthSessions(user.id)).length > 0) return;
	await scheduleSystemDeletion(
		{users: ctx.services.users, gateway: ctx.services.gateway, deletionQueue: getKVAccountDeletionQueue()},
		user,
		{
			reasonCode: DeletionReasons.UNCLAIMED_ACCOUNT_ABANDONED,
			delayMs: AccountCleanupDefaults.SCHEDULED_DELETION_GRACE_MS,
			cancelledBySignIn: true,
			endSessions: false,
		},
	);
}
