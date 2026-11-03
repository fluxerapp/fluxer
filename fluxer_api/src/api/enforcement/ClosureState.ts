// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SuspensionStateDeps} from '@app/api/enforcement/SuspensionState';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import type {User} from '@app/api/models/User';
import {clearNewConversationLimit} from '@app/api/user/NewConversationLimit';
import {propagateUserUpdate} from '@app/api/user/services/PartialUserChangePropagation';
import {clearPendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import {UserFlags} from '@fluxer/constants/src/UserConstants';

export interface ClosureStateDeps extends SuspensionStateDeps {
	kvDeletionQueue: KVAccountDeletionQueueService;
}

export async function cancelClosure(
	{apiContext, userUpdateDeps, kvDeletionQueue}: ClosureStateDeps,
	user: User,
): Promise<User> {
	const updated = await apiContext.services.users.updateDeletionSchedule(user, {
		flags: user.flags & ~UserFlags.DELETED & ~UserFlags.SELF_DELETED,
		pending_deletion_at: null,
		deletion_reason_code: null,
		deletion_public_reason: null,
		deletion_audit_log_reason: null,
		enforcement_decision_id: null,
	});
	await clearPendingDeletion({userId: user.id, deletionQueue: kvDeletionQueue});
	await propagateUserUpdate(userUpdateDeps, user, updated);
	await clearNewConversationLimit(user.id, {cache: apiContext.services.cache});
	return updated;
}
