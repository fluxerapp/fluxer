// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import type {UserID} from '@app/api/BrandedTypes';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {ClosureStateDeps} from '@app/api/enforcement/ClosureState';
import type {EnforcementDecisionRepository} from '@app/api/enforcement/EnforcementDecisionRepository';
import type {User} from '@app/api/models/User';
import {clearNewConversationLimit} from '@app/api/user/NewConversationLimit';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {
	type PartialUserChangePropagationDeps,
	propagateUserUpdate,
} from '@app/api/user/services/PartialUserChangePropagation';
import {clearPendingDeletion, resetTiedBulkMessageDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {EnforcementDecisionReplacedError} from '@fluxer/errors/src/domains/moderation/EnforcementDecisionReplacedError';

export interface SuspensionStateDeps {
	apiContext: ApiContext;
	userUpdateDeps: PartialUserChangePropagationDeps;
}

export function hasHolderDeletionPending(user: User): boolean {
	return (user.flags & UserFlags.SELF_DELETED) !== 0n && user.pendingDeletionAt !== null;
}

async function cancelHolderDeletionAndSuspend(
	{apiContext, kvDeletionQueue}: ClosureStateDeps,
	user: User,
	until: Date,
	decisionId: bigint,
): Promise<User> {
	const updated = await apiContext.services.users.updateDeletionSchedule(user, {
		flags: (user.flags | UserFlags.DISABLED) & ~UserFlags.SELF_DELETED,
		pending_deletion_at: null,
		deletion_reason_code: null,
		deletion_public_reason: null,
		deletion_audit_log_reason: null,
		...resetTiedBulkMessageDeletion(user),
		temp_banned_until: until,
		enforcement_decision_id: decisionId,
	});
	await clearPendingDeletion({userId: user.id, deletionQueue: kvDeletionQueue});
	return updated;
}

const SESSION_REVOKE_ATTEMPTS = 3;

async function revokeSessionsOfSuspendedUser(services: ApiContext['services'], userId: UserID): Promise<void> {
	for (let attempt = 1; ; attempt++) {
		try {
			await revokeAllAuthSessions(services, userId);
			return;
		} catch (error) {
			if (attempt >= SESSION_REVOKE_ATTEMPTS) throw error;
		}
	}
}

export async function applySuspension(
	deps: ClosureStateDeps,
	user: User,
	until: Date,
	decisionId: bigint,
): Promise<User> {
	const {apiContext, userUpdateDeps} = deps;
	const {users} = apiContext.services;
	let updated: User;
	if (hasHolderDeletionPending(user)) {
		updated = await cancelHolderDeletionAndSuspend(deps, user, until, decisionId);
	} else {
		const suspended = await users.patchUpsert(
			user.id,
			{temp_banned_until: until, enforcement_decision_id: decisionId},
			user.toRow(),
		);
		updated = (await users.updateFlags(user.id, (flags) => flags | UserFlags.DISABLED)) ?? suspended;
	}
	await revokeSessionsOfSuspendedUser(apiContext.services, user.id);
	await propagateUserUpdate(userUpdateDeps, user, updated);
	return updated;
}

const SUSPENSION_CLEAR_ATTEMPTS = 3;

async function clearSuspensionState(
	users: Pick<UserRepository, 'findUnique' | 'compareAndClearSuspension'>,
	userId: UserID,
	stillClearable: (current: User) => boolean,
): Promise<User | null> {
	for (let attempt = 0; attempt < SUSPENSION_CLEAR_ATTEMPTS; attempt++) {
		const current = await users.findUnique(userId);
		if (!current) return null;
		if (
			(current.flags & UserFlags.DISABLED) === 0n &&
			current.tempBannedUntil === null &&
			current.enforcementDecisionId === null
		) {
			return current;
		}
		if (!stillClearable(current)) return null;
		const cleared = await users.compareAndClearSuspension(current, current.flags & ~UserFlags.DISABLED);
		if (cleared) return cleared;
	}
	throw new Error(`User ${userId} suspension state kept changing during update`);
}

export interface ExpiredSuspensionDeps {
	users: Pick<UserRepository, 'findUnique' | 'compareAndClearSuspension'>;
	repository: Pick<EnforcementDecisionRepository, 'get' | 'endIfActive'>;
}

export function hasExpiredSuspension(
	user: User,
	now: Date = new Date(),
): user is User & {readonly tempBannedUntil: Date} {
	return (
		(user.flags & UserFlags.DISABLED) !== 0n &&
		user.tempBannedUntil !== null &&
		user.tempBannedUntil.getTime() <= now.getTime()
	);
}

export async function clearExpiredSuspension(
	{users, repository}: ExpiredSuspensionDeps,
	user: User,
	now: Date = new Date(),
): Promise<{previous: User; cleared: User} | null> {
	if (!hasExpiredSuspension(user, now)) return null;
	const current = await users.findUnique(user.id);
	if (!current || !hasExpiredSuspension(current, now)) return null;
	const decision = current.enforcementDecisionId === null ? null : await repository.get(current.enforcementDecisionId);
	if (decision?.kind === 'suspension' && decision.status === 'active' && decision.ends_at !== null) {
		await repository.endIfActive(decision.decision_id, {
			outcome: 'expired',
			endedAt: current.tempBannedUntil,
			endedBy: SYSTEM_USER_ID,
			statement: null,
		});
	}
	const cleared = await clearSuspensionState(
		users,
		current.id,
		(latest) => hasExpiredSuspension(latest, now) && latest.enforcementDecisionId === current.enforcementDecisionId,
	);
	return cleared ? {previous: current, cleared} : null;
}

export async function liftSuspension({apiContext, userUpdateDeps}: SuspensionStateDeps, user: User): Promise<User> {
	const {users, cache} = apiContext.services;
	const updated = await clearSuspensionState(
		users,
		user.id,
		(latest) => latest.enforcementDecisionId === user.enforcementDecisionId,
	);
	if (!updated) throw new EnforcementDecisionReplacedError();
	await propagateUserUpdate(userUpdateDeps, user, updated);
	await clearNewConversationLimit(user.id, {cache});
	return updated;
}
