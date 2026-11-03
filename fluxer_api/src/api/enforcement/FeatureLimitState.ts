// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {EnforcementDecisionRow} from '@app/api/database/types/EnforcementTypes';
import {isDecisionExpired} from '@app/api/enforcement/DecisionStatus';
import type {EnforcementDecisionRepository} from '@app/api/enforcement/EnforcementDecisionRepository';
import type {SuspensionStateDeps} from '@app/api/enforcement/SuspensionState';
import {emitAdminAction} from '@app/api/infrastructure/activity/AccountChangeEvents';
import type {User} from '@app/api/models/User';
import {clearNewConversationLimit} from '@app/api/user/NewConversationLimit';
import {propagateUserUpdate} from '@app/api/user/services/PartialUserChangePropagation';
import type {EnforcementLimit} from '@fluxer/constants/src/EnforcementReasons';
import {PremiumFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';

export const FEATURE_LIMIT_USER_FLAGS: Readonly<Partial<Record<EnforcementLimit, bigint>>> = {
	direct_messages: UserFlags.SPAMMER,
	reports: UserFlags.REPORT_BANNED,
};

export const FEATURE_LIMIT_PREMIUM_FLAGS: Readonly<Partial<Record<EnforcementLimit, number>>> = {
	purchases: PremiumFlags.PURCHASE_DISABLED,
};

export async function listActiveFeatureLimits(
	repository: EnforcementDecisionRepository,
	subjectId: UserID,
	limit: EnforcementLimit,
): Promise<Array<EnforcementDecisionRow>> {
	const decisions = await repository.listBySubject('user', subjectId);
	return decisions.filter(
		(decision) => decision.kind === 'feature_limit' && decision.status === 'active' && decision.feature_limit === limit,
	);
}

export async function findHoldingFeatureLimit(
	repository: EnforcementDecisionRepository,
	subjectId: UserID,
	limit: EnforcementLimit,
	exceptDecisionId: bigint | null = null,
): Promise<EnforcementDecisionRow | null> {
	const active = await listActiveFeatureLimits(repository, subjectId, limit);
	return active.find((decision) => decision.decision_id !== exceptDecisionId && !isDecisionExpired(decision)) ?? null;
}

export async function setFeatureLimit(
	{apiContext, userUpdateDeps}: SuspensionStateDeps,
	user: User,
	limit: EnforcementLimit,
	enabled: boolean,
	actorId: UserID,
): Promise<User> {
	const {users, cache} = apiContext.services;
	const userFlag = FEATURE_LIMIT_USER_FLAGS[limit];
	let updated: User;
	if (userFlag !== undefined) {
		const flagged = await users.updateFlags(user.id, (flags) => (enabled ? flags | userFlag : flags & ~userFlag));
		if (!flagged) {
			throw new UnknownUserError();
		}
		updated = flagged;
	} else {
		const premiumFlag = FEATURE_LIMIT_PREMIUM_FLAGS[limit]!;
		updated = await users.patchUpsert(
			user.id,
			{premium_flags: enabled ? user.premiumFlags | premiumFlag : user.premiumFlags & ~premiumFlag},
			user.toRow(),
		);
	}
	await propagateUserUpdate(userUpdateDeps, user, updated);
	if (!enabled) {
		await clearNewConversationLimit(user.id, {cache});
	}
	if (userFlag !== undefined) {
		await emitAdminAction(actorId, user.id, 'update_flags');
	}
	return updated;
}
