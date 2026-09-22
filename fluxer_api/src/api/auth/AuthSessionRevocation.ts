import type {UserID} from '@app/api/BrandedTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {IUserRepositoryAggregate} from '@app/api/user/repositories/IUserRepositoryAggregate';
import {runAllInOrder} from '@app/api/utils/ConcurrencyUtils';

interface SessionRevocationDependencies {
	users: IUserRepositoryAggregate;
	gateway: IGatewayService;
}

interface SessionRevocationTarget {
	sessionIdHash: Buffer;
	encodedIdHash: string;
}

export async function revokeAuthSessions(
	deps: SessionRevocationDependencies,
	userId: UserID,
	targets: ReadonlyArray<SessionRevocationTarget>,
): Promise<void> {
	await revokeSessionTargets(deps, userId, targets, 'selected');
}

export async function revokeAllAuthSessions(deps: SessionRevocationDependencies, userId: UserID): Promise<number> {
	const sessions = await deps.users.listAuthSessions(userId);
	const targets = sessions.map((session) => ({
		sessionIdHash: session.sessionIdHash,
		encodedIdHash: session.sessionIdHash.toString('base64url'),
	}));
	await revokeSessionTargets(deps, userId, targets, 'all');
	return sessions.length;
}

async function revokeSessionTargets(
	{users, gateway}: SessionRevocationDependencies,
	userId: UserID,
	targets: ReadonlyArray<SessionRevocationTarget>,
	scope: 'selected' | 'all',
): Promise<void> {
	const sessionIdHashes = targets.map((target) => target.encodedIdHash);
	const steps: Array<() => Promise<unknown>> = [
		() =>
			scope === 'all'
				? users.deleteAllPushSubscriptions(userId)
				: users.deletePushSubscriptionsForAuthSessions(userId, sessionIdHashes, {deleteUnboundSubscriptions: true}),
		() => gateway.invalidatePushSubscriptions({userId}),
	];
	if (scope === 'selected' || targets.length > 0) {
		steps.push(
			() =>
				users.deleteAuthSessions(
					userId,
					targets.map((target) => target.sessionIdHash),
				),
			() => gateway.terminateSession({userId, sessionIdHashes}),
		);
	}
	await runAllInOrder(steps, 'Failed to revoke auth sessions');
}
