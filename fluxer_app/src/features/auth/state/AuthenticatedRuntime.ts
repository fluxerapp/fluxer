// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig, {
	type RuntimeConfigSnapshot,
	runtimeConfigSnapshotsAreSameInstance,
} from '@app/features/app/state/RuntimeConfig';
import SessionManager, {type Account} from '@app/features/platform/state/AuthSession';

export interface AuthenticatedRuntimeContext {
	readonly account: Account;
	readonly runtime: RuntimeConfigSnapshot;
}

export class AuthenticatedRuntimeInvariantError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'AuthenticatedRuntimeInvariantError';
	}
}

export function readAuthenticatedRuntimeContext(): AuthenticatedRuntimeContext | null {
	if (!SessionManager.isInitialized || !SessionManager.isAuthenticated) {
		return null;
	}
	const runtime = RuntimeConfig.getSnapshotOrNull();
	if (runtime === null) {
		return null;
	}
	const account = SessionManager.currentAccount;
	if (account?.instance == null) {
		throw new AuthenticatedRuntimeInvariantError('An authenticated session requires an active account runtime');
	}
	if (!runtimeConfigSnapshotsAreSameInstance(account.instance, runtime)) {
		throw new AuthenticatedRuntimeInvariantError('The active account and runtime belong to different instances');
	}
	return {account, runtime};
}
