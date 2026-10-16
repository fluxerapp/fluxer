// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AccountScopedWorkTransitionReason} from '@app/features/platform/state/AccountScopedWork';
import type {InstanceHTTPTarget} from '@app/features/platform/transport/InstanceHTTP';

const MAX_QUEUED_SESSION_OPERATIONS = 1;

export interface AuthSessionTransitionContext {
	notifyLogout(target: InstanceHTTPTarget, token: string): Promise<void>;
}

export interface AuthSessionTransitionDependencies {
	runSuspended<Result>(
		reason: AccountScopedWorkTransitionReason,
		operation: (context: AuthSessionTransitionContext) => Promise<Result>,
	): Promise<Result>;
}

export class AuthSessionTransitionCapacityError extends Error {
	constructor() {
		super('Auth session transitions already have one active operation and one queued operation');
		this.name = 'AuthSessionTransitionCapacityError';
	}
}

export class AuthSessionTransitionInvariantError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'AuthSessionTransitionInvariantError';
	}
}

export class AuthSessionTransitionGate {
	private queueTail: Promise<void> = Promise.resolve();
	private pendingOperations = 0;
	private accountTransitionDepth = 0;

	constructor(private readonly dependencies: AuthSessionTransitionDependencies) {}

	get isAccountTransitionActive(): boolean {
		return this.accountTransitionDepth > 0;
	}

	async runExclusive<Result>(operation: () => Promise<Result>): Promise<Result> {
		if (this.pendingOperations > MAX_QUEUED_SESSION_OPERATIONS) {
			throw new AuthSessionTransitionCapacityError();
		}
		this.pendingOperations += 1;
		const next = this.queueTail.then(operation, operation);
		this.queueTail = next.then(
			() => this.releaseOperation(),
			() => this.releaseOperation(),
		);
		return await next;
	}

	async runAccountTransition<Result>(operation: () => Promise<Result>): Promise<Result> {
		return await this.runExclusive(async () => {
			this.accountTransitionDepth += 1;
			let operationFailed = false;
			let operationError: unknown;
			let result!: Result;
			try {
				result = await operation();
			} catch (error) {
				operationFailed = true;
				operationError = error;
			}
			this.finishAccountTransition();
			if (operationFailed) {
				throw operationError;
			}
			return result;
		});
	}

	async runAccountScoped<Result>(
		reason: AccountScopedWorkTransitionReason,
		operation: (context: AuthSessionTransitionContext) => Promise<Result>,
	): Promise<Result> {
		return await this.dependencies.runSuspended(reason, operation);
	}

	private releaseOperation(): void {
		if (this.pendingOperations <= 0) {
			throw new AuthSessionTransitionInvariantError('Pending auth session operations underflowed');
		}
		this.pendingOperations -= 1;
	}

	private finishAccountTransition(): void {
		if (this.accountTransitionDepth <= 0) {
			throw new AuthSessionTransitionInvariantError('Account transition depth underflowed');
		}
		this.accountTransitionDepth -= 1;
	}
}
