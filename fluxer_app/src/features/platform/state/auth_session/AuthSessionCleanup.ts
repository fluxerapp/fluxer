// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AccountScopedWorkTransitionReason} from '@app/features/platform/state/AccountScopedWork';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('AuthSessionCleanup');

export type AuthSessionCleanupOperation = () => void | Promise<void>;

export interface AuthSessionCleanupDependencies {
	readonly sendInvisiblePresence: (reason: AccountScopedWorkTransitionReason) => void | Promise<void>;
	readonly cleanupGatewaySession: (reason: AccountScopedWorkTransitionReason) => void | Promise<void>;
}

export class AuthSessionCleanup {
	constructor(private readonly dependencies: AuthSessionCleanupDependencies) {}

	async retireSession(reason: AccountScopedWorkTransitionReason): Promise<void> {
		const notificationErrors = await this.collect([() => this.dependencies.sendInvisiblePresence(reason)]);
		this.report(notificationErrors, `Failed to send the retiring session presence during ${reason}`);
		await this.dependencies.cleanupGatewaySession(reason);
	}

	async collect(operations: ReadonlyArray<AuthSessionCleanupOperation>): Promise<Array<unknown>> {
		const errors: Array<unknown> = [];
		for (const operation of operations) {
			try {
				await operation();
			} catch (error) {
				errors.push(error);
			}
		}
		return errors;
	}

	report(errors: ReadonlyArray<unknown>, message: string): void {
		if (errors.length > 0) {
			logger.error(message, new AggregateError(errors, message));
		}
	}
}
