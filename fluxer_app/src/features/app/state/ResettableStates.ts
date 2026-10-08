// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';

type AccountTransitionReason = 'logout' | 'account-switch';

class ResettableStateAlreadyRegisteredError extends Error {
	constructor() {
		super('Resettable state already has a registration owner');
		this.name = 'ResettableStateAlreadyRegisteredError';
	}
}

class ResettableStateRegistry {
	private readonly logger = new Logger('ResettableStates');
	private readonly resets = new Map<object, () => void>();

	register(store: object, reset: () => void): () => void {
		if (this.resets.has(store)) {
			throw new ResettableStateAlreadyRegisteredError();
		}
		this.resets.set(store, reset);
		let registered = true;
		return () => {
			if (!registered) {
				return;
			}
			registered = false;
			this.resets.delete(store);
		};
	}

	resetAll(): void {
		for (const reset of [...this.resets.values()]) {
			try {
				reset();
			} catch (error) {
				this.logger.warn('Registered store failed to reset', error);
			}
		}
	}

	prepareForAccountTransition(reason: AccountTransitionReason): void {
		this.logger.info('Resetting registered stores for account transition', reason);
		this.resetAll();
	}
}

export const ResettableStates = new ResettableStateRegistry();
