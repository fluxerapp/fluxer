// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';

const logger = new Logger('AccountReadyWork');

let pendingUserId: string | null = null;

function runAccountReadyWork(userId: string): void {
	void import('@app/features/experiment/state/ExperimentAssignments')
		.then((module) => module.default.start(userId))
		.catch((error) => {
			logger.warn('Failed to start experiment assignments after READY', error);
		});
	void import('@app/features/premium/commands/PremiumCommands')
		.then((module) => module.refreshPremiumState())
		.catch((error) => {
			logger.warn('Failed to refresh premium state after READY', error);
		});
	void import('@app/features/premium/utils/OperatorPassUtils')
		.then(async (utils) => {
			if (!utils.isOperatorPassAvailable()) return;
			const commands = await import('@app/features/premium/commands/OperatorPassCommands');
			await commands.loadPassesForAccount(userId);
		})
		.catch((error) => {
			logger.warn('Failed to load Operator Passes after READY', error);
		});
	void import('@app/features/user/commands/OwnProfileBadgesCommands')
		.then((module) => module.refresh())
		.catch((error) => {
			logger.warn('Failed to load your badges after READY', error);
		});
	void import('@app/features/platform/state/LegacySharedContentReview')
		.then((module) => module.reviewLegacySharedContent())
		.catch((error) => {
			logger.warn('Failed to review migrated shared content after READY', error);
		});
	void import('@app/features/auth/passkey_migration/PasskeyMigration')
		.then((module) => module.default.handleGatewayReady(userId))
		.catch((error) => {
			logger.warn('Failed to run the passkey migration check after READY', error);
		});
	if (isDesktop()) {
		void import('@app/features/voice/utils/GpuEncoderCapabilities')
			.then((module) => module.prewarmGpuEncoderReport())
			.catch((error) => {
				logger.warn('Failed to schedule the GPU encoder probe after READY', error);
			});
	}
}

export function scheduleAccountReadyWork(userId: string): void {
	if (AccountScopedWork.isSuspended) {
		pendingUserId = userId;
		return;
	}
	pendingUserId = null;
	runAccountReadyWork(userId);
}

AccountScopedWork.registerTransition({
	suspend: () => {
		pendingUserId = null;
	},
	resume: () => {},
	released: () => {
		const userId = pendingUserId;
		pendingUserId = null;
		if (userId === null || SessionManager.userId !== userId) {
			return;
		}
		runAccountReadyWork(userId);
	},
});
