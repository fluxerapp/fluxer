// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, describe, expect, it, vi} from 'vitest';

const mocks = vi.hoisted(() => ({
	userId: null as string | null,
	start: vi.fn(),
	refreshPremiumState: vi.fn(() => Promise.resolve()),
	passkeyReady: vi.fn(),
}));

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = vi.fn();
	},
}));

vi.mock('@app/features/platform/state/AuthSession', () => ({
	default: {
		get userId() {
			return mocks.userId;
		},
	},
}));

vi.mock('@app/features/experiment/state/ExperimentAssignments', () => ({default: {start: mocks.start}}));
vi.mock('@app/features/premium/commands/PremiumCommands', () => ({refreshPremiumState: mocks.refreshPremiumState}));
vi.mock('@app/features/auth/passkey_migration/PasskeyMigration', () => ({
	default: {handleGatewayReady: mocks.passkeyReady},
}));

const {AccountScopedWork, AccountScopedWorkTransitionReason} = await import(
	'@app/features/platform/state/AccountScopedWork'
);
const {scheduleAccountReadyWork} = await import('@app/features/gateway/events/AccountReadyWork');

async function settle(): Promise<void> {
	for (let i = 0; i < 10; i++) {
		await Promise.resolve();
	}
	await new Promise((resolve) => setTimeout(resolve, 0));
}

function expectRanFor(userId: string): void {
	expect(mocks.start).toHaveBeenCalledTimes(1);
	expect(mocks.start).toHaveBeenCalledWith(userId);
	expect(mocks.refreshPremiumState).toHaveBeenCalledTimes(1);
	expect(mocks.passkeyReady).toHaveBeenCalledWith(userId);
}

describe('AccountReadyWork', () => {
	afterEach(() => {
		mocks.userId = null;
		vi.clearAllMocks();
	});

	it('runs the READY work at once when admission is open', async () => {
		mocks.userId = 'a';
		scheduleAccountReadyWork('a');
		await settle();
		expectRanFor('a');
	});

	it('defers READY work scheduled during an account transition until admission opens', async () => {
		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {
			mocks.userId = 'b';
			scheduleAccountReadyWork('b');
			await settle();
			expect(mocks.start).not.toHaveBeenCalled();
			expect(mocks.refreshPremiumState).not.toHaveBeenCalled();
		});
		await settle();
		expectRanFor('b');
	});

	it('runs only the last account scheduled within one transition', async () => {
		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {
			scheduleAccountReadyWork('b');
			mocks.userId = 'a';
			scheduleAccountReadyWork('a');
		});
		await settle();
		expectRanFor('a');
	});

	it('drops deferred work when the transition ended on another account', async () => {
		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {
			scheduleAccountReadyWork('b');
			mocks.userId = 'a';
		});
		await settle();
		expect(mocks.start).not.toHaveBeenCalled();
		expect(mocks.refreshPremiumState).not.toHaveBeenCalled();
	});

	it('does not replay work from an earlier transition on a later one', async () => {
		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {
			mocks.userId = 'b';
			scheduleAccountReadyWork('b');
		});
		await settle();
		vi.clearAllMocks();
		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {});
		await settle();
		expect(mocks.start).not.toHaveBeenCalled();
	});
});
