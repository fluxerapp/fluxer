// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, describe, expect, it, vi} from 'vitest';

const mocks = vi.hoisted(() => ({patch: vi.fn()}));

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: unknown) => descriptor,
	t: (descriptor: unknown) => descriptor,
}));

vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {get: vi.fn(), post: vi.fn(), patch: mocks.patch},
}));

const {AccountScopedWork, AccountScopedWorkTransitionReason, accountScopedWorkAbortError} = await import(
	'@app/features/platform/state/AccountScopedWork'
);
const {default: UserSettings} = await import('@app/features/user/state/UserSettings');

async function settle(): Promise<void> {
	for (let i = 0; i < 10; i++) {
		await Promise.resolve();
	}
	await new Promise((resolve) => setTimeout(resolve, 0));
}

function hydrate(): void {
	UserSettings.updateUserSettings({
		status: 'online',
		flags: 0,
		locale: 'en-US',
		developer_mode: false,
		message_display_compact: false,
		restricted_guilds: [],
		bot_restricted_guilds: [],
		guild_folders: [],
		trusted_domains: [],
		staff_dm_access_user_ids: [],
		suppress_unprivileged_self_mentions_bypass_user_ids: [],
	});
}

describe('UserSettings synced preference flush across account transitions', () => {
	afterEach(() => {
		UserSettings.handleAccountTransition();
		mocks.patch.mockReset();
	});

	it('holds a flush requested during a transition until admission opens', async () => {
		hydrate();
		mocks.patch.mockResolvedValue({ok: true, status: 200, body: {}});
		let saved: Promise<void> | null = null;
		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {
			saved = UserSettings.setSanitizeUrls(!UserSettings.getSanitizeUrls());
			await settle();
			expect(mocks.patch).not.toHaveBeenCalled();
		});
		await settle();
		expect(mocks.patch).toHaveBeenCalledTimes(1);
		await expect(saved).resolves.toBeUndefined();
	});

	it('retries a flush aborted by an account transition instead of failing it', async () => {
		hydrate();
		mocks.patch.mockRejectedValueOnce(accountScopedWorkAbortError());
		mocks.patch.mockResolvedValue({ok: true, status: 200, body: {}});
		const saved = UserSettings.setSanitizeUrls(!UserSettings.getSanitizeUrls());
		await settle();
		expect(mocks.patch).toHaveBeenCalledTimes(2);
		await expect(saved).resolves.toBeUndefined();
	});
});
