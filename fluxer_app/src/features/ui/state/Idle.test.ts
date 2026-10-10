// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const IDLE_THRESHOLD_PASSED_MS = 11 * 60 * 1000;

const mobileLayoutLoad = vi.hoisted(() => ({delayMs: 0}));

vi.mock('@app/features/ui/state/MobileLayout', async () => {
	await vi.advanceTimersByTimeAsync(mobileLayoutLoad.delayMs);
	return {default: {isMobileLayout: () => false}};
});

vi.mock('@app/features/user/state/CustomStatus', () => ({
	customStatusToKey: () => 'none',
	normalizeCustomStatus: () => null,
	toGatewayCustomStatus: () => null,
}));

function hydratedOnlineSettings(afkTimeoutSeconds = 600) {
	return {
		status: StatusTypes.ONLINE,
		isHydrated: () => true,
		markSessionChanging: () => {},
		getAfkTimeout: () => afkTimeoutSeconds,
		getCustomStatus: () => null,
		getStatusResetsAt: () => null,
		getStatusResetsTo: () => null,
	};
}

describe('Idle', () => {
	beforeEach(() => {
		vi.resetModules();
		vi.useFakeTimers();
		mobileLayoutLoad.delayMs = 0;
	});

	afterEach(async () => {
		const {default: Idle} = await import('@app/features/ui/state/Idle');
		Idle.destroy();
		vi.useRealTimers();
	});

	it('flips idle on its own timer while LocalPresence is still loading', async () => {
		mobileLayoutLoad.delayMs = IDLE_THRESHOLD_PASSED_MS;
		const {default: LocalPresence} = await import('@app/features/presence/state/LocalPresence');
		const {default: Idle} = await import('@app/features/ui/state/Idle');
		expect(Idle.isIdle()).toBe(true);
		expect(LocalPresence).toBeDefined();
	});

	it('moves LocalPresence to idle and back to online with the idle state', async () => {
		const {default: LocalPresence, setLocalPresenceUserSettings} = await import(
			'@app/features/presence/state/LocalPresence'
		);
		const {default: Idle} = await import('@app/features/ui/state/Idle');
		setLocalPresenceUserSettings(hydratedOnlineSettings());
		LocalPresence.updatePresence();
		await vi.advanceTimersByTimeAsync(0);
		expect(LocalPresence.getStatus()).toBe(StatusTypes.ONLINE);

		await vi.advanceTimersByTimeAsync(IDLE_THRESHOLD_PASSED_MS);
		expect(Idle.isIdle()).toBe(true);
		expect(LocalPresence.getStatus()).toBe(StatusTypes.IDLE);
		expect(LocalPresence.getPresence().since).toBeGreaterThan(0);

		Idle.recordActivity();
		expect(Idle.isIdle()).toBe(false);
		expect(LocalPresence.getStatus()).toBe(StatusTypes.ONLINE);
		expect(LocalPresence.getPresence().since).toBe(0);
	});

	it('reports afk once the user afk timeout passes', async () => {
		const {default: LocalPresence, setLocalPresenceUserSettings} = await import(
			'@app/features/presence/state/LocalPresence'
		);
		const {default: Idle} = await import('@app/features/ui/state/Idle');
		setLocalPresenceUserSettings(hydratedOnlineSettings(60));
		LocalPresence.updatePresence();
		await vi.advanceTimersByTimeAsync(0);
		expect(LocalPresence.getPresence().afk).toBe(false);

		await vi.advanceTimersByTimeAsync(45 * 1000);
		expect(LocalPresence.getPresence().afk).toBe(false);

		await vi.advanceTimersByTimeAsync(45 * 1000);
		expect(LocalPresence.getPresence().afk).toBe(true);

		Idle.recordActivity();
		expect(LocalPresence.getPresence().afk).toBe(false);
	});
});
