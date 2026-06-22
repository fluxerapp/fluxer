// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {beforeEach, describe, expect, it, vi} from 'vitest';

interface RpcActivityPayload {
	activity: UserActivity | null;
	gatewayActivity?: UserActivity;
	receivedAt?: number;
	source: string;
}

const applyActivityImmediate = vi.fn();
const clearImmediately = vi.fn();
const onRpcActivityUpdate = vi.fn();
const debug = vi.fn();
let isAuthenticated = true;
let rpcActivityCallback: ((payload: RpcActivityPayload) => void) | null = null;

vi.mock('@app/features/auth/state/Authentication', () => ({
	default: {
		get isAuthenticated() {
			return isAuthenticated;
		},
	},
}));

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug(...args: Parameters<typeof debug>) {
			debug(...args);
		}
	},
}));

vi.mock('@app/features/presence/state/LocalRpcPresence', () => ({
	default: {
		applyActivityImmediate,
		clearImmediately,
	},
}));

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	isDesktop: () => true,
	getElectronAPI: () => ({
		onRpcActivityUpdate,
	}),
}));

function activity(name: string) {
	return {
		type: 0,
		name,
	};
}

describe('DesktopRpcBridge', () => {
	beforeEach(() => {
		vi.resetModules();
		vi.clearAllMocks();
		isAuthenticated = true;
		rpcActivityCallback = null;
		onRpcActivityUpdate.mockImplementation((callback) => {
			rpcActivityCallback = callback;
			return vi.fn();
		});
	});

	it('ignores stale RPC activity updates after a newer event has been applied', async () => {
		const {initializeDesktopRpcBridge} = await import('@app/features/platform/utils/DesktopRpcBridge');

		initializeDesktopRpcBridge();
		expect(rpcActivityCallback).toBeTruthy();

		rpcActivityCallback?.({activity: activity('Newer game'), receivedAt: 200, source: 'ipc'});
		rpcActivityCallback?.({activity: activity('Older game'), receivedAt: 100, source: 'process-scan'});

		expect(applyActivityImmediate).toHaveBeenCalledTimes(1);
		expect(applyActivityImmediate).toHaveBeenCalledWith(activity('Newer game'), activity('Newer game'));
		expect(debug).toHaveBeenCalledWith('Ignored stale RPC activity update', {receivedAt: 100, lastReceivedAt: 200});
	});

	it('does not apply RPC activity while unauthenticated', async () => {
		const {initializeDesktopRpcBridge} = await import('@app/features/platform/utils/DesktopRpcBridge');

		initializeDesktopRpcBridge();
		isAuthenticated = false;
		rpcActivityCallback?.({activity: activity('Hidden game'), receivedAt: 300, source: 'ipc'});

		expect(applyActivityImmediate).not.toHaveBeenCalled();
		expect(clearImmediately).not.toHaveBeenCalled();
	});
});
