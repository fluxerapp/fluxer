// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, describe, expect, test, vi} from 'vitest';

const mocks = vi.hoisted(() => ({transitioning: false}));

vi.mock('@app/features/auth/state/Accounts', () => ({
	default: {
		get transitioning() {
			return mocks.transitioning;
		},
	},
}));
vi.mock('@app/features/devtools/state/DeveloperOptions', () => ({
	default: {bypassLoadingSkeleton: false, forceLoadingSkeleton: false},
}));
vi.mock('@app/features/gateway/transport/GatewayConnection', () => ({default: {isConnectionInterrupted: false}}));

const {default: Initialization} = await import('@app/features/app/state/Initialization');
const {isClientReconnecting} = await import('@app/features/app/state/ClientReadiness');

describe('isClientReconnecting', () => {
	afterEach(() => {
		mocks.transitioning = false;
		Initialization.reset();
	});

	test('a loaded client waiting for its gateway is reconnecting', () => {
		Initialization.setReady();
		Initialization.setConnecting();
		expect(isClientReconnecting()).toBe(true);
	});

	test('an account transition waiting for the next gateway is not a reconnect', () => {
		Initialization.setReady();
		Initialization.setLoading();
		mocks.transitioning = true;
		expect(isClientReconnecting()).toBe(false);
	});
});
