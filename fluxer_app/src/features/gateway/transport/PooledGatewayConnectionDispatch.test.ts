// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import Initialization from '@app/features/app/state/Initialization';
import type {GatewaySocket, GatewaySocketProperties} from '@app/features/gateway/transport/GatewaySocket';
import {PooledGatewayConnection} from '@app/features/gateway/transport/PooledGatewayConnection';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

await vi.hoisted(async () => {
	const {installHarnessBootstrap} = await import('@app/features/auth/state/__fixtures__/AccountSwitchHarness');
	installHarnessBootstrap();
});

const mocks = vi.hoisted(() => ({
	createHandlerRegistry: vi.fn(() => new Map()),
	currentAccountKey: null as string | null,
}));

vi.mock('@app/features/channel/state/ChannelFrecency', () => ({default: {handleLogout: vi.fn()}}));

vi.mock('@app/features/forum/state/ForumPosts', () => ({default: {reset: vi.fn(), handleGatewayReady: vi.fn()}}));

vi.mock('@app/features/forum/state/ForumReadState', () => ({default: {handleGatewayReady: vi.fn()}}));

vi.mock('@app/features/navigation/state/NavigationSideEffects', () => ({default: {handleGatewayReady: vi.fn()}}));

vi.mock('@app/features/threads/state/ChannelThreads', () => ({default: {handleGatewayReady: vi.fn()}}));

vi.mock('@app/features/threads/state/ThreadGuilds', () => ({
	default: {reset: vi.fn(), handleGatewayReady: vi.fn(), isPurgedEvent: () => false},
}));

vi.mock('@app/features/threads/state/ThreadPanel', () => ({default: {closeCreate: vi.fn()}}));

vi.mock('@app/features/threads/state/ThreadRoster', () => ({default: {reset: vi.fn()}}));

vi.mock('@app/features/threads/state/ThreadSubscriptions', () => ({default: {handleConnectionReady: vi.fn()}}));

vi.mock('@app/features/channel/state/Channels', () => ({default: {getChannel: () => undefined}}));

vi.mock('@app/features/navigation/state/Navigation', () => ({default: {channelId: null}}));

vi.mock('@app/features/messaging/state/AttachmentUrlRefresher', () => ({default: {reset: vi.fn()}}));

vi.mock('@app/features/gateway/events/EventRouter', () => ({createHandlerRegistry: mocks.createHandlerRegistry}));

vi.mock('@app/features/app/state/GeoIP', () => ({
	default: {
		latitude: null,
		longitude: null,
		applyConnectionFallbackGeo: vi.fn(),
	},
}));

vi.mock('@app/features/gateway/transport/GatewayCompression', () => ({
	isCompressionSupported: () => true,
	isGatewayCompressionError: () => false,
	GatewayCompressionError: class GatewayCompressionError extends Error {},
	GatewayCompression: class GatewayCompression {
		warmup(): Promise<void> {
			return Promise.resolve();
		}
		destroy(): void {}
	},
}));

vi.mock('@app/features/platform/state/AuthSession', () => ({
	default: {
		get currentAccountKey() {
			return mocks.currentAccountKey;
		},
		getAccount(accountKey: string) {
			return accountKey === mocks.currentAccountKey ? {userId: 'user-primary', token: 'token-primary'} : null;
		},
		userId: 'user-primary',
		token: 'token-primary',
		markAccountInvalid: vi.fn(),
		handleConnectionStarted: vi.fn(),
		handleConnectionFailed: vi.fn(),
		handleConnectionReady: vi.fn(),
		handleConnectionClosed: vi.fn(async () => undefined),
	},
}));

vi.mock('@app/features/auth/state/AccountAccess', () => ({
	default: {markGatewayReady: vi.fn(), markGatewayUnavailable: vi.fn()},
}));

vi.mock('@app/features/platform/state/ResetClientState', () => ({
	ResetClientStateReason: {GATEWAY_AUTH_FAILURE: 'gateway-auth-failure'},
	resetClientState: vi.fn(() => Promise.resolve()),
}));

vi.mock('@app/features/ui/state/LayerManager', () => ({default: {closeAll: vi.fn()}}));

vi.mock('@app/features/app/state/RuntimeCrash', () => ({default: {triggerFatalCrash: vi.fn()}}));

vi.mock('@app/features/auth/state/Authentication', () => ({
	default: {handleConnectionClosed: vi.fn(async () => undefined)},
}));

vi.mock('@app/features/expressions/state/FavoriteMemes', () => ({default: {reset: vi.fn()}}));

vi.mock('@app/features/member/state/MemberSearch', () => ({default: {handleLogout: vi.fn()}}));

vi.mock('@app/features/moderation/state/ReportFlows', () => ({default: {reset: vi.fn()}}));

vi.mock('@app/features/messaging/state/MessagingMessages', () => ({
	default: {handleSessionInvalidated: vi.fn(), handleConnectionClosed: vi.fn()},
}));

vi.mock('@app/features/navigation/state/SelectedGuild', () => ({
	default: {selectedGuildId: null, selectionNonce: 0},
}));

vi.mock('@app/features/presence/state/LocalPresence', () => ({
	default: {
		presenceKey: '',
		mobile: false,
		updatePresence: vi.fn(),
		getGatewayPresence: () => null,
		handleSessionChanging: vi.fn(),
	},
}));

vi.mock('@app/features/presence/state/Presence', () => ({default: {handleSessionInvalidated: vi.fn()}}));

vi.mock('@app/features/search/state/QuickSwitcher', () => ({default: {recomputeIfOpen: vi.fn()}}));

vi.mock('@app/features/typing/state/TypingIndicator', () => ({default: {reset: vi.fn()}}));

vi.mock('@app/features/ui/state/MobileLayout', () => ({default: {isMobileLayout: () => false}}));

vi.mock('@app/features/voice/VoiceGatewayLifecycle', () => ({
	reportGatewayErrorToVoice: vi.fn(),
	teardownVoiceForFatalGatewayCrash: vi.fn(),
}));

const PROPERTIES: GatewaySocketProperties = {
	os: 'macos',
	browser: 'Fluxer Client',
	device: 'desktop',
	locale: 'en-US',
	user_agent: 'test',
	browser_version: '1',
	os_version: '1',
	build_version: '1',
};

class FakeWebSocket {
	static readonly CONNECTING = 0;
	static readonly OPEN = 1;
	static readonly CLOSING = 2;
	static readonly CLOSED = 3;
	static instances: Array<FakeWebSocket> = [];

	readyState: number = FakeWebSocket.CONNECTING;
	binaryType = 'blob';

	constructor(readonly url: string) {
		FakeWebSocket.instances.push(this);
	}

	addEventListener(): void {}
	removeEventListener(): void {}
	send(): void {}
	close(): void {
		this.readyState = FakeWebSocket.CLOSED;
	}
}

beforeEach(() => {
	vi.useFakeTimers();
	FakeWebSocket.instances = [];
	mocks.currentAccountKey = 'primary.test::100';
	mocks.createHandlerRegistry.mockClear();
	vi.stubGlobal('WebSocket', FakeWebSocket);
});

afterEach(() => {
	vi.restoreAllMocks();
	vi.unstubAllGlobals();
	vi.useRealTimers();
});

describe('pooled gateway connection', () => {
	test('a foreground connection builds exactly one handler registry', () => {
		new PooledGatewayConnection();

		expect(mocks.createHandlerRegistry).toHaveBeenCalledTimes(1);
	});

	test('retiring the foreground for an account switch keeps the shell mounted', () => {
		const connection = new PooledGatewayConnection();
		Initialization.setReady();

		connection.retireForAccountSwitch();

		expect(Initialization.hasCompletedInitialLoad).toBe(true);
		expect(Initialization.isReady).toBe(false);
	});

	test('logging out returns the client to its booting state', () => {
		const connection = new PooledGatewayConnection();
		Initialization.setReady();

		connection.logout();

		expect(Initialization.hasCompletedInitialLoad).toBe(false);
		expect(Initialization.isReady).toBe(false);
	});

	test('a dispatch delivered on a retired socket invokes no handler', () => {
		const handler = vi.fn();
		mocks.createHandlerRegistry.mockImplementationOnce(() => new Map([['MESSAGE_CREATE', handler]]));
		const connection = new PooledGatewayConnection();
		const internals = connection as unknown as {
			createGatewaySocket: (
				gatewayUrl: string,
				token: string,
				properties: GatewaySocketProperties,
				generation: number,
				accountKey: string | null,
			) => GatewaySocket;
			handleGatewayDispatch: (
				socket: GatewaySocket,
				delivery: unknown,
				accountKey: string | null,
				isCurrent: () => boolean,
			) => void;
			socketOwner: {socket: GatewaySocket; accountKey: string | null} | null;
			generation: number;
		};
		const socket = internals.createGatewaySocket(
			'wss://gateway.primary.test',
			'token-primary',
			PROPERTIES,
			internals.generation,
			'primary.test::100',
		);
		const delivery = {
			type: 'MESSAGE_CREATE',
			data: {},
			retainedByteSize: 2,
			receipt: {sequence: 1, generation: 0},
		};

		internals.socketOwner = null;
		internals.handleGatewayDispatch(socket, delivery, 'primary.test::100', () => true);
		expect(handler).not.toHaveBeenCalled();

		internals.socketOwner = {socket, accountKey: 'other.test::999'};
		internals.handleGatewayDispatch(socket, delivery, 'primary.test::100', () => true);
		expect(handler).not.toHaveBeenCalled();

		internals.socketOwner = {socket, accountKey: 'primary.test::100'};
		internals.handleGatewayDispatch(socket, delivery, 'primary.test::100', () => true);
		expect(handler).toHaveBeenCalledTimes(1);
	});
});
