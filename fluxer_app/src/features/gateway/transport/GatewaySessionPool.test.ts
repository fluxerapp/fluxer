// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import Initialization from '@app/features/app/state/Initialization';
import {
	ForegroundGatewayConnectionRecoverableError,
	ForegroundGatewayRecoveryCause,
} from '@app/features/gateway/transport/ForegroundGatewayConnectionFailure';
import GatewaySessions, {
	MAX_BACKGROUND_GATEWAY_CONNECTIONS,
	readMaxBackgroundGatewayConnections,
} from '@app/features/gateway/transport/GatewaySessionPool';
import {
	GATEWAY_FOREGROUND_READY_TIMEOUT_MS,
	GatewayReadyTimeoutError,
} from '@app/features/gateway/transport/PooledGatewayConnection';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import LocalPresence from '@app/features/presence/state/LocalPresence';
import {runInAction} from 'mobx';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const mocks = vi.hoisted(() => ({token: 'token-primary' as string | null, clientPropertiesFailure: false}));

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

vi.mock('@app/features/app/state/GeoIP', () => ({
	default: {latitude: null, longitude: null, applyConnectionFallbackGeo: vi.fn()},
}));

vi.mock('@app/features/app/state/RuntimeConfig', () => ({
	default: {gatewayEndpoint: 'wss://gateway.primary.test'},
}));

vi.mock('@app/features/gateway/transport/ForegroundGatewayHandoff', () => ({
	ForegroundGatewayHandoff: class ForegroundGatewayHandoff {},
}));

vi.mock('@app/features/gateway/transport/BackgroundAccountPresence', () => ({
	default: {appearOffline: true},
}));

vi.mock('@app/features/gateway/events/EventRouter', () => ({createHandlerRegistry: () => new Map()}));

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

vi.mock('@app/features/platform/utils/ClientInfo', () => ({
	getGatewayClientProperties: () =>
		mocks.clientPropertiesFailure
			? Promise.reject(new Error('client metadata unavailable'))
			: Promise.resolve({
					os: 'macos',
					browser: 'Fluxer Client',
					device: 'desktop',
					locale: 'en-US',
					user_agent: 'test',
					browser_version: '1',
					os_version: '1',
					build_version: '1',
				}),
}));

vi.mock('@app/features/platform/state/AuthSession', () => ({
	default: {
		currentAccountKey: 'primary.test::100',
		userId: '100',
		get token() {
			return mocks.token;
		},
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

vi.mock('@app/features/app/state/Initialization', () => ({
	default: {setConnecting: vi.fn(), setReady: vi.fn(), reset: vi.fn(), setError: vi.fn()},
}));

vi.mock('@app/features/app/state/RuntimeCrash', () => ({default: {triggerFatalCrash: vi.fn()}}));

vi.mock('@app/features/auth/state/Authentication', () => ({
	default: {handleConnectionClosed: vi.fn(async () => undefined)},
}));

vi.mock('@app/features/expressions/state/FavoriteMemes', () => ({default: {reset: vi.fn()}}));

vi.mock('@app/features/member/state/MemberSearch', () => ({default: {handleLogout: vi.fn()}}));

vi.mock('@app/features/messaging/state/MessagingMessages', () => ({
	default: {handleSessionInvalidated: vi.fn(), handleConnectionClosed: vi.fn()},
}));

vi.mock('@app/features/navigation/state/SelectedGuild', async () => {
	const {observable} = await import('mobx');
	return {default: observable({selectedGuildId: null as string | null, selectionNonce: 0})};
});

vi.mock('@app/features/permissions/state/Permission', () => ({default: {}}));

vi.mock('@app/features/presence/state/LocalPresence', async () => {
	const {observable} = await import('mobx');
	return {
		default: observable(
			{
				presenceKey: '',
				mobile: false,
				updatePresence: () => undefined,
				getGatewayPresence: () => null,
				handleSessionChanging: () => undefined,
			},
			{},
			{deep: false},
		),
	};
});

vi.mock('@app/features/presence/state/Presence', () => ({default: {handleSessionInvalidated: vi.fn()}}));

vi.mock('@app/features/search/state/QuickSwitcher', () => ({default: {recomputeIfOpen: vi.fn()}}));

vi.mock('@app/features/typing/state/TypingIndicator', () => ({default: {reset: vi.fn()}}));

vi.mock('@app/features/ui/state/MobileLayout', () => ({default: {isMobileLayout: () => false}}));

vi.mock('@app/features/voice/VoiceGatewayLifecycle', () => ({
	reportGatewayErrorToVoice: vi.fn(),
	teardownVoiceForFatalGatewayCrash: vi.fn(),
}));

const ROUTER_GLOBAL_KEY = '__fluxerDesktopGatewayTransport';

class FakeWebSocket {
	static readonly CONNECTING = 0;
	static readonly OPEN = 1;
	static readonly CLOSING = 2;
	static readonly CLOSED = 3;
	static instances: Array<FakeWebSocket> = [];

	readyState: number = FakeWebSocket.CONNECTING;
	binaryType = 'blob';
	readonly closeCalls: Array<{code: number; reason: string}> = [];

	constructor(readonly url: string) {
		FakeWebSocket.instances.push(this);
	}

	addEventListener(): void {}
	removeEventListener(): void {}
	send(): void {}
	close(code: number, reason: string): void {
		this.closeCalls.push({code, reason});
		this.readyState = FakeWebSocket.CLOSED;
	}
}

function liveSocketCount(): number {
	return FakeWebSocket.instances.filter((socket) => socket.readyState !== FakeWebSocket.CLOSED).length;
}

interface ForegroundConnectionInternals {
	guildSubscriptions: {pendingGuildId: string | null};
}

function pendingGuildSyncId(): string | null {
	const connection = GatewaySessions.foregroundConnection as unknown as ForegroundConnectionInternals;
	return connection.guildSubscriptions.pendingGuildId;
}

async function settleSocketOpen(expectedSockets: number): Promise<void> {
	for (let attempt = 0; attempt < 20 && FakeWebSocket.instances.length < expectedSockets; attempt += 1) {
		await vi.advanceTimersByTimeAsync(0);
	}
	expect(FakeWebSocket.instances).toHaveLength(expectedSockets);
}

beforeEach(() => {
	vi.useFakeTimers();
	FakeWebSocket.instances = [];
	mocks.token = 'token-primary';
	mocks.clientPropertiesFailure = false;
	vi.stubGlobal('WebSocket', FakeWebSocket);
	Reflect.deleteProperty(globalThis as unknown as Record<string, unknown>, ROUTER_GLOBAL_KEY);
});

afterEach(() => {
	GatewaySessions.foregroundConnection.logout();
	GatewaySessions.dispose();
	runInAction(() => {
		SelectedGuild.selectedGuildId = null;
		SelectedGuild.selectionNonce = 0;
	});
	vi.unstubAllGlobals();
	vi.useRealTimers();
});

describe('background connection cap', () => {
	test('defaults to six when the instance declares nothing', () => {
		expect(readMaxBackgroundGatewayConnections(undefined)).toBe(MAX_BACKGROUND_GATEWAY_CONNECTIONS);
	});

	test('an instance may reduce the cap, including to zero', () => {
		expect(readMaxBackgroundGatewayConnections(0)).toBe(0);
		expect(readMaxBackgroundGatewayConnections(2)).toBe(2);
	});

	test('an instance can never raise the cap above the client bound', () => {
		expect(readMaxBackgroundGatewayConnections(64)).toBe(MAX_BACKGROUND_GATEWAY_CONNECTIONS);
	});

	test('a malformed declaration is rejected', () => {
		expect(() => readMaxBackgroundGatewayConnections(-1)).toThrow(
			'Background gateway connection limit must be a non-negative integer, got -1',
		);
		expect(() => readMaxBackgroundGatewayConnections(1.5)).toThrow(
			'Background gateway connection limit must be a non-negative integer, got 1.5',
		);
	});
});

describe('web inertness', () => {
	test('no background transport is available without an electron bridge', () => {
		expect(window.electron).toBeUndefined();
		expect(GatewaySessions.isBackgroundTransportAvailable()).toBe(false);
	});

	test('bootstrapBackground returns early and creates no background session', () => {
		GatewaySessions.start();

		GatewaySessions.bootstrapBackground();

		expect(GatewaySessions.isBackgroundBootstrapped).toBe(false);
		expect(GatewaySessions.backgroundConnectionCount).toBe(0);
	});

	test('a full account switch keeps exactly one live WebSocket and installs no desktop router', async () => {
		GatewaySessions.start();

		await GatewaySessions.foregroundConnection.startSession('primary.test::100');
		await settleSocketOpen(1);
		expect(liveSocketCount()).toBe(1);

		mocks.token = 'token-secondary';
		await GatewaySessions.foregroundConnection.startSession('secondary.test::200');
		await settleSocketOpen(2);

		expect(liveSocketCount()).toBe(1);
		expect(GatewaySessions.backgroundConnectionCount).toBe(0);
		expect((globalThis as unknown as Record<string, unknown>)[ROUTER_GLOBAL_KEY]).toBeUndefined();
	});
});

describe('pool reaction lifecycle', () => {
	test('start installs the foreground reactions and dispose removes them', async () => {
		GatewaySessions.start();
		await vi.advanceTimersByTimeAsync(0);

		runInAction(() => {
			SelectedGuild.selectedGuildId = 'guild-1';
			SelectedGuild.selectionNonce = 1;
		});
		expect(pendingGuildSyncId()).toBe('guild-1');

		GatewaySessions.dispose();
		runInAction(() => {
			SelectedGuild.selectedGuildId = 'guild-2';
			SelectedGuild.selectionNonce = 2;
			(LocalPresence as unknown as {presenceKey: string}).presenceKey = 'changed';
		});

		expect(pendingGuildSyncId()).toBe('guild-1');
	});

	test('dispose before the deferred install still leaves no reaction behind', async () => {
		GatewaySessions.start();
		GatewaySessions.dispose();
		await vi.advanceTimersByTimeAsync(0);

		runInAction(() => {
			SelectedGuild.selectedGuildId = 'guild-3';
			SelectedGuild.selectionNonce = 3;
		});

		expect(pendingGuildSyncId()).toBeNull();
	});
});

describe('waitForForegroundReady', () => {
	test('rejects with a recoverable error and keeps one socket reconnecting when READY never arrives', async () => {
		const pending = GatewaySessions.waitForForegroundReady('primary.test::100');
		const settled = expect(pending).rejects.toSatisfy(
			(error: unknown) =>
				error instanceof ForegroundGatewayConnectionRecoverableError &&
				error.recoveryCause === ForegroundGatewayRecoveryCause.TRANSPORT_DISCONNECTED,
		);
		await settleSocketOpen(1);
		const socket = GatewaySessions.foregroundConnection.socket;

		await vi.advanceTimersByTimeAsync(GATEWAY_FOREGROUND_READY_TIMEOUT_MS);
		await settled;

		expect(GatewaySessions.foregroundConnection.socket).toBe(socket);
		expect(GatewaySessions.foregroundConnection.isConnecting).toBe(true);
		expect(liveSocketCount()).toBeLessThanOrEqual(1);
	});

	test('a readiness timeout while still connecting reconnects the same socket with backoff', async () => {
		const pending = GatewaySessions.waitForForegroundReady('primary.test::100');
		const settled = expect(pending).rejects.toSatisfy(
			(error: unknown) =>
				error instanceof ForegroundGatewayConnectionRecoverableError &&
				error.recoveryCause === ForegroundGatewayRecoveryCause.READINESS_TIMEOUT &&
				error.cause instanceof GatewayReadyTimeoutError,
		);
		await settleSocketOpen(1);
		const socket = GatewaySessions.foregroundConnection.socket;
		const internals = socket as unknown as {clearHelloTimeout: () => void};
		internals.clearHelloTimeout();

		await vi.advanceTimersByTimeAsync(GATEWAY_FOREGROUND_READY_TIMEOUT_MS);
		await settled;

		expect(GatewaySessions.foregroundConnection.socket).toBe(socket);
		expect(socket?.getState()).toBe('RECONNECTING');
	});

	test('resolves once the foreground session reaches READY', async () => {
		const pending = GatewaySessions.waitForForegroundReady('primary.test::100');
		await settleSocketOpen(1);

		GatewaySessions.foregroundConnection.handleConnectionOpen('session-1');
		await pending;

		expect(GatewaySessions.foregroundConnection.isReady).toBe(true);
		expect(GatewaySessions.foregroundConnection.sessionId).toBe('session-1');
		expect(liveSocketCount()).toBe(1);
	});

	test('rejects without opening a socket when there is no stored token', async () => {
		mocks.token = null;

		await expect(GatewaySessions.waitForForegroundReady('primary.test::100')).rejects.toThrow(
			/Gateway session did not start/,
		);
		expect(FakeWebSocket.instances).toHaveLength(0);
	});
});

function failForegroundTransport(): void {
	const socket = GatewaySessions.foregroundConnection.socket;
	if (socket === null) {
		throw new Error('No foreground socket to fail');
	}
	socket.disconnect(4000, 'Gateway unreachable', true);
}

describe('foreground session during a gateway outage', () => {
	beforeEach(() => {
		vi.mocked(Initialization.setError).mockClear();
	});

	test('a connection failure before READY rejects the wait promptly and keeps the socket reconnecting', async () => {
		const pending = GatewaySessions.waitForForegroundReady('primary.test::100');
		const settled = expect(pending).rejects.toSatisfy(
			(error: unknown) =>
				error instanceof ForegroundGatewayConnectionRecoverableError &&
				error.recoveryCause === ForegroundGatewayRecoveryCause.TRANSPORT_DISCONNECTED,
		);
		await settleSocketOpen(1);

		failForegroundTransport();
		await vi.advanceTimersByTimeAsync(0);
		await settled;

		expect(GatewaySessions.foregroundConnection.socket).not.toBeNull();
		expect(GatewaySessions.foregroundConnection.isConnecting).toBe(true);
	});

	test('a readiness timeout during reconnect backoff leaves the socket to its own retries', async () => {
		GatewaySessions.startRestoredSession();
		await settleSocketOpen(1);
		const socket = GatewaySessions.foregroundConnection.socket;

		for (let elapsedMs = 0; elapsedMs < 6 * GATEWAY_FOREGROUND_READY_TIMEOUT_MS; elapsedMs += 5_000) {
			if (GatewaySessions.foregroundConnection.socket?.getState() === 'CONNECTING') {
				failForegroundTransport();
			}
			await vi.advanceTimersByTimeAsync(5_000);
		}

		expect(GatewaySessions.foregroundConnection.socket).toBe(socket);
		expect(vi.mocked(Initialization.setError)).not.toHaveBeenCalled();
		expect(liveSocketCount()).toBeLessThanOrEqual(1);
	});

	test('an outage longer than the recovery budget reconnects as soon as the gateway is back', async () => {
		GatewaySessions.startRestoredSession();
		await settleSocketOpen(1);

		for (let elapsedMs = 0; elapsedMs < 10 * GATEWAY_FOREGROUND_READY_TIMEOUT_MS; elapsedMs += 5_000) {
			if (GatewaySessions.foregroundConnection.socket?.getState() === 'CONNECTING') {
				failForegroundTransport();
			}
			await vi.advanceTimersByTimeAsync(5_000);
		}
		expect(vi.mocked(Initialization.setError)).not.toHaveBeenCalled();
		expect(GatewaySessions.foregroundConnection.socket).not.toBeNull();

		await vi.advanceTimersByTimeAsync(GATEWAY_FOREGROUND_READY_TIMEOUT_MS);
		GatewaySessions.foregroundConnection.handleConnectionOpen('session-after-outage');
		await vi.advanceTimersByTimeAsync(0);

		expect(GatewaySessions.foregroundConnection.isReady).toBe(true);
		expect(GatewaySessions.foregroundConnection.foregroundAccountKey).toBe('primary.test::100');
	});
});

describe('foreground recovery after the capped budget', () => {
	const EXHAUSTED_RECOVERY_MS = 1_000 + 3_000 + 10_000 + 1_000;

	async function exhaustForegroundRecovery(): Promise<void> {
		mocks.clientPropertiesFailure = true;
		GatewaySessions.startRestoredSession();
		await vi.advanceTimersByTimeAsync(EXHAUSTED_RECOVERY_MS);
		expect(vi.mocked(Initialization.setError)).toHaveBeenCalledTimes(1);
		expect(GatewaySessions.foregroundConnection.socket).toBeNull();
		expect(liveSocketCount()).toBe(0);
		mocks.clientPropertiesFailure = false;
	}

	beforeEach(() => {
		vi.mocked(Initialization.setError).mockClear();
	});

	test('the network coming back restarts the foreground session', async () => {
		await exhaustForegroundRecovery();
		const socketsBeforeOnline = FakeWebSocket.instances.length;

		window.dispatchEvent(new Event('online'));
		await settleSocketOpen(socketsBeforeOnline + 1);
		GatewaySessions.foregroundConnection.handleConnectionOpen('session-after-outage');
		await vi.advanceTimersByTimeAsync(0);

		expect(GatewaySessions.foregroundConnection.isReady).toBe(true);
		expect(liveSocketCount()).toBe(1);
	});

	test('a gateway outage with no online event is retried on a slow interval', async () => {
		await exhaustForegroundRecovery();
		const socketsBeforeRetry = FakeWebSocket.instances.length;

		await vi.advanceTimersByTimeAsync(60_000);

		expect(FakeWebSocket.instances).toHaveLength(socketsBeforeRetry + 1);
		expect(liveSocketCount()).toBe(1);
	});

	test('dispose removes the re-arm listeners', async () => {
		await exhaustForegroundRecovery();
		const socketsBeforeDispose = FakeWebSocket.instances.length;
		GatewaySessions.dispose();

		window.dispatchEvent(new Event('online'));
		await vi.advanceTimersByTimeAsync(120_000);

		expect(FakeWebSocket.instances).toHaveLength(socketsBeforeDispose);
	});
});
