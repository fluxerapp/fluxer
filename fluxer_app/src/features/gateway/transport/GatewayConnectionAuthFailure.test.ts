// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import GatewaySessions from '@app/features/gateway/transport/GatewaySessionPool';
import type {GatewaySocket, GatewaySocketProperties} from '@app/features/gateway/transport/GatewaySocket';
import {resetClientState} from '@app/features/platform/state/ResetClientState';
import LayerManager from '@app/features/ui/state/LayerManager';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const mocks = vi.hoisted(() => ({sequence: [] as Array<string>}));

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

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? '', locale: 'en'}}),
}));

vi.mock('@app/features/channel/state/Channels', () => ({default: {getChannel: () => undefined}}));

vi.mock('@app/features/navigation/state/Navigation', () => ({default: {channelId: null}}));

vi.mock('@app/features/messaging/state/AttachmentUrlRefresher', () => ({default: {reset: vi.fn()}}));

vi.mock('@app/features/app/state/GeoIP', () => ({
	default: {latitude: null, longitude: null, applyConnectionFallbackGeo: vi.fn()},
}));

vi.mock('@app/features/app/state/RuntimeConfig', () => ({
	default: {gatewayEndpoint: 'wss://gateway.test'},
}));

vi.mock('@app/features/gateway/transport/ForegroundGatewayHandoff', () => ({
	ForegroundGatewayHandoff: class ForegroundGatewayHandoff {},
}));

vi.mock('@app/features/gateway/transport/BackgroundAccountPresence', () => ({
	default: {appearOffline: true},
}));

vi.mock('@app/features/gateway/events/EventRouter', () => ({createHandlerRegistry: () => new Map()}));

vi.mock('@app/features/platform/utils/DeferUntilModulesLoaded', () => ({deferUntilModulesLoaded: vi.fn()}));

vi.mock('@app/features/platform/state/ResetClientState', () => ({
	ResetClientStateReason: {GATEWAY_AUTH_FAILURE: 'gateway-auth-failure'},
	resetClientState: vi.fn(() => {
		mocks.sequence.push('resetClientState');
		return Promise.resolve();
	}),
}));

vi.mock('@app/features/ui/state/LayerManager', () => ({
	default: {
		closeAll: vi.fn(() => {
			mocks.sequence.push('closeAllLayers');
		}),
	},
}));

vi.mock('@app/features/presence/state/Presence', () => ({
	default: {
		handleSessionInvalidated: vi.fn(() => {
			mocks.sequence.push('logout');
		}),
	},
}));

vi.mock('@app/features/auth/state/Authentication', () => ({
	default: {
		handleConnectionClosed: vi.fn(async () => {
			mocks.sequence.push('handleConnectionClosed');
		}),
	},
}));

vi.mock('@app/features/app/state/Initialization', () => ({
	default: {setConnecting: vi.fn(), setReady: vi.fn(), reset: vi.fn()},
}));

vi.mock('@app/features/app/state/RuntimeCrash', () => ({default: {triggerFatalCrash: vi.fn()}}));

vi.mock('@app/features/expressions/state/FavoriteMemes', () => ({default: {reset: vi.fn()}}));

vi.mock('@app/features/guild/state/GuildMatureContentAgree', () => ({default: {reset: vi.fn()}}));

vi.mock('@app/features/member/state/MemberSearch', () => ({default: {handleLogout: vi.fn()}}));

vi.mock('@app/features/messaging/state/MessagingMessages', () => ({
	default: {handleSessionInvalidated: vi.fn(), handleConnectionClosed: vi.fn()},
}));

vi.mock('@app/features/navigation/state/SelectedGuild', () => ({
	default: {selectedGuildId: null, selectionNonce: 0},
}));

vi.mock('@app/features/permissions/state/Permission', () => ({default: {}}));

vi.mock('@app/features/presence/state/LocalPresence', () => ({
	default: {
		presenceKey: '',
		mobile: false,
		updatePresence: vi.fn(),
		getGatewayPresence: () => null,
		handleSessionChanging: vi.fn(),
	},
}));

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

const ACCOUNT_KEY = 'instance::100';

interface ConnectionInternals {
	createGatewaySocket: (
		gatewayUrl: string,
		token: string,
		properties: GatewaySocketProperties,
		generation: number,
		accountKey: string | null,
	) => GatewaySocket;
	socketOwner: {socket: GatewaySocket; accountKey: string | null} | null;
	generation: number;
}

function adoptSocket(): GatewaySocket {
	const connection = GatewaySessions.foregroundConnection as unknown as ConnectionInternals;
	const socket = connection.createGatewaySocket(
		'wss://gateway.test',
		'token-100',
		PROPERTIES,
		connection.generation,
		ACCOUNT_KEY,
	);
	connection.socketOwner = {socket, accountKey: ACCOUNT_KEY};
	return socket;
}

function closeSocket(socket: GatewaySocket, code: number): void {
	(
		socket as unknown as {handleSocketClose: (event: {code: number; reason: string; wasClean: boolean}) => void}
	).handleSocketClose({code, reason: 'closed', wasClean: false});
	vi.advanceTimersByTime(0);
}

beforeEach(() => {
	vi.useFakeTimers();
	mocks.sequence.length = 0;
	vi.mocked(resetClientState).mockClear();
	vi.mocked(LayerManager.closeAll).mockClear();
	vi.mocked(Authentication.handleConnectionClosed).mockClear();
});

afterEach(() => {
	vi.useRealTimers();
});

async function flushMicrotasks(): Promise<void> {
	for (let turn = 0; turn < 8; turn += 1) {
		await Promise.resolve();
	}
}

describe('gateway authentication failure', () => {
	test('a 4004 close runs the four-step logout in order, from the connection', async () => {
		const socket = adoptSocket();

		closeSocket(socket, 4004);
		await flushMicrotasks();

		expect(mocks.sequence).toEqual(['resetClientState', 'closeAllLayers', 'logout', 'handleConnectionClosed']);
		expect(resetClientState).toHaveBeenCalledWith({reason: 'gateway-auth-failure', keepDrafts: true});
		expect(Authentication.handleConnectionClosed).toHaveBeenCalledWith({code: 4004, accountKey: ACCOUNT_KEY});
		expect(GatewayConnection.socket).toBeNull();
		expect(GatewayConnection.isReady).toBe(false);
		expect(GatewayConnection.sessionId).toBeNull();
	});

	test('any other close code leaves the account signed in', () => {
		const socket = adoptSocket();

		closeSocket(socket, 1006);

		expect(mocks.sequence).toEqual([]);
		expect(resetClientState).not.toHaveBeenCalled();
		expect(LayerManager.closeAll).not.toHaveBeenCalled();
		expect(Authentication.handleConnectionClosed).not.toHaveBeenCalled();
	});

	test('a retired socket never logs the current account out', () => {
		const socket = adoptSocket();
		(GatewaySessions.foregroundConnection as unknown as ConnectionInternals).socketOwner = null;

		closeSocket(socket, 4004);

		expect(mocks.sequence).toEqual([]);
		expect(resetClientState).not.toHaveBeenCalled();
	});
});
