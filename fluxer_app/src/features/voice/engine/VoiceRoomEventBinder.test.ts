// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {installVoiceMenuTestBootstrap} from '@app/features/ui/action_menu/items/__fixtures__/VoiceMenuTestBootstrap';
import type {RoomEventCallbacks, RoomEventDependencies} from '@app/features/voice/engine/VoiceRoomEventBinder';
import {ConnectionState, type Room, RoomEvent} from 'livekit-client';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const mocks = vi.hoisted(() => ({
	bindCodecNegotiation: vi.fn(() => vi.fn()),
	disposeCodecNegotiation: vi.fn(),
	bindScreenShareMigration: vi.fn(() => vi.fn()),
	disposeScreenShareMigration: vi.fn(),
}));

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? '', locale: 'en'}}),
}));

vi.mock('@app/features/voice/engine/ScreenShareCodecNegotiation', () => ({
	default: {
		bind: mocks.bindCodecNegotiation,
		dispose: mocks.disposeCodecNegotiation,
		getSelectedCodec: vi.fn(() => null),
		publishLocalCapabilities: vi.fn(async () => null),
		selectNativeScreenShareCodec: vi.fn(() => null),
		selectScreenShareCodec: vi.fn(() => null),
		setSelectionChangeListener: vi.fn(),
	},
}));

vi.mock('@app/features/voice/engine/ScreenSharePublicationMigration', () => ({
	default: {
		bind: mocks.bindScreenShareMigration,
		dispose: mocks.disposeScreenShareMigration,
		getManagedScreenSharePublications: vi.fn(() => []),
		getScreenSharePublicationsToDisable: vi.fn(() => []),
		isScreenShareBuffering: vi.fn(() => false),
		selectScreenSharePublication: vi.fn(() => null),
		subscribe: vi.fn(() => vi.fn()),
		version: 0,
	},
}));

installVoiceMenuTestBootstrap();

const {bindRoomEvents} = await import('@app/features/voice/engine/VoiceRoomEventBinder');

class FakeRoom {
	private handlers = new Map<string, Set<(...args: Array<unknown>) => unknown>>();
	localParticipant = {identity: 'local', on: vi.fn(), off: vi.fn()};
	remoteParticipants = new Map<string, {identity: string}>();

	constructor(public state: ConnectionState) {}

	on(event: string, handler: (...args: Array<unknown>) => unknown): this {
		if (!this.handlers.has(event)) this.handlers.set(event, new Set());
		this.handlers.get(event)?.add(handler);
		return this;
	}

	off(event: string, handler: (...args: Array<unknown>) => unknown): this {
		this.handlers.get(event)?.delete(handler);
		return this;
	}

	async emit(event: string, ...args: Array<unknown>): Promise<void> {
		for (const handler of this.handlers.get(event) ?? []) {
			await handler(...args);
		}
	}
}

function createCallbacks(): RoomEventCallbacks {
	return {
		onConnected: vi.fn(async () => undefined),
		onDisconnected: vi.fn(),
		onReconnecting: vi.fn(),
		onReconnected: vi.fn(),
	};
}

function createDependencies(options: {attemptIsStale?: boolean} = {}): RoomEventDependencies {
	return {
		connection: {
			createGuardedHandler: (_attemptId, handler) => {
				return (...args) => {
					if (options.attemptIsStale) return;
					void handler(...args);
				};
			},
			isDisconnecting: () => false,
			isUserMovePending: () => true,
			markConnected: vi.fn(),
			markDisconnected: vi.fn(),
			markReconnecting: vi.fn(),
			markReconnected: vi.fn(),
		},
		media: {
			applyAllLocalAudioPreferences: vi.fn(),
			ensureMicrophone: vi.fn(async () => undefined),
			playEntranceSound: vi.fn(async () => undefined),
			resetStreamTracking: vi.fn(),
		},
		mediaState: {
			handleLocalTrackStateChange: vi.fn(() => false),
			resetLocalMediaState: vi.fn(),
		},
		participants: {
			clear: vi.fn(),
			hydrateFromRoom: vi.fn(),
			removeParticipant: vi.fn(),
			updateActiveSpeakers: vi.fn(),
			upsertParticipant: vi.fn(),
		},
		permissions: {
			applyDeafen: vi.fn(),
			syncWithPermissionState: vi.fn(),
		},
		remoteSpeaking: {
			attachIfApplicable: vi.fn(),
			clear: vi.fn(),
			detachByIdentity: vi.fn(),
			detachIfTrackMatches: vi.fn(),
			hydrateFromRoom: vi.fn(),
		},
		screenShare: {
			cleanupLingeringScreenShareTracks: vi.fn(async () => undefined),
			handleLocalScreenShareTrackUnpublished: vi.fn(),
			isScreenSharePublicationReplaceInFlight: () => false,
		},
		subscriptions: {
			isScreenShareSubscribed: () => false,
			reattachScreenShareAfterPublish: vi.fn(),
			reconcileSubscriptions: vi.fn(),
		},
	};
}

function bindFakeRoom(room: FakeRoom, options: {attemptIsStale?: boolean} = {}): void {
	bindRoomEvents(room as unknown as Room, 1, 'guild-1', 'channel-1', createCallbacks(), createDependencies(options));
}

describe('binding room events for screen share negotiation', () => {
	beforeEach(() => {
		vi.clearAllMocks();
	});

	it('binds codec negotiation and publication migration to a room that is already connected', () => {
		bindFakeRoom(new FakeRoom(ConnectionState.Connected));
		expect(mocks.bindCodecNegotiation).toHaveBeenCalledTimes(1);
		expect(mocks.bindScreenShareMigration).toHaveBeenCalledTimes(1);
	});

	it('binds neither until a room that is not connected yet reports it has connected', async () => {
		const room = new FakeRoom(ConnectionState.Disconnected);
		bindFakeRoom(room);
		expect(mocks.bindCodecNegotiation).not.toHaveBeenCalled();
		expect(mocks.bindScreenShareMigration).not.toHaveBeenCalled();
		room.state = ConnectionState.Connected;
		await room.emit(RoomEvent.Connected);
		expect(mocks.bindCodecNegotiation).toHaveBeenCalledTimes(1);
		expect(mocks.bindScreenShareMigration).toHaveBeenCalledTimes(1);
	});

	it('binds once however often an already connected room reports it has connected', async () => {
		const room = new FakeRoom(ConnectionState.Connected);
		bindFakeRoom(room);
		await room.emit(RoomEvent.Connected);
		await room.emit(RoomEvent.Connected);
		expect(mocks.bindCodecNegotiation).toHaveBeenCalledTimes(1);
		expect(mocks.bindScreenShareMigration).toHaveBeenCalledTimes(1);
	});

	it('binds again after an already connected room disconnects and connects once more', async () => {
		const room = new FakeRoom(ConnectionState.Connected);
		bindFakeRoom(room);
		await room.emit(RoomEvent.Disconnected);
		expect(mocks.disposeCodecNegotiation).toHaveBeenCalledTimes(1);
		expect(mocks.disposeScreenShareMigration).toHaveBeenCalledTimes(1);
		await room.emit(RoomEvent.Connected);
		expect(mocks.bindCodecNegotiation).toHaveBeenCalledTimes(2);
		expect(mocks.bindScreenShareMigration).toHaveBeenCalledTimes(2);
	});

	it('binds nothing to an already connected room whose connection attempt is stale', () => {
		bindFakeRoom(new FakeRoom(ConnectionState.Connected), {attemptIsStale: true});
		expect(mocks.bindCodecNegotiation).not.toHaveBeenCalled();
		expect(mocks.bindScreenShareMigration).not.toHaveBeenCalled();
	});
});
