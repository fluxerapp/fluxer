// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PiPContent} from '@app/features/ui/state/PiP';
import type {VoiceMediaGraphSnapshot} from '@app/features/voice/engine/VoiceMediaGraph';
import {type Room, Track} from 'livekit-client';
import {beforeEach, describe, expect, test, vi} from 'vitest';

const viewerStreamKeys = {current: [] as Array<string>};
const pipContent = {current: null as PiPContent | null};
const selfStream = {current: false};

const mediaEngine = {
	room: null as Room | null,
	participants: {} as Record<string, {identity: string; isScreenShareEnabled: boolean}>,
	connectionVoiceStates: {} as Record<string, {channel_id: string}>,
	channelId: null as string | null,
	guildId: null as string | null,
	connectionId: null as string | null,
	subscribe: vi.fn(),
};

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = vi.fn();
	},
}));

vi.mock('@app/features/platform/utils/DeferUntilModulesLoaded', () => ({
	deferUntilModulesLoaded: vi.fn(),
}));

vi.mock('@app/features/navigation/state/SelectedChannel', () => ({
	default: {currentChannelId: null},
}));

vi.mock('@app/features/ui/commands/PiPCommands', () => ({
	openPiP: vi.fn(),
	closePiP: vi.fn(),
}));

vi.mock('@app/features/ui/state/MobileLayout', () => ({
	default: {isMobileLayout: () => false},
}));

vi.mock('@app/features/ui/state/PiP', () => ({
	default: {
		getContent: () => pipContent.current,
		getSessionDisable: () => false,
	},
}));

vi.mock('@app/features/user/state/Users', () => ({
	default: {currentUser: {id: '1'}},
}));

vi.mock('@app/features/voice/engine/MediaEngineFacade', () => ({
	default: mediaEngine,
}));

vi.mock('@app/features/voice/engine/VoiceMediaGraphStore', () => ({
	voiceMediaGraphStore: {
		getGraphSnapshot: () =>
			({watchIntent: {viewerStreamKeys: viewerStreamKeys.current}}) as unknown as VoiceMediaGraphSnapshot,
		subscribe: vi.fn(),
	},
}));

vi.mock('@app/features/voice/state/LocalVoiceState', () => ({
	default: {
		getSelfStream: () => selfStream.current,
		subscribe: vi.fn(),
	},
}));

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {disablePictureInPicturePopoutScreenShare: false},
}));

const {detectScreenSharePiPContent} = await import('@app/features/voice/state/ScreenSharePiPController');

function makeParticipant(identity: string, sharing: boolean) {
	const publications = new Map<string, {source: Track.Source; isMuted: boolean; trackSid: string}>();
	publications.set('camera', {source: Track.Source.Camera, isMuted: false, trackSid: `${identity}-camera`});
	if (sharing) {
		publications.set('screen', {source: Track.Source.ScreenShare, isMuted: false, trackSid: `${identity}-screen`});
	}
	return {identity, trackPublications: publications};
}

function setRoom(remoteIdentities: Array<string>) {
	mediaEngine.room = {
		localParticipant: makeParticipant('user_1_connection-self', false),
		remoteParticipants: new Map(remoteIdentities.map((identity) => [identity, makeParticipant(identity, true)])),
	} as unknown as Room;
}

beforeEach(() => {
	viewerStreamKeys.current = [];
	pipContent.current = null;
	selfStream.current = false;
	mediaEngine.room = null;
	mediaEngine.participants = {};
	mediaEngine.connectionVoiceStates = {};
	mediaEngine.channelId = 'channel-1';
	mediaEngine.guildId = 'guild-1';
	mediaEngine.connectionId = 'connection-self';
});

describe('detectScreenSharePiPContent', () => {
	test('ignores a remote screen share the user is not watching', () => {
		setRoom(['user_2_connection-2']);
		expect(detectScreenSharePiPContent('channel-1', 'guild-1')).toBeNull();
	});

	test('returns a remote screen share the user is watching', () => {
		setRoom(['user_2_connection-2']);
		viewerStreamKeys.current = ['guild-1:channel-1:connection-2'];
		expect(detectScreenSharePiPContent('channel-1', 'guild-1')).toEqual({
			participantIdentity: 'user_2_connection-2',
			userId: '2',
			connectionId: 'connection-2',
			source: 'livekit',
		});
	});

	test('returns the watched share when several participants are sharing', () => {
		setRoom(['user_2_connection-2', 'user_3_connection-3']);
		viewerStreamKeys.current = ['guild-1:channel-1:connection-3'];
		expect(detectScreenSharePiPContent('channel-1', 'guild-1')).toEqual({
			participantIdentity: 'user_3_connection-3',
			userId: '3',
			connectionId: 'connection-3',
			source: 'livekit',
		});
	});

	test('ignores an unwatched participant snapshot screen share', () => {
		mediaEngine.participants = {
			'user_2_connection-2': {identity: 'user_2_connection-2', isScreenShareEnabled: true},
		};
		mediaEngine.connectionVoiceStates = {'connection-2': {channel_id: 'channel-1'}};
		expect(detectScreenSharePiPContent('channel-1', 'guild-1')).toBeNull();
	});

	test('returns a watched participant snapshot screen share', () => {
		mediaEngine.participants = {
			'user_2_connection-2': {identity: 'user_2_connection-2', isScreenShareEnabled: true},
		};
		mediaEngine.connectionVoiceStates = {'connection-2': {channel_id: 'channel-1'}};
		viewerStreamKeys.current = ['guild-1:channel-1:connection-2'];
		expect(detectScreenSharePiPContent('channel-1', 'guild-1')).toEqual({
			participantIdentity: 'user_2_connection-2',
			userId: '2',
			connectionId: 'connection-2',
			source: 'participant-snapshot',
		});
	});

	test('ignores an unwatched remote screen share in a DM call', () => {
		setRoom(['user_2_connection-2']);
		mediaEngine.guildId = null;
		expect(detectScreenSharePiPContent('channel-1', null)).toBeNull();
	});

	test('returns a watched remote screen share in a DM call', () => {
		setRoom(['user_2_connection-2']);
		mediaEngine.guildId = null;
		viewerStreamKeys.current = ['dm:channel-1:connection-2'];
		expect(detectScreenSharePiPContent('channel-1', null)).toEqual({
			participantIdentity: 'user_2_connection-2',
			userId: '2',
			connectionId: 'connection-2',
			source: 'livekit',
		});
	});

	test('keeps returning the local share while watch intent is empty', () => {
		selfStream.current = true;
		setRoom([]);
		expect(detectScreenSharePiPContent('channel-1', 'guild-1')).toEqual({
			participantIdentity: 'user_1_connection-self',
			userId: '1',
			connectionId: 'connection-self',
			source: 'local-self-state',
		});
	});

	test('keeps an open PiP alive for a watched stream that stopped publishing', () => {
		pipContent.current = {
			type: 'stream',
			participantIdentity: 'user_2_connection-2',
			channelId: 'channel-1',
			guildId: 'guild-1',
			connectionId: 'connection-2',
			userId: '2',
		};
		viewerStreamKeys.current = ['guild-1:channel-1:connection-2'];
		expect(detectScreenSharePiPContent('channel-1', 'guild-1')).toEqual({
			participantIdentity: 'user_2_connection-2',
			userId: '2',
			connectionId: 'connection-2',
			source: 'watched-stream',
		});
	});
});
