// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceChannelPermissions} from '@app/features/voice/utils/VoicePermissionUtils';
import {describe, expect, it} from 'vitest';
import {
	createVoicePermissionSnapshot,
	getVoicePermissionWatchStateValue,
	transitionVoicePermissionSnapshot,
	type VoicePermissionCommand,
	type VoicePermissionEvent,
	type VoicePermissionSnapshot,
} from './VoicePermissionStateMachine';

const DEFAULT_PERMISSIONS: VoiceChannelPermissions = {
	canSpeak: true,
	canStream: true,
	canUseVideo: true,
	canConnect: true,
	canPrioritySpeaker: false,
	canUseVad: true,
};

function permissions(overrides: Partial<VoiceChannelPermissions> = {}): VoiceChannelPermissions {
	return {...DEFAULT_PERMISSIONS, ...overrides};
}

function transition(snapshot: VoicePermissionSnapshot, event: VoicePermissionEvent): VoicePermissionSnapshot {
	return transitionVoicePermissionSnapshot(snapshot, event);
}

function clearCommands(snapshot: VoicePermissionSnapshot): VoicePermissionSnapshot {
	return transition(snapshot, {type: 'permission.clearCommands'});
}

function commands(snapshot: VoicePermissionSnapshot): ReadonlyArray<VoicePermissionCommand> {
	return snapshot.context.commands;
}

describe('VoicePermissionStateMachine', () => {
	it('keeps unchanged permissions as a no-op', () => {
		let snapshot = createVoicePermissionSnapshot();
		snapshot = transition(snapshot, {type: 'permission.update', permissions: permissions(), roomPresent: true});

		expect(snapshot.context.permissions).toEqual(permissions());
		expect(snapshot.context.roomPresent).toBe(true);
		expect(commands(snapshot)).toEqual([]);
	});

	it('emits mute sync and local audio revoke decisions when speak is revoked', () => {
		let snapshot = createVoicePermissionSnapshot();
		snapshot = transition(snapshot, {
			type: 'permission.update',
			permissions: permissions({canSpeak: false}),
			roomPresent: true,
		});

		expect(commands(snapshot)).toEqual([
			{type: 'syncSpeakPermission', canSpeak: false, selfMute: true},
			{type: 'revokeLocalAudio'},
		]);
		expect(snapshot.context.permissions.canSpeak).toBe(false);
	});

	it('emits a screen-share stop decision when stream permission is revoked', () => {
		let snapshot = createVoicePermissionSnapshot();
		snapshot = transition(snapshot, {
			type: 'permission.update',
			permissions: permissions({canStream: false}),
			roomPresent: true,
		});

		expect(commands(snapshot)).toEqual([{type: 'stopScreenShare'}]);
		expect(snapshot.context.permissions.canStream).toBe(false);
	});

	it('emits a camera-off decision when video permission is revoked', () => {
		let snapshot = createVoicePermissionSnapshot();
		snapshot = transition(snapshot, {
			type: 'permission.update',
			permissions: permissions({canUseVideo: false}),
			roomPresent: true,
		});

		expect(commands(snapshot)).toEqual([{type: 'disableCamera'}]);
		expect(snapshot.context.permissions.canUseVideo).toBe(false);
	});

	it('turns remote microphone subscriptions off and back on for deafen toggles', () => {
		let snapshot = createVoicePermissionSnapshot();
		const microphonePublications = [
			{publicationId: 'mic-a', isDesired: true},
			{publicationId: 'mic-b', isDesired: false},
		];

		snapshot = transition(snapshot, {
			type: 'deafen.apply',
			deafened: true,
			microphonePublications,
		});
		expect(snapshot.context.deafened).toBe(true);
		expect(commands(snapshot)).toEqual([
			{type: 'setRemoteMicrophoneEnabled', publicationId: 'mic-a', enabled: false},
			{type: 'setRemoteMicrophoneSubscribed', publicationId: 'mic-a', subscribed: false},
			{type: 'setRemoteMicrophoneSubscribed', publicationId: 'mic-b', subscribed: false},
		]);

		snapshot = clearCommands(snapshot);
		snapshot = transition(snapshot, {
			type: 'deafen.apply',
			deafened: false,
			microphonePublications,
		});
		expect(snapshot.context.deafened).toBe(false);
		expect(commands(snapshot)).toEqual([
			{type: 'setRemoteMicrophoneSubscribed', publicationId: 'mic-a', subscribed: true},
			{type: 'setRemoteMicrophoneEnabled', publicationId: 'mic-a', enabled: true},
			{type: 'setRemoteMicrophoneSubscribed', publicationId: 'mic-b', subscribed: true},
			{type: 'setRemoteMicrophoneEnabled', publicationId: 'mic-b', enabled: true},
		]);
	});

	it('initializes remote subscription policy from the current deafened state', () => {
		let snapshot = createVoicePermissionSnapshot();
		snapshot = transition(snapshot, {
			type: 'deafen.apply',
			deafened: true,
			microphonePublications: [],
		});
		snapshot = clearCommands(snapshot);
		snapshot = transition(snapshot, {
			type: 'subscription.initialize',
			microphonePublications: [{publicationId: 'mic-a', isDesired: true}],
			videoPublicationIds: ['video-a'],
		});

		expect(commands(snapshot)).toEqual([
			{type: 'setRemoteMicrophoneSubscribed', publicationId: 'mic-a', subscribed: false},
			{type: 'setRemoteVideoSubscribed', publicationId: 'video-a', subscribed: false},
		]);
	});

	it('updates permissions without room-bound revoke decisions when no room is present', () => {
		let snapshot = createVoicePermissionSnapshot();
		snapshot = transition(snapshot, {
			type: 'permission.update',
			permissions: permissions({canSpeak: false, canStream: false, canUseVideo: false}),
			roomPresent: false,
		});

		expect(snapshot.context.permissions).toEqual(permissions({canSpeak: false, canStream: false, canUseVideo: false}));
		expect(snapshot.context.roomPresent).toBe(false);
		expect(commands(snapshot)).toEqual([{type: 'syncSpeakPermission', canSpeak: false, selfMute: true}]);
	});

	it('tracks watcher activation and emits cleanup on reset', () => {
		let snapshot = createVoicePermissionSnapshot();
		snapshot = transition(snapshot, {
			type: 'permission.watch.start',
			guildId: 'guild-a',
			channelId: 'channel-a',
			roomPresent: true,
		});
		expect(getVoicePermissionWatchStateValue(snapshot)).toBe('watching');
		expect(snapshot.context).toMatchObject({
			guildId: 'guild-a',
			channelId: 'channel-a',
			roomPresent: true,
			watcherActive: true,
		});

		snapshot = transition(snapshot, {type: 'permission.reset'});
		expect(getVoicePermissionWatchStateValue(snapshot)).toBe('inactive');
		expect(snapshot.context.permissions).toEqual(DEFAULT_PERMISSIONS);
		expect(snapshot.context.deafened).toBe(false);
		expect(snapshot.context.roomPresent).toBe(false);
		expect(commands(snapshot)).toEqual([{type: 'stopPermissionWatch'}]);
	});

	it('handles repeated grant and revoke cycles idempotently', () => {
		let snapshot = createVoicePermissionSnapshot();
		snapshot = transition(snapshot, {
			type: 'permission.change',
			permission: 'speak',
			allowed: false,
			roomPresent: true,
		});
		expect(commands(snapshot)).toEqual([
			{type: 'syncSpeakPermission', canSpeak: false, selfMute: true},
			{type: 'revokeLocalAudio'},
		]);

		snapshot = clearCommands(snapshot);
		snapshot = transition(snapshot, {
			type: 'permission.change',
			permission: 'speak',
			allowed: true,
			roomPresent: true,
		});
		expect(commands(snapshot)).toEqual([{type: 'syncSpeakPermission', canSpeak: true, selfMute: false}]);

		snapshot = clearCommands(snapshot);
		snapshot = transition(snapshot, {
			type: 'permission.change',
			permission: 'speak',
			allowed: false,
			roomPresent: true,
		});
		expect(commands(snapshot)).toEqual([
			{type: 'syncSpeakPermission', canSpeak: false, selfMute: true},
			{type: 'revokeLocalAudio'},
		]);

		snapshot = clearCommands(snapshot);
		snapshot = transition(snapshot, {
			type: 'permission.change',
			permission: 'speak',
			allowed: false,
			roomPresent: true,
		});
		expect(commands(snapshot)).toEqual([]);
	});
});
