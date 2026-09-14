// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createLocalVoiceStateSnapshot,
	getActiveLocalVoiceState,
	hasLocalVoiceConnectionState,
	type LocalVoiceStateEvent,
	type LocalVoiceStateSnapshot,
	transitionLocalVoiceStateSnapshot,
} from '@app/features/voice/state/LocalVoiceStateMachine';
import {describe, expect, it} from 'vitest';

const CONNECTION_A = 'connection-a';
const CONNECTION_B = 'connection-b';
const STREAM_A = 'guild-a:channel-a:connection-a';
const STREAM_B = 'guild-a:channel-a:connection-b';
const STREAM_C = 'guild-a:channel-a:connection-c';

function send(snapshot: LocalVoiceStateSnapshot, event: LocalVoiceStateEvent): LocalVoiceStateSnapshot {
	return transitionLocalVoiceStateSnapshot(snapshot, event);
}

function active(snapshot: LocalVoiceStateSnapshot, connectionId: string | null = null) {
	return getActiveLocalVoiceState(snapshot, connectionId);
}

describe('LocalVoiceStateMachine', () => {
	it('initializes fallback state from persisted defaults and canonicalizes viewer keys', () => {
		const snapshot = createLocalVoiceStateSnapshot({
			microphonePermissionGranted: true,
			persistedDefaults: {
				selfMute: true,
				selfDeaf: true,
				hasUserSetMute: true,
				hasUserSetDeaf: true,
			},
			fallback: {
				selfVideo: true,
				selfStream: true,
				viewerStreamKeys: [STREAM_A, '', STREAM_A, STREAM_B],
			},
		});
		expect(active(snapshot)).toMatchObject({
			selfMute: true,
			selfDeaf: true,
			selfVideo: true,
			selfStream: true,
			viewerStreamKeys: [STREAM_A, STREAM_B],
			hasUserSetMute: true,
			hasUserSetDeaf: true,
		});
	});

	it('applies persisted defaults to fallback only when no connection is active', () => {
		let snapshot = createLocalVoiceStateSnapshot({
			microphonePermissionGranted: true,
			persistedDefaults: {selfMute: false},
		});
		snapshot = send(snapshot, {
			type: 'connection.seed',
			connectionId: CONNECTION_A,
			seed: {selfMute: true, selfDeaf: true},
		});
		snapshot = send(snapshot, {
			type: 'defaults.apply',
			activeConnectionId: CONNECTION_A,
			persistedDefaults: {selfMute: false, selfDeaf: false},
		});
		expect(active(snapshot, CONNECTION_A)).toMatchObject({selfMute: true, selfDeaf: true});
		snapshot = send(snapshot, {
			type: 'defaults.apply',
			activeConnectionId: null,
			persistedDefaults: {selfMute: false, selfDeaf: false},
		});
		expect(active(snapshot)).toMatchObject({selfMute: false, selfDeaf: false});
	});

	it('defaults to unmuted on initial permission grant when the user never set mute', () => {
		let snapshot = createLocalVoiceStateSnapshot({
			microphonePermissionGranted: true,
			persistedDefaults: {selfMute: true, hasUserSetMute: false},
		});
		snapshot = send(snapshot, {
			type: 'permission.sync',
			activeConnectionId: null,
			microphoneGranted: true,
			defaultMuteInitialized: false,
		});
		expect(active(snapshot)).toMatchObject({selfMute: false, mutedByPermission: false, hasUserSetMute: false});
		expect(snapshot.context.persistedDefaults.selfMute).toBe(false);
	});

	it('permission denial mutes transiently and grant restores persisted mute preference', () => {
		let snapshot = createLocalVoiceStateSnapshot({
			microphonePermissionGranted: true,
			persistedDefaults: {selfMute: false, hasUserSetMute: true},
		});
		snapshot = send(snapshot, {type: 'permission.deny', activeConnectionId: null});
		expect(active(snapshot)).toMatchObject({selfMute: true, mutedByPermission: true});
		expect(snapshot.context.persistedDefaults).toMatchObject({selfMute: false, hasUserSetMute: true});
		snapshot = send(snapshot, {type: 'permission.grant', activeConnectionId: null});
		expect(active(snapshot)).toMatchObject({selfMute: false, mutedByPermission: false, hasUserSetMute: true});
	});

	it('unmutes on first permission grant when the user never set mute', () => {
		let snapshot = createLocalVoiceStateSnapshot({
			microphonePermissionGranted: false,
			persistedDefaults: {selfMute: true, hasUserSetMute: false},
			fallback: {selfMute: true, mutedByPermission: true},
		});
		snapshot = send(snapshot, {type: 'permission.grant', activeConnectionId: null});
		expect(active(snapshot)).toMatchObject({selfMute: false, mutedByPermission: false, hasUserSetMute: false});
		expect(snapshot.context.persistedDefaults).toMatchObject({selfMute: false, hasUserSetMute: false});
	});

	it('keeps user-requested unmute muted while permission is denied', () => {
		let snapshot = createLocalVoiceStateSnapshot({
			microphonePermissionGranted: false,
			persistedDefaults: {selfMute: false},
			fallback: {selfMute: true, mutedByPermission: true},
		});
		snapshot = send(snapshot, {type: 'mute.toggle', activeConnectionId: null});
		expect(active(snapshot)).toMatchObject({selfMute: true, mutedByPermission: true, hasUserSetMute: true});
		snapshot = send(snapshot, {type: 'mute.update', activeConnectionId: null, muted: false});
		expect(active(snapshot)).toMatchObject({selfMute: true, mutedByPermission: true});
	});

	it('unmuting while deafened also undeafens, but denied permission keeps mute enabled', () => {
		let snapshot = createLocalVoiceStateSnapshot({
			microphonePermissionGranted: true,
			fallback: {selfMute: true, selfDeaf: true, shouldUnmuteOnUndeafen: true},
		});
		snapshot = send(snapshot, {type: 'mute.toggle', activeConnectionId: null});
		expect(active(snapshot)).toMatchObject({
			selfMute: false,
			selfDeaf: false,
			hasUserSetMute: true,
			hasUserSetDeaf: true,
			shouldUnmuteOnUndeafen: false,
		});

		snapshot = createLocalVoiceStateSnapshot({
			microphonePermissionGranted: false,
			fallback: {selfMute: true, selfDeaf: true, mutedByPermission: true, shouldUnmuteOnUndeafen: true},
		});
		snapshot = send(snapshot, {type: 'mute.toggle', activeConnectionId: null});
		expect(active(snapshot)).toMatchObject({
			selfMute: true,
			selfDeaf: false,
			mutedByPermission: true,
			hasUserSetMute: true,
			hasUserSetDeaf: true,
		});
	});

	it('tracks shouldUnmuteOnUndeafen only when deafening from an unmuted state', () => {
		let snapshot = createLocalVoiceStateSnapshot({microphonePermissionGranted: true});
		snapshot = send(snapshot, {type: 'deaf.toggle', activeConnectionId: null});
		expect(active(snapshot)).toMatchObject({
			selfMute: true,
			selfDeaf: true,
			hasUserSetDeaf: true,
			shouldUnmuteOnUndeafen: true,
		});
		snapshot = send(snapshot, {type: 'deaf.toggle', activeConnectionId: null});
		expect(active(snapshot)).toMatchObject({selfMute: false, selfDeaf: false, shouldUnmuteOnUndeafen: false});

		snapshot = send(snapshot, {type: 'mute.toggle', activeConnectionId: null});
		snapshot = send(snapshot, {type: 'deaf.toggle', activeConnectionId: null});
		expect(active(snapshot)).toMatchObject({selfMute: true, selfDeaf: true, shouldUnmuteOnUndeafen: false});
		snapshot = send(snapshot, {type: 'deaf.toggle', activeConnectionId: null});
		expect(active(snapshot)).toMatchObject({selfMute: true, selfDeaf: false, shouldUnmuteOnUndeafen: false});
	});

	it('keeps mute and deaf persisted defaults synchronized with audio changes', () => {
		let snapshot = createLocalVoiceStateSnapshot({microphonePermissionGranted: true});
		snapshot = send(snapshot, {type: 'mute.toggle', activeConnectionId: null});
		expect(snapshot.context.persistedDefaults).toMatchObject({selfMute: true, hasUserSetMute: true});
		snapshot = send(snapshot, {type: 'mute.clearUserSet', activeConnectionId: null});
		expect(snapshot.context.persistedDefaults).toMatchObject({selfMute: true, hasUserSetMute: false});
		snapshot = send(snapshot, {type: 'deaf.update', activeConnectionId: null, deafened: true});
		expect(snapshot.context.persistedDefaults).toMatchObject({selfMute: true, selfDeaf: true});
	});

	it('toggles and updates local video and screen share state', () => {
		let snapshot = createLocalVoiceStateSnapshot({microphonePermissionGranted: true});
		snapshot = send(snapshot, {type: 'video.toggle', activeConnectionId: null});
		snapshot = send(snapshot, {type: 'stream.toggle', activeConnectionId: null});
		expect(active(snapshot)).toMatchObject({selfVideo: true, selfStream: true});
		snapshot = send(snapshot, {type: 'video.update', activeConnectionId: null, video: false});
		snapshot = send(snapshot, {type: 'stream.update', activeConnectionId: null, streaming: false});
		expect(active(snapshot)).toMatchObject({selfVideo: false, selfStream: false});
		expect(snapshot.context.persistedDefaults).toMatchObject({selfMute: false, selfDeaf: false});
	});

	it('canonicalizes viewer stream keys for replace, seed, and server sync', () => {
		let snapshot = createLocalVoiceStateSnapshot({microphonePermissionGranted: true});
		snapshot = send(snapshot, {
			type: 'viewer.replace',
			activeConnectionId: null,
			keys: [STREAM_A, '', STREAM_A, STREAM_B, STREAM_B, STREAM_C],
		});
		expect(active(snapshot).viewerStreamKeys).toEqual([STREAM_A, STREAM_B, STREAM_C]);

		snapshot = send(snapshot, {
			type: 'connection.seed',
			connectionId: CONNECTION_A,
			seed: {viewerStreamKeys: [STREAM_C, STREAM_C, STREAM_A]},
		});
		expect(active(snapshot, CONNECTION_A).viewerStreamKeys).toEqual([STREAM_C, STREAM_A]);

		snapshot = send(snapshot, {
			type: 'connection.sync',
			connectionId: CONNECTION_A,
			seed: {viewerStreamKeys: [STREAM_B, '', STREAM_B, STREAM_A]},
		});
		expect(active(snapshot, CONNECTION_A).viewerStreamKeys).toEqual([STREAM_B, STREAM_A]);
	});

	it('isolates video, stream, and viewer state per connection while persisting audio defaults', () => {
		let snapshot = createLocalVoiceStateSnapshot({microphonePermissionGranted: true});
		snapshot = send(snapshot, {
			type: 'connection.seed',
			connectionId: CONNECTION_A,
			seed: {
				selfMute: true,
				selfDeaf: true,
				selfVideo: true,
				selfStream: true,
				viewerStreamKeys: [STREAM_A],
			},
		});
		snapshot = send(snapshot, {
			type: 'connection.seed',
			connectionId: CONNECTION_B,
			seed: {
				selfVideo: false,
				selfStream: false,
				viewerStreamKeys: [STREAM_B],
			},
		});
		expect(active(snapshot, CONNECTION_A)).toMatchObject({
			selfMute: true,
			selfDeaf: true,
			selfVideo: true,
			selfStream: true,
			viewerStreamKeys: [STREAM_A],
		});
		expect(active(snapshot, CONNECTION_B)).toMatchObject({
			selfMute: true,
			selfDeaf: true,
			selfVideo: false,
			selfStream: false,
			viewerStreamKeys: [STREAM_B],
		});
		expect(snapshot.context.persistedDefaults).toMatchObject({selfMute: true, selfDeaf: true});
	});

	it('does not overwrite an existing connection on seed, but server sync applies partial updates', () => {
		let snapshot = createLocalVoiceStateSnapshot({microphonePermissionGranted: true});
		snapshot = send(snapshot, {
			type: 'connection.seed',
			connectionId: CONNECTION_A,
			seed: {selfMute: true, selfVideo: true, viewerStreamKeys: [STREAM_A]},
		});
		snapshot = send(snapshot, {
			type: 'connection.seed',
			connectionId: CONNECTION_A,
			seed: {selfMute: false, selfVideo: false, viewerStreamKeys: [STREAM_B]},
		});
		expect(active(snapshot, CONNECTION_A)).toMatchObject({
			selfMute: true,
			selfVideo: true,
			viewerStreamKeys: [STREAM_A],
		});

		snapshot = send(snapshot, {
			type: 'connection.sync',
			connectionId: CONNECTION_A,
			seed: {
				selfMute: false,
				hasUserSetMute: false,
				mutedByPermission: true,
				shouldUnmuteOnUndeafen: true,
			},
		});
		expect(active(snapshot, CONNECTION_A)).toMatchObject({
			selfMute: false,
			selfVideo: true,
			hasUserSetMute: false,
			mutedByPermission: true,
			shouldUnmuteOnUndeafen: true,
		});
	});

	it('clears a connection and resets fallback media watcher state', () => {
		let snapshot = createLocalVoiceStateSnapshot({
			microphonePermissionGranted: true,
			fallback: {selfVideo: true, selfStream: true, viewerStreamKeys: [STREAM_A]},
		});
		snapshot = send(snapshot, {
			type: 'connection.seed',
			connectionId: CONNECTION_A,
			seed: {selfVideo: true, selfStream: true, viewerStreamKeys: [STREAM_B]},
		});
		expect(hasLocalVoiceConnectionState(snapshot, CONNECTION_A)).toBe(true);
		snapshot = send(snapshot, {type: 'connection.clear', connectionId: CONNECTION_A});
		expect(hasLocalVoiceConnectionState(snapshot, CONNECTION_A)).toBe(false);
		expect(active(snapshot)).toMatchObject({selfVideo: false, selfStream: false, viewerStreamKeys: []});
	});

	it('resets preferences, clears connections, and keeps permission mute when permission is denied', () => {
		let snapshot = createLocalVoiceStateSnapshot({
			microphonePermissionGranted: false,
			persistedDefaults: {selfMute: true, selfDeaf: true, hasUserSetMute: true, hasUserSetDeaf: true},
			fallback: {selfVideo: true, selfStream: true, viewerStreamKeys: [STREAM_A], mutedByPermission: true},
		});
		snapshot = send(snapshot, {
			type: 'connection.seed',
			connectionId: CONNECTION_A,
			seed: {selfVideo: true, viewerStreamKeys: [STREAM_B]},
		});
		snapshot = send(snapshot, {type: 'preferences.reset'});
		expect(snapshot.context.persistedDefaults).toEqual({
			selfMute: false,
			selfDeaf: false,
			hasUserSetMute: false,
			hasUserSetDeaf: false,
		});
		expect(active(snapshot)).toMatchObject({
			selfMute: true,
			selfDeaf: false,
			selfVideo: false,
			selfStream: false,
			viewerStreamKeys: [],
			mutedByPermission: true,
		});
		expect(hasLocalVoiceConnectionState(snapshot, CONNECTION_A)).toBe(false);
	});

	it('survives repeated toggles and permission denied/granted cycles without losing canonical state', () => {
		let snapshot = createLocalVoiceStateSnapshot({microphonePermissionGranted: true});
		for (let i = 0; i < 25; i++) {
			snapshot = send(snapshot, {type: 'mute.toggle', activeConnectionId: null});
			snapshot = send(snapshot, {type: 'deaf.toggle', activeConnectionId: null});
			snapshot = send(snapshot, {type: 'deaf.toggle', activeConnectionId: null});
			snapshot = send(snapshot, {type: 'viewer.replace', activeConnectionId: null, keys: [STREAM_A, STREAM_A]});
			snapshot = send(snapshot, {type: 'permission.deny', activeConnectionId: null});
			snapshot = send(snapshot, {type: 'mute.update', activeConnectionId: null, muted: false});
			snapshot = send(snapshot, {type: 'permission.grant', activeConnectionId: null});
		}
		expect(active(snapshot).viewerStreamKeys).toEqual([STREAM_A]);
		expect(active(snapshot)).toMatchObject({
			selfDeaf: false,
			mutedByPermission: false,
			shouldUnmuteOnUndeafen: false,
		});
	});
});
