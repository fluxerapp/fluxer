// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, it, vi} from 'vitest';

const playSound = vi.fn();

const localVoiceState = {
	selfMute: false,
	selfDeaf: false,
	getSelfMute: () => localVoiceState.selfMute,
	getSelfDeaf: () => localVoiceState.selfDeaf,
	updateSelfMute: (muted: boolean) => {
		localVoiceState.selfMute = muted;
	},
	toggleSelfMute: () => {
		if (localVoiceState.selfDeaf) {
			localVoiceState.selfDeaf = false;
			localVoiceState.selfMute = false;
			return;
		}
		localVoiceState.selfMute = !localVoiceState.selfMute;
	},
};

const permissionMuted = {active: false};
const microphoneFailureLatch = {active: false};

vi.mock('@app/features/input/state/InputKeybind', () => ({
	default: {isPushToTalkEffective: () => true, pushToTalkHeld: true},
}));
const updateVoiceStateExplicit = vi.fn();
const gatewayVoiceStates: Record<
	string,
	{
		guild_id: string;
		channel_id: string;
		self_mute: boolean;
		self_deaf: boolean;
		self_video: boolean;
		self_stream: boolean;
	}
> = {};

vi.mock('@app/features/gateway/transport/GatewaySessionPool', () => ({
	default: {foregroundConnection: {socket: {updateVoiceStateExplicit}}},
}));
vi.mock('@app/features/ui/commands/SoundCommands', () => ({
	playSound: (...args: Array<unknown>) => playSound(...args),
}));
vi.mock('@app/features/permissions/system/commands/MacPermissionsModalCommands', () => ({
	handleMediaPermissionBlocked: () => undefined,
}));
vi.mock('@app/features/permissions/system/state/MediaPermission', () => ({
	default: {
		getMicrophonePermissionState: () => 'granted',
		isMicrophoneGranted: () => true,
		isMicrophoneExplicitlyDenied: () => false,
	},
}));
vi.mock('@app/features/permissions/system/utils/NativePermissions', () => ({
	ensureNativePermission: async () => 'granted',
}));
vi.mock('@app/features/ui/utils/NativeUtils', () => ({isDesktop: () => false}));
vi.mock('@app/features/voice/engine/MediaEngineFacade', () => ({
	default: {
		room: null,
		guildId: '1',
		channelId: '2',
		connectionId: 'local',
		getVoiceStateByConnectionId: (connectionId: string) => gatewayVoiceStates[connectionId] ?? null,
		isMicrophoneFailureLatched: () => microphoneFailureLatch.active,
		clearMicrophoneFailureLatch: () => {
			microphoneFailureLatch.active = false;
		},
		getCurrentUserVoiceState: () => null,
		syncLocalVoiceStateWithServer: () => undefined,
	},
}));
vi.mock('@app/features/voice/state/LocalVoiceState', () => ({default: localVoiceState}));
vi.mock('@app/features/voice/utils/VoicePermissionUtils', () => ({
	isVoicePermissionMuteActive: () => permissionMuted.active,
}));

const {bulkMuteConnections, getCurrentSelfMute, getSelfMuteForConnection, toggleSelfMute, toggleSelfMuteForConnection} =
	await import('@app/features/devtools/commands/VoiceStateCommands');
const {SoundType} = await import('@app/features/notification/utils/SoundUtils');

describe('toggleSelfMute with push to talk enabled', () => {
	beforeEach(() => {
		localVoiceState.selfMute = false;
		localVoiceState.selfDeaf = false;
		permissionMuted.active = false;
		microphoneFailureLatch.active = false;
		playSound.mockClear();
	});

	it('mutes and unmutes', async () => {
		await toggleSelfMute(null);
		expect(localVoiceState.selfMute).toBe(true);
		expect(playSound).toHaveBeenLastCalledWith(SoundType.Mute);
		await toggleSelfMute(null);
		expect(localVoiceState.selfMute).toBe(false);
		expect(playSound).toHaveBeenLastCalledWith(SoundType.Unmute);
	});

	it('undeafens like voice activity mode', async () => {
		localVoiceState.selfMute = true;
		localVoiceState.selfDeaf = true;
		await toggleSelfMute(null);
		expect(localVoiceState.selfDeaf).toBe(false);
		expect(localVoiceState.selfMute).toBe(false);
	});

	it('keeps a permission-muted user muted', async () => {
		localVoiceState.selfMute = true;
		permissionMuted.active = true;
		await toggleSelfMute(null);
		expect(localVoiceState.selfMute).toBe(true);
	});
});

describe('device-scoped self-mute with push to talk enabled', () => {
	beforeEach(() => {
		localVoiceState.selfMute = false;
		localVoiceState.selfDeaf = false;
		permissionMuted.active = false;
		microphoneFailureLatch.active = false;
		playSound.mockClear();
		updateVoiceStateExplicit.mockClear();
		const idle = {
			guild_id: '1',
			channel_id: '2',
			self_mute: true,
			self_deaf: false,
			self_video: false,
			self_stream: false,
		};
		gatewayVoiceStates.local = {...idle};
		gatewayVoiceStates.other = {...idle, self_mute: false};
	});

	it('reads the local device from local self-mute instead of the push-to-talk gateway value', () => {
		expect(getSelfMuteForConnection('local')).toBe(false);
		localVoiceState.selfMute = true;
		gatewayVoiceStates.local.self_mute = false;
		expect(getSelfMuteForConnection('local')).toBe(true);
		expect(getSelfMuteForConnection('other')).toBe(false);
	});

	it('toggles the local device through local self-mute without a direct gateway write', async () => {
		await toggleSelfMuteForConnection('local');
		expect(localVoiceState.selfMute).toBe(true);
		expect(playSound).toHaveBeenLastCalledWith(SoundType.Mute);
		await toggleSelfMuteForConnection('local');
		expect(localVoiceState.selfMute).toBe(false);
		expect(playSound).toHaveBeenLastCalledWith(SoundType.Unmute);
		expect(updateVoiceStateExplicit).not.toHaveBeenCalled();
	});

	it('toggles another device through the gateway without touching local self-mute', async () => {
		await toggleSelfMuteForConnection('other');
		expect(updateVoiceStateExplicit).toHaveBeenCalledTimes(1);
		expect(updateVoiceStateExplicit).toHaveBeenLastCalledWith(
			expect.objectContaining({connection_id: 'other', self_mute: true}),
		);
		expect(localVoiceState.selfMute).toBe(false);
	});

	it('reports a latched microphone failure as self-muted and unmutes it on toggle', async () => {
		microphoneFailureLatch.active = true;
		expect(getCurrentSelfMute()).toBe(true);
		expect(getSelfMuteForConnection('local')).toBe(true);
		await toggleSelfMuteForConnection('local');
		expect(microphoneFailureLatch.active).toBe(false);
		expect(getCurrentSelfMute()).toBe(false);
		expect(playSound).toHaveBeenLastCalledWith(SoundType.Unmute);
	});

	it('bulk mutes the local device through local self-mute and other devices through the gateway', async () => {
		await bulkMuteConnections(['local', 'other'], true);
		expect(localVoiceState.selfMute).toBe(true);
		expect(updateVoiceStateExplicit).toHaveBeenCalledTimes(1);
		expect(updateVoiceStateExplicit).toHaveBeenLastCalledWith(
			expect.objectContaining({connection_id: 'other', self_mute: true}),
		);
		updateVoiceStateExplicit.mockClear();
		gatewayVoiceStates.other.self_mute = true;
		microphoneFailureLatch.active = true;
		await bulkMuteConnections(['local', 'other'], false);
		expect(localVoiceState.selfMute).toBe(false);
		expect(microphoneFailureLatch.active).toBe(false);
		expect(updateVoiceStateExplicit).toHaveBeenCalledTimes(1);
		expect(updateVoiceStateExplicit).toHaveBeenLastCalledWith(
			expect.objectContaining({connection_id: 'other', self_mute: false}),
		);
	});
});
