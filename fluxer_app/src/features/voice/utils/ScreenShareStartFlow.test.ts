// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test, vi} from 'vitest';

const platform = {current: 'darwin' as NodeJS.Platform | string};

const setScreenShareEnabled = vi.fn(async () => {});
const replaceActiveDisplayScreenShare = vi.fn(async () => true);
const armNativeAudioForNextCapture = vi.fn(async () => true);
const armNativeSystemAudioForNextCapture = vi.fn(async () => true);
const armNativeAudioForLinuxRouting = vi.fn(async () => true);

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	getElectronAPI: () => ({platform: platform.current}),
	supportsDesktopScreenShareAudioCapture: () => true,
}));

vi.mock('@app/features/voice/engine/MediaEngineFacade', () => ({
	default: {
		room: {localParticipant: {isScreenShareEnabled: true}},
		setScreenShareEnabled,
		replaceActiveDisplayScreenShare,
	},
}));

vi.mock('@app/features/voice/utils/ScreenShareEnvironment', () => ({
	getDisplayShareEnvironment: async () => 'desktop-custom',
	usesNativeDisplayShareAudioSelection: (environment: string) => environment !== 'desktop-custom',
}));

vi.mock('@app/features/voice/utils/NativeAudioCaptureBridge', () => ({
	armNativeAudioForLinuxRouting,
	armNativeAudioForNextCapture,
	armNativeSystemAudioForNextCapture,
	disarmNativeAudio: vi.fn(),
	disarmPendingNativeAudio: vi.fn(),
	getLastNativeAudioArmFailure: () => null,
}));

vi.mock('@app/features/voice/utils/LinuxScreenShareAudio', () => ({
	disarmVirtmic: vi.fn(),
}));

vi.mock('@app/features/voice/state/ActiveScreenShareSource', () => ({
	default: {setPublishedSource: vi.fn(), clear: vi.fn(), getSourceId: () => null},
}));

vi.mock('@app/features/voice/state/DesktopSourceIntent', () => ({
	setDesktopSourceIntent: vi.fn(),
	clearDesktopSourceIntent: vi.fn(),
}));

vi.mock('@app/features/voice/state/LocalVoiceState', () => ({
	default: {getSelfStream: () => null},
}));

vi.mock('@app/features/voice/utils/ScreenShareUtils', () => ({
	executeScreenShareOperation: async (operation: () => Promise<void>) => {
		await operation();
	},
}));

vi.mock('@app/features/voice/engine/ScreenShareCodecNegotiation', () => ({
	default: {selectScreenShareCodec: () => 'vp8'},
}));

vi.mock('@app/features/voice/utils/CodecCapabilityDetector', () => ({
	resolveScreenShareContentHintForContext: () => 'motion',
}));

vi.mock('@app/features/app/utils/LimitResolverAdapter', () => ({
	LimitResolver: {resolve: () => 0},
}));

vi.mock('@app/features/app/utils/LimitUtils', () => ({
	isLimitToggleEnabled: () => false,
}));

vi.mock('@app/features/voice/commands/VoiceSettingsCommands', () => ({
	update: vi.fn(),
}));

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {
		getShareAppAudio: () => true,
		getShareDesktopAudio: () => true,
		getShareDeviceAudio: () => true,
		getScreenshareResolution: () => 'medium',
		getStreamingMode: () => 'screenshare',
		getVideoFrameRate: () => 30,
		getPreferredScreenShareCodec: () => 'auto',
		getScreenShareContentHintOverride: () => 'auto',
		getScreenShareMaxBitrateBpsOverride: () => null,
		getEffectiveScreenShareAudioDeviceId: () => '',
		getScreenShareAudioSourceMode: () => 'system',
		getScreenShareAudioIncludeSources: () => [],
		getScreenShareAudioExcludeSources: () => [],
	},
}));

const {startConfiguredDisplayScreenShare, switchConfiguredDisplayScreenShare} = await import(
	'@app/features/voice/utils/ScreenShareStartFlow'
);
const {isScreenShareAudioCaptureError} = await import('@app/features/voice/utils/ScreenShareAudioCaptureError');
const ActiveScreenShareSource = (await import('@app/features/voice/state/ActiveScreenShareSource')).default;

beforeEach(() => {
	platform.current = 'darwin';
	vi.clearAllMocks();
	setScreenShareEnabled.mockResolvedValue(undefined);
	armNativeAudioForNextCapture.mockResolvedValue(true);
});

describe('sharing a Fluxer-owned window while app audio is enabled', () => {
	test('starts the share video-only instead of failing', async () => {
		const started = await startConfiguredDisplayScreenShare('window:99:0', {
			isOwnWindow: true,
			preferredDisplaySurface: 'window',
		});

		expect(started).toBe(true);
		expect(setScreenShareEnabled).toHaveBeenCalledTimes(1);
		const call = setScreenShareEnabled.mock.calls[0] as unknown as [
			boolean,
			{audio: boolean; systemAudio: string; windowAudio: string},
			unknown,
		];
		expect(call[0]).toBe(true);
		expect(call[1].audio).toBe(false);
		expect(call[1].systemAudio).toBe('exclude');
		expect(call[1].windowAudio).toBe('exclude');
		expect(armNativeAudioForNextCapture).not.toHaveBeenCalled();
	});
});

describe('sharing another application window while app audio is enabled', () => {
	test('arms native per-window audio and hard-fails when it cannot be armed', async () => {
		armNativeAudioForNextCapture.mockResolvedValue(false);

		await expect(
			startConfiguredDisplayScreenShare('window:42:0', {preferredDisplaySurface: 'window'}),
		).rejects.toSatisfy(isScreenShareAudioCaptureError);
		expect(armNativeAudioForNextCapture).toHaveBeenCalledWith('window:42:0');
		expect(setScreenShareEnabled).not.toHaveBeenCalled();
	});
});

describe('switching the display source', () => {
	test('preserves the running share state when the switch fails', async () => {
		replaceActiveDisplayScreenShare.mockResolvedValue(false);

		const switched = await switchConfiguredDisplayScreenShare('window:7:0', {preferredDisplaySurface: 'window'});

		expect(switched).toBe(false);
		expect(ActiveScreenShareSource.clear).not.toHaveBeenCalled();
		expect(ActiveScreenShareSource.setPublishedSource).not.toHaveBeenCalled();
	});

	test('records the new published source when the switch succeeds', async () => {
		replaceActiveDisplayScreenShare.mockResolvedValue(true);

		const switched = await switchConfiguredDisplayScreenShare('window:7:0', {preferredDisplaySurface: 'window'});

		expect(switched).toBe(true);
		expect(ActiveScreenShareSource.setPublishedSource).toHaveBeenCalledWith('app', 'window:7:0', {isOwnWindow: false});
		expect(ActiveScreenShareSource.clear).not.toHaveBeenCalled();
	});
});
