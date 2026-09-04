// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test, vi} from 'vitest';

const platform = {current: 'darwin' as NodeJS.Platform | string};

const voiceSettings = {
	audioSourceMode: 'system' as 'none' | 'system' | 'specific',
	includeSources: [] as Array<Record<string, string>>,
	excludeSources: [] as Array<Record<string, string>>,
	audioDeviceId: 'default',
};

const activeShareContext = {current: null as 'app' | 'device' | 'display' | null};
const activeShareSourceId = {current: null as string | null};
const activeShareOwnWindow = {current: false};
const activeWindowAudioScope = {current: 'window' as 'window' | 'system'};
const pendingWindowAudioScope = {current: null as 'window' | 'system' | null};
const activeShareVideoDeviceId = {current: ''};

const nativeAudioAvailability = {
	current: {
		available: true,
		backend: 'linux-pipewire',
		capabilities: {process: true, system: true, systemExcludesSelf: true},
	} as {available: boolean; backend?: string; capabilities?: {process: boolean; system: boolean}},
};

const setScreenShareEnabled = vi.fn(async () => {});
const replaceActiveDisplayScreenShare = vi.fn(async () => true);
const startDeviceScreenShare = vi.fn(async () => {});
const replaceActiveDeviceScreenShare = vi.fn(async () => true);
const disarmNativeAudio = vi.fn();
const ensureLinuxScreenShareAudioPublication = vi.fn(async (_rule: Record<string, unknown>) => true);
const ensureDeviceScreenShareMicPublication = vi.fn(async () => true);
const ensureWindowScreenShareAudioPublication = vi.fn(async (_sourceId: string) => true);
const armNativeAudioForNextCapture = vi.fn(async () => true);
const armNativeSystemAudioForNextCapture = vi.fn(async () => true);
const armNativeAudioForLinuxRouting = vi.fn(async (_rule: Record<string, unknown>) => true);

const enumerateDevices = vi.fn(async (): Promise<Array<Record<string, string>>> => []);
Object.defineProperty(globalThis, 'navigator', {
	configurable: true,
	value: {mediaDevices: {enumerateDevices}},
});

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	getElectronAPI: () => ({platform: platform.current}),
	supportsDesktopScreenShareAudioCapture: () => true,
}));

vi.mock('@app/features/voice/engine/MediaEngineFacade', () => ({
	default: {
		room: {localParticipant: {isScreenShareEnabled: true}},
		setScreenShareEnabled,
		replaceActiveDisplayScreenShare,
		startDeviceScreenShare,
		replaceActiveDeviceScreenShare,
		ensureLinuxScreenShareAudioPublication,
		ensureDeviceScreenShareMicPublication,
		ensureWindowScreenShareAudioPublication,
		getActiveScreenShareVideoDeviceId: () => activeShareVideoDeviceId.current,
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
	disarmNativeAudio,
	disarmPendingNativeAudio: vi.fn(),
	getLastNativeAudioArmFailure: () => null,
	getNativeAudioAvailabilityCached: async () => nativeAudioAvailability.current,
}));

vi.mock('@app/features/voice/utils/LinuxScreenShareAudio', () => ({
	disarmVirtmic: vi.fn(),
}));

vi.mock('@app/features/voice/state/ActiveScreenShareSource', () => ({
	default: {
		setPublishedSource: vi.fn(),
		clear: vi.fn(),
		getSourceId: () => activeShareSourceId.current,
		getShareContext: () => activeShareContext.current,
		isOwnWindow: () => activeShareOwnWindow.current,
		getWindowAudioScope: () => activeWindowAudioScope.current,
		setWindowAudioScope: vi.fn((scope: 'window' | 'system') => {
			activeWindowAudioScope.current = scope;
		}),
		getPendingWindowAudioScope: () => pendingWindowAudioScope.current ?? activeWindowAudioScope.current,
	},
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
		getScreenShareAudioDeviceId: () => voiceSettings.audioDeviceId,
		getEffectiveScreenShareAudioDeviceId: () =>
			voiceSettings.audioDeviceId === 'default' ? 'mic-1' : voiceSettings.audioDeviceId,
		getScreenShareAudioSourceMode: () => voiceSettings.audioSourceMode,
		getScreenShareAudioIncludeSources: () => voiceSettings.includeSources,
		getScreenShareAudioExcludeSources: () => voiceSettings.excludeSources,
		getEffectiveScreenShareAudioSourceMode: () => voiceSettings.audioSourceMode,
		getEffectiveScreenShareAudioIncludeSources: () => voiceSettings.includeSources,
		getEffectiveScreenShareAudioExcludeSources: () => voiceSettings.excludeSources,
		getLinuxAudioCaptureIgnoreInputMedia: () => true,
		getLinuxAudioCaptureIgnoreVirtual: () => false,
		getLinuxAudioCaptureIgnoreDevices: () => true,
		getLinuxAudioCaptureOnlySpeakers: () => true,
		getLinuxAudioCaptureOnlyDefaultSpeakers: () => true,
	},
}));

const {
	applyLiveScreenShareAudioSourceChange,
	reconfigureActiveDeviceShareAudio,
	reconfigureActiveLinuxAppShareAudio,
	startConfiguredDeviceScreenShare,
	startConfiguredDisplayScreenShare,
	switchConfiguredDeviceScreenShare,
	switchConfiguredDisplayScreenShare,
} = await import('@app/features/voice/utils/ScreenShareStartFlow');
const {isScreenShareAudioCaptureError} = await import('@app/features/voice/utils/ScreenShareAudioCaptureError');
const ActiveScreenShareSource = (await import('@app/features/voice/state/ActiveScreenShareSource')).default;

beforeEach(() => {
	platform.current = 'darwin';
	voiceSettings.audioSourceMode = 'system';
	voiceSettings.includeSources = [];
	voiceSettings.excludeSources = [];
	voiceSettings.audioDeviceId = 'default';
	activeShareContext.current = null;
	activeShareSourceId.current = null;
	activeShareOwnWindow.current = false;
	activeWindowAudioScope.current = 'window';
	pendingWindowAudioScope.current = null;
	activeShareVideoDeviceId.current = '';
	nativeAudioAvailability.current = {
		available: true,
		backend: 'linux-pipewire',
		capabilities: {process: true, system: true},
	};
	vi.clearAllMocks();
	setScreenShareEnabled.mockResolvedValue(undefined);
	armNativeAudioForNextCapture.mockResolvedValue(true);
	replaceActiveDeviceScreenShare.mockResolvedValue(true);
	ensureLinuxScreenShareAudioPublication.mockResolvedValue(true);
	ensureDeviceScreenShareMicPublication.mockResolvedValue(true);
	ensureWindowScreenShareAudioPublication.mockResolvedValue(true);
	enumerateDevices.mockResolvedValue([]);
});

function deviceShareAudioDeviceId(call: unknown): string | undefined {
	return (call as [{audioDeviceId?: string}, unknown])[0].audioDeviceId;
}

describe('sharing a video device', () => {
	test('takes its audio from the configured input device on every desktop platform', async () => {
		for (const current of ['linux', 'win32', 'darwin']) {
			vi.clearAllMocks();
			platform.current = current;

			expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);

			expect(deviceShareAudioDeviceId(startDeviceScreenShare.mock.calls[0])).toBe('mic-1');
			expect(ensureLinuxScreenShareAudioPublication).not.toHaveBeenCalled();
		}
	});

	test('opens no microphone and links the selected applications', async () => {
		platform.current = 'linux';
		voiceSettings.audioSourceMode = 'specific';
		voiceSettings.includeSources = [{'application.name': 'mpv'}];

		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);

		expect(deviceShareAudioDeviceId(startDeviceScreenShare.mock.calls[0])).toBeUndefined();
		expect(ensureLinuxScreenShareAudioPublication).toHaveBeenCalledTimes(1);
	});

	test('keeps the microphone while no application is selected', async () => {
		platform.current = 'linux';

		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);

		expect(deviceShareAudioDeviceId(startDeviceScreenShare.mock.calls[0])).toBe('mic-1');
		expect(ensureLinuxScreenShareAudioPublication).not.toHaveBeenCalled();
	});

	test('keeps the microphone on a device share whose stored source mode says none', async () => {
		platform.current = 'linux';
		voiceSettings.audioSourceMode = 'none';

		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);
		expect(deviceShareAudioDeviceId(startDeviceScreenShare.mock.calls[0])).toBe('mic-1');

		expect(await reconfigureActiveDeviceShareAudio()).toBe(true);
		expect(ensureDeviceScreenShareMicPublication).toHaveBeenCalledWith('mic-1');
		expect(ensureLinuxScreenShareAudioPublication).not.toHaveBeenCalled();
	});

	test('keeps the microphone when the picked applications cannot be routed', async () => {
		platform.current = 'linux';
		voiceSettings.audioSourceMode = 'specific';
		voiceSettings.includeSources = [{'fluxer.display.name': 'mpv'}];

		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);

		expect(deviceShareAudioDeviceId(startDeviceScreenShare.mock.calls[0])).toBe('mic-1');
		expect(ensureLinuxScreenShareAudioPublication).not.toHaveBeenCalled();
	});

	test('never routes application audio into a device share off Linux', async () => {
		voiceSettings.audioSourceMode = 'specific';
		voiceSettings.includeSources = [{'application.name': 'mpv'}];

		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);

		expect(deviceShareAudioDeviceId(startDeviceScreenShare.mock.calls[0])).toBe('mic-1');
		expect(ensureLinuxScreenShareAudioPublication).not.toHaveBeenCalled();
	});

	test('never routes application audio when the capture layer cannot express a selection', async () => {
		platform.current = 'linux';
		nativeAudioAvailability.current = {available: false, backend: 'linux-pipewire'};
		voiceSettings.audioSourceMode = 'specific';
		voiceSettings.includeSources = [{'application.name': 'mpv'}];

		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);

		expect(deviceShareAudioDeviceId(startDeviceScreenShare.mock.calls[0])).toBe('mic-1');
		expect(ensureLinuxScreenShareAudioPublication).not.toHaveBeenCalled();
	});

	test('prefers the shared capture card own audio input while no device was ever chosen', async () => {
		platform.current = 'linux';
		enumerateDevices.mockResolvedValue([
			{kind: 'videoinput', deviceId: 'camera-1', groupId: 'elgato'},
			{kind: 'audioinput', deviceId: 'default', groupId: 'elgato'},
			{kind: 'audioinput', deviceId: 'elgato-line-in', groupId: 'elgato'},
			{kind: 'audioinput', deviceId: 'headset', groupId: 'usb-headset'},
		]);

		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);

		expect(deviceShareAudioDeviceId(startDeviceScreenShare.mock.calls[0])).toBe('elgato-line-in');
	});

	test('keeps a chosen audio device instead of pairing one with the video device', async () => {
		platform.current = 'linux';
		voiceSettings.audioDeviceId = 'headset';
		enumerateDevices.mockResolvedValue([
			{kind: 'videoinput', deviceId: 'camera-1', groupId: 'elgato'},
			{kind: 'audioinput', deviceId: 'elgato-line-in', groupId: 'elgato'},
		]);

		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);

		expect(deviceShareAudioDeviceId(startDeviceScreenShare.mock.calls[0])).toBe('headset');
	});

	test('falls back to the voice input device when the capture card exposes no audio input', async () => {
		platform.current = 'linux';
		enumerateDevices.mockResolvedValue([
			{kind: 'videoinput', deviceId: 'camera-1', groupId: 'webcam'},
			{kind: 'audioinput', deviceId: 'headset', groupId: 'usb-headset'},
		]);

		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);

		expect(deviceShareAudioDeviceId(startDeviceScreenShare.mock.calls[0])).toBe('mic-1');
	});

	test('relinks the selected applications after switching the capture device', async () => {
		platform.current = 'linux';
		voiceSettings.audioSourceMode = 'specific';
		voiceSettings.includeSources = [{'application.name': 'mpv'}];

		expect(await switchConfiguredDeviceScreenShare('camera-2')).toBe(true);

		expect(deviceShareAudioDeviceId(replaceActiveDeviceScreenShare.mock.calls[0])).toBeUndefined();
		expect(ensureLinuxScreenShareAudioPublication).toHaveBeenCalledTimes(1);
	});

	test('does not relink application audio when the device switch fails', async () => {
		platform.current = 'linux';
		voiceSettings.audioSourceMode = 'specific';
		voiceSettings.includeSources = [{'application.name': 'mpv'}];
		replaceActiveDeviceScreenShare.mockResolvedValue(false);

		expect(await switchConfiguredDeviceScreenShare('camera-2')).toBe(false);

		expect(ensureLinuxScreenShareAudioPublication).not.toHaveBeenCalled();
	});

	test('swaps a live device share between the microphone and the selected applications', async () => {
		platform.current = 'linux';
		voiceSettings.audioSourceMode = 'specific';
		voiceSettings.includeSources = [{'application.name': 'mpv'}];

		expect(await reconfigureActiveDeviceShareAudio()).toBe(true);
		expect(ensureLinuxScreenShareAudioPublication).toHaveBeenCalledTimes(1);
		expect(ensureDeviceScreenShareMicPublication).not.toHaveBeenCalled();

		voiceSettings.includeSources = [];
		expect(await reconfigureActiveDeviceShareAudio()).toBe(true);
		expect(ensureLinuxScreenShareAudioPublication).toHaveBeenCalledTimes(1);
		expect(ensureDeviceScreenShareMicPublication).toHaveBeenCalledWith('mic-1');

		voiceSettings.includeSources = [{'application.name': 'mpv'}];
		expect(await reconfigureActiveDeviceShareAudio()).toBe(true);
		expect(ensureLinuxScreenShareAudioPublication).toHaveBeenCalledTimes(2);
		expect(ensureDeviceScreenShareMicPublication).toHaveBeenCalledTimes(1);
	});

	test('keeps the paired capture card input when the live share rebinds its microphone', async () => {
		platform.current = 'linux';
		activeShareVideoDeviceId.current = 'camera-1';
		enumerateDevices.mockResolvedValue([
			{kind: 'videoinput', deviceId: 'camera-1', groupId: 'elgato'},
			{kind: 'audioinput', deviceId: 'elgato-line-in', groupId: 'elgato'},
			{kind: 'audioinput', deviceId: 'mic-1', groupId: 'usb-headset'},
		]);

		expect(await reconfigureActiveDeviceShareAudio()).toBe(true);

		expect(ensureDeviceScreenShareMicPublication).toHaveBeenCalledWith('elgato-line-in');
	});
});

describe('sharing a whole display', () => {
	test('captures the desktop mix without Fluxer on Linux when nothing is selected', async () => {
		platform.current = 'linux';
		voiceSettings.excludeSources = [{'application.name': 'Discord'}];

		expect(await startConfiguredDisplayScreenShare('screen:1')).toBe(true);

		expect(armNativeAudioForLinuxRouting).toHaveBeenCalledTimes(1);
		expect(armNativeAudioForLinuxRouting.mock.calls[0][0]).toMatchObject({
			include: [],
			exclude: [{'application.name': 'Discord'}],
			ignoreInputMedia: true,
			onlySpeakers: true,
			onlyDefaultSpeakers: true,
		});
	});

	test('publishes no audio at all when the stored selection asks for none', async () => {
		platform.current = 'linux';
		voiceSettings.audioSourceMode = 'none';

		expect(await startConfiguredDisplayScreenShare('screen:1')).toBe(true);

		expect(armNativeAudioForLinuxRouting).not.toHaveBeenCalled();
		const call = setScreenShareEnabled.mock.calls[0] as unknown as [boolean, {audio: boolean}, unknown];
		expect(call[1].audio).toBe(false);
	});

	test('honours the stored include list', async () => {
		platform.current = 'linux';
		voiceSettings.audioSourceMode = 'specific';
		voiceSettings.includeSources = [{'application.name': 'mpv'}];

		expect(await startConfiguredDisplayScreenShare('screen:1')).toBe(true);

		expect(armNativeAudioForLinuxRouting.mock.calls[0][0]).toMatchObject({
			include: [{'application.name': 'mpv'}],
			ignoreInputMedia: true,
		});
	});

	test('arms native system audio on Windows and macOS', async () => {
		for (const current of ['win32', 'darwin']) {
			vi.clearAllMocks();
			platform.current = current;

			expect(await startConfiguredDisplayScreenShare('screen:1')).toBe(true);

			expect(armNativeSystemAudioForNextCapture).toHaveBeenCalledTimes(1);
			expect(armNativeAudioForLinuxRouting).not.toHaveBeenCalled();
		}
	});
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

	test('takes the shared window own audio by default on Linux, never the whole system mix', async () => {
		platform.current = 'linux';
		voiceSettings.excludeSources = [{'application.name': 'Discord'}];

		expect(await startConfiguredDisplayScreenShare('window:42:0', {preferredDisplaySurface: 'window'})).toBe(true);

		expect(armNativeAudioForNextCapture).toHaveBeenCalledWith('window:42:0');
		expect(armNativeAudioForLinuxRouting).not.toHaveBeenCalled();
	});

	test('widens a Linux window share to the system mix minus the stored exclusions on request', async () => {
		platform.current = 'linux';
		pendingWindowAudioScope.current = 'system';
		voiceSettings.excludeSources = [{'application.name': 'Discord'}];

		expect(await startConfiguredDisplayScreenShare('window:42:0', {preferredDisplaySurface: 'window'})).toBe(true);

		expect(armNativeAudioForNextCapture).not.toHaveBeenCalled();
		expect(armNativeAudioForLinuxRouting).toHaveBeenCalledTimes(1);
		expect(armNativeAudioForLinuxRouting.mock.calls[0][0]).toMatchObject({
			include: [],
			exclude: [{'application.name': 'Discord'}],
			onlySpeakers: true,
			onlyDefaultSpeakers: true,
		});
	});

	test('ignores a stored display-share selection until the window scope is widened', async () => {
		for (const audioSourceMode of ['none', 'specific'] as const) {
			vi.clearAllMocks();
			platform.current = 'linux';
			voiceSettings.audioSourceMode = audioSourceMode;
			voiceSettings.includeSources = [{'application.name': 'mpv'}];

			expect(await startConfiguredDisplayScreenShare('window:42:0', {preferredDisplaySurface: 'window'})).toBe(true);

			expect(armNativeAudioForNextCapture).toHaveBeenCalledWith('window:42:0');
			expect(armNativeAudioForLinuxRouting).not.toHaveBeenCalled();
		}
	});

	test('routes the picked applications once the window share was widened to the system', async () => {
		platform.current = 'linux';
		pendingWindowAudioScope.current = 'system';
		voiceSettings.audioSourceMode = 'specific';
		voiceSettings.includeSources = [{'application.name': 'mpv'}];

		expect(await startConfiguredDisplayScreenShare('window:42:0', {preferredDisplaySurface: 'window'})).toBe(true);

		expect(armNativeAudioForNextCapture).not.toHaveBeenCalled();
		expect(armNativeAudioForLinuxRouting.mock.calls[0][0]).toMatchObject({
			include: [{'application.name': 'mpv'}],
		});
	});

	test('captures the shared window rather than the whole desktop when the picked apps cannot be routed', async () => {
		platform.current = 'linux';
		voiceSettings.audioSourceMode = 'specific';
		voiceSettings.includeSources = [{'fluxer.display.name': 'mpv'}];

		expect(await startConfiguredDisplayScreenShare('window:42:0', {preferredDisplaySurface: 'window'})).toBe(true);

		expect(armNativeAudioForNextCapture).toHaveBeenCalledWith('window:42:0');
		expect(armNativeAudioForLinuxRouting).not.toHaveBeenCalled();
	});

	test('publishes no audio at all on a widened Linux window share asked for none', async () => {
		platform.current = 'linux';
		pendingWindowAudioScope.current = 'system';
		voiceSettings.audioSourceMode = 'none';

		expect(await startConfiguredDisplayScreenShare('window:42:0', {preferredDisplaySurface: 'window'})).toBe(true);

		expect(armNativeAudioForNextCapture).not.toHaveBeenCalled();
		expect(armNativeAudioForLinuxRouting).not.toHaveBeenCalled();
		const call = setScreenShareEnabled.mock.calls[0] as unknown as [boolean, {audio: boolean}, unknown];
		expect(call[1].audio).toBe(false);
	});

	test('commits the scope it armed to the running share', async () => {
		platform.current = 'linux';
		pendingWindowAudioScope.current = 'system';

		expect(await startConfiguredDisplayScreenShare('window:42:0', {preferredDisplaySurface: 'window'})).toBe(true);

		expect(ActiveScreenShareSource.setWindowAudioScope).toHaveBeenCalledWith('system');
	});

	test('never carries a window scope over into a display share', async () => {
		platform.current = 'linux';
		pendingWindowAudioScope.current = 'system';

		expect(await startConfiguredDisplayScreenShare('screen:1')).toBe(true);

		expect(ActiveScreenShareSource.setWindowAudioScope).toHaveBeenCalledWith('window');
	});
});

describe('changing the audio sources of a live window share', () => {
	test('re-publishes the shared window own audio while the scope is the window', async () => {
		platform.current = 'linux';
		activeShareSourceId.current = 'window:42:0';

		expect(await reconfigureActiveLinuxAppShareAudio()).toBe(true);

		expect(ensureWindowScreenShareAudioPublication).toHaveBeenCalledWith('window:42:0');
		expect(ensureLinuxScreenShareAudioPublication).not.toHaveBeenCalled();
	});

	test('relinks a widened window share through the manual rule instead', async () => {
		platform.current = 'linux';
		activeShareSourceId.current = 'window:42:0';
		activeWindowAudioScope.current = 'system';
		voiceSettings.excludeSources = [{'application.name': 'Discord'}];

		expect(await reconfigureActiveLinuxAppShareAudio()).toBe(true);

		expect(ensureWindowScreenShareAudioPublication).not.toHaveBeenCalled();
		expect(ensureLinuxScreenShareAudioPublication.mock.calls[0][0]).toMatchObject({
			include: [],
			exclude: [{'application.name': 'Discord'}],
		});
	});

	test('relinks the picked applications on a live window share that was widened', async () => {
		platform.current = 'linux';
		activeShareSourceId.current = 'window:42:0';
		activeWindowAudioScope.current = 'system';
		voiceSettings.audioSourceMode = 'specific';
		voiceSettings.includeSources = [{'application.name': 'mpv'}];

		expect(await reconfigureActiveLinuxAppShareAudio()).toBe(true);

		expect(ensureWindowScreenShareAudioPublication).not.toHaveBeenCalled();
		expect(ensureLinuxScreenShareAudioPublication.mock.calls[0][0]).toMatchObject({
			include: [{'application.name': 'mpv'}],
		});
	});

	test('leaves a window share alone off Linux, and drops its audio when no window source is published', async () => {
		platform.current = 'darwin';
		activeShareSourceId.current = 'window:42:0';
		expect(await reconfigureActiveLinuxAppShareAudio()).toBe(false);
		expect(disarmNativeAudio).not.toHaveBeenCalled();

		platform.current = 'linux';
		activeShareSourceId.current = null;
		expect(await reconfigureActiveLinuxAppShareAudio()).toBe(false);
		expect(disarmNativeAudio).toHaveBeenCalledTimes(1);

		expect(ensureWindowScreenShareAudioPublication).not.toHaveBeenCalled();
		expect(ensureLinuxScreenShareAudioPublication).not.toHaveBeenCalled();
	});

	test('drops the audio rather than keeping the system mix live when narrowing back to the window fails', async () => {
		platform.current = 'linux';
		activeShareSourceId.current = 'window:42:0';
		activeWindowAudioScope.current = 'system';
		ensureWindowScreenShareAudioPublication.mockResolvedValue(false);

		expect(await applyLiveScreenShareAudioSourceChange('app', 'window')).toBe(false);

		expect(ensureWindowScreenShareAudioPublication).toHaveBeenCalledWith('window:42:0');
		expect(disarmNativeAudio).toHaveBeenCalledTimes(1);
		expect(ActiveScreenShareSource.getWindowAudioScope()).toBe('system');
	});

	test('drops the audio rather than keeping the system mix live when the window publication throws', async () => {
		platform.current = 'linux';
		activeShareSourceId.current = 'window:42:0';
		activeWindowAudioScope.current = 'system';
		ensureWindowScreenShareAudioPublication.mockRejectedValue(new Error('window vanished'));

		expect(await applyLiveScreenShareAudioSourceChange('app', 'window')).toBe(false);

		expect(disarmNativeAudio).toHaveBeenCalledTimes(1);
		expect(ActiveScreenShareSource.getWindowAudioScope()).toBe('system');
	});

	test('commits the narrowed scope only once the window audio is actually publishing', async () => {
		platform.current = 'linux';
		activeShareSourceId.current = 'window:42:0';
		activeWindowAudioScope.current = 'system';

		expect(await applyLiveScreenShareAudioSourceChange('app', 'window')).toBe(true);

		expect(ActiveScreenShareSource.getWindowAudioScope()).toBe('window');
	});

	test('never routes audio into a Fluxer-owned window share', async () => {
		platform.current = 'linux';
		activeShareSourceId.current = 'window:99:0';
		activeShareOwnWindow.current = true;

		expect(await reconfigureActiveLinuxAppShareAudio()).toBe(false);

		expect(ensureWindowScreenShareAudioPublication).not.toHaveBeenCalled();
		expect(ensureLinuxScreenShareAudioPublication).not.toHaveBeenCalled();
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
