// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';
import type {ScreenShareEncoderVerificationAction} from '@app/features/voice/utils/CodecCapabilityDetector';
import type {ScreenShareAudioSummaryInput} from '@app/features/voice/utils/ScreenShareAudioSummary';
import {BackupCodecPolicy, ParticipantEvent, type Room, Track, type VideoCodec} from 'livekit-client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const pushWithKey = vi.fn();
const publishLocalCapabilities = vi.fn(async () => null);
const refreshSelection = vi.fn(async () => null);
const selectScreenShareCodec = vi.fn((): VideoCodec => 'vp8');
const markRuntimeFailure = vi.fn();
let selfStream = true;
let codecDenial: string | null = null;
let vp8Available = false;
let av1Available = false;
let knownDecode: Array<Set<VideoCodec>> = [];
let unknownParticipants = 0;
let preferenceOrder: Array<VideoCodec> = ['h264'];
let selectedCodec: VideoCodec | null = 'vp9';
let stallAction: ScreenShareEncoderVerificationAction = {kind: 'recover-stalled', codec: 'h264'};
let menuEntitled = true;
const gpuReport: {gpuLabel: string} | null = null;
const gpuReportLoad: Promise<unknown> = Promise.resolve(null);
let displayShareEnvironment: 'web' | 'desktop-custom' | 'desktop-wayland' = 'web';
const voiceConnectionContext: {guildId: string | null; channelId: string | null; connectionId: string | null} | null =
	null;
const voiceStates: Record<string, Record<string, Record<string, unknown>>> = {};
const settingsUpdate = vi.fn();
const openPremiumModal = vi.fn();

vi.mock('@app/features/voice/utils/GpuEncoderCapabilities', () => ({
	getGpuEncoderReportSync: () => gpuReport,
	loadGpuEncoderReport: () => gpuReportLoad,
}));

vi.mock('@app/features/voice/utils/ScreenShareEnvironment', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/features/voice/utils/ScreenShareEnvironment')>()),
	getDisplayShareEnvironment: async () => displayShareEnvironment,
}));

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({
		i18n: {
			locale: 'en-GB',
			_: (descriptor: {message?: string}, values?: Record<string, unknown>) =>
				(descriptor.message ?? '').replace(/{(\w+)}/g, (token, key: string) =>
					values && key in values ? String(values[key]) : token,
				),
			number: (value: number) => String(value),
		},
	}),
}));

vi.mock('@app/app/I18n', () => ({default: {_: (descriptor: {message?: string}) => descriptor.message ?? ''}}));

vi.mock('@app/features/app/components/alerts/GenericErrorModal', () => ({GenericErrorModal: () => null}));

vi.mock('@app/features/voice/components/StreamKeys', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/features/voice/components/StreamKeys')>()),
	getStreamKey: () => 'stream',
}));

vi.mock('@app/features/voice/engine/VoiceMediaGraphStore', () => ({
	voiceMediaGraphStore: {getGraphSnapshot: () => null},
}));

vi.mock('@app/features/voice/engine/VoiceMediaGraph', () => ({
	normalizeVoiceMediaGraphViewerStreamKeys: () => [],
	selectVoiceMediaGraphViewerStreamKeys: () => [],
}));

vi.mock('@app/features/voice/engine/VoiceStreamWatchState', () => ({
	addWatchedStreamKey: () => undefined,
	stopWatchingStreamKey: () => undefined,
}));

vi.mock('@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareStateSync', () => ({
	applyVoiceEngineV2AppScreenShareState: () => undefined,
}));

vi.mock('@app/features/voice/engine/voice_screen_share_manager/NativePermissionGate', () => ({
	ensureNativeCameraPermissionForDeviceShare: async () => undefined,
	ensureNativeMicrophonePermissionForDeviceShare: async () => undefined,
}));

vi.mock('@app/features/voice/state/LocalVoiceState', () => ({
	default: {
		getSelfStream: () => selfStream,
		updateSelfStreamAudio: () => undefined,
		getViewerStreamKeys: () => [],
	},
}));

vi.mock('@app/features/voice/utils/LinuxScreenShareAudio', () => ({disarmVirtmic: () => undefined}));

vi.mock('@app/features/voice/utils/NativeAudioCaptureBridge', () => ({
	armNativeAudioForLinuxRouting: async () => false,
	armNativeAudioForNextCapture: async () => false,
	armNativeSystemAudioForNextCapture: async () => false,
	captureNativeAudioTrackForLinuxRouting: async () => null,
	captureNativeAudioTrackForWindowPid: async () => null,
	commitNativeAudioBridgeReplacement: () => undefined,
	disarmNativeAudio: () => undefined,
	disarmPendingNativeAudio: () => undefined,
	getLastNativeAudioArmFailure: () => null,
	getNativeAudioAvailabilityCached: async () => null,
	getNativeAudioAvailabilitySnapshot: () => null,
	reconfigureLinuxNativeAudioRouting: async () => 'unsupported',
}));

vi.mock('@app/features/voice/utils/ScreenShareUtils', () => ({
	executeScreenShareOperation: async (operation: () => Promise<void>) => {
		await operation();
	},
	handleScreenShareError: () => undefined,
}));

vi.mock('@app/features/voice/engine/MediaEngineFacade', () => ({default: {}, useMediaEngineVersion: () => 0}));

vi.mock('@app/features/voice/engine/VoiceMediaEngineBridge', () => ({
	getAllVoiceStatesFromMediaEngine: () => voiceStates,
	getVoiceConnectionContextFromMediaEngine: () => voiceConnectionContext,
	updateLocalParticipantFromRoom: () => undefined,
}));

vi.mock('@app/features/auth/state/Authentication', () => ({default: {currentUserId: 'publisher'}}));

vi.mock('@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareLiveKitFlows', () => ({
	VoiceEngineV2AppScreenShareLiveKitFlows: class {
		async republishActiveShareWithCodec(): Promise<boolean> {
			return true;
		}
	},
}));

vi.mock('@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareControllerRouting', () => ({
	selectVoiceEngineV2AppScreenShareSetEnabledOptions: () => ({}),
	VoiceEngineV2AppScreenShareControllerRouting: class {},
}));

vi.mock('@app/features/ui/commands/ModalCommands', () => ({
	modal: (render: unknown) => render,
	pushWithKey: (...args: Array<unknown>) => pushWithKey(...args),
}));

vi.mock('@app/features/ui/commands/SoundCommands', () => ({playSound: () => undefined}));

vi.mock('@app/features/ui/commands/ToastCommands', () => ({createToast: () => undefined}));

vi.mock('@app/features/voice/commands/VoiceSettingsCommands', () => ({
	update: (...args: Array<unknown>) => settingsUpdate(...args),
}));

vi.mock('@app/features/premium/commands/PremiumModalCommands', () => ({open: () => openPremiumModal()}));

vi.mock('@app/features/ui/action_menu/ContextMenu', async () => {
	const React = await import('react');
	const box = ({children}: {children?: React.ReactNode}) => React.createElement('div', null, children);
	return {
		CheckboxItem: box,
		MenuGroup: box,
		MenuGroupLabel: box,
		MenuItem: box,
		MenuSeparator: () => null,
		SubMenu: ({label, render}: {label: string; render: () => React.ReactNode}) =>
			React.createElement('div', {'data-submenu': label}, render()),
		SelectableMenuItem: ({
			children,
			selected,
			textValue,
			onAction,
		}: {
			children?: React.ReactNode;
			selected: boolean;
			textValue: string;
			onAction: () => void;
		}) =>
			React.createElement(
				'button',
				{type: 'button', 'data-selected': String(selected), 'data-label': textValue, onClick: onAction},
				children,
			),
		useContextMenuClose: () => () => undefined,
	};
});

vi.mock('@app/features/voice/components/modals/CameraPreviewModal', () => ({
	CameraPreviewModalStandalone: () => null,
}));

vi.mock('@app/features/user/components/modals/tabs/hooks/useMediaPermission', () => ({
	useMediaPermission: () => ({
		devices: [{deviceId: 'camera-1', kind: 'videoinput', label: 'Camera one'}],
		deviceState: {
			inputDevices: [],
			outputDevices: [],
			videoDevices: [{deviceId: 'camera-1', kind: 'videoinput', label: 'Camera one'}],
			permissionStatus: {audio: 'granted', video: 'granted'},
		},
		status: 'granted',
		requestPermission: async () => true,
	}),
}));

vi.mock('@app/features/user/components/modals/tabs/components/CompactComboboxRow', async () => {
	const React = await import('react');
	return {
		CompactComboboxRow: ({
			dataFlx,
			onChange,
			options,
			value,
		}: {
			dataFlx: string;
			onChange: (value: unknown) => void;
			options: ReadonlyArray<{label: string; value: unknown}>;
			value: unknown;
		}) =>
			React.createElement(
				'div',
				{
					'data-combobox': dataFlx,
					'data-value': String(value),
					'data-options': options.map((option) => option.label).join(','),
				},
				options.map((option) =>
					React.createElement('button', {
						key: String(option.value),
						type: 'button',
						'data-option': String(option.value),
						onClick: () => onChange(option.value),
					}),
				),
			),
	};
});

vi.mock('@app/features/voice/utils/VideoQualityEntitlement', () => ({hasHigherVideoQuality: () => menuEntitled}));

vi.mock('@app/features/voice/engine/ScreenShareCodecNegotiation', () => ({
	default: {
		getRemoteDecodeInputs: () => ({knownDecode, unknownParticipants}),
		getSelectedCodec: () => selectedCodec,
		publishLocalCapabilities: () => publishLocalCapabilities(),
		refreshSelection: () => refreshSelection(),
		selectScreenShareCodec: () => selectScreenShareCodec(),
	},
	getScreenShareCodecPreferenceOrder: () => preferenceOrder,
}));

vi.mock('@app/features/voice/utils/CodecCapabilityDetector', () => {
	const unavailable = {allowed: false, supported: false, hardware: false};
	return {
		LIVEKIT_SUPPORTED_CODECS: ['vp8', 'h264', 'vp9', 'av1', 'h265'],
		buildScreenShareCodecProfile: () => ({
			browser: 'chromium',
			desktop: true,
			codecs: {
				av1: {allowed: av1Available, supported: av1Available, hardware: false},
				h265: unavailable,
				h264: {allowed: true, supported: true, hardware: true},
				vp9: unavailable,
				vp8: {allowed: vp8Available, supported: vp8Available, hardware: false},
			},
		}),
		findVideoPublishCodecPolicyViolation: () => null,
		getCodecCapabilityReport: () => ({h264: {hardwareAccelerated: 'hardware'}}),
		getVideoPublishCodecDenial: () => codecDenial,
		markScreenShareCodecEncodeRuntimeFailure: (...args: Array<unknown>) => markRuntimeFailure(...args),
		markScreenShareCodecSoftwareEncodeObserved: () => undefined,
		resolveScreenShareEncoderVerificationAction: () => stallAction,
		resolveVideoPublishCodecPolicy: (requested: string) => ({
			allowed: [requested, 'h264'],
			requested,
			primary: requested,
			backupCodec: requested === 'h264' || requested === 'vp8' ? false : {codec: 'h264'},
		}),
	};
});

const BOOTSTRAP_ENDPOINT = 'https://primary.test/api';

(globalThis.window as unknown as Record<string, unknown>).__FLUXER_BOOTSTRAP__ = {
	config: {
		releaseChannel: 'stable',
		bootstrapApiEndpoint: BOOTSTRAP_ENDPOINT,
		bootstrapApiPublicEndpoint: BOOTSTRAP_ENDPOINT,
	},
	instance: {
		api_code_version: Number.MAX_SAFE_INTEGER,
		endpoints: {
			api: BOOTSTRAP_ENDPOINT,
			api_client: BOOTSTRAP_ENDPOINT,
			api_public: BOOTSTRAP_ENDPOINT,
			gateway: 'wss://gateway.primary.test',
			media: 'https://media.primary.test',
			static_cdn: 'https://cdn.primary.test',
			marketing: 'https://primary.test',
			admin: 'https://admin.primary.test',
			invite: 'https://primary.test/invite',
			gift: 'https://primary.test/gift',
			webapp: 'https://app.primary.test',
			upload_relay: 'https://upload.primary.test',
		},
		captcha: {provider: 'none', hcaptcha_site_key: null, turnstile_site_key: null},
		features: {
			voice_enabled: false,
			stripe_enabled: false,
			self_hosted: false,
			presigned_attachment_uploads: false,
			emails_enabled: false,
		},
		gif: {provider: 'klipy', display_name: 'Klipy', attribution_required: false},
		sso: {enabled: false, enforced: false, display_name: null, redirect_uri: ''},
		registration: {mode: 'open', admin_registration_urls_enabled: true},
		community: {single_community: false, single_community_guild_id: null, direct_messages_disabled: false},
		services: {gif_enabled: true, youtube_enabled: false, bluesky_enabled: false},
		limits: undefined,
		push: {public_vapid_key: null},
		app_public: {
			branding: {
				product_name: 'Fluxer',
				icon_url: null,
				symbol_url: null,
				logo_url: null,
				wordmark_url: null,
				favicon_url: null,
				theme_color: null,
			},
			setup: {configured: true, admin_url: null},
			legal: {terms_url: null, privacy_url: null},
			registration: {collect_date_of_birth: true},
		},
	},
	geoip: {
		countryCode: null,
		regionCode: null,
		latitude: null,
		longitude: null,
		ageRestrictedGeos: [],
		ageBlockedGeos: [],
	},
};

const {VoiceEngineV2AppScreenShareExecutionAdapter, shouldRestoreScreenShareAfterReconnect} = await import(
	'@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareExecutionAdapter'
);
const {default: ActiveScreenShareSource} = await import('@app/features/voice/state/ActiveScreenShareSource');
const {default: AppStorage} = await import('@app/features/platform/state/PersistentStorage');
const {
	applyScreenShareBackupCodecModeRetiredMigrationV1,
	applyScreenShareSoftwareQualityRetiredMigrationV1,
	default: VoiceSettings,
} = await import('@app/features/voice/state/VoiceSettings');
const {recordScreenShareStarted, resetRecentScreenSharesForTests} = await import(
	'@app/features/voice/utils/ScreenShareLifecycleLog'
);
const {ensureCommittedScreenShareTarget, getEffectivePublishOptions, resolveConfiguredScreenShareTarget} = await import(
	'@app/features/voice/engine/voice_screen_share_manager/shared'
);
const {resolveScreenShareDegradationPreference} = await import('@app/features/voice/utils/ScreenShareOptions');
const {rankScreenShareCodecs} = await import('@app/features/voice/utils/ScreenShareCodecSelection');
const {voiceVideoIndex} = await import('@app/features/user/components/settings_utils/search_index/VoiceVideoIndex');

const FIRST_TICK_MS = 2500;
const TICK_MS = 2000;

interface StatsFeed {
	encodedFps: number;
	sourceFps: number;
	targetBitrate: number;
	active: boolean;
	sentWidth: number;
	sentHeight: number;
}

interface FakeSender {
	sender: RTCRtpSender;
	setParameters: ReturnType<typeof vi.fn>;
	getStats: ReturnType<typeof vi.fn>;
	getParameters: ReturnType<typeof vi.fn>;
	current: () => RTCRtpSendParameters;
}

function createStatsReader(feed: StatsFeed): () => RTCStatsReport {
	let last = Date.now();
	let framesEncoded = 0;
	let sourceFrames = 0;
	let bytesSent = 0;
	return () => {
		const now = Date.now();
		const seconds = (now - last) / 1000;
		last = now;
		framesEncoded += Math.round(feed.encodedFps * seconds);
		sourceFrames += Math.round(feed.sourceFps * seconds);
		bytesSent += Math.round((feed.targetBitrate * seconds) / 8);
		const entries: Array<Record<string, unknown>> = [
			{type: 'codec', id: 'codec', mimeType: 'video/H264'},
			{
				type: 'media-source',
				id: 'source',
				kind: 'video',
				frames: sourceFrames,
				framesPerSecond: feed.sourceFps,
				width: 3840,
				height: 2160,
			},
			{
				type: 'outbound-rtp',
				id: 'outbound',
				kind: 'video',
				codecId: 'codec',
				mediaSourceId: 'source',
				active: feed.active,
				framesEncoded,
				framesSent: framesEncoded,
				bytesSent,
				headerBytesSent: 0,
				frameWidth: feed.sentWidth,
				frameHeight: feed.sentHeight,
				targetBitrate: feed.targetBitrate,
				qualityLimitationReason: 'none',
				encoderImplementation: 'NVENC',
				timestamp: now,
			},
		];
		return new Map(entries.map((entry) => [entry.id as string, entry])) as unknown as RTCStatsReport;
	};
}

function createSender(
	feed: StatsFeed,
	options: {mimeType?: string; encodings?: Array<RTCRtpEncodingParameters>} = {},
): FakeSender {
	let params = {
		codecs: [{mimeType: options.mimeType ?? 'video/H264'}],
		encodings: options.encodings ?? [{scalabilityMode: 'L1T3'}],
	} as unknown as RTCRtpSendParameters;
	const getParameters = vi.fn(() => structuredClone(params));
	const readStats = createStatsReader(feed);
	const setParameters = vi.fn(async (next: RTCRtpSendParameters) => {
		params = next;
	});
	const getStats = vi.fn(async () => readStats());
	const sender = {
		track: {getSettings: () => ({width: 3840, height: 2160}), contentHint: ''},
		getParameters,
		setParameters,
		getStats,
	} as unknown as RTCRtpSender;
	return {sender, setParameters, getStats, getParameters, current: () => params};
}

function createShare(sender: RTCRtpSender) {
	const applyConstraints = vi.fn(async () => undefined);
	const track = Object.assign(new EventEmitter(), {
		sender,
		mediaStreamTrack: Object.assign(new EventTarget(), {
			contentHint: '',
			readyState: 'live',
			getConstraints: () => ({}),
			applyConstraints,
		}),
		simulcastCodecs: new Map<string, {sender: RTCRtpSender}>(),
		attach: () => undefined,
		detach: () => undefined,
		setDegradationPreference: async () => undefined,
		getProcessor: () => null,
	});
	const publication = {source: Track.Source.ScreenShare, videoTrack: track, track, options: {videoCodec: 'h264'}};
	const participant = Object.assign(new EventEmitter(), {
		isScreenShareEnabled: true,
		trackPublications: new Map([['screen', publication]]),
		getTrackPublication: (source: Track.Source) => (source === Track.Source.ScreenShare ? publication : undefined),
		publishTrack: vi.fn(async () => publication),
		unpublishTrack: vi.fn(async () => undefined),
		setScreenShareEnabled: vi.fn(async () => undefined),
	});
	const room = {localParticipant: participant, on: () => undefined, off: () => undefined} as unknown as Room;
	return {track, publication, participant, room, applyConstraints};
}

function createAdapter() {
	const adapter = new VoiceEngineV2AppScreenShareExecutionAdapter();
	const republish = vi.spyOn(adapter.liveKitFlows, 'republishActiveShareWithCodec').mockResolvedValue(true);
	const setEnabled = vi.spyOn(adapter, 'setScreenShareEnabled').mockResolvedValue(undefined);
	return {adapter, republish, setEnabled};
}

function sentFrameRate(fake: FakeSender): number | undefined {
	return fake.current().encodings[0].maxFramerate;
}

describe('the screen share wiring', () => {
	let feed: StatsFeed;

	beforeEach(() => {
		vi.useFakeTimers();
		feed = {
			encodedFps: 30,
			sourceFps: 30,
			targetBitrate: 4_500_000,
			active: true,
			sentWidth: 1920,
			sentHeight: 1080,
		};
		codecDenial = null;
		vp8Available = false;
		av1Available = false;
		knownDecode = [];
		unknownParticipants = 0;
		preferenceOrder = ['h264'];
		selectedCodec = 'vp9';
		stallAction = {kind: 'recover-stalled', codec: 'h264'};
		pushWithKey.mockClear();
		publishLocalCapabilities.mockReset();
		publishLocalCapabilities.mockImplementation(async () => null);
		selectScreenShareCodec.mockReset();
		selectScreenShareCodec.mockImplementation(() => 'vp8');
		markRuntimeFailure.mockReset();
		vi.spyOn(VoiceSettings, 'getStreamingMode').mockReturnValue('custom');
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue('high');
		vi.spyOn(VoiceSettings, 'getVideoFrameRate').mockReturnValue(30);
		vi.spyOn(VoiceSettings, 'getScreenShareContentHint').mockReturnValue('text');
		vi.spyOn(VoiceSettings, 'getPreferredScreenShareCodec').mockReturnValue('auto');
		resetRecentScreenSharesForTests();
		recordScreenShareStarted();
		ActiveScreenShareSource.setPublishedSource('display', 'screen:1');
		ActiveScreenShareSource.setSourceDimensions({width: 3840, height: 2160});
		ensureCommittedScreenShareTarget();
	});

	afterEach(() => {
		vi.useRealTimers();
		vi.restoreAllMocks();
		ActiveScreenShareSource.clear();
	});

	it('keeps the committed target across a reconnect restore', () => {
		const primary = createSender(feed);
		const {room} = createShare(primary.sender);
		const {adapter} = createAdapter();
		const committed = ActiveScreenShareSource.getTarget();
		const snapshot = adapter.prepareScreenShareReconnect(room);
		expect(snapshot).not.toBeNull();
		expect(shouldRestoreScreenShareAfterReconnect(false, snapshot)).toBe(true);
		expect(ActiveScreenShareSource.getTarget()).toEqual(committed);
	});

	it('recovers a stalled encoder once and stops the share when it stalls again', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, republish, setEnabled} = createAdapter();
		feed.encodedFps = 0;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(publishLocalCapabilities).toHaveBeenCalledTimes(1);
		expect(setEnabled).not.toHaveBeenCalled();
		expect(pushWithKey).not.toHaveBeenCalled();
		republish.mockClear();
		stallAction = {kind: 'stop-stalled', codec: 'h264'};
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(setEnabled).toHaveBeenCalledWith(room, false, {sendUpdate: true, playSound: true});
		expect(pushWithKey).toHaveBeenCalledTimes(1);
		adapter.cancelEncoderVerificationInternal();
	});

	it('keeps verifying until a sample shows frames, and reports encoding only while they keep coming', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 0;
		feed.sourceFps = 0;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(ActiveScreenShareSource.encoding).toBe(false);
		expect(publishLocalCapabilities).not.toHaveBeenCalled();
		feed.sourceFps = 30;
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(publishLocalCapabilities).toHaveBeenCalledTimes(1);
		expect(ActiveScreenShareSource.encoding).toBe(false);
		adapter.cancelEncoderVerificationInternal();
	});

	it('reports the share as encoding only once frames arrive and stops when they stop', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(ActiveScreenShareSource.encoding).toBe(false);
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(ActiveScreenShareSource.encoding).toBe(true);
		feed.encodedFps = 0;
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(ActiveScreenShareSource.encoding).toBe(false);
		adapter.cancelEncoderVerificationInternal();
	});

	it('re-enforces the sender parameters on every tick', async () => {
		const first = createSender(feed);
		const second = createSender(feed);
		const {room, participant, track} = createShare(first.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(first.current().degradationPreference).toBe('maintain-resolution');
		expect(first.current().encodings).toEqual([
			{maxBitrate: 4_500_000, maxFramerate: 30, scaleResolutionDownBy: 2, priority: 'high', networkPriority: 'high'},
		]);
		track.sender = second.sender;
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(second.current().degradationPreference).toBe('maintain-resolution');
		expect(sentFrameRate(second)).toBe(30);
		adapter.cancelEncoderVerificationInternal();
	});

	it('enforces a backup sender created later and on every tick after it', async () => {
		const primary = createSender(feed);
		const later = createSender(feed, {mimeType: 'video/VP9'});
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		await adapter.enforceScreenShareSenderParametersInternal(participant as never, {videoCodec: 'h264'});
		participant.emit(ParticipantEvent.LocalSenderCreated, later.sender, track, 'h264');
		await vi.advanceTimersByTimeAsync(0);
		expect(later.current().degradationPreference).toBe('maintain-resolution');
		expect(later.current().encodings).toEqual([
			{maxBitrate: 4_500_000, maxFramerate: 30, scaleResolutionDownBy: 2, priority: 'high', networkPriority: 'high'},
		]);
		track.simulcastCodecs.set('h264', {sender: later.sender});
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		later.current().encodings[0].maxFramerate = 60;
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(sentFrameRate(later)).toBe(30);
		adapter.cancelEncoderVerificationInternal();
	});

	it('leaves an encoding that is not active alone', async () => {
		const primary = createSender(feed, {
			encodings: [{rid: 'q', active: false, scaleResolutionDownBy: 4}, {rid: 'f'}],
		});
		const {participant} = createShare(primary.sender);
		const {adapter} = createAdapter();
		await adapter.enforceScreenShareSenderParametersInternal(participant as never, {videoCodec: 'h264'});
		expect(primary.current().encodings[0]).toEqual({rid: 'q', active: false, scaleResolutionDownBy: 4});
		expect(primary.current().encodings[1]).toMatchObject({rid: 'f', maxFramerate: 30, priority: 'high'});
	});

	it('never lets a screen share trade resolution for frame rate', () => {
		expect(resolveScreenShareDegradationPreference(resolveConfiguredScreenShareTarget('display', null))).toBe(
			'maintain-resolution',
		);
		expect(resolveScreenShareDegradationPreference(resolveConfiguredScreenShareTarget('app', null))).toBe(
			'maintain-resolution',
		);
		expect(resolveScreenShareDegradationPreference(resolveConfiguredScreenShareTarget('device', null))).toBe(
			'balanced',
		);
	});

	it('drops a codec the sender cannot encode instead of flooring the order to it', () => {
		const entry = (supported: boolean) => ({allowed: supported, supported, hardware: false});
		const {order} = rankScreenShareCodecs({
			profile: {
				browser: 'chromium',
				desktop: true,
				codecs: {vp8: entry(true), vp9: entry(true), h264: entry(false), h265: entry(false), av1: entry(true)},
			},
			encoderModeSetting: 'auto',
			pin: 'auto',
		});
		expect(order).toEqual(['av1', 'vp9', 'vp8']);
	});

	it('ranks hardware encoders ahead of software ones unless software encoding is forced', () => {
		const entry = (hardware: boolean) => ({allowed: true, supported: true, hardware});
		const profile = {
			browser: 'chromium' as const,
			desktop: true,
			codecs: {vp8: entry(false), vp9: entry(false), h264: entry(true), h265: entry(true), av1: entry(false)},
		};
		expect(rankScreenShareCodecs({profile, encoderModeSetting: 'auto', pin: 'auto'}).order).toEqual([
			'h265',
			'h264',
			'av1',
			'vp9',
			'vp8',
		]);
		expect(rankScreenShareCodecs({profile, encoderModeSetting: 'software', pin: 'auto'}).order).toEqual([
			'av1',
			'h264',
			'vp9',
			'vp8',
		]);
	});

	it('refuses to publish h264 when the sender cannot encode it', async () => {
		const detector = await vi.importActual<typeof import('@app/features/voice/utils/CodecCapabilityDetector')>(
			'@app/features/voice/utils/CodecCapabilityDetector',
		);
		const capabilities = {codecs: [{mimeType: 'video/vp8'}, {mimeType: 'video/vp9'}]};
		const host = globalThis as Record<string, unknown>;
		host.RTCRtpSender = {getCapabilities: () => capabilities};
		host.RTCRtpReceiver = {getCapabilities: () => capabilities};
		try {
			detector.resetCachedCodecCapabilities();
			expect(detector.getVideoPublishCodecDenial('h264')).toBe('sender-cannot-encode');
			expect(detector.getAllowedVideoPublishCodecs()).not.toContain('h264');
			expect(detector.resolveVideoPublishCodecPolicy('h264').primary).toBe('vp9');
			expect(detector.resolveVideoPublishCodecPolicy('vp9').backupCodec).toBe(false);
		} finally {
			detector.resetCachedCodecCapabilities();
			Reflect.deleteProperty(host, 'RTCRtpSender');
			Reflect.deleteProperty(host, 'RTCRtpReceiver');
		}
	});

	it('refuses to publish a codec the decode probe excluded', async () => {
		const decoder = await vi.importActual<typeof import('@app/features/voice/utils/VideoDecoderCapabilities')>(
			'@app/features/voice/utils/VideoDecoderCapabilities',
		);
		const detector = await vi.importActual<typeof import('@app/features/voice/utils/CodecCapabilityDetector')>(
			'@app/features/voice/utils/CodecCapabilityDetector',
		);
		const capabilities = {codecs: [{mimeType: 'video/h265'}, {mimeType: 'video/h264'}]};
		const host = globalThis as Record<string, unknown>;
		host.RTCRtpSender = {getCapabilities: () => capabilities};
		host.RTCRtpReceiver = {getCapabilities: () => capabilities};
		vi.spyOn(VoiceSettings, 'getScreenShareHevcOptIn').mockReturnValue(true);
		try {
			detector.resetCachedCodecCapabilities();
			expect(detector.isVideoCodecAllowedForPublish('h265')).toBe(true);
			expect(decoder.markScreenShareDecodeFailure('h265', 'decode stall')).toBe(true);
			detector.resetCachedCodecCapabilities();
			expect(detector.getVideoPublishCodecDenial('h265')).toBe('decoder-excluded');
			expect(detector.isVideoCodecAllowedForPublish('h265')).toBe(false);
			expect(detector.isVideoCodecAllowedForPublish('h264')).toBe(true);
			expect(detector.getAllowedVideoPublishCodecs()).toContain('h264');
			expect(detector.resolveVideoPublishCodecPolicy('h265').primary).not.toBe('h265');
		} finally {
			decoder.resetVideoDecoderExclusions();
			detector.resetCachedCodecCapabilities();
			Reflect.deleteProperty(host, 'RTCRtpSender');
			Reflect.deleteProperty(host, 'RTCRtpReceiver');
		}
	});
});

describe('the retired software encoder quality', () => {
	it('turns a stored real-time quality with automatic layering into a single layer, once', () => {
		const unset: Record<string, unknown> = {screenShareSoftwareQualityPrefV2: 'realtime'};
		expect(applyScreenShareSoftwareQualityRetiredMigrationV1(unset)).toBe(true);
		expect(unset.screenShareScalabilityModePrefV2).toBe('single_layer');

		const automatic: Record<string, unknown> = {
			screenShareSoftwareQualityPrefV2: 'realtime',
			screenShareScalabilityModePrefV2: 'auto',
		};
		expect(applyScreenShareSoftwareQualityRetiredMigrationV1(automatic)).toBe(true);
		expect(automatic.screenShareScalabilityModePrefV2).toBe('single_layer');
		expect(applyScreenShareSoftwareQualityRetiredMigrationV1(automatic)).toBe(false);
		expect(automatic.screenShareScalabilityModePrefV2).toBe('single_layer');
	});

	it('reads a stored spatial layering as automatic and moves it too', () => {
		const spatial: Record<string, unknown> = {
			screenShareSoftwareQualityPrefV2: 'realtime',
			screenShareScalabilityModePrefV2: 'spatial',
		};
		expect(applyScreenShareSoftwareQualityRetiredMigrationV1(spatial)).toBe(true);
		expect(spatial.screenShareScalabilityModePrefV2).toBe('single_layer');
	});

	it('leaves stored temporal layers and every other quality alone', () => {
		const temporal: Record<string, unknown> = {
			screenShareSoftwareQualityPrefV2: 'realtime',
			screenShareScalabilityModePrefV2: 'temporal',
		};
		expect(applyScreenShareSoftwareQualityRetiredMigrationV1(temporal)).toBe(true);
		expect(temporal.screenShareScalabilityModePrefV2).toBe('temporal');

		const balanced: Record<string, unknown> = {screenShareSoftwareQualityPrefV2: 'balanced'};
		expect(applyScreenShareSoftwareQualityRetiredMigrationV1(balanced)).toBe(true);
		expect(balanced.screenShareScalabilityModePrefV2).toBeUndefined();
	});

	it('drops the stored quality, even for a store the earlier migration already flagged', () => {
		const fresh: Record<string, unknown> = {screenShareSoftwareQualityPrefV2: 'quality'};
		expect(applyScreenShareSoftwareQualityRetiredMigrationV1(fresh)).toBe(true);
		expect(fresh.screenShareSoftwareQualityPrefV2).toBeUndefined();

		const flagged: Record<string, unknown> = {
			screenShareSoftwareQualityPrefV2: 'quality',
			screenShareSoftwareQualityRetiredV1: true,
		};
		expect(applyScreenShareSoftwareQualityRetiredMigrationV1(flagged)).toBe(true);
		expect(flagged.screenShareSoftwareQualityPrefV2).toBeUndefined();
		expect(applyScreenShareSoftwareQualityRetiredMigrationV1(flagged)).toBe(false);
	});

	it('leaves the store with no quality field and no reader for it', () => {
		expect(VoiceSettings).not.toHaveProperty('screenShareSoftwareQualityPrefV2');
		expect(VoiceSettings).not.toHaveProperty('getScreenShareSoftwareQuality');
		expect(VoiceSettings).not.toHaveProperty('getScreenShareSoftwareQualityOverride');
	});
});

describe('the retired H.264 backup stream mode', () => {
	it('drops the stored mode once', () => {
		const stored: Record<string, unknown> = {screenShareBackupCodecModePrefV2: 'h264_simulcast'};
		expect(applyScreenShareBackupCodecModeRetiredMigrationV1(stored)).toBe(true);
		expect(stored.screenShareBackupCodecModePrefV2).toBeUndefined();
		expect(applyScreenShareBackupCodecModeRetiredMigrationV1(stored)).toBe(false);

		const off: Record<string, unknown> = {screenShareBackupCodecModePrefV2: 'off'};
		expect(applyScreenShareBackupCodecModeRetiredMigrationV1(off)).toBe(true);
		expect(off.screenShareBackupCodecModePrefV2).toBeUndefined();

		const untouched: Record<string, unknown> = {screenShareScalabilityModePrefV2: 'temporal'};
		expect(applyScreenShareBackupCodecModeRetiredMigrationV1(untouched)).toBe(false);
		expect(untouched.screenShareScalabilityModePrefV2).toBe('temporal');
	});

	it('leaves the store with no mode field and no reader for it', () => {
		expect(VoiceSettings).not.toHaveProperty('screenShareBackupCodecModePrefV2');
		expect(VoiceSettings).not.toHaveProperty('getScreenShareBackupCodecMode');
		expect(VoiceSettings).not.toHaveProperty('getScreenShareBackupCodecModeOverride');
	});

	it('still publishes the automatic H.264 backup simulcast for an SVC primary', async () => {
		const svc = await getEffectivePublishOptions(true, {videoCodec: 'av1'});
		expect(svc?.backupCodec).toEqual({codec: 'h264'});
		expect(svc?.backupCodecPolicy).toBe(BackupCodecPolicy.SIMULCAST);

		const h264 = await getEffectivePublishOptions(true, {videoCodec: 'h264'});
		expect(h264?.backupCodec).toBe(false);
		expect(h264?.backupCodecPolicy).toBeUndefined();
	});
});

describe('the retired screen share settings on boot', () => {
	it('drops both stored keys when the store migrates its persisted settings', () => {
		const previous = AppStorage.getItem('VoiceSettings');
		AppStorage.setItem(
			'VoiceSettings',
			JSON.stringify({
				screenShareSoftwareQualityPrefV2: 'quality',
				screenShareBackupCodecModePrefV2: 'h264_simulcast',
			}),
		);
		(VoiceSettings as unknown as {migratePersistedSettings: () => void}).migratePersistedSettings();
		const stored = JSON.parse(AppStorage.getItem('VoiceSettings') ?? '{}') as Record<string, unknown>;
		expect(stored).not.toHaveProperty('screenShareSoftwareQualityPrefV2');
		expect(stored).not.toHaveProperty('screenShareBackupCodecModePrefV2');
		if (previous === null) {
			AppStorage.removeItem('VoiceSettings');
		} else {
			AppStorage.setItem('VoiceSettings', previous);
		}
	});
});

describe('the retired screen share settings in the settings search index', () => {
	it('no longer sends a backup stream search to the advanced video section', () => {
		const entry = voiceVideoIndex.find((candidate) => candidate.id === 'voice-video-screen-share-encoder-controls');
		expect(entry?.description).toEqual({
			message: 'Encoder, SVC, and bitrate presets',
			comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
		});
	});
});

const {resolveUserVideoTabScreenShareState, SCREEN_SHARE_PRESET_DESCRIPTORS} = await import(
	'@app/features/user/components/modals/tabs/UserVideoTabState'
);
const {selectScreenShareEncoderPathDescription} = await import(
	'@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedVideoControlsState'
);
const {resolveScreenShareQualityPick, resolveScreenShareTarget, SUPPORTED_SCREEN_SHARE_FRAME_RATES} = await import(
	'@app/features/voice/utils/ScreenShareOptions'
);

describe('the video settings tab', () => {
	const FREE_RESOLUTION_OPTIONS = [
		{value: 'low_480p', isDisabled: false},
		{value: 'medium', isDisabled: false},
		{value: 'high', isDisabled: true},
		{value: 'ultra', isDisabled: true},
		{value: 'source', isDisabled: true},
	];
	const PREMIUM_RESOLUTION_OPTIONS = [
		{value: 'low_480p', isDisabled: false},
		{value: 'medium', isDisabled: false},
		{value: 'high', isDisabled: false},
		{value: 'ultra', isDisabled: false},
		{value: 'source', isDisabled: false},
	];
	const stateOf = (quality: Parameters<typeof resolveScreenShareQualityPick>[0], selfHosted = false) =>
		resolveUserVideoTabScreenShareState({quality, hintSetting: 'auto', selfHosted});

	it('shows a free account the clamped values, the saved ones and no preset note', () => {
		const quality = {
			mode: 'custom',
			storedResolution: 'source',
			storedFrameRate: 30,
			entitled: false,
			context: 'display',
		} as const;
		expect(stateOf(quality)).toEqual({
			resolution: 'medium',
			frameRate: 30,
			resolutionOptions: FREE_RESOLUTION_OPTIONS,
			frameRateOptions: [15, 30],
			preset: null,
			presetOverriddenByContext: false,
			saved: {resolution: 'source', frameRate: 30},
		});
	});

	it('shows a premium Screen share account what the preset sends and writes nothing for those values', () => {
		const quality = {
			mode: 'screenshare',
			storedResolution: 'medium',
			storedFrameRate: 30,
			entitled: true,
			context: 'display',
		} as const;
		expect(stateOf(quality)).toEqual({
			resolution: 'source',
			frameRate: 15,
			resolutionOptions: PREMIUM_RESOLUTION_OPTIONS,
			frameRateOptions: [15, 30, 60],
			preset: 'screenshare',
			presetOverriddenByContext: false,
			saved: null,
		});
		expect(resolveScreenShareQualityPick(quality, {axis: 'resolution', resolution: 'source'})).toBeNull();
		expect(resolveScreenShareQualityPick(quality, {axis: 'frameRate', frameRate: 15})).toBeNull();
	});

	it('names the preset that owns a free Gaming account and keeps the saved values out of the note', () => {
		expect(
			stateOf({
				mode: 'gaming',
				storedResolution: 'high',
				storedFrameRate: 60,
				entitled: false,
				context: 'display',
			}),
		).toEqual({
			resolution: 'medium',
			frameRate: 30,
			resolutionOptions: FREE_RESOLUTION_OPTIONS,
			frameRateOptions: [15, 30],
			preset: 'gaming',
			presetOverriddenByContext: false,
			saved: null,
		});
	});

	it('offers a self-hosted free account nothing it cannot send', () => {
		expect(
			stateOf(
				{mode: 'custom', storedResolution: 'medium', storedFrameRate: 30, entitled: false, context: 'display'},
				true,
			),
		).toMatchObject({
			resolutionOptions: [
				{value: 'low_480p', isDisabled: false},
				{value: 'medium', isDisabled: false},
			],
			frameRateOptions: [15, 30],
		});
	});

	it('always offers the value it says the share uses', () => {
		const resolutions = ['low_240p', 'low_480p', 'medium', 'high', 'ultra', 'source'] as const;
		for (const mode of ['custom', 'gaming', 'screenshare'] as const) {
			for (const storedResolution of resolutions) {
				for (const storedFrameRate of SUPPORTED_SCREEN_SHARE_FRAME_RATES) {
					for (const entitled of [false, true]) {
						for (const selfHosted of [false, true]) {
							const state = stateOf(
								{mode, storedResolution, storedFrameRate, entitled, context: 'display'},
								selfHosted,
							);
							const scenario = `${mode}|${storedResolution}|${storedFrameRate}|${entitled}|${selfHosted}`;
							expect(`${scenario}|${state.frameRateOptions.includes(state.frameRate)}`).toBe(`${scenario}|true`);
							const option = state.resolutionOptions.find((entry) => entry.value === state.resolution);
							expect(`${scenario}|${option?.isDisabled}`).toBe(`${scenario}|false`);
						}
					}
				}
			}
		}
	});

	it('never writes a premium resolution for a free frame rate pick', () => {
		const quality = {
			mode: 'screenshare',
			storedResolution: 'source',
			storedFrameRate: 60,
			entitled: false,
			context: 'display',
		} as const;
		expect(resolveScreenShareQualityPick(quality, {axis: 'frameRate', frameRate: 60})).toBeNull();
		expect(resolveScreenShareQualityPick(quality, {axis: 'frameRate', frameRate: 15})).toEqual({
			streamingMode: 'custom',
			videoFrameRate: 15,
		});
	});

	it('labels each preset with its own name', () => {
		expect(SCREEN_SHARE_PRESET_DESCRIPTORS.gaming.message).toBe('Gaming');
		expect(SCREEN_SHARE_PRESET_DESCRIPTORS.screenshare.message).toBe('Screen share');
	});

	it('explains the encoder path only when this device has no hardware encoder', () => {
		expect(selectScreenShareEncoderPathDescription(true).message).toBe(
			'No hardware encoder was found on this device, so Prefer hardware works like Automatic.',
		);
		expect(selectScreenShareEncoderPathDescription(false).message).toBe('Encoder preference for new screen shares.');
	});
});

const {
	selectStreamSettingsPresetOverriddenByContext,
	selectStreamSettingsQualityMenuState,
	selectStreamSettingsQualityWrite,
} = await import('@app/features/voice/components/StreamSettingsMenuContentStateMachine');
const {StreamSettingsMenuContent} = await import('@app/features/voice/components/StreamSettingsMenuContent');
const {act, createElement} = await import('react');
const {createRoot} = await import('react-dom/client');

describe('the in-call stream menu', () => {
	const freeScreenShare = {
		mode: 'screenshare',
		storedResolution: 'medium',
		storedFrameRate: 30,
		entitled: false,
		context: 'display',
	} as const;
	const writeFor = (
		quality: Parameters<typeof resolveScreenShareQualityPick>[0],
		pick: Parameters<typeof resolveScreenShareQualityPick>[1],
		premiumOption = false,
	) => selectStreamSettingsQualityWrite({quality, pick, premiumOption, showPremiumFeatures: true});
	const menuStateOf = (quality: Parameters<typeof resolveScreenShareQualityPick>[0], showPremiumFeatures = true) =>
		selectStreamSettingsQualityMenuState({
			quality,
			target: resolveScreenShareTarget({...quality, sourceDimensions: null, hintSetting: 'auto'}),
			showPremiumFeatures,
		});
	const selectedValues = <T>(options: ReadonlyArray<{value: T; selected: boolean}>) =>
		options.filter((option) => option.selected).map((option) => option.value);

	it('keeps the frame rate of a free account that picks a resolution', () => {
		expect(writeFor(freeScreenShare, {axis: 'resolution', resolution: 'low_480p'})).toEqual({
			kind: 'write',
			patch: {streamingMode: 'custom', screenshareResolution: 'low_480p'},
		});
	});

	it('writes no premium resolution when a free account picks a frame rate', () => {
		expect(writeFor(freeScreenShare, {axis: 'frameRate', frameRate: 15})).toEqual({
			kind: 'write',
			patch: {streamingMode: 'custom', videoFrameRate: 15},
		});
	});

	it('offers premium instead of writing a premium pick for a free account', () => {
		expect(writeFor(freeScreenShare, {axis: 'resolution', resolution: 'ultra'}, true)).toEqual({kind: 'premium'});
		expect(writeFor(freeScreenShare, {axis: 'resolution', resolution: 'source'}, true)).toEqual({kind: 'premium'});
		expect(writeFor(freeScreenShare, {axis: 'frameRate', frameRate: 60}, true)).toEqual({kind: 'premium'});
		expect(
			selectStreamSettingsQualityWrite({
				quality: freeScreenShare,
				pick: {axis: 'frameRate', frameRate: 60},
				premiumOption: true,
				showPremiumFeatures: false,
			}),
		).toEqual({kind: 'none'});
	});

	it('writes nothing for the value a preset already sends', () => {
		expect(writeFor(freeScreenShare, {axis: 'resolution', resolution: 'medium'})).toEqual({kind: 'none'});
		expect(writeFor(freeScreenShare, {axis: 'frameRate', frameRate: 30})).toEqual({kind: 'none'});
	});

	it('offers the resolution the share actually uses and marks it selected', () => {
		const state = menuStateOf({
			mode: 'custom',
			storedResolution: 'high',
			storedFrameRate: 30,
			entitled: true,
			context: 'display',
		});
		expect(state.resolutions.map((option) => option.value)).toEqual(['low_480p', 'medium', 'high', 'ultra', 'source']);
		expect(selectedValues(state.resolutions)).toEqual(['high']);
	});

	it('offers no retired 240p rung and lands a persisted 240p store on the lowest real one', () => {
		const restore = {
			mode: VoiceSettings.getStreamingMode(),
			resolution: VoiceSettings.getScreenshareResolution(),
			frameRate: VoiceSettings.getVideoFrameRate(),
		};
		VoiceSettings.updateSettings({streamingMode: 'custom', screenshareResolution: 'low_240p', videoFrameRate: 30});
		expect(VoiceSettings.getScreenshareResolution()).toBe('low_480p');
		const quality = {
			mode: VoiceSettings.getStreamingMode(),
			storedResolution: VoiceSettings.getScreenshareResolution(),
			storedFrameRate: VoiceSettings.getVideoFrameRate(),
			entitled: true,
			context: 'display',
		} as const;
		const menuState = menuStateOf(quality);
		expect(menuState.resolutions.map((option) => option.value)).toEqual([
			'low_480p',
			'medium',
			'high',
			'ultra',
			'source',
		]);
		expect(selectedValues(menuState.resolutions)).toEqual(['low_480p']);
		const tabState = resolveUserVideoTabScreenShareState({quality, hintSetting: 'auto', selfHosted: false});
		expect(tabState.resolution).toBe('low_480p');
		expect(tabState.resolutionOptions.map((option) => option.value)).toEqual([
			'low_480p',
			'medium',
			'high',
			'ultra',
			'source',
		]);
		VoiceSettings.updateSettings({
			streamingMode: restore.mode,
			screenshareResolution: restore.resolution,
			videoFrameRate: restore.frameRate,
		});
	});

	it('offers no 240p rung for any value the store can hold', () => {
		const restore = {
			mode: VoiceSettings.getStreamingMode(),
			resolution: VoiceSettings.getScreenshareResolution(),
			frameRate: VoiceSettings.getVideoFrameRate(),
		};
		for (const stored of ['low_240p', 'low_480p', 'medium', 'high', 'ultra', 'source'] as const) {
			VoiceSettings.updateSettings({streamingMode: 'custom', screenshareResolution: stored, videoFrameRate: 30});
			const quality = {
				mode: 'custom',
				storedResolution: VoiceSettings.getScreenshareResolution(),
				storedFrameRate: 30,
				entitled: true,
				context: 'display',
			} as const;
			expect(
				`${stored}|${menuStateOf(quality)
					.resolutions.map((option) => option.value)
					.join()}`,
			).toBe(`${stored}|low_480p,medium,high,ultra,source`);
			const tabState = resolveUserVideoTabScreenShareState({quality, hintSetting: 'auto', selfHosted: false});
			expect(`${stored}|${tabState.resolutionOptions.map((option) => option.value).join()}`).toBe(
				`${stored}|low_480p,medium,high,ultra,source`,
			);
		}
		VoiceSettings.updateSettings({
			streamingMode: restore.mode,
			screenshareResolution: restore.resolution,
			videoFrameRate: restore.frameRate,
		});
	});

	it('selects what a capture device sends rather than the saved value', () => {
		const state = menuStateOf({
			mode: 'custom',
			storedResolution: 'source',
			storedFrameRate: 60,
			entitled: true,
			context: 'device',
		});
		expect(state.resolutions.map((option) => option.value)).toEqual(['low_480p', 'medium', 'high', 'ultra']);
		expect(selectedValues(state.resolutions)).toEqual(['ultra']);
		expect(state.resolutions.find((option) => option.value === 'ultra')?.write).toEqual({kind: 'none'});
		expect(state.frameRates.map((option) => option.value)).toEqual([15, 30, 60]);
		expect(selectedValues(state.frameRates)).toEqual([60]);
	});

	it('routes every menu pick through the picker', () => {
		const state = menuStateOf(freeScreenShare);
		expect(state.resolutions.map((option) => [option.value, option.write])).toEqual([
			['low_480p', {kind: 'write', patch: {streamingMode: 'custom', screenshareResolution: 'low_480p'}}],
			['medium', {kind: 'none'}],
			['high', {kind: 'premium'}],
			['ultra', {kind: 'premium'}],
			['source', {kind: 'premium'}],
		]);
		expect(state.frameRates.map((option) => [option.value, option.write])).toEqual([
			[15, {kind: 'write', patch: {streamingMode: 'custom', videoFrameRate: 15}}],
			[30, {kind: 'none'}],
			[60, {kind: 'premium'}],
		]);
	});

	it('keeps the premium rungs for an entitled account that sees no premium upsell', () => {
		const state = menuStateOf(
			{mode: 'custom', storedResolution: 'ultra', storedFrameRate: 30, entitled: true, context: 'display'},
			false,
		);
		expect(state.resolutions.map((option) => option.value)).toEqual(['low_480p', 'medium', 'high', 'ultra', 'source']);
		expect(state.frameRates.map((option) => option.value)).toEqual([15, 30, 60]);
	});

	it('always offers the value it says the share uses', () => {
		const resolutions = ['low_240p', 'low_480p', 'medium', 'high', 'ultra', 'source'] as const;
		for (const mode of ['custom', 'gaming', 'screenshare'] as const) {
			for (const storedResolution of resolutions) {
				for (const storedFrameRate of SUPPORTED_SCREEN_SHARE_FRAME_RATES) {
					for (const entitled of [false, true]) {
						for (const context of ['display', 'app', 'device'] as const) {
							for (const showPremiumFeatures of [false, true]) {
								const quality = {mode, storedResolution, storedFrameRate, entitled, context};
								const target = resolveScreenShareTarget({...quality, sourceDimensions: null, hintSetting: 'auto'});
								const state = menuStateOf(quality, showPremiumFeatures);
								const scenario = `${mode}|${storedResolution}|${storedFrameRate}|${entitled}|${context}|${showPremiumFeatures}`;
								expect(`${scenario}|${selectedValues(state.resolutions).join()}`).toBe(
									`${scenario}|${target.resolution === 'low_240p' ? 'low_480p' : target.resolution}`,
								);
								expect(`${scenario}|${selectedValues(state.frameRates).join()}`).toBe(
									`${scenario}|${target.frameRate}`,
								);
							}
						}
					}
				}
			}
		}
	});

	const mounted: Array<{root: ReturnType<typeof createRoot>; container: HTMLElement}> = [];
	const renderStreamMenu = async (input: {
		variant: 'full' | 'compactLive';
		shareContext: 'display' | 'app' | 'device';
		mode: 'custom' | 'gaming' | 'screenshare';
		storedResolution: 'low_480p' | 'medium' | 'high' | 'ultra' | 'source';
		storedFrameRate: number;
		entitled: boolean;
	}) => {
		menuEntitled = input.entitled;
		vi.spyOn(VoiceSettings, 'getStreamingMode').mockReturnValue(input.mode);
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue(input.storedResolution);
		vi.spyOn(VoiceSettings, 'getVideoFrameRate').mockReturnValue(input.storedFrameRate);
		vi.spyOn(VoiceSettings, 'getScreenShareContentHint').mockReturnValue('auto');
		const container = document.createElement('div');
		document.body.append(container);
		const root = createRoot(container);
		mounted.push({root, container});
		await act(async () => {
			root.render(
				createElement(StreamSettingsMenuContent, {
					applyToLiveStream: false,
					shareContext: input.shareContext,
					displayShareEnvironment: 'web',
					variant: input.variant,
				}),
			);
		});
		return container;
	};
	const radios = (container: HTMLElement, submenu: string) =>
		Array.from(container.querySelectorAll(`[data-submenu="${submenu}"] button`)).map((node) => ({
			label: node.getAttribute('data-label') ?? '',
			selected: node.getAttribute('data-selected') === 'true',
		}));
	const pick = async (container: HTMLElement, submenu: string, label: string) => {
		const node = Array.from(container.querySelectorAll(`[data-submenu="${submenu}"] button`)).find(
			(candidate) => candidate.getAttribute('data-label') === label,
		);
		if (node == null) throw new Error(`no ${label} option in ${submenu}`);
		await act(async () => {
			(node as HTMLButtonElement).click();
		});
	};

	afterEach(async () => {
		for (const entry of mounted) {
			await act(async () => {
				entry.root.unmount();
			});
			entry.container.remove();
		}
		mounted.length = 0;
		menuEntitled = true;
		settingsUpdate.mockClear();
		openPremiumModal.mockClear();
		vi.restoreAllMocks();
	});

	it('shows a capture device what it sends rather than the saved value', async () => {
		const container = await renderStreamMenu({
			variant: 'full',
			shareContext: 'device',
			mode: 'custom',
			storedResolution: 'source',
			storedFrameRate: 60,
			entitled: true,
		});
		expect(radios(container, 'Resolution')).toEqual([
			{label: '480p', selected: false},
			{label: '720p', selected: false},
			{label: '1080p', selected: false},
			{label: '1440p', selected: true},
		]);
		expect(radios(container, 'Frame rate')).toEqual([
			{label: '15 FPS', selected: false},
			{label: '30 FPS', selected: false},
			{label: '60 FPS', selected: true},
		]);
	});

	it('offers the 90 FPS a stored preference asks for', async () => {
		const container = await renderStreamMenu({
			variant: 'full',
			shareContext: 'display',
			mode: 'custom',
			storedResolution: 'medium',
			storedFrameRate: 90,
			entitled: true,
		});
		expect(radios(container, 'Frame rate')).toEqual([
			{label: '15 FPS', selected: false},
			{label: '30 FPS', selected: false},
			{label: '60 FPS', selected: false},
			{label: '90 FPS', selected: true},
		]);
	});

	it('sends a menu pick through the picker instead of storing the raw value', async () => {
		const container = await renderStreamMenu({
			variant: 'full',
			shareContext: 'display',
			mode: 'custom',
			storedResolution: 'medium',
			storedFrameRate: 30,
			entitled: false,
		});
		await pick(container, 'Resolution', '1080p');
		expect(openPremiumModal).toHaveBeenCalledTimes(1);
		expect(settingsUpdate).not.toHaveBeenCalled();
		await pick(container, 'Frame rate', '60 FPS');
		expect(openPremiumModal).toHaveBeenCalledTimes(2);
		expect(settingsUpdate).not.toHaveBeenCalled();
		await pick(container, 'Resolution', '480p');
		expect(settingsUpdate).toHaveBeenCalledWith({streamingMode: 'custom', screenshareResolution: 'low_480p'});
	});

	it('sends a compact menu pick through the picker as well', async () => {
		const container = await renderStreamMenu({
			variant: 'compactLive',
			shareContext: 'display',
			mode: 'screenshare',
			storedResolution: 'medium',
			storedFrameRate: 30,
			entitled: false,
		});
		expect(radios(container, 'Stream quality')).toEqual([
			{label: '15 FPS', selected: false},
			{label: '30 FPS', selected: true},
			{label: '60 FPS', selected: false},
			{label: '480p', selected: false},
			{label: '720p', selected: true},
			{label: '1080p', selected: false},
			{label: '1440p', selected: false},
			{label: 'Source', selected: false},
		]);
		await pick(container, 'Stream quality', '60 FPS');
		expect(openPremiumModal).toHaveBeenCalledTimes(1);
		expect(settingsUpdate).not.toHaveBeenCalled();
		await pick(container, 'Stream quality', '480p');
		expect(settingsUpdate).toHaveBeenCalledWith({streamingMode: 'custom', screenshareResolution: 'low_480p'});
	});

	it('serves a capture device the Gaming preset and says so', () => {
		expect(selectStreamSettingsPresetOverriddenByContext('screenshare', 'device')).toBe(true);
		expect(selectStreamSettingsPresetOverriddenByContext('gaming', 'device')).toBe(false);
		expect(selectStreamSettingsPresetOverriddenByContext('screenshare', 'display')).toBe(false);
	});
});

const {default: MediaEngine} = await import('@app/features/voice/engine/MediaEngineFacade');
const {
	buildConfiguredScreenShareOptions,
	startConfiguredDeviceScreenShare,
	startConfiguredDisplayScreenShare,
	switchConfiguredDeviceScreenShare,
	switchConfiguredDisplayScreenShare,
} = await import('@app/features/voice/utils/ScreenShareStartFlow');

describe('the configured screen share start flow', () => {
	const engine = MediaEngine as unknown as {
		startDeviceScreenShare: ReturnType<typeof vi.fn>;
		replaceActiveDeviceScreenShare: ReturnType<typeof vi.fn>;
	};
	let storedResolution: 'high' | 'medium' | 'source' = 'high';
	let started: Array<{resolution: unknown; publishOptions: unknown}> = [];

	beforeEach(() => {
		selfStream = true;
		storedResolution = 'high';
		started = [];
		ActiveScreenShareSource.clear();
		vi.spyOn(VoiceSettings, 'getStreamingMode').mockReturnValue('custom');
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockImplementation(() => storedResolution);
		vi.spyOn(VoiceSettings, 'getVideoFrameRate').mockReturnValue(30);
		vi.spyOn(VoiceSettings, 'getScreenShareContentHint').mockReturnValue('text');
		vi.spyOn(VoiceSettings, 'getPreferredScreenShareCodec').mockReturnValue('auto');
		vi.spyOn(VoiceSettings, 'getShareDeviceAudio').mockReturnValue(false);
		refreshSelection.mockReset();
		refreshSelection.mockResolvedValue(null);
		selectScreenShareCodec.mockReset();
		selectScreenShareCodec.mockReturnValue('h264');
		engine.startDeviceScreenShare = vi.fn(async (options: {resolution: unknown}, publishOptions: unknown) => {
			started.push({resolution: options.resolution, publishOptions});
		});
		engine.replaceActiveDeviceScreenShare = vi.fn(async () => true);
	});

	afterEach(() => {
		vi.restoreAllMocks();
		ActiveScreenShareSource.clear();
		selfStream = true;
	});

	it('publishes the target the settings push builds', async () => {
		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);
		const target = resolveConfiguredScreenShareTarget('device', null);
		const expected = buildConfiguredScreenShareOptions({
			target,
			sourceDimensions: null,
			includeAudio: false,
			videoCodec: 'h264',
		});
		expect(started).toEqual([
			{resolution: expected.captureOptions.resolution, publishOptions: expected.publishOptions},
		]);
		expect(expected.publishOptions.videoCodec).toBe('h264');
		expect(expected.publishOptions.degradationPreference).toBe('balanced');
		expect(ActiveScreenShareSource.getTarget()).toEqual(target);
	});

	it('clears the recorded source dimensions for a capture device', async () => {
		ActiveScreenShareSource.setPublishedSource('display', 'screen:1');
		ActiveScreenShareSource.setSourceDimensions({width: 3840, height: 2160});
		await startConfiguredDeviceScreenShare('camera-1');
		expect(ActiveScreenShareSource.getPublishedSource()).toBe('device');
		expect(ActiveScreenShareSource.getSourceDimensions()).toBeNull();
	});

	it('commits the target of the share the start flow asked for', async () => {
		storedResolution = 'source';
		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);
		const device = resolveConfiguredScreenShareTarget('device', null);
		expect(device).not.toEqual(resolveConfiguredScreenShareTarget('display', null));
		expect(ActiveScreenShareSource.getTarget()).toEqual(device);
		expect(started).toEqual([
			{
				resolution: buildConfiguredScreenShareOptions({
					target: device,
					sourceDimensions: null,
					includeAudio: false,
					videoCodec: 'h264',
				}).captureOptions.resolution,
				publishOptions: buildConfiguredScreenShareOptions({
					target: device,
					sourceDimensions: null,
					includeAudio: false,
					videoCodec: 'h264',
				}).publishOptions,
			},
		]);
	});

	it('retargets the committed target on a source switch', async () => {
		await startConfiguredDeviceScreenShare('camera-1');
		const startedTarget = ActiveScreenShareSource.getTarget();
		storedResolution = 'medium';
		expect(await switchConfiguredDeviceScreenShare('camera-2')).toBe(true);
		const switched = resolveConfiguredScreenShareTarget('device', null);
		expect(switched).not.toEqual(startedTarget);
		expect(ActiveScreenShareSource.getTarget()).toEqual(switched);
	});

	it('puts the committed target back when a switch does not take', async () => {
		await startConfiguredDeviceScreenShare('camera-1');
		const before = ActiveScreenShareSource.getTarget();
		engine.replaceActiveDeviceScreenShare = vi.fn(async () => false);
		storedResolution = 'medium';
		expect(await switchConfiguredDeviceScreenShare('camera-2')).toBe(false);
		expect(ActiveScreenShareSource.getTarget()).toEqual(before);
	});
});

describe('the configured display screen share start flow', () => {
	const engine = MediaEngine as unknown as {
		setScreenShareEnabled: ReturnType<typeof vi.fn>;
		replaceActiveDisplayScreenShare: ReturnType<typeof vi.fn>;
	};
	const electronHost = window as unknown as {electron?: {platform: string}};

	beforeEach(() => {
		selfStream = true;
		displayShareEnvironment = 'desktop-custom';
		electronHost.electron = {platform: 'win32'};
		ActiveScreenShareSource.clear();
		vi.spyOn(VoiceSettings, 'getStreamingMode').mockReturnValue('custom');
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue('source');
		vi.spyOn(VoiceSettings, 'getVideoFrameRate').mockReturnValue(30);
		vi.spyOn(VoiceSettings, 'getScreenShareContentHint').mockReturnValue('text');
		vi.spyOn(VoiceSettings, 'getPreferredScreenShareCodec').mockReturnValue('auto');
		vi.spyOn(VoiceSettings, 'getShareDesktopAudio').mockReturnValue(false);
		vi.spyOn(VoiceSettings, 'getShareAppAudio').mockReturnValue(false);
		refreshSelection.mockReset();
		refreshSelection.mockResolvedValue(null);
		selectScreenShareCodec.mockReset();
		selectScreenShareCodec.mockReturnValue('h264');
		engine.setScreenShareEnabled = vi.fn(async () => undefined);
		engine.replaceActiveDisplayScreenShare = vi.fn(async () => true);
	});

	afterEach(() => {
		vi.restoreAllMocks();
		ActiveScreenShareSource.clear();
		displayShareEnvironment = 'web';
		Reflect.deleteProperty(electronHost, 'electron');
	});

	it('commits the target of the picked source rather than the one still recorded', async () => {
		ActiveScreenShareSource.setPublishedSource('display', 'screen:0');
		ActiveScreenShareSource.setSourceDimensions({width: 3840, height: 2160});
		const picked = {width: 1280, height: 720};
		const pickedTarget = resolveConfiguredScreenShareTarget('display', picked);
		expect(pickedTarget).not.toEqual(resolveConfiguredScreenShareTarget('display', {width: 3840, height: 2160}));
		expect(await startConfiguredDisplayScreenShare('screen:1', {sourceDimensions: picked})).toBe(true);
		expect(ActiveScreenShareSource.getTarget()).toEqual(pickedTarget);
	});

	it('rebuilds the target from the newly picked source on a switch', async () => {
		const first = {width: 1280, height: 720};
		const second = {width: 3840, height: 2160};
		expect(await startConfiguredDisplayScreenShare('screen:1', {sourceDimensions: first})).toBe(true);
		ActiveScreenShareSource.setSourceDimensions(first);
		const secondTarget = resolveConfiguredScreenShareTarget('display', second);
		expect(secondTarget).not.toEqual(resolveConfiguredScreenShareTarget('display', first));
		expect(await switchConfiguredDisplayScreenShare('screen:2', {sourceDimensions: second})).toBe(true);
		expect(ActiveScreenShareSource.getTarget()).toEqual(secondTarget);
		expect(ActiveScreenShareSource.getSourceDimensions()).toEqual(second);
	});

	it('asks for the tracking to be held when it stops the share a Wayland source switch restarts', async () => {
		displayShareEnvironment = 'desktop-wayland';
		electronHost.electron = {platform: 'linux'};
		const stops: Array<unknown> = [];
		engine.setScreenShareEnabled = vi.fn(async (enabled: boolean, options: unknown) => {
			if (!enabled) stops.push(options);
		});
		expect(await startConfiguredDisplayScreenShare(null, {preferredDisplaySurface: 'monitor'})).toBe(true);
		expect(await switchConfiguredDisplayScreenShare(null, {preferredDisplaySurface: 'monitor'})).toBe(true);
		expect(stops).toEqual([
			{sendUpdate: false, playSound: false, preserveStreamAudioPreferences: true, keepShareTracking: true},
		]);
	});
});

const {pushActiveStreamSettings} = await import('@app/features/voice/components/StreamSettingsMenuContent');

describe('a live settings push', () => {
	const engine = MediaEngine as unknown as {
		startDeviceScreenShare: ReturnType<typeof vi.fn>;
		updateActiveScreenShareSettings: ReturnType<typeof vi.fn>;
	};

	beforeEach(() => {
		selfStream = true;
		ActiveScreenShareSource.clear();
		vi.spyOn(VoiceSettings, 'getStreamingMode').mockReturnValue('custom');
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue('high');
		vi.spyOn(VoiceSettings, 'getVideoFrameRate').mockReturnValue(30);
		vi.spyOn(VoiceSettings, 'getScreenShareContentHint').mockReturnValue('text');
		vi.spyOn(VoiceSettings, 'getPreferredScreenShareCodec').mockReturnValue('auto');
		vi.spyOn(VoiceSettings, 'getShareDeviceAudio').mockReturnValue(false);
		refreshSelection.mockReset();
		refreshSelection.mockResolvedValue(null);
		selectScreenShareCodec.mockReset();
		selectScreenShareCodec.mockReturnValue('h264');
	});

	afterEach(() => {
		vi.restoreAllMocks();
		ActiveScreenShareSource.clear();
	});

	it('puts the committed target back when the live share refuses the push', async () => {
		engine.startDeviceScreenShare = vi.fn(async () => undefined);
		engine.updateActiveScreenShareSettings = vi.fn(async () => false);
		await startConfiguredDeviceScreenShare('camera-1');
		const before = ActiveScreenShareSource.getTarget();
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue('low_480p');
		expect(resolveConfiguredScreenShareTarget('device', null)).not.toEqual(before);
		await pushActiveStreamSettings('device', 'desktop-custom');
		expect(engine.updateActiveScreenShareSettings).toHaveBeenCalledTimes(1);
		expect(ActiveScreenShareSource.getTarget()).toEqual(before);
	});

	it('keeps the retargeted target when the live share takes the push', async () => {
		engine.startDeviceScreenShare = vi.fn(async () => undefined);
		engine.updateActiveScreenShareSettings = vi.fn(async () => true);
		await startConfiguredDeviceScreenShare('camera-1');
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue('low_480p');
		const retargeted = resolveConfiguredScreenShareTarget('device', null);
		await pushActiveStreamSettings('device', 'desktop-custom');
		expect(ActiveScreenShareSource.getTarget()).toEqual(retargeted);
	});

	it('sends the live share what the start flow published', async () => {
		const published: Array<{resolution: unknown; publishOptions: unknown}> = [];
		engine.startDeviceScreenShare = vi.fn(async (options: {resolution: unknown}, publishOptions: unknown) => {
			published.push({resolution: options.resolution, publishOptions});
		});
		engine.updateActiveScreenShareSettings = vi.fn(
			async (captureOptions: {resolution: unknown}, publishOptions: unknown) => {
				published.push({resolution: captureOptions.resolution, publishOptions});
			},
		);
		await startConfiguredDeviceScreenShare('camera-1');
		await pushActiveStreamSettings('device', 'desktop-custom');
		expect(published).toHaveLength(2);
		expect(published[1]).toEqual(published[0]);
	});
});

const {reconfigureActiveDeviceShareAudio} = await import('@app/features/voice/utils/ScreenShareStartFlow');
const {
	DEVICE_AUDIO_ONLY_DESCRIPTOR,
	DEVICE_AUDIO_WITH_DEVICE_DESCRIPTOR,
	MICROPHONE_WITH_DEVICE_DESCRIPTOR,
	NO_DEVICE_AUDIO_DESCRIPTOR,
	resolveDeviceShareAudioPairing,
	resolveScreenShareAudioSummary,
} = await import('@app/features/voice/utils/ScreenShareAudioSummary');

describe('the device share audio input', () => {
	const engine = MediaEngine as unknown as {
		startDeviceScreenShare: ReturnType<typeof vi.fn>;
		ensureDeviceScreenShareMicPublication: ReturnType<typeof vi.fn>;
		getActiveScreenShareVideoDeviceId: ReturnType<typeof vi.fn>;
		setScreenShareAudioMuted: ReturnType<typeof vi.fn>;
	};
	const CARD = {deviceId: 'card-video', groupId: 'card', kind: 'videoinput', label: 'Capture card'} as MediaDeviceInfo;
	const CARD_AUDIO = {
		deviceId: 'card-audio',
		groupId: 'card',
		kind: 'audioinput',
		label: 'Capture card audio',
	} as MediaDeviceInfo;
	const HEADSET = {
		deviceId: 'microphone-1',
		groupId: 'headset',
		kind: 'audioinput',
		label: 'Headset',
	} as MediaDeviceInfo;
	let devices: Array<MediaDeviceInfo> = [];
	let published: Array<string | undefined> = [];
	let chosenAudioDeviceId = 'default';
	let usesMicrophone = false;

	beforeEach(() => {
		selfStream = true;
		devices = [CARD, HEADSET];
		published = [];
		chosenAudioDeviceId = 'default';
		usesMicrophone = false;
		ActiveScreenShareSource.clear();
		Object.defineProperty(navigator, 'mediaDevices', {
			configurable: true,
			value: {enumerateDevices: async () => devices},
		});
		vi.spyOn(VoiceSettings, 'getStreamingMode').mockReturnValue('custom');
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue('high');
		vi.spyOn(VoiceSettings, 'getVideoFrameRate').mockReturnValue(30);
		vi.spyOn(VoiceSettings, 'getScreenShareContentHint').mockReturnValue('text');
		vi.spyOn(VoiceSettings, 'getPreferredScreenShareCodec').mockReturnValue('auto');
		vi.spyOn(VoiceSettings, 'getShareDeviceAudio').mockReturnValue(true);
		vi.spyOn(VoiceSettings, 'getInputDeviceId').mockReturnValue('microphone-1');
		vi.spyOn(VoiceSettings, 'getScreenShareAudioDeviceId').mockImplementation(() => chosenAudioDeviceId);
		vi.spyOn(VoiceSettings, 'getScreenShareDeviceAudioUsesMicrophone').mockImplementation(() => usesMicrophone);
		refreshSelection.mockReset();
		refreshSelection.mockResolvedValue(null);
		selectScreenShareCodec.mockReset();
		selectScreenShareCodec.mockReturnValue('h264');
		engine.startDeviceScreenShare = vi.fn(async (options: {audioDeviceId?: string}) => {
			published.push(options.audioDeviceId);
		});
		engine.ensureDeviceScreenShareMicPublication = vi.fn(async (audioDeviceId: string) => {
			published.push(audioDeviceId);
			return true;
		});
		engine.getActiveScreenShareVideoDeviceId = vi.fn(() => 'card-video');
		engine.setScreenShareAudioMuted = vi.fn();
	});

	afterEach(() => {
		vi.restoreAllMocks();
		ActiveScreenShareSource.clear();
		selfStream = true;
	});

	it('never publishes the voice input for a capture device that has no audio of its own', async () => {
		expect(await startConfiguredDeviceScreenShare('card-video')).toBe(true);
		expect(published).toEqual([undefined]);
	});

	it('publishes the capture device own audio input when it has one', async () => {
		devices = [CARD, CARD_AUDIO, HEADSET];
		expect(await startConfiguredDeviceScreenShare('card-video')).toBe(true);
		expect(published).toEqual(['card-audio']);
	});

	it('publishes the input the user picked in the audio device menu', async () => {
		chosenAudioDeviceId = 'microphone-1';
		expect(await startConfiguredDeviceScreenShare('card-video')).toBe(true);
		expect(published).toEqual(['microphone-1']);
	});

	it('publishes the voice input when the audio source picker asked for the microphone', async () => {
		usesMicrophone = true;
		expect(await startConfiguredDeviceScreenShare('card-video')).toBe(true);
		expect(published).toEqual(['microphone-1']);
	});

	it('silences a live device share rather than leaving an earlier input publishing', async () => {
		expect(await reconfigureActiveDeviceShareAudio()).toBe(true);
		expect(engine.setScreenShareAudioMuted).toHaveBeenCalledWith(true);
		expect(engine.ensureDeviceScreenShareMicPublication).not.toHaveBeenCalled();
	});

	const summarise = (input: Partial<ScreenShareAudioSummaryInput>) =>
		resolveScreenShareAudioSummary({
			sourceMode: 'system',
			includeSources: [],
			shareContext: 'device',
			microphoneLabel: 'Headset',
			...input,
		});

	it('never names the microphone for a capture device that has no audio of its own', () => {
		expect(summarise({deviceAudioPairing: resolveDeviceShareAudioPairing([CARD, HEADSET], 'card-video')})).toEqual({
			kind: 'message',
			descriptor: NO_DEVICE_AUDIO_DESCRIPTOR,
		});
	});

	it('names the capture device own audio input when it has one', () => {
		expect(
			summarise({deviceAudioPairing: resolveDeviceShareAudioPairing([CARD, CARD_AUDIO, HEADSET], 'card-video')}),
		).toEqual({
			kind: 'message',
			descriptor: DEVICE_AUDIO_WITH_DEVICE_DESCRIPTOR,
			values: {deviceLabel: 'Capture card audio'},
		});
	});

	it('names the input the user picked in the audio device menu', () => {
		expect(
			summarise({
				chosenAudioDeviceId: 'microphone-1',
				deviceAudioPairing: resolveDeviceShareAudioPairing([CARD, HEADSET], 'card-video'),
			}),
		).toEqual({
			kind: 'message',
			descriptor: MICROPHONE_WITH_DEVICE_DESCRIPTOR,
			values: {deviceLabel: 'Headset'},
		});
	});

	it('names the microphone when the audio source picker asked for it', () => {
		expect(
			summarise({
				usesDeviceMicrophone: true,
				deviceAudioPairing: resolveDeviceShareAudioPairing([CARD, HEADSET], 'card-video'),
			}),
		).toEqual({
			kind: 'message',
			descriptor: MICROPHONE_WITH_DEVICE_DESCRIPTOR,
			values: {deviceLabel: 'Headset'},
		});
	});

	it('promises nothing about a device the user has not picked yet', () => {
		expect(summarise({deviceAudioPairing: resolveDeviceShareAudioPairing([CARD, CARD_AUDIO], '')})).toEqual({
			kind: 'message',
			descriptor: DEVICE_AUDIO_ONLY_DESCRIPTOR,
		});
	});
});
