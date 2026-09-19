// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';
import type {ScreenShareEncoderVerificationAction} from '@app/features/voice/utils/CodecCapabilityDetector';
import {
	BackupCodecPolicy,
	ParticipantEvent,
	type Room,
	RoomEvent,
	Track,
	type TrackPublishOptions,
	type VideoCodec,
} from 'livekit-client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const pushWithKey = vi.fn();
const createToast = vi.fn();
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
let gpuReport: {gpuLabel: string} | null = null;
let gpuReportLoad: Promise<unknown> = Promise.resolve(null);
let displayShareEnvironment: 'web' | 'desktop-custom' | 'desktop-wayland' = 'web';
let voiceConnectionContext: {guildId: string | null; channelId: string | null; connectionId: string | null} | null =
	null;
let voiceStates: Record<string, Record<string, Record<string, unknown>>> = {};
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

vi.mock('@app/features/ui/commands/ToastCommands', () => ({
	createToast: (...args: Array<unknown>) => createToast(...args),
}));

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
const {default: ScreenShareDelivery} = await import('@app/features/voice/state/ScreenShareDelivery');
const {default: AppStorage} = await import('@app/features/platform/state/PersistentStorage');
const {default: ActiveScreenShareSource} = await import('@app/features/voice/state/ActiveScreenShareSource');
const {
	applyScreenShareBackupCodecModeRetiredMigrationV1,
	applyScreenShareSoftwareQualityRetiredMigrationV1,
	default: VoiceSettings,
} = await import('@app/features/voice/state/VoiceSettings');
const {getRecentScreenShares, recordScreenShareStarted, resetRecentScreenSharesForTests} = await import(
	'@app/features/voice/utils/ScreenShareLifecycleLog'
);
const {buildScreenShareDeliveryHeader, getEffectivePublishOptions, resolveConfiguredScreenShareLadder} = await import(
	'@app/features/voice/engine/voice_screen_share_manager/shared'
);
const {bindRoomEvents} = await import('@app/features/voice/engine/VoiceRoomEventBinder');
const {voiceVideoIndex} = await import('@app/features/user/components/settings_utils/search_index/VoiceVideoIndex');
const {VoiceEngineV2AppScreenShareLiveKitFlows: ScreenShareLiveKitFlows} = await vi.importActual<
	typeof import('@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareLiveKitFlows')
>('@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareLiveKitFlows');
const {
	readScreenShareDeliverySample,
	resolveScreenShareDeliveryNoticeTone,
	resolveScreenShareDeliverySentInfo,
	showsScreenShareDeliverySentInfo,
} = await import('@app/features/voice/engine/ScreenShareUnderperformance');
const {shouldShowTileControlPill} = await import('@app/features/voice/components/VoiceParticipantTileStateMachine');
const {countStreamViewers, getStreamKey} = await import('@app/features/voice/components/StreamKeys');

const FIRST_TICK_MS = 2500;
const TICK_MS = 2000;

interface StatsFeed {
	encodedFps: number;
	sourceFps: number;
	targetBitrate: number;
	fill: number;
	headerShare: number;
	encodeSecondsPerFrame: number | null;
	reason: string;
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
	setRejecting: (reject: boolean) => void;
}

function createStatsReader(feed: StatsFeed): () => RTCStatsReport {
	let last = Date.now();
	let framesEncoded = 0;
	let sourceFrames = 0;
	let bytesSent = 0;
	let headerBytesSent = 0;
	let totalEncodeTime = 0;
	return () => {
		const now = Date.now();
		const seconds = (now - last) / 1000;
		last = now;
		const encodedDelta = Math.round(feed.encodedFps * seconds);
		const sentDelta = Math.round((feed.targetBitrate * feed.fill * seconds) / 8);
		const headerDelta = Math.round(sentDelta * feed.headerShare);
		framesEncoded += encodedDelta;
		sourceFrames += Math.round(feed.sourceFps * seconds);
		bytesSent += sentDelta - headerDelta;
		headerBytesSent += headerDelta;
		if (feed.encodeSecondsPerFrame !== null) {
			totalEncodeTime += feed.encodeSecondsPerFrame * encodedDelta;
		}
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
				headerBytesSent,
				...(feed.encodeSecondsPerFrame === null ? {} : {totalEncodeTime}),
				frameWidth: feed.sentWidth,
				frameHeight: feed.sentHeight,
				targetBitrate: feed.targetBitrate,
				qualityLimitationReason: feed.reason,
				encoderImplementation: 'NVENC',
				timestamp: now,
			},
		];
		return new Map(entries.map((entry) => [entry.id as string, entry])) as unknown as RTCStatsReport;
	};
}

function createSender(
	feed: StatsFeed,
	options: {mimeType?: string; reject?: boolean; encodings?: Array<RTCRtpEncodingParameters>} = {},
): FakeSender {
	let params = {
		codecs: [{mimeType: options.mimeType ?? 'video/H264'}],
		encodings: options.encodings ?? [{scalabilityMode: 'L1T3'}],
	} as unknown as RTCRtpSendParameters;
	const getParameters = vi.fn(() => structuredClone(params));
	const readStats = createStatsReader(feed);
	const setParameters = vi.fn(async (next: RTCRtpSendParameters) => {
		if (options.reject) throw new Error('setParameters rejected');
		params = next;
	});
	const getStats = vi.fn(async () => readStats());
	const sender = {
		track: {getSettings: () => ({width: 3840, height: 2160}), contentHint: ''},
		getParameters,
		setParameters,
		getStats,
	} as unknown as RTCRtpSender;
	return {
		sender,
		setParameters,
		getStats,
		getParameters,
		current: () => params,
		setRejecting: (reject: boolean) => {
			options.reject = reject;
		},
	};
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

interface ScreenShareReplacementHandle {
	captureScreenShareReplacementSnapshot: (participant: unknown, track: unknown, publishOptions: unknown) => unknown;
	restorePublishedScreenShareSource: (snapshot: unknown) => void;
}

function createFlowHooks(queued: Array<string> = []) {
	const calls: Array<string> = [];
	const adapter = {
		isScreenSharePending: false,
		encoderVerificationTimer: null,
		streamingPriorityHeld: false,
		transitionScreenShareLifecycleInternal: (event: {type: string}) => {
			calls.push(event.type);
		},
		getEffectivePublishOptionsInternal: async (
			_enabled: boolean,
			_publishOptions?: TrackPublishOptions,
		): Promise<TrackPublishOptions | undefined> => undefined,
		getActiveScreenShareContentSourceInternal: () => 'display',
		getActiveScreenShareSourceTypeInternal: () => 'display',
		getScreenShareSourceTypeForContentSourceInternal: () => 'display',
		setStreamingPriorityInternal: () => undefined,
		applyScreenShareStateInternal: () => undefined,
		applyScreenShareContentHintInternal: () => undefined,
		enforceScreenShareSenderParametersInternal: async () => true,
		ensureScreenShareKeepAliveSinkInternal: () => undefined,
		applyScreenShareAudioContentHintInternal: () => undefined,
		monitorActiveScreenShareEndInternal: () => undefined,
		noteScreenShareCodecRecoveryInternal: () => undefined,
		cancelEncoderVerificationInternal: () => undefined,
		cleanupActiveScreenShareEndListenerInternal: () => undefined,
		cleanupLingeringScreenShareTracks: async () => {
			calls.push('cleanup');
			ActiveScreenShareSource.clear();
			ScreenShareDelivery.endShare();
		},
		startEncoderVerificationInternal: () => undefined,
		syncLocalStreamWatchStateInternal: () => undefined,
		syncLocalScreenShareAudioStateInternal: () => undefined,
		syncPersistedScreenShareAudioPreferenceInternal: () => undefined,
		applyPendingScreenShareRequestsInternal: async () => {
			calls.push('drain');
			calls.push(...queued.splice(0, queued.length));
		},
		reconcileScreenShareCodecInternal: async () => {
			calls.push('reconcile');
		},
	};
	return {flows: new ScreenShareLiveKitFlows(adapter as never), calls, adapter};
}

function createAdapter() {
	const adapter = new VoiceEngineV2AppScreenShareExecutionAdapter();
	const republish = vi.spyOn(adapter.liveKitFlows, 'republishActiveShareWithCodec').mockResolvedValue(true);
	const setEnabled = vi.spyOn(adapter, 'setScreenShareEnabled').mockResolvedValue(undefined);
	return {adapter, republish, setEnabled};
}

function createRealFlowsAdapter() {
	const adapter = new VoiceEngineV2AppScreenShareExecutionAdapter();
	Object.assign(adapter, {liveKitFlows: new ScreenShareLiveKitFlows(adapter as never)});
	const setEnabled = vi.spyOn(adapter, 'setScreenShareEnabled').mockResolvedValue(undefined);
	return {adapter, setEnabled};
}

function createTeardownAdapter(participant: unknown, afterCleanup: () => void) {
	const adapter = new VoiceEngineV2AppScreenShareExecutionAdapter();
	const routing = adapter.controllerRouting as unknown as {setEnabled: () => Promise<void>};
	routing.setEnabled = async () => {
		await adapter.cleanupLingeringScreenShareTracks(participant as never).catch(() => undefined);
		afterCleanup();
	};
	return adapter;
}

function recordedToasts(): number {
	return getRecentScreenShares()[0].deliveryDecisions.filter((record) => record.toast).length;
}

function lastDecision() {
	return getRecentScreenShares()[0].deliveryDecisions.at(-1);
}

async function tickUntilDecision(kind: string, maxTicks: number): Promise<void> {
	for (let tick = 0; tick < maxTicks; tick++) {
		const before = lastDecision();
		await vi.advanceTimersByTimeAsync(TICK_MS);
		const after = lastDecision();
		if (after !== before && after?.decision === kind) return;
	}
	throw new Error(`no ${kind} decision within ${maxTicks} ticks`);
}

function sentFrameRate(fake: FakeSender): number | undefined {
	return fake.current().encodings[0].maxFramerate;
}

function capturedSource() {
	return {
		publishedSource: 'display' as const,
		sourceId: 'screen:1',
		isOwnWindow: false,
		sourceDimensions: {width: 3840, height: 2160},
	};
}

function rememberLevel(settledIndex: number, probeBlockedUntil: number | null = null): void {
	ScreenShareDelivery.verdicts = {
		h264: {short: false, ratio: 1, settledIndex, probeBlockedUntil, at: Date.now()},
	};
}

describe('the screen share delivery wiring', () => {
	let feed: StatsFeed;

	beforeEach(() => {
		vi.useFakeTimers();
		feed = {
			encodedFps: 30,
			sourceFps: 30,
			targetBitrate: 4_500_000,
			fill: 1,
			headerShare: 0,
			encodeSecondsPerFrame: null,
			reason: 'none',
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
		createToast.mockClear();
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
		AppStorage.removeItem('ScreenShareDeliveryMemoryV1');
		resetRecentScreenSharesForTests();
		recordScreenShareStarted();
		ActiveScreenShareSource.setPublishedSource('display', 'screen:1');
		ActiveScreenShareSource.setSourceDimensions({width: 3840, height: 2160});
		ScreenShareDelivery.beginShare({
			header: {chromiumMajor: 140, gpuKey: 'none', encoderMode: 'auto', hardwareAccelerationDisabled: false},
			...resolveConfiguredScreenShareLadder('display', {width: 3840, height: 2160}),
			hardware: {h264: true},
			adaptive: true,
		});
	});

	afterEach(() => {
		vi.useRealTimers();
		vi.restoreAllMocks();
		ScreenShareDelivery.endShare();
		ActiveScreenShareSource.clear();
	});

	it('steps down a level with the new scale and frame rate and drops the H.264 layering', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(primary.current().encodings).toEqual([
			{maxBitrate: 4_500_000, maxFramerate: 30, scaleResolutionDownBy: 2, priority: 'high', networkPriority: 'high'},
		]);
		await vi.advanceTimersByTimeAsync(TICK_MS * 8);
		expect(primary.setParameters).toHaveBeenCalledTimes(2);
		expect(primary.current().degradationPreference).toBe('maintain-resolution');
		expect(primary.current().encodings).toEqual([
			{maxBitrate: 3_000_000, maxFramerate: 15, scaleResolutionDownBy: 2, priority: 'high', networkPriority: 'high'},
		]);
		expect(ScreenShareDelivery.notice).toMatchObject({kind: 'device-framerate', frameRate: 15});
		adapter.cancelEncoderVerificationInternal();
	});

	it('drives the own tile badge and pill from the store while nobody is watching', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		const notice = ScreenShareDelivery.notice;
		expect(notice).toMatchObject({kind: 'device-framerate', frameRate: 15});
		expect(notice && resolveScreenShareDeliveryNoticeTone(notice)).toBe('info');
		expect(resolveScreenShareDeliverySentInfo(ScreenShareDelivery.plan)).toEqual({
			width: 1920,
			height: 1080,
			fps: 15,
		});
		expect(
			shouldShowTileControlPill({
				isFocusedPlaceholderTile: false,
				hasDeliveryNotice: notice !== null,
				showStreamAudioControls: false,
				showSpectatorPill: false,
				showGroupHiddenPill: false,
				showDeviceCollapseControl: false,
			}),
		).toBe(true);
		adapter.cancelEncoderVerificationInternal();
	});

	it('keeps the own tile control pill hidden when there is nothing to put in it', () => {
		const quiet = {
			isFocusedPlaceholderTile: false,
			hasDeliveryNotice: false,
			showStreamAudioControls: false,
			showSpectatorPill: false,
			showGroupHiddenPill: false,
			showDeviceCollapseControl: false,
		};
		expect(shouldShowTileControlPill(quiet)).toBe(false);
		expect(shouldShowTileControlPill({...quiet, hasDeliveryNotice: true})).toBe(true);
		expect(shouldShowTileControlPill({...quiet, hasDeliveryNotice: true, isFocusedPlaceholderTile: true})).toBe(false);
		expect(shouldShowTileControlPill({...quiet, showStreamAudioControls: true})).toBe(true);
		expect(shouldShowTileControlPill({...quiet, showSpectatorPill: true})).toBe(true);
		expect(shouldShowTileControlPill({...quiet, showGroupHiddenPill: true})).toBe(true);
		expect(shouldShowTileControlPill({...quiet, showDeviceCollapseControl: true})).toBe(true);
	});

	it('turns the own tile badge to a warning when the device runs out of room', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		feed.encodedFps = 12;
		await vi.advanceTimersByTimeAsync(TICK_MS * 8);
		const notice = ScreenShareDelivery.notice;
		expect(notice).toMatchObject({kind: 'device-short'});
		expect(notice && resolveScreenShareDeliveryNoticeTone(notice)).toBe('warning');
		adapter.cancelEncoderVerificationInternal();
	});

	it('clears the own tile badge when full quality is asked for again', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(ScreenShareDelivery.notice).not.toBeNull();
		ScreenShareDelivery.resetToFullQuality();
		expect(ScreenShareDelivery.notice).toBeNull();
		expect(
			shouldShowTileControlPill({
				isFocusedPlaceholderTile: false,
				hasDeliveryNotice: ScreenShareDelivery.notice !== null,
				showStreamAudioControls: false,
				showSpectatorPill: false,
				showGroupHiddenPill: false,
				showDeviceCollapseControl: false,
			}),
		).toBe(false);
		adapter.cancelEncoderVerificationInternal();
	});

	it('shows no own tile pill before the first sample carries a sent size', () => {
		expect(resolveScreenShareDeliverySentInfo(null)).toBeNull();
		expect(ScreenShareDelivery.plan).toBeNull();
		expect(resolveScreenShareDeliverySentInfo({deliveredFrameRate: 15, sentHeight: null, sentWidth: null})).toBeNull();
		expect(resolveScreenShareDeliverySentInfo({deliveredFrameRate: null, sentHeight: 1080, sentWidth: 1920})).toEqual({
			width: 1920,
			height: 1080,
			fps: 0,
		});
	});

	it('leaves a control arm own tile on the captured track info', async () => {
		ScreenShareDelivery.beginShare({
			header: {chromiumMajor: 140, gpuKey: 'none', encoderMode: 'auto', hardwareAccelerationDisabled: false},
			...resolveConfiguredScreenShareLadder('display', {width: 3840, height: 2160}),
			hardware: {h264: true},
			adaptive: false,
		});
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(ScreenShareDelivery.plan).toBeNull();
		expect(resolveScreenShareDeliverySentInfo(ScreenShareDelivery.plan)).toBeNull();
		expect(showsScreenShareDeliverySentInfo(true, ScreenShareDelivery.adaptive)).toBe(false);
		expect(showsScreenShareDeliverySentInfo(true, true)).toBe(true);
		expect(showsScreenShareDeliverySentInfo(false, true)).toBe(false);
		adapter.cancelEncoderVerificationInternal();
	});

	it('shows only one toast for a share whose notices keep changing', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(ScreenShareDelivery.notice?.kind).toBe('device-framerate');
		feed.encodedFps = 12;
		await vi.advanceTimersByTimeAsync(TICK_MS * 8);
		expect(ScreenShareDelivery.notice?.kind).toBe('device-short');
		expect(recordedToasts()).toBe(1);
		adapter.cancelEncoderVerificationInternal();
	});

	it('toasts the first notice of a share and nothing after it', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(ScreenShareDelivery.notice?.kind).toBe('device-framerate');
		expect(createToast).toHaveBeenCalledTimes(1);
		expect(createToast).toHaveBeenCalledWith({type: 'info', children: expect.any(String)});
		feed.encodedFps = 12;
		await vi.advanceTimersByTimeAsync(TICK_MS * 8);
		expect(ScreenShareDelivery.notice?.kind).toBe('device-short');
		expect(createToast).toHaveBeenCalledTimes(1);
		adapter.cancelEncoderVerificationInternal();
	});

	it('toasts the first notice written straight to the store and nothing after it', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		vi.spyOn(VoiceSettings, 'getPreferredScreenShareCodec').mockReturnValue('vp9');
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		expect(createToast).toHaveBeenCalledTimes(1);
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		expect(ScreenShareDelivery.notice?.kind).toBe('codec-device');
		expect(createToast).toHaveBeenCalledTimes(1);
		adapter.cancelEncoderVerificationInternal();
	});

	it('toasts a warning notice in the tone the badge and the dot use', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 7.5;
		feed.targetBitrate = 600_000;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 12);
		const notice = ScreenShareDelivery.notice;
		expect(notice?.kind).toBe('connection-short');
		expect(notice && resolveScreenShareDeliveryNoticeTone(notice)).toBe('warning');
		expect(createToast).toHaveBeenCalledTimes(1);
		expect(createToast).toHaveBeenCalledWith({type: 'error', children: expect.any(String)});
		adapter.cancelEncoderVerificationInternal();
	});

	async function runShortWindow(primary: FakeSender): Promise<void> {
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 9);
		adapter.cancelEncoderVerificationInternal();
	}

	it('calls a short window device when the encoder is not filling the allocation it has', async () => {
		feed.encodedFps = 14;
		feed.targetBitrate = 1_566_000;
		feed.fill = 0.45;
		const primary = createSender(feed);
		await runShortWindow(primary);
		expect(ScreenShareDelivery.notice).toMatchObject({kind: 'device-framerate', frameRate: 15});
		expect(sentFrameRate(primary)).toBe(15);
	});

	it('calls a short window device when the encoder is over its frame budget and the link is short too', async () => {
		feed.encodedFps = 14;
		feed.targetBitrate = 1_012_500;
		feed.fill = 0.72;
		feed.encodeSecondsPerFrame = 1.55 / 30;
		const primary = createSender(feed);
		await runShortWindow(primary);
		expect(ScreenShareDelivery.notice).toMatchObject({kind: 'device-framerate', frameRate: 15});
		expect(sentFrameRate(primary)).toBe(15);
	});

	it('calls a short window device when Chromium has already blamed the processor', async () => {
		feed.encodedFps = 14;
		feed.reason = 'cpu';
		feed.targetBitrate = 1_012_500;
		feed.fill = 0.72;
		feed.encodeSecondsPerFrame = 0.6 / 30;
		const primary = createSender(feed);
		await runShortWindow(primary);
		expect(ScreenShareDelivery.notice).toMatchObject({kind: 'device-framerate', frameRate: 15});
		expect(sentFrameRate(primary)).toBe(15);
	});

	it('calls a short window device when the encoder holds the whole budget and still falls behind', async () => {
		feed.encodedFps = 20;
		feed.encodeSecondsPerFrame = 0.6 / 30;
		const primary = createSender(feed);
		await runShortWindow(primary);
		expect(ScreenShareDelivery.notice).toMatchObject({kind: 'device-framerate', frameRate: 15});
		expect(sentFrameRate(primary)).toBe(15);
	});

	it('calls a short window connection when the link is the only thing that is short', async () => {
		feed.encodedFps = 7.5;
		feed.targetBitrate = 600_000;
		feed.encodeSecondsPerFrame = 0.8 / 30;
		const primary = createSender(feed);
		await runShortWindow(primary);
		expect(ScreenShareDelivery.notice).toMatchObject({kind: 'connection-short', deliveredFrameRate: 8});
		expect(sentFrameRate(primary)).toBe(30);
		expect(lastDecision()?.shortCause).toBe('connection');
	});

	it('calls a short window device when one stalled tick sits in it and the rest are over budget', async () => {
		feed.encodedFps = 14;
		feed.targetBitrate = 1_012_500;
		feed.fill = 0.9;
		feed.encodeSecondsPerFrame = 1.5 / 30;
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 5);
		feed.encodedFps = 0;
		await vi.advanceTimersByTimeAsync(TICK_MS);
		feed.encodedFps = 14;
		await vi.advanceTimersByTimeAsync(TICK_MS * 3);
		adapter.cancelEncoderVerificationInternal();
		expect(ScreenShareDelivery.notice).toMatchObject({kind: 'device-framerate', frameRate: 15});
		expect(lastDecision()?.shortCause).toBe('duty');
		expect(sentFrameRate(primary)).toBe(15);
	});

	it('records that a connection notice was raised on a browser that never times an encode', async () => {
		feed.encodedFps = 7.5;
		feed.targetBitrate = 600_000;
		const primary = createSender(feed);
		await runShortWindow(primary);
		expect(ScreenShareDelivery.notice).toMatchObject({kind: 'connection-short'});
		expect(lastDecision()?.shortCause).toBe('connection-unmeasured');
	});

	it('reads the duty and the fill from the outbound report and copes when the browser reports neither', () => {
		const report = (fields: Record<string, unknown>) =>
			new Map<string, unknown>([
				['codec', {type: 'codec', id: 'codec', mimeType: 'video/H264'}],
				[
					'outbound',
					{
						type: 'outbound-rtp',
						id: 'outbound',
						kind: 'video',
						codecId: 'codec',
						active: true,
						frameWidth: 1920,
						frameHeight: 1080,
						targetBitrate: 1_250_000,
						...fields,
					},
				],
			]) as unknown as RTCStatsReport;
		const start = readScreenShareDeliverySample(
			report({timestamp: 0, framesEncoded: 0, bytesSent: 0, headerBytesSent: 0, totalEncodeTime: 0}),
			null,
		);
		const measured = readScreenShareDeliverySample(
			report({timestamp: 2000, framesEncoded: 60, bytesSent: 240_000, headerBytesSent: 10_000, totalEncodeTime: 1}),
			start.counters,
		);
		expect(measured.fill).toBeCloseTo(0.8, 10);
		expect(measured.encodeSecondsPerFrame).toBeCloseTo(1 / 60, 10);
		const bareStart = readScreenShareDeliverySample(report({timestamp: 0, framesEncoded: 0, bytesSent: 0}), null);
		const bare = readScreenShareDeliverySample(
			report({timestamp: 2000, framesEncoded: 60, bytesSent: 240_000}),
			bareStart.counters,
		);
		expect(bare.encodeSecondsPerFrame).toBeNull();
		expect(bare.fill).toBeCloseTo(0.768, 10);
	});

	it('keeps the live share tracked through the teardown a restart makes', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const adapter = createTeardownAdapter(participant, () => undefined);
		rememberLevel(2);
		ScreenShareDelivery.counters.switches = 1;
		ScreenShareDelivery.counters.probes = 3;
		await adapter.setScreenShareEnabled(room, true, {restartIfEnabled: true});
		expect(ScreenShareDelivery.isSharing).toBe(true);
		expect(ScreenShareDelivery.counters).toMatchObject({switches: 1, probes: 3});
		expect(ScreenShareDelivery.verdicts.h264?.settledIndex).toBe(2);
		expect(ActiveScreenShareSource.getPublishedSource()).toBe('display');
		expect(ActiveScreenShareSource.getSourceDimensions()).toEqual({width: 3840, height: 2160});
	});

	it('keeps writing the delivery plan after a restart tears the publication down', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const adapter = createTeardownAdapter(participant, () => undefined);
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(ScreenShareDelivery.plan).not.toBeNull();
		await adapter.setScreenShareEnabled(room, true, {restartIfEnabled: true});
		ScreenShareDelivery.plan = null;
		await vi.advanceTimersByTimeAsync(TICK_MS * 2);
		expect(ScreenShareDelivery.plan).not.toBeNull();
		adapter.cancelEncoderVerificationInternal();
	});

	it('drops an apply-failed notice that the restart leaves behind', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const adapter = createTeardownAdapter(participant, () => undefined);
		ScreenShareDelivery.setNotice({kind: 'apply-failed'});
		await adapter.setScreenShareEnabled(room, true, {restartIfEnabled: true});
		expect(ScreenShareDelivery.isSharing).toBe(true);
		expect(ScreenShareDelivery.notice).toBeNull();
	});

	it('ends the tracking when a restart leaves no live share behind', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const adapter = createTeardownAdapter(participant, () => {
			participant.isScreenShareEnabled = false;
		});
		await adapter.setScreenShareEnabled(room, true, {restartIfEnabled: true});
		expect(ScreenShareDelivery.isSharing).toBe(false);
		expect(ActiveScreenShareSource.getPublishedSource()).toBeNull();
	});

	it('keeps the tracking through the stop a source switch makes', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const adapter = createTeardownAdapter(participant, () => {
			participant.isScreenShareEnabled = false;
		});
		await adapter.setScreenShareEnabled(room, false, {
			sendUpdate: false,
			preserveStreamAudioPreferences: true,
			keepShareTracking: true,
		});
		expect(ScreenShareDelivery.isSharing).toBe(true);
		expect(ActiveScreenShareSource.getPublishedSource()).toBe('display');
	});

	it('ends the tracking when the server stops the share behind the user', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const adapter = createTeardownAdapter(participant, () => {
			participant.isScreenShareEnabled = false;
		});
		await adapter.setScreenShareEnabled(room, false, {sendUpdate: false});
		expect(ScreenShareDelivery.isSharing).toBe(false);
		expect(ActiveScreenShareSource.getPublishedSource()).toBeNull();
	});

	it('does not carry an apply-failed notice into the share a reconnect restores', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		ScreenShareDelivery.setNotice({kind: 'apply-failed'});
		const snapshot = adapter.prepareScreenShareReconnect(room);
		expect(snapshot?.delivery.notice).toBeNull();
		ScreenShareDelivery.endShare();
		ScreenShareDelivery.restore(snapshot?.delivery as never);
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 10);
		expect(ScreenShareDelivery.notice).toBeNull();
		adapter.cancelEncoderVerificationInternal();
	});

	it('carries a notice the share can still clear into the reconnect snapshot', () => {
		const {room} = createShare(createSender(feed).sender);
		const {adapter} = createAdapter();
		ScreenShareDelivery.setNotice({kind: 'codec-switched', codec: 'h264', previousCodec: 'vp9'});
		expect(adapter.prepareScreenShareReconnect(room)?.delivery.notice?.kind).toBe('codec-switched');
	});

	it('clears a notice written straight to the store after five good ticks', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		vi.spyOn(VoiceSettings, 'getPreferredScreenShareCodec').mockReturnValue('vp9');
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'codec-device', codec: 'h264', pinnedCodec: 'vp9'});
		expect(createToast).toHaveBeenCalledTimes(1);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 10);
		expect(ScreenShareDelivery.notice).toBeNull();
		adapter.cancelEncoderVerificationInternal();
	});

	it('clears a notice that survived a republish after five good ticks', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		ScreenShareDelivery.setNotice({kind: 'codec-switched', codec: 'h264', previousCodec: 'vp9'});
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		expect(ScreenShareDelivery.notice?.kind).toBe('codec-switched');
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 10);
		expect(ScreenShareDelivery.notice).toBeNull();
		adapter.cancelEncoderVerificationInternal();
	});

	it('gives a backup sender created later maintain-resolution and the current level', async () => {
		const primary = createSender(feed);
		const backup = createSender(feed, {mimeType: 'video/VP9'});
		const {participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		await adapter.enforceScreenShareSenderParametersInternal(participant as never, {videoCodec: 'vp9'});
		participant.emit(ParticipantEvent.LocalSenderCreated, backup.sender, track, 'h264');
		await vi.advanceTimersByTimeAsync(0);
		expect(backup.current().degradationPreference).toBe('maintain-resolution');
		expect(backup.current().encodings).toEqual([
			{maxBitrate: 4_500_000, maxFramerate: 30, scaleResolutionDownBy: 2, priority: 'high', networkPriority: 'high'},
		]);
		expect((backup.sender.track as unknown as {contentHint: string}).contentHint).toBe('text');
	});

	it('shows no modal when a listener republish takes over during recovery', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, republish, setEnabled} = createAdapter();
		publishLocalCapabilities.mockImplementationOnce(async () => {
			adapter.transitionScreenShareLifecycleInternal({
				type: 'share.replace',
				sourceType: 'display',
				publicationReplaceInFlight: true,
			});
			return null;
		});
		feed.encodedFps = 0;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(publishLocalCapabilities).toHaveBeenCalledTimes(1);
		expect(republish).not.toHaveBeenCalled();
		expect(setEnabled).not.toHaveBeenCalled();
		expect(pushWithKey).not.toHaveBeenCalled();
		adapter.cancelEncoderVerificationInternal();
	});

	it('stops with the modal when an already failed codec stalls again', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, setEnabled} = createAdapter();
		codecDenial = 'runtime-failed';
		vp8Available = true;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS);
		feed.encodedFps = 0;
		await vi.advanceTimersByTimeAsync(TICK_MS * 2);
		expect(pushWithKey).not.toHaveBeenCalled();
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(pushWithKey).toHaveBeenCalledTimes(1);
		expect(setEnabled).toHaveBeenCalledWith(room, false, {sendUpdate: true, playSound: true});
		expect(markRuntimeFailure).not.toHaveBeenCalled();
		adapter.cancelEncoderVerificationInternal();
	});

	it('raises apply-failed once and stops retrying a rejecting sender', async () => {
		const primary = createSender(feed, {reject: true});
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 10);
		expect(primary.setParameters).toHaveBeenCalledTimes(1);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'apply-failed'});
		expect(recordedToasts()).toBe(1);
		expect(createToast).toHaveBeenCalledWith({type: 'error', children: expect.any(String)});
		adapter.cancelEncoderVerificationInternal();
	});

	it('applies the level to the sender the track holds on each tick', async () => {
		const first = createSender(feed);
		const second = createSender(feed);
		const {room, participant, track} = createShare(first.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		track.sender = second.sender;
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(second.getStats).toHaveBeenCalledTimes(1);
		expect(second.current().degradationPreference).toBe('maintain-resolution');
		adapter.cancelEncoderVerificationInternal();
	});

	it('starts at the remembered level and says so', async () => {
		rememberLevel(1);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(0);
		expect(ScreenShareDelivery.notice).toMatchObject({kind: 'started-lower', frameRate: 15, targetFrameRate: 30});
		expect(primary.current().encodings[0].maxFramerate).toBe(15);
		await adapter.resetScreenShareDeliveryToFullQuality();
		expect(ScreenShareDelivery.notice).toBeNull();
		expect(primary.current().encodings[0].maxFramerate).toBe(30);
		adapter.cancelEncoderVerificationInternal();
	});

	it('says so when a delivery tick cannot be measured, then keeps measuring', async () => {
		const warned = vi.spyOn(console, 'warn').mockImplementation(() => undefined);
		const primary = createSender(feed);
		primary.getStats.mockRejectedValueOnce(new Error('stats unavailable'));
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(warned.mock.calls.some((call) => call.includes('Failed to measure screen share delivery'))).toBe(true);
		expect(ScreenShareDelivery.plan).toBeNull();
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(primary.getStats).toHaveBeenCalledTimes(2);
		expect(ScreenShareDelivery.plan?.levelIndex).toBe(0);
		adapter.cancelEncoderVerificationInternal();
	});

	it('starts at the top level when the memory holds a level the ladder never had', async () => {
		rememberLevel(-2);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(ScreenShareDelivery.notice).toBeNull();
		expect(ScreenShareDelivery.plan?.levelIndex).toBe(0);
		expect(sentFrameRate(primary)).toBe(30);
		adapter.cancelEncoderVerificationInternal();
	});

	it('names the substitution when the pinned codec is not what was published', () => {
		vi.spyOn(VoiceSettings, 'getPreferredScreenShareCodec').mockReturnValue('av1');
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'codec-device', codec: 'h264', pinnedCodec: 'av1'});
		adapter.cancelEncoderVerificationInternal();
	});

	it('reconciles a codec pair once per share', async () => {
		const primary = createSender(feed);
		const {room} = createShare(primary.sender);
		const {adapter, republish} = createAdapter();
		await adapter.reconcileScreenShareCodecInternal(room);
		await adapter.reconcileScreenShareCodecInternal(room);
		expect(republish).toHaveBeenCalledTimes(1);
		expect(republish).toHaveBeenCalledWith(room, expect.anything(), 'vp9');
	});

	it('recovers a codec that stalls after warm-up on another codec and says so', async () => {
		vp8Available = true;
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, setEnabled} = createRealFlowsAdapter();
		markRuntimeFailure.mockImplementation(() => {
			codecDenial = 'runtime-failed';
		});
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 3);
		feed.encodedFps = 0;
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(markRuntimeFailure).not.toHaveBeenCalled();
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(markRuntimeFailure).toHaveBeenCalledWith('h264', 'screen-share-encode-stalled');
		expect(publishLocalCapabilities).toHaveBeenCalledTimes(1);
		expect(participant.unpublishTrack).toHaveBeenCalledTimes(1);
		expect(participant.publishTrack).toHaveBeenCalledTimes(1);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'encoder-recovered', codec: 'vp8'});
		expect(setEnabled).not.toHaveBeenCalled();
		expect(pushWithKey).not.toHaveBeenCalled();
		adapter.cancelEncoderVerificationInternal();
	});

	it('says the encoder recovered when a negotiated codec republish takes the stalled share over', async () => {
		selectedCodec = 'h264';
		codecDenial = 'runtime-failed';
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, setEnabled} = createRealFlowsAdapter();
		publishLocalCapabilities.mockImplementation(async () => {
			void adapter.republishActiveScreenShareForNegotiatedCodecInternal(room, 'vp8');
			return null;
		});
		feed.encodedFps = 0;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(publishLocalCapabilities).toHaveBeenCalledTimes(1);
		expect(participant.unpublishTrack).toHaveBeenCalledTimes(1);
		expect(participant.publishTrack).toHaveBeenCalledTimes(1);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'encoder-recovered', codec: 'vp8'});
		expect(createToast).toHaveBeenCalledTimes(1);
		expect(setEnabled).not.toHaveBeenCalled();
		expect(pushWithKey).not.toHaveBeenCalled();
		adapter.cancelEncoderVerificationInternal();
	});

	it('leaves a codec switch notice alone when the republish did not follow a stalled encoder', async () => {
		selectedCodec = 'h264';
		const primary = createSender(feed);
		const {room, participant} = createShare(primary.sender);
		const {adapter} = createRealFlowsAdapter();
		ScreenShareDelivery.setNotice({kind: 'codec-switched', codec: 'vp8', previousCodec: 'h264'});
		await adapter.republishActiveScreenShareForNegotiatedCodecInternal(room, 'vp8');
		expect(participant.publishTrack).toHaveBeenCalledTimes(1);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'codec-switched', codec: 'vp8', previousCodec: 'h264'});
		adapter.cancelEncoderVerificationInternal();
	});

	it('needs a full warm-up run of stalls after a failed first tick', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		publishLocalCapabilities.mockImplementation(() => new Promise(() => undefined));
		feed.encodedFps = 0;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(publishLocalCapabilities).toHaveBeenCalledTimes(1);
		codecDenial = 'runtime-failed';
		await vi.advanceTimersByTimeAsync(TICK_MS * 2);
		expect(pushWithKey).not.toHaveBeenCalled();
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(pushWithKey).toHaveBeenCalledTimes(1);
		adapter.cancelEncoderVerificationInternal();
	});

	it('ignores stalled ticks while a share operation is pending', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, setEnabled} = createAdapter();
		codecDenial = 'runtime-failed';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 3);
		adapter.transitionScreenShareLifecycleInternal({type: 'share.restore', sourceType: 'display'});
		feed.encodedFps = 0;
		await vi.advanceTimersByTimeAsync(TICK_MS * 6);
		expect(pushWithKey).not.toHaveBeenCalled();
		expect(setEnabled).not.toHaveBeenCalled();
		adapter.cancelEncoderVerificationInternal();
	});

	it('ignores a viewerless share whose encodings are all paused', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, setEnabled} = createAdapter();
		codecDenial = 'runtime-failed';
		feed.active = false;
		feed.encodedFps = 0;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 12);
		expect(pushWithKey).not.toHaveBeenCalled();
		expect(setEnabled).not.toHaveBeenCalled();
		expect(ScreenShareDelivery.notice).toBeNull();
		expect(sentFrameRate(primary)).toBe(30);
		adapter.cancelEncoderVerificationInternal();
	});

	it('switches codec once when the top level is short and then steps down', async () => {
		vp8Available = true;
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(publishLocalCapabilities).toHaveBeenCalledTimes(1);
		expect(ScreenShareDelivery.verdicts.h264).toMatchObject({short: true, ratio: 0.5});
		expect(ScreenShareDelivery.counters.switches).toBe(1);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'codec-switched', codec: 'vp8', previousCodec: 'h264'});
		expect(sentFrameRate(primary)).toBe(30);
		await vi.advanceTimersByTimeAsync(TICK_MS * 40);
		expect(publishLocalCapabilities).toHaveBeenCalledTimes(1);
		expect(sentFrameRate(primary)).toBe(15);
		adapter.cancelEncoderVerificationInternal();
	});

	it('steps down instead of switching when no viewer can decode the other codec', async () => {
		vp8Available = true;
		knownDecode = [new Set(['h264'])];
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(publishLocalCapabilities).not.toHaveBeenCalled();
		expect(sentFrameRate(primary)).toBe(15);
		adapter.cancelEncoderVerificationInternal();
	});

	const watchStreamKey = () => getStreamKey('guild', 'channel', 'me');

	const watchers = (...users: ReadonlyArray<string>) => {
		voiceConnectionContext = {guildId: 'guild', channelId: 'channel', connectionId: 'me'};
		voiceStates = {
			guild: {
				channel: Object.fromEntries(
					users.map((user, index) => [
						`connection-${index}`,
						{user_id: user, connection_id: `connection-${index}`, viewer_stream_keys: [watchStreamKey()]},
					]),
				),
			},
		};
	};

	const startMediumShare = () => {
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue('medium');
		commitLadder('display', {width: 1280, height: 720});
		feed.targetBitrate = 3_000_000;
		feed.sentWidth = 1280;
		feed.sentHeight = 720;
	};

	it('counts one viewer per user and leaves the publisher out', () => {
		expect(
			countStreamViewers(
				{
					guild: {
						channel: {
							a: {user_id: 'watcher', viewer_stream_keys: ['stream:me']},
							b: {user_id: 'watcher', viewer_stream_keys: ['stream:me']},
							c: {user_id: 'publisher', viewer_stream_keys: ['stream:me']},
							d: {user_id: 'idle', viewer_stream_keys: ['stream:other']},
							e: {user_id: 'quiet'},
						},
					},
				},
				'stream:me',
				'publisher',
			),
		).toBe(1);
	});

	it('turns extra layers on for a second viewer and republishes to carry them', async () => {
		startMediumShare();
		watchers('first', 'second');
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, republish} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(republish).toHaveBeenCalledTimes(1);
		expect(ScreenShareDelivery.multiLayer).toBe(true);
		expect(ScreenShareDelivery.counters.layerings).toBe(1);
		watchers('first');
		await vi.advanceTimersByTimeAsync(TICK_MS * 30);
		expect(republish).toHaveBeenCalledTimes(2);
		expect(ScreenShareDelivery.multiLayer).toBe(false);
		expect(ScreenShareDelivery.counters.layerings).toBe(2);
		watchers('first', 'second');
		await vi.advanceTimersByTimeAsync(TICK_MS * 30);
		expect(republish).toHaveBeenCalledTimes(2);
		adapter.cancelEncoderVerificationInternal();
	});

	it('republishes a crossing with the layering it crossed to, not the one it left', async () => {
		startMediumShare();
		watchers('first', 'second');
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, republish} = createAdapter();
		const {flows, adapter: flowAdapter} = createFlowHooks();
		flowAdapter.getEffectivePublishOptionsInternal = async (enabled: boolean, options?: TrackPublishOptions) =>
			getEffectivePublishOptions(enabled, options);
		republish.mockImplementation((activeRoom, activeTrack, codec) =>
			flows.republishActiveShareWithCodec(activeRoom, activeTrack, codec),
		);
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(republish).toHaveBeenCalledTimes(1);
		const [, options] = participant.publishTrack.mock.calls[0] as unknown as [unknown, TrackPublishOptions];
		expect(options.simulcast).toBe(true);
		expect(ScreenShareDelivery.multiLayer).toBe(true);
		adapter.cancelEncoderVerificationInternal();
	});

	it('keeps the single layering when the republish a crossing asked for never happens', async () => {
		startMediumShare();
		watchers('first', 'second');
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, republish} = createAdapter();
		republish.mockResolvedValue(false);
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(republish).toHaveBeenCalledTimes(1);
		expect(ScreenShareDelivery.multiLayer).toBe(false);
		expect(ScreenShareDelivery.counters.layerings).toBe(1);
		expect((await getEffectivePublishOptions(true, {videoCodec: 'av1'}))?.scalabilityMode).toBe('L1T3');
		await vi.advanceTimersByTimeAsync(TICK_MS * 30);
		expect(ScreenShareDelivery.notice).not.toEqual({kind: 'apply-failed'});
		adapter.cancelEncoderVerificationInternal();
	});

	it('publishes the layering a crossing asked for and refuses simulcast above the medium rung', async () => {
		startMediumShare();
		expect((await getEffectivePublishOptions(true, {videoCodec: 'h264'}))?.simulcast).toBe(false);
		ScreenShareDelivery.multiLayer = true;
		const h264 = await getEffectivePublishOptions(true, {videoCodec: 'h264'});
		expect(h264?.simulcast).toBe(true);
		expect(h264?.scalabilityMode).toBeUndefined();
		const av1 = await getEffectivePublishOptions(true, {videoCodec: 'av1'});
		expect(av1?.simulcast).toBe(false);
		expect(av1?.scalabilityMode).toBe('L3T3_KEY');
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue('high');
		commitLadder('display', {width: 3840, height: 2160});
		ScreenShareDelivery.multiLayer = true;
		expect((await getEffectivePublishOptions(true, {videoCodec: 'h264'}))?.simulcast).toBe(false);
		expect((await getEffectivePublishOptions(true, {videoCodec: 'av1'}))?.scalabilityMode).toBe('L3T3_KEY');
	});

	it('keeps a single viewer on one encoding', async () => {
		startMediumShare();
		watchers('first');
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, republish} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 20);
		expect(republish).not.toHaveBeenCalled();
		expect(ScreenShareDelivery.multiLayer).toBe(false);
		adapter.cancelEncoderVerificationInternal();
	});

	it('refuses simulcast above the medium rung however many people watch', async () => {
		watchers('first', 'second');
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, republish} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 20);
		expect(republish).not.toHaveBeenCalled();
		expect(ScreenShareDelivery.multiLayer).toBe(false);
		adapter.cancelEncoderVerificationInternal();
	});

	it('takes the extra layers off a share retargeted above the rung that admits them', async () => {
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue('ultra');
		commitLadder('display', {width: 3840, height: 2160});
		feed.targetBitrate = 5_500_000;
		feed.sentWidth = 2560;
		feed.sentHeight = 1440;
		watchers('first', 'second');
		ScreenShareDelivery.multiLayer = true;
		const primary = createSender(feed, {encodings: [{rid: 'f'}, {rid: 'h', scaleResolutionDownBy: 2}]});
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, republish} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(republish).toHaveBeenCalledTimes(1);
		expect(ScreenShareDelivery.multiLayer).toBe(false);
		expect(ScreenShareDelivery.counters.layerings).toBe(1);
		adapter.cancelEncoderVerificationInternal();
	});

	it('never crosses to extra layers while an apply failure stands', async () => {
		startMediumShare();
		watchers('first', 'second');
		const primary = createSender(feed, {reject: true});
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, republish} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 20);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'apply-failed'});
		expect(republish).not.toHaveBeenCalled();
		expect(ScreenShareDelivery.multiLayer).toBe(false);
		adapter.cancelEncoderVerificationInternal();
	});

	it('never crosses to extra layers while a probe is in flight', async () => {
		startMediumShare();
		rememberLevel(1);
		watchers('first', 'second');
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, republish} = createAdapter();
		feed.encodedFps = 15;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		await tickUntilDecision('probe', 60);
		for (let tick = 0; tick < 24; tick++) {
			feed.encodedFps = tick % 2 === 0 ? 30 : 20;
			await vi.advanceTimersByTimeAsync(TICK_MS);
		}
		expect(lastDecision()?.decision).toBe('probe');
		expect(republish).not.toHaveBeenCalled();
		expect(ScreenShareDelivery.multiLayer).toBe(false);
		adapter.cancelEncoderVerificationInternal();
	});

	it('waits for a settled level before it crosses to extra layers', async () => {
		startMediumShare();
		watchers('first', 'second');
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, republish} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 20);
		expect(ScreenShareDelivery.plan?.levelIndex).toBeGreaterThan(0);
		expect(republish).not.toHaveBeenCalled();
		adapter.cancelEncoderVerificationInternal();
	});

	it('switches to AV1 when every viewer has advertised it', async () => {
		av1Available = true;
		knownDecode = [new Set(['h264', 'av1'])];
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(publishLocalCapabilities).toHaveBeenCalledTimes(1);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'codec-switched', codec: 'av1', previousCodec: 'h264'});
		adapter.cancelEncoderVerificationInternal();
	});

	it('steps down instead of switching to AV1 while a viewer has not advertised its codecs', async () => {
		av1Available = true;
		knownDecode = [new Set(['h264', 'av1'])];
		unknownParticipants = 1;
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(publishLocalCapabilities).not.toHaveBeenCalled();
		expect(sentFrameRate(primary)).toBe(15);
		adapter.cancelEncoderVerificationInternal();
	});

	it('steps down instead of switching to AV1 in a room where the only viewer is silent', async () => {
		av1Available = true;
		knownDecode = [];
		unknownParticipants = 1;
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(publishLocalCapabilities).not.toHaveBeenCalled();
		expect(sentFrameRate(primary)).toBe(15);
		adapter.cancelEncoderVerificationInternal();
	});

	const commitLadder = (context: 'display' | 'device', dimensions: {width: number; height: number} | null) => {
		ScreenShareDelivery.endShare();
		ScreenShareDelivery.beginShare({
			header: {chromiumMajor: 140, gpuKey: 'none', encoderMode: 'auto', hardwareAccelerationDisabled: false},
			...resolveConfiguredScreenShareLadder(context, dimensions),
			hardware: {h264: true},
			adaptive: true,
		});
	};

	const leaveStaleSource = (
		published: 'display' | 'device' | null,
		dimensions: {width: number; height: number} | null,
	) => {
		if (published === null) {
			ActiveScreenShareSource.clear();
			return;
		}
		ActiveScreenShareSource.setPublishedSource(published, published === 'display' ? 'screen:9' : null);
		ActiveScreenShareSource.setSourceDimensions(dimensions);
	};

	const expectMonitoredLadderWins = async (stale: {
		context: 'display' | 'device';
		dimensions: {width: number; height: number} | null;
	}) => {
		const committed = ScreenShareDelivery.ladder;
		if (!committed) throw new Error('no committed ladder');
		const staleLevel = resolveConfiguredScreenShareLadder(stale.context, stale.dimensions).levels[0];
		expect(staleLevel.maxBitrate).not.toBe(committed.levels[0].maxBitrate);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		await adapter.enforceScreenShareSenderParametersInternal(participant as never, {videoCodec: 'h264'});
		expect(primary.current().encodings[0].maxBitrate).toBe(committed.levels[0].maxBitrate);
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(primary.current().encodings[0].maxBitrate).toBe(committed.levels[0].maxBitrate);
		expect(primary.current().encodings[0].maxFramerate).toBe(committed.levels[0].frameRate);
		adapter.cancelEncoderVerificationInternal();
	};

	it('monitors the committed ladder on the first share of a session', async () => {
		commitLadder('display', {width: 1280, height: 720});
		leaveStaleSource(null, null);
		await expectMonitoredLadderWins({context: 'display', dimensions: null});
	});

	it('monitors the committed ladder when the share kind changes', async () => {
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue('source');
		commitLadder('device', null);
		leaveStaleSource('display', {width: 3840, height: 2160});
		await expectMonitoredLadderWins({context: 'display', dimensions: {width: 3840, height: 2160}});
	});

	it('monitors the committed ladder across a source switch', async () => {
		commitLadder('display', {width: 1280, height: 720});
		leaveStaleSource('display', {width: 3840, height: 2160});
		await expectMonitoredLadderWins({context: 'display', dimensions: {width: 3840, height: 2160}});
	});

	it('monitors the ladder the store holds after a failed switch puts it back', async () => {
		commitLadder('display', {width: 1280, height: 720});
		const before = ScreenShareDelivery.snapshot();
		const restoredLevel = before.share?.levels[0];
		if (!restoredLevel) throw new Error('no committed ladder');
		ScreenShareDelivery.retarget(resolveConfiguredScreenShareLadder('display', {width: 3840, height: 2160}));
		const switched = ScreenShareDelivery.ladder;
		if (!switched) throw new Error('no retargeted ladder');
		expect(switched.levels[0].maxBitrate).not.toBe(restoredLevel.maxBitrate);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		ScreenShareDelivery.restore(before);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(primary.current().encodings[0].maxBitrate).toBe(restoredLevel.maxBitrate);
		adapter.cancelEncoderVerificationInternal();
	});

	it('names the capture when a device share falls short of the committed resolution', async () => {
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue('source');
		commitLadder('device', null);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.sentWidth = 640;
		feed.sentHeight = 360;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 20);
		expect(ScreenShareDelivery.notice).toMatchObject({kind: 'capture-resolution'});
		adapter.cancelEncoderVerificationInternal();
	});

	it('spends the kept axis of a committed ladder before it gives the other one up', async () => {
		vi.spyOn(VoiceSettings, 'getScreenShareContentHint').mockReturnValue('motion');
		vi.spyOn(VoiceSettings, 'getVideoFrameRate').mockReturnValue(60);
		commitLadder('display', {width: 3840, height: 2160});
		const ladder = ScreenShareDelivery.ladder;
		if (!ladder) throw new Error('no committed ladder');
		expect(ladder.target.keep).toBe('frameRate');
		const bottom = ladder.levels[ladder.levels.length - 1];
		expect([bottom.segment, bottom.frameRate]).toEqual([1, 30]);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 30;
		feed.reason = 'cpu';
		feed.targetBitrate = 8_000_000;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 40);
		expect(sentFrameRate(primary)).toBe(bottom.frameRate);
		expect(primary.current().encodings[0].maxBitrate).toBe(bottom.maxBitrate);
		adapter.cancelEncoderVerificationInternal();
	});

	it('steps past the kept axis only when the committed ladder says the kept one is spent', async () => {
		commitLadder('display', {width: 3840, height: 2160});
		const ladder = ScreenShareDelivery.ladder;
		if (!ladder) throw new Error('no committed ladder');
		expect(ladder.target.keep).toBe('resolution');
		const bottom = ladder.levels[ladder.levels.length - 1];
		expect(bottom.segment).toBe(1);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 5;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 40);
		expect(primary.current().encodings[0].maxBitrate).toBe(bottom.maxBitrate);
		expect(sentFrameRate(primary)).toBe(bottom.frameRate);
		adapter.cancelEncoderVerificationInternal();
	});

	it('keeps the failed sender note while the share is held live', async () => {
		const primary = createSender(feed, {reject: true});
		const {room, participant, track} = createShare(primary.sender);
		const adapter = createTeardownAdapter(participant, () => undefined);
		await adapter.enforceScreenShareSenderParametersInternal(participant as never, {videoCodec: 'h264'});
		expect(primary.setParameters).toHaveBeenCalledTimes(1);
		await adapter.setScreenShareEnabled(room, false, {
			sendUpdate: false,
			preserveStreamAudioPreferences: true,
			keepShareTracking: true,
		});
		primary.setRejecting(false);
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS);
		expect(primary.setParameters).toHaveBeenCalledTimes(1);
		adapter.cancelEncoderVerificationInternal();
	});

	it('keeps the codec reconcile dedupe while the share is held live', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const adapter = createTeardownAdapter(participant, () => undefined);
		const republish = vi.spyOn(adapter.liveKitFlows, 'republishActiveShareWithCodec').mockResolvedValue(true);
		await adapter.reconcileScreenShareCodecInternal(room);
		await adapter.setScreenShareEnabled(room, false, {
			sendUpdate: false,
			preserveStreamAudioPreferences: true,
			keepShareTracking: true,
		});
		await adapter.reconcileScreenShareCodecInternal(room);
		expect(republish).toHaveBeenCalledTimes(1);
	});

	it('holds the tracking until the outermost held operation finishes', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const held: Array<boolean> = [];
		const adapter = createTeardownAdapter(participant, () => undefined);
		const routing = adapter.controllerRouting as unknown as {setEnabled: () => Promise<void>};
		const inner = routing.setEnabled;
		routing.setEnabled = async () => {
			routing.setEnabled = inner;
			await adapter.setScreenShareEnabled(room, false, {
				sendUpdate: false,
				preserveStreamAudioPreferences: true,
				keepShareTracking: true,
			});
			held.push(ScreenShareDelivery.isSharing);
			await inner();
		};
		await adapter.setScreenShareEnabled(room, false, {
			sendUpdate: false,
			preserveStreamAudioPreferences: true,
			keepShareTracking: true,
		});
		expect(held).toEqual([true]);
		expect(ScreenShareDelivery.isSharing).toBe(true);
		expect(ActiveScreenShareSource.getPublishedSource()).toBe('display');
	});

	it('runs the reconnect restore whenever a snapshot waits or the stream is meant to come back', () => {
		const {room} = createShare(createSender(feed).sender);
		const {adapter} = createAdapter();
		const snapshot = adapter.prepareScreenShareReconnect(room);
		expect(snapshot).not.toBeNull();
		expect(shouldRestoreScreenShareAfterReconnect(true, snapshot)).toBe(true);
		expect(shouldRestoreScreenShareAfterReconnect(false, snapshot)).toBe(true);
		expect(shouldRestoreScreenShareAfterReconnect(true, null)).toBe(true);
		expect(shouldRestoreScreenShareAfterReconnect(false, null)).toBe(false);
	});

	it('restores the reconnect tracking when the new room already reports the share', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const {adapter} = createAdapter();
		ScreenShareDelivery.counters.probes = 2;
		const snapshot = adapter.prepareScreenShareReconnect(room);
		adapter.resetStreamTracking();
		expect(ScreenShareDelivery.isSharing).toBe(false);
		const {flows} = createFlowHooks();
		expect(await flows.restoreReconnect(room, snapshot as never)).toBe(true);
		expect(participant.isScreenShareEnabled).toBe(true);
		expect(ScreenShareDelivery.counters.probes).toBe(2);
		expect(ActiveScreenShareSource.getPublishedSource()).toBe('display');
		expect(ActiveScreenShareSource.getSourceDimensions()).toEqual({width: 3840, height: 2160});
	});

	it('runs the reconnect restore for a snapshot the room already reports as enabled', async () => {
		const {room} = createShare(createSender(feed).sender);
		const adapter = new VoiceEngineV2AppScreenShareExecutionAdapter();
		const {flows} = createFlowHooks();
		Object.assign(adapter.liveKitFlows, {
			restoreReconnect: (...args: Parameters<typeof flows.restoreReconnect>) => flows.restoreReconnect(...args),
		});
		ScreenShareDelivery.counters.probes = 4;
		const snapshot = adapter.prepareScreenShareReconnect(room);
		adapter.resetStreamTracking();
		expect(ScreenShareDelivery.isSharing).toBe(false);
		expect(await adapter.restoreScreenShareAfterReconnect(room, snapshot)).toBe(true);
		expect(ScreenShareDelivery.counters.probes).toBe(4);
		expect(ActiveScreenShareSource.getPublishedSource()).toBe('display');
	});

	it('commits a ladder for the share a reconnect restarts without a snapshot', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const adapter = new VoiceEngineV2AppScreenShareExecutionAdapter();
		const routing = adapter.controllerRouting as unknown as {setEnabled: () => Promise<void>};
		let published: TrackPublishOptions | undefined;
		routing.setEnabled = async () => {
			published = await getEffectivePublishOptions(true, undefined);
		};
		ScreenShareDelivery.endShare();
		ActiveScreenShareSource.clear();
		expect(await adapter.restoreScreenShareAfterReconnect(room, null)).toBe(true);
		const ladder = ScreenShareDelivery.ladder;
		if (!ladder) throw new Error('no committed ladder');
		expect(published?.screenShareEncoding).toEqual({
			maxBitrate: ladder.target.maxBitrate,
			maxFramerate: ladder.target.frameRate,
			priority: 'high',
		});
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(primary.current().encodings[0].maxBitrate).toBe(ladder.levels[0].maxBitrate);
		adapter.cancelEncoderVerificationInternal();
	});

	it('publishes the ladder a reconnect restores rather than the settings of the moment', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const {adapter} = createAdapter();
		commitLadder('display', {width: 1280, height: 720});
		const restored = ScreenShareDelivery.ladder;
		if (!restored) throw new Error('no committed ladder');
		const snapshot = adapter.prepareScreenShareReconnect(room);
		adapter.resetStreamTracking();
		participant.isScreenShareEnabled = false;
		expect(resolveConfiguredScreenShareLadder('display', {width: 3840, height: 2160}).target.maxBitrate).not.toBe(
			restored.target.maxBitrate,
		);
		const {flows, adapter: flowAdapter} = createFlowHooks();
		flowAdapter.getEffectivePublishOptionsInternal = async (enabled: boolean, options?: TrackPublishOptions) =>
			getEffectivePublishOptions(enabled, options);
		expect(await flows.restoreReconnect(room, snapshot as never)).toBe(true);
		const [, options] = participant.publishTrack.mock.calls[0] as unknown as [unknown, TrackPublishOptions];
		expect(options.screenShareEncoding?.maxBitrate).toBe(restored.target.maxBitrate);
	});

	it('leaves the reconnect tracking alone when a pending operation has already published', async () => {
		const {room} = createShare(createSender(feed).sender);
		const {adapter} = createAdapter();
		const snapshot = adapter.prepareScreenShareReconnect(room);
		adapter.resetStreamTracking();
		commitLadder('device', null);
		const owned = ScreenShareDelivery.ladder;
		const {flows, adapter: flowAdapter} = createFlowHooks();
		flowAdapter.isScreenSharePending = true;
		expect(await flows.restoreReconnect(room, snapshot as never)).toBe(false);
		expect(ScreenShareDelivery.ladder).toBe(owned);
		expect(ActiveScreenShareSource.getPublishedSource()).toBeNull();
	});

	it('leaves the tracking a reset handed to the room that took over', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const adapter = new VoiceEngineV2AppScreenShareExecutionAdapter();
		const routing = adapter.controllerRouting as unknown as {setEnabled: () => Promise<void>};
		routing.setEnabled = async () => {
			adapter.resetStreamTracking();
			participant.isScreenShareEnabled = false;
			commitLadder('device', null);
			ActiveScreenShareSource.setPublishedSource('device', null);
		};
		await adapter.setScreenShareEnabled(room, true, {sendUpdate: false, restartIfEnabled: true});
		expect(ScreenShareDelivery.isSharing).toBe(true);
		expect(ActiveScreenShareSource.getPublishedSource()).toBe('device');
	});

	it('holds the tracking until the outermost held restart finishes', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		participant.isScreenShareEnabled = false;
		const adapter = new VoiceEngineV2AppScreenShareExecutionAdapter();
		const routing = adapter.controllerRouting as unknown as {setEnabled: () => Promise<void>};
		const held: Array<boolean> = [];
		routing.setEnabled = async () => {
			routing.setEnabled = async () => undefined;
			await adapter.setScreenShareEnabled(room, true, {sendUpdate: false, restartIfEnabled: true});
			held.push(ScreenShareDelivery.isSharing);
		};
		await adapter.setScreenShareEnabled(room, true, {sendUpdate: false, restartIfEnabled: true});
		expect(held).toEqual([true]);
		expect(ScreenShareDelivery.isSharing).toBe(false);
	});

	it('restores the reconnect tracking when another screen share operation is pending', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const {adapter} = createAdapter();
		ScreenShareDelivery.counters.probes = 3;
		const snapshot = adapter.prepareScreenShareReconnect(room);
		adapter.resetStreamTracking();
		participant.isScreenShareEnabled = false;
		const {flows, adapter: flowAdapter} = createFlowHooks();
		flowAdapter.isScreenSharePending = true;
		expect(await flows.restoreReconnect(room, snapshot as never)).toBe(false);
		expect(ScreenShareDelivery.counters.probes).toBe(3);
		expect(ActiveScreenShareSource.getPublishedSource()).toBe('display');
	});

	it('leaves the reconnect tracking alone when a pending operation already owns a share', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const {adapter} = createAdapter();
		const snapshot = adapter.prepareScreenShareReconnect(room);
		adapter.resetStreamTracking();
		participant.isScreenShareEnabled = false;
		commitLadder('device', null);
		const owned = ScreenShareDelivery.ladder;
		const {flows, adapter: flowAdapter} = createFlowHooks();
		flowAdapter.isScreenSharePending = true;
		expect(await flows.restoreReconnect(room, snapshot as never)).toBe(false);
		expect(ScreenShareDelivery.ladder).toBe(owned);
		expect(ActiveScreenShareSource.getPublishedSource()).toBeNull();
	});

	it('keeps the screen share publication while a codec republish is in flight', () => {
		const room = Object.assign(new EventEmitter(), {
			localParticipant: Object.assign(new EventEmitter(), {isLocal: true, identity: 'local'}),
			remoteParticipants: new Map(),
		}) as unknown as Room;
		const unpublished: Array<boolean> = [];
		let replaceInFlight = true;
		const localStateChanges: Array<unknown> = [];
		bindRoomEvents(
			room,
			1,
			null,
			'channel',
			{} as never,
			{
				connection: {createGuardedHandler: (_id: number, handler: unknown) => handler},
				media: {},
				mediaState: {
					handleLocalTrackStateChange: (source: unknown) => {
						localStateChanges.push(source);
						return true;
					},
				},
				participants: {upsertParticipant: () => undefined},
				permissions: {},
				remoteSpeaking: {},
				screenShare: {
					handleLocalScreenShareTrackUnpublished: () => {
						unpublished.push(true);
					},
					isScreenSharePublicationReplaceInFlight: () => replaceInFlight,
				},
				subscriptions: {},
			} as never,
		);
		const publication = {source: Track.Source.ScreenShare};
		(room as unknown as EventEmitter).emit(RoomEvent.LocalTrackUnpublished, publication, {isLocal: true});
		expect(unpublished).toEqual([]);
		expect(localStateChanges).toEqual([]);
		replaceInFlight = false;
		(room as unknown as EventEmitter).emit(RoomEvent.LocalTrackUnpublished, publication, {isLocal: true});
		expect(unpublished).toEqual([true]);
		expect(localStateChanges).toEqual([Track.Source.ScreenShare]);
	});

	it('never monitors a share the store never began', async () => {
		ScreenShareDelivery.endShare();
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 88);
		expect(publishLocalCapabilities).not.toHaveBeenCalled();
		expect(primary.setParameters).not.toHaveBeenCalled();
	});

	it('neither switches nor probes once the store stops tracking a monitored share', async () => {
		vp8Available = true;
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		ScreenShareDelivery.endShare();
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(publishLocalCapabilities).not.toHaveBeenCalled();
		expect(sentFrameRate(primary)).toBe(15);
		feed.reason = 'none';
		await vi.advanceTimersByTimeAsync(TICK_MS * 80);
		expect(primary.setParameters.mock.calls.map(([params]) => params.encodings[0].maxFramerate)).toEqual([30, 15]);
		adapter.cancelEncoderVerificationInternal();
	});

	const beginControlArmShare = () => {
		ScreenShareDelivery.endShare();
		ScreenShareDelivery.beginShare({
			header: {chromiumMajor: 140, gpuKey: 'none', encoderMode: 'auto', hardwareAccelerationDisabled: false},
			...resolveConfiguredScreenShareLadder('display', {width: 3840, height: 2160}),
			hardware: {h264: true},
			adaptive: false,
		});
	};

	it('holds the published target for a control-arm share and stores nothing', async () => {
		vp8Available = true;
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		beginControlArmShare();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 88);
		expect(publishLocalCapabilities).not.toHaveBeenCalled();
		expect(ScreenShareDelivery.notice).toBeNull();
		expect(ScreenShareDelivery.plan).toBeNull();
		expect(primary.setParameters.mock.calls.length).toBeGreaterThan(0);
		expect(new Set(primary.setParameters.mock.calls.map(([params]) => params.encodings[0].maxFramerate))).toEqual(
			new Set([30]),
		);
		adapter.cancelEncoderVerificationInternal();
	});

	it('recovers a stalled encoder for a control-arm share without showing a notice', async () => {
		vp8Available = true;
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, republish} = createAdapter();
		beginControlArmShare();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 3);
		feed.encodedFps = 0;
		await vi.advanceTimersByTimeAsync(TICK_MS * 2);
		expect(markRuntimeFailure).toHaveBeenCalledWith('h264', 'screen-share-encode-stalled');
		expect(republish).toHaveBeenCalledWith(room, track, 'vp8');
		expect(ScreenShareDelivery.notice).toBeNull();
		adapter.cancelEncoderVerificationInternal();
	});

	it('probes up at once after a minute of clean ticks and reverts at once when the probe falls short', async () => {
		rememberLevel(1);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(sentFrameRate(primary)).toBe(15);
		await tickUntilDecision('probe', 40);
		expect(sentFrameRate(primary)).toBe(30);
		await tickUntilDecision('revert-probe', 12);
		expect(sentFrameRate(primary)).toBe(15);
		expect(ScreenShareDelivery.counters.probeBlocked).toBe(true);
		await vi.advanceTimersByTimeAsync(TICK_MS * 80);
		expect(primary.setParameters.mock.calls.map(([params]) => params.encodings[0].maxFramerate)).toEqual([15, 30, 15]);
		adapter.cancelEncoderVerificationInternal();
	});

	it('reverts a probe onto a level the shrunken ladder still holds', async () => {
		expect(ScreenShareDelivery.ladder?.levels).toHaveLength(3);
		rememberLevel(2);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		await tickUntilDecision('probe', 60);
		const shrunken = resolveConfiguredScreenShareLadder('display', {width: 1280, height: 720});
		expect(shrunken.levels).toHaveLength(2);
		ScreenShareDelivery.retarget(shrunken);
		feed.encodedFps = 5;
		await tickUntilDecision('revert-probe', 20);
		expect(lastDecision()).toMatchObject({decision: 'revert-probe', levelIndex: 1});
		expect(ScreenShareDelivery.notice).toEqual({
			kind: 'device-framerate',
			width: 1280,
			height: 720,
			frameRate: 15,
		});
		adapter.cancelEncoderVerificationInternal();
	});

	it('settles onto a level the shrunken ladder still holds when it never probed', async () => {
		expect(ScreenShareDelivery.ladder?.levels).toHaveLength(3);
		rememberLevel(2, Date.now() + 3_600_000);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 4);
		expect(ScreenShareDelivery.plan?.levelIndex).toBe(2);
		const shrunken = resolveConfiguredScreenShareLadder('display', {width: 1280, height: 720});
		expect(shrunken.levels).toHaveLength(2);
		ScreenShareDelivery.retarget(shrunken);
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(ScreenShareDelivery.plan?.levelIndex).toBe(1);
		expect(ScreenShareDelivery.plan?.level).toEqual(shrunken.levels[1]);
		adapter.cancelEncoderVerificationInternal();
	});

	it('probes at most four times in one share', async () => {
		rememberLevel(1);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		let capacity = 30;
		let ticksAtTop = 0;
		let probes = 0;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		for (let tick = 0; tick < 400; tick++) {
			const before = sentFrameRate(primary) ?? 0;
			feed.encodedFps = Math.min(capacity, before);
			await vi.advanceTimersByTimeAsync(TICK_MS);
			const after = sentFrameRate(primary) ?? 0;
			if (after > before) probes += 1;
			ticksAtTop = after === 30 ? ticksAtTop + 1 : 0;
			capacity = ticksAtTop > 10 ? 15 : after === 15 ? 30 : capacity;
		}
		expect(probes).toBe(4);
		adapter.cancelEncoderVerificationInternal();
	});

	it('does not probe while the memory blocks it, until Try full quality again', async () => {
		rememberLevel(1, Date.now() + 3_600_000);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 60);
		expect(sentFrameRate(primary)).toBe(15);
		expect(primary.setParameters).toHaveBeenCalledTimes(1);
		await adapter.resetScreenShareDeliveryToFullQuality();
		expect(sentFrameRate(primary)).toBe(30);
		await tickUntilDecision('step', 12);
		expect(sentFrameRate(primary)).toBe(15);
		await tickUntilDecision('probe', 40);
		expect(sentFrameRate(primary)).toBe(30);
		adapter.cancelEncoderVerificationInternal();
	});

	it('allows a codec switch again after Try full quality again', async () => {
		vp8Available = true;
		ScreenShareDelivery.counters.switches = 1;
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.encodedFps = 15;
		feed.reason = 'cpu';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await adapter.resetScreenShareDeliveryToFullQuality();
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 8);
		expect(publishLocalCapabilities).toHaveBeenCalledTimes(1);
		adapter.cancelEncoderVerificationInternal();
	});

	it('re-applies the level on the tick a resolution-short window closes', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		feed.sentWidth = 1280;
		feed.sentHeight = 720;
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 6);
		let reads = primary.getParameters.mock.calls.length;
		await vi.advanceTimersByTimeAsync(TICK_MS);
		const plainTickReads = primary.getParameters.mock.calls.length - reads;
		expect(lastDecision()).toBeUndefined();
		reads = primary.getParameters.mock.calls.length;
		await vi.advanceTimersByTimeAsync(TICK_MS);
		const decisionTickReads = primary.getParameters.mock.calls.length - reads;
		expect(lastDecision()?.decision).toBe('reapply-level');
		expect(plainTickReads).toBeGreaterThan(0);
		expect(decisionTickReads).toBe(plainTickReads * 2);
		adapter.cancelEncoderVerificationInternal();
	});

	it('restarts warm-up on a settings push, so one stalled sample does not stop the share', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		codecDenial = 'runtime-failed';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 3);
		expect(await adapter.updateActiveScreenShareSettings(room, {})).toBe(true);
		feed.encodedFps = 0;
		await vi.advanceTimersByTimeAsync(TICK_MS * 2);
		expect(pushWithKey).not.toHaveBeenCalled();
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(pushWithKey).toHaveBeenCalledTimes(1);
		adapter.cancelEncoderVerificationInternal();
	});

	it('restarts warm-up on Try full quality again, so one stalled sample does not stop the share', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		codecDenial = 'runtime-failed';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 3);
		await adapter.resetScreenShareDeliveryToFullQuality();
		feed.encodedFps = 0;
		await vi.advanceTimersByTimeAsync(TICK_MS * 2);
		expect(pushWithKey).not.toHaveBeenCalled();
		adapter.cancelEncoderVerificationInternal();
	});

	it('returns to the top level and clears the notice on a settings push', async () => {
		rememberLevel(1);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(0);
		expect(sentFrameRate(primary)).toBe(15);
		await adapter.updateActiveScreenShareSettings(room, {});
		expect(ScreenShareDelivery.notice).toBeNull();
		expect(sentFrameRate(primary)).toBe(30);
		adapter.cancelEncoderVerificationInternal();
	});

	it('raises apply-failed when a settings push cannot apply the capture constraints', async () => {
		const primary = createSender(feed);
		const {room, participant, track, applyConstraints} = createShare(primary.sender);
		const {adapter} = createAdapter();
		applyConstraints.mockRejectedValue(new Error('rejected'));
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		await adapter.updateActiveScreenShareSettings(room, {resolution: {width: 1280, height: 720, frameRate: 30}});
		expect(applyConstraints).toHaveBeenCalledTimes(1);
		expect(ScreenShareDelivery.notice).toBeNull();
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'apply-failed'});
		adapter.cancelEncoderVerificationInternal();
	});

	it('enforces again on a new sender after apply-failed', async () => {
		const rejecting = createSender(feed, {reject: true});
		const accepting = createSender(feed);
		const {room, participant, track} = createShare(rejecting.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 2);
		track.sender = accepting.sender;
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(accepting.setParameters).toHaveBeenCalledTimes(1);
		adapter.cancelEncoderVerificationInternal();
	});

	it.each([
		[
			'a settings push',
			(adapter: InstanceType<typeof VoiceEngineV2AppScreenShareExecutionAdapter>, room: Room) =>
				adapter.updateActiveScreenShareSettings(room, {}),
		],
		[
			'Try full quality again',
			(adapter: InstanceType<typeof VoiceEngineV2AppScreenShareExecutionAdapter>) =>
				adapter.resetScreenShareDeliveryToFullQuality(),
		],
	])('clears apply-failed and enforces on every tick again after %s', async (_name, clear) => {
		const primary = createSender(feed, {reject: true});
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 2);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'apply-failed'});
		primary.setRejecting(false);
		await clear(adapter, room);
		expect(ScreenShareDelivery.notice).toBeNull();
		const applied = primary.setParameters.mock.calls.length;
		primary.current().degradationPreference = undefined;
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(primary.setParameters).toHaveBeenCalledTimes(applied + 1);
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(ScreenShareDelivery.notice).toBeNull();
		adapter.cancelEncoderVerificationInternal();
	});

	it('gives a backup sender the level current when it arrives', async () => {
		rememberLevel(1);
		const primary = createSender(feed);
		const backup = createSender(feed, {mimeType: 'video/VP9'});
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await adapter.enforceScreenShareSenderParametersInternal(participant as never, {videoCodec: 'h264'});
		expect(sentFrameRate(primary)).toBe(15);
		await adapter.resetScreenShareDeliveryToFullQuality();
		participant.emit(ParticipantEvent.LocalSenderCreated, backup.sender, track, 'h264');
		await vi.advanceTimersByTimeAsync(0);
		expect(sentFrameRate(backup)).toBe(30);
		adapter.cancelEncoderVerificationInternal();
	});

	it('leaves senders of other tracks and the primary sender to their own paths', async () => {
		const primary = createSender(feed);
		const other = createSender(feed);
		const {participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		await adapter.enforceScreenShareSenderParametersInternal(participant as never, {videoCodec: 'h264'});
		expect(primary.setParameters).toHaveBeenCalledTimes(1);
		participant.emit(ParticipantEvent.LocalSenderCreated, other.sender, {}, 'h264');
		primary.current().degradationPreference = undefined;
		participant.emit(ParticipantEvent.LocalSenderCreated, primary.sender, track, 'h264');
		await vi.advanceTimersByTimeAsync(0);
		expect(other.setParameters).not.toHaveBeenCalled();
		expect(primary.setParameters).toHaveBeenCalledTimes(1);
	});

	it('enforces the backup senders on every tick', async () => {
		const primary = createSender(feed);
		const backup = createSender(feed, {mimeType: 'video/VP9'});
		const {room, participant, track} = createShare(primary.sender);
		track.simulcastCodecs.set('h264', {sender: backup.sender});
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS);
		expect(backup.current().degradationPreference).toBe('maintain-resolution');
		expect(backup.current().encodings).toEqual([
			{maxBitrate: 4_500_000, maxFramerate: 30, scaleResolutionDownBy: 2, priority: 'high', networkPriority: 'high'},
		]);
		backup.current().encodings[0].maxFramerate = 60;
		await vi.advanceTimersByTimeAsync(TICK_MS);
		expect(sentFrameRate(backup)).toBe(30);
		adapter.cancelEncoderVerificationInternal();
	});

	it('applies the content hint to the cloned backup tracks', () => {
		const primary = createSender(feed);
		const backup = createSender(feed, {mimeType: 'video/VP9'});
		const {participant, track} = createShare(primary.sender);
		track.simulcastCodecs.set('h264', {sender: backup.sender});
		const {adapter} = createAdapter();
		adapter.applyScreenShareContentHintInternal(participant as never);
		expect(track.mediaStreamTrack.contentHint).toBe('text');
		expect((backup.sender.track as unknown as {contentHint: string}).contentHint).toBe('text');
	});

	it('names the viewer when the pinned codec is advertised but not what was published', () => {
		vi.spyOn(VoiceSettings, 'getPreferredScreenShareCodec').mockReturnValue('av1');
		preferenceOrder = ['av1', 'h264'];
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'codec-viewer', codec: 'h264', pinnedCodec: 'av1'});
		adapter.cancelEncoderVerificationInternal();
	});

	it('reports the lower start instead of the substitution when both apply', () => {
		vi.spyOn(VoiceSettings, 'getPreferredScreenShareCodec').mockReturnValue('av1');
		rememberLevel(1);
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter} = createAdapter();
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		expect(ScreenShareDelivery.notice?.kind).toBe('started-lower');
		adapter.cancelEncoderVerificationInternal();
	});

	it('shows no modal when the stalled track is no longer published', async () => {
		const primary = createSender(feed);
		const {room, participant, track, publication} = createShare(primary.sender);
		const {adapter, setEnabled} = createAdapter();
		codecDenial = 'runtime-failed';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 3);
		const replacement = createShare(createSender(feed).sender).track;
		Object.assign(publication, {videoTrack: replacement, track: replacement});
		feed.encodedFps = 0;
		await vi.advanceTimersByTimeAsync(TICK_MS * 4);
		expect(pushWithKey).not.toHaveBeenCalled();
		expect(setEnabled).not.toHaveBeenCalled();
		adapter.cancelEncoderVerificationInternal();
	});

	it('shows one modal while the stop it started is still running', async () => {
		const primary = createSender(feed);
		const {room, participant, track} = createShare(primary.sender);
		const {adapter, setEnabled} = createAdapter();
		setEnabled.mockImplementation(() => new Promise(() => undefined));
		codecDenial = 'runtime-failed';
		adapter.startEncoderVerificationInternal(room, participant as never, 'h264', track as never);
		await vi.advanceTimersByTimeAsync(FIRST_TICK_MS + TICK_MS * 3);
		feed.encodedFps = 0;
		await vi.advanceTimersByTimeAsync(TICK_MS * 12);
		expect(pushWithKey).toHaveBeenCalledTimes(1);
		expect(setEnabled).toHaveBeenCalledTimes(1);
		adapter.cancelEncoderVerificationInternal();
	});

	it('does not reconcile while a share operation is pending', async () => {
		const {room} = createShare(createSender(feed).sender);
		const {adapter, republish} = createAdapter();
		adapter.transitionScreenShareLifecycleInternal({type: 'share.restore', sourceType: 'display'});
		await adapter.reconcileScreenShareCodecInternal(room);
		expect(republish).not.toHaveBeenCalled();
	});

	it('does not reconcile a track that is not published', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const {adapter, republish} = createAdapter();
		participant.trackPublications.clear();
		await adapter.reconcileScreenShareCodecInternal(room);
		expect(republish).not.toHaveBeenCalled();
	});

	it('reconciles the same codec pair again in the next share', async () => {
		const {room} = createShare(createSender(feed).sender);
		const {adapter, republish} = createAdapter();
		await adapter.reconcileScreenShareCodecInternal(room);
		adapter.resetStreamTracking();
		await adapter.reconcileScreenShareCodecInternal(room);
		expect(republish).toHaveBeenCalledTimes(2);
	});

	it('does not reconcile a codec pair again after a publish policy correction', async () => {
		const {room, publication} = createShare(createSender(feed).sender);
		const {adapter, republish} = createAdapter();
		await adapter.reconcileScreenShareCodecInternal(room);
		publication.options.videoCodec = 'vp9';
		await adapter.reconcileScreenShareCodecInternal(room);
		publication.options.videoCodec = 'h264';
		await adapter.reconcileScreenShareCodecInternal(room);
		expect(republish).toHaveBeenCalledTimes(1);
	});

	it('reconciles the codec once a share has been enabled', async () => {
		const {room} = createShare(createSender(feed).sender);
		const {flows, calls} = createFlowHooks();
		await flows.setEnabled(room, true);
		expect(calls).toEqual(['share.start', 'share.resolve', 'reconcile']);
	});

	it('drains the queue and then reconciles after a codec republish', async () => {
		const {room, track} = createShare(createSender(feed).sender);
		const {flows, calls} = createFlowHooks();
		const republished = await flows.republishActiveShareWithCodec(room, track as never, 'vp9');
		expect(republished).toBe(true);
		expect(calls).toEqual(['share.replace', 'share.resolve', 'drain', 'reconcile']);
	});

	it('does not reconcile a republish that moved off the negotiated codec', async () => {
		const {room, track} = createShare(createSender(feed).sender);
		const {flows, calls} = createFlowHooks();
		const republished = await flows.republishActiveShareWithCodec(room, track as never, 'h264');
		expect(republished).toBe(true);
		expect(calls).toEqual(['share.replace', 'share.resolve', 'drain']);
	});

	it('reconciles a republish that was made before anything was negotiated', async () => {
		selectedCodec = null;
		const {room, track} = createShare(createSender(feed).sender);
		const {flows, calls} = createFlowHooks();
		await flows.republishActiveShareWithCodec(room, track as never, 'h264');
		expect(calls).toEqual(['share.replace', 'share.resolve', 'drain', 'reconcile']);
	});

	it('reconciles a republish that the negotiation overtook while it was in flight', async () => {
		const {room, track, participant, publication} = createShare(createSender(feed).sender);
		participant.publishTrack.mockImplementationOnce(async () => {
			selectedCodec = 'av1';
			return publication;
		});
		const {flows, calls} = createFlowHooks();
		await flows.republishActiveShareWithCodec(room, track as never, 'vp9');
		expect(calls).toEqual(['share.replace', 'share.resolve', 'drain', 'reconcile']);
	});

	it('drains the queue and then reconciles after a device share attempt', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		participant.isScreenShareEnabled = false;
		participant.trackPublications.clear();
		const {flows, calls} = createFlowHooks();
		await flows.startDeviceScreenShare(room);
		expect(calls[0]).toBe('share.start');
		expect(calls.slice(-2)).toEqual(['drain', 'reconcile']);
	});

	it('drains the queue and then reconciles once a reconnect restore has finished', async () => {
		const {room, participant, track} = createShare(createSender(feed).sender);
		participant.isScreenShareEnabled = false;
		participant.trackPublications.clear();
		const {flows, calls} = createFlowHooks(['stop']);
		const restored = await flows.restoreReconnect(room, {
			videoTrack: track.mediaStreamTrack as never,
			audioMuted: false,
			contentSource: 'display',
			source: capturedSource(),
			delivery: ScreenShareDelivery.snapshot(),
		});
		expect(restored).toBe(true);
		expect(calls).toEqual(['share.restore', 'share.resolve', 'drain', 'stop', 'reconcile']);
	});

	it('drains the queue and then reconciles when a reconnect restore fails', async () => {
		const {room, participant, track} = createShare(createSender(feed).sender);
		participant.isScreenShareEnabled = false;
		participant.trackPublications.clear();
		participant.publishTrack.mockRejectedValueOnce(new Error('publish failed'));
		const {flows, calls} = createFlowHooks();
		const restored = await flows.restoreReconnect(room, {
			videoTrack: track.mediaStreamTrack as never,
			audioMuted: false,
			contentSource: 'display',
			source: capturedSource(),
			delivery: ScreenShareDelivery.snapshot(),
		});
		expect(restored).toBe(false);
		expect(calls).toEqual(['share.restore', 'share.reject', 'drain', 'reconcile']);
		expect(ScreenShareDelivery.isSharing).toBe(false);
		expect(ActiveScreenShareSource.getPublishedSource()).toBeNull();
	});

	it('keeps tracking the same delivery share across a reconnect restore', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const {adapter} = createAdapter();
		rememberLevel(2);
		ScreenShareDelivery.counters.switches = 1;
		ScreenShareDelivery.counters.probes = 3;
		const snapshot = adapter.prepareScreenShareReconnect(room);
		expect(snapshot?.delivery.counters).toMatchObject({switches: 1, probes: 3});
		participant.isScreenShareEnabled = false;
		const {flows, calls} = createFlowHooks();
		expect(await flows.restoreReconnect(room, snapshot as never)).toBe(true);
		expect(calls).toContain('cleanup');
		expect(ScreenShareDelivery.isSharing).toBe(true);
		expect(ScreenShareDelivery.counters).toMatchObject({switches: 1, probes: 3});
		expect(ScreenShareDelivery.verdicts.h264?.settledIndex).toBe(2);
	});

	it('keeps the capture source across a reconnect restore', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const {adapter} = createAdapter();
		const ladder = resolveConfiguredScreenShareLadder('display', {width: 3840, height: 2160});
		const snapshot = adapter.prepareScreenShareReconnect(room);
		participant.isScreenShareEnabled = false;
		const {flows, calls} = createFlowHooks();
		expect(await flows.restoreReconnect(room, snapshot as never)).toBe(true);
		expect(calls).toContain('cleanup');
		expect(ActiveScreenShareSource.getPublishedSource()).toBe('display');
		expect(ActiveScreenShareSource.getSourceId()).toBe('screen:1');
		expect(ActiveScreenShareSource.getSourceDimensions()).toEqual({width: 3840, height: 2160});
		expect(ScreenShareDelivery.ladder).toMatchObject(ladder);
		expect(snapshot?.delivery.share?.target).toEqual(ladder.target);
	});

	it('keeps the delivery plan across a reconnect restore', async () => {
		const {room, participant} = createShare(createSender(feed).sender);
		const {adapter} = createAdapter();
		const {target, levels} = resolveConfiguredScreenShareLadder('display', {width: 3840, height: 2160});
		ScreenShareDelivery.plan = {
			target,
			codec: 'h264',
			level: levels[1],
			levelIndex: 1,
			deliveredFrameRate: 12,
			sentWidth: 1920,
			sentHeight: 1080,
			sourceFrameRate: null,
			holdingShort: true,
		};
		const snapshot = adapter.prepareScreenShareReconnect(room);
		participant.isScreenShareEnabled = false;
		const {flows} = createFlowHooks();
		expect(await flows.restoreReconnect(room, snapshot as never)).toBe(true);
		expect(ScreenShareDelivery.plan).toMatchObject({levelIndex: 1, deliveredFrameRate: 12, holdingShort: true});
	});

	it('keeps the delivery share and the capture source when the reconnect finds the share enabled', async () => {
		const {room} = createShare(createSender(feed).sender);
		const {adapter} = createAdapter();
		rememberLevel(2);
		ScreenShareDelivery.counters.probes = 2;
		const snapshot = adapter.prepareScreenShareReconnect(room);
		ScreenShareDelivery.endShare();
		ActiveScreenShareSource.clear();
		const {flows} = createFlowHooks();
		expect(await flows.restoreReconnect(room, snapshot as never)).toBe(true);
		expect(ScreenShareDelivery.isSharing).toBe(true);
		expect(ScreenShareDelivery.counters.probes).toBe(2);
		expect(ScreenShareDelivery.verdicts.h264?.settledIndex).toBe(2);
		expect(ActiveScreenShareSource.getPublishedSource()).toBe('display');
		expect(ActiveScreenShareSource.getSourceDimensions()).toEqual({width: 3840, height: 2160});
	});

	it('restores the recorded source dimensions when a replacement rolls back', () => {
		const {participant, track} = createShare(createSender(feed).sender);
		const {flows} = createFlowHooks();
		const replacement = flows as unknown as ScreenShareReplacementHandle;
		const snapshot = replacement.captureScreenShareReplacementSnapshot(participant, track, {videoCodec: 'h264'});
		ActiveScreenShareSource.setPublishedSource('app', 'window:2');
		ActiveScreenShareSource.setSourceDimensions({width: 1280, height: 720});
		replacement.restorePublishedScreenShareSource(snapshot);
		expect(ActiveScreenShareSource.getPublishedSource()).toBe('display');
		expect(ActiveScreenShareSource.getSourceId()).toBe('screen:1');
		expect(ActiveScreenShareSource.getSourceDimensions()).toEqual({width: 3840, height: 2160});
	});

	it('clears the source dimensions when the replacement rolls back to no published source', () => {
		const {participant, track} = createShare(createSender(feed).sender);
		const {flows} = createFlowHooks();
		const replacement = flows as unknown as ScreenShareReplacementHandle;
		ActiveScreenShareSource.clear();
		const snapshot = replacement.captureScreenShareReplacementSnapshot(participant, track, {videoCodec: 'h264'});
		ActiveScreenShareSource.setPublishedSource('display', 'screen:2');
		ActiveScreenShareSource.setSourceDimensions({width: 1280, height: 720});
		replacement.restorePublishedScreenShareSource(snapshot);
		expect(ActiveScreenShareSource.getPublishedSource()).toBeNull();
		expect(ActiveScreenShareSource.getSourceDimensions()).toBeNull();
	});

	it('ends the delivery share when the share state is cleaned up', () => {
		const {adapter} = createAdapter();
		rememberLevel(1);
		ScreenShareDelivery.counters.switches = 1;
		ScreenShareDelivery.setNotice({kind: 'apply-failed'});
		adapter.resetStreamTracking();
		expect(ScreenShareDelivery.notice).toBeNull();
		expect(ScreenShareDelivery.verdicts).toEqual({});
		expect(ScreenShareDelivery.counters.switches).toBe(0);
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
const {
	resolveScreenShareLevels,
	resolveScreenShareQualityPick,
	resolveScreenShareTarget,
	SUPPORTED_SCREEN_SHARE_FRAME_RATES,
} = await import('@app/features/voice/utils/ScreenShareOptions');

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
	resolveScreenShareDeliveryOutcome,
	startConfiguredDeviceScreenShare,
	startConfiguredDisplayScreenShare,
	switchConfiguredDeviceScreenShare,
	switchConfiguredDisplayScreenShare,
} = await import('@app/features/voice/utils/ScreenShareStartFlow');
const {resolveConfiguredScreenShareTarget} = await import(
	'@app/features/voice/engine/voice_screen_share_manager/shared'
);

describe('the configured screen share start flow', () => {
	const engine = MediaEngine as unknown as {
		startDeviceScreenShare: ReturnType<typeof vi.fn>;
		replaceActiveDeviceScreenShare: ReturnType<typeof vi.fn>;
	};
	let storedResolution: 'high' | 'medium' | 'source' = 'high';
	let order: Array<string> = [];
	let started: Array<{resolution: unknown; publishOptions: unknown}> = [];

	beforeEach(() => {
		selfStream = true;
		storedResolution = 'high';
		order = [];
		started = [];
		ScreenShareDelivery.endShare();
		ActiveScreenShareSource.clear();
		AppStorage.removeItem('ScreenShareDeliveryMemoryV1');
		vi.spyOn(VoiceSettings, 'getStreamingMode').mockReturnValue('custom');
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockImplementation(() => storedResolution);
		vi.spyOn(VoiceSettings, 'getVideoFrameRate').mockReturnValue(30);
		vi.spyOn(VoiceSettings, 'getScreenShareContentHint').mockReturnValue('text');
		vi.spyOn(VoiceSettings, 'getPreferredScreenShareCodec').mockReturnValue('auto');
		vi.spyOn(VoiceSettings, 'getShareDeviceAudio').mockReturnValue(false);
		refreshSelection.mockReset();
		refreshSelection.mockImplementation(async () => {
			order.push(`refresh:${ScreenShareDelivery.isSharing}`);
			return null;
		});
		selectScreenShareCodec.mockReset();
		selectScreenShareCodec.mockImplementation(() => {
			order.push(`codec:${ScreenShareDelivery.isSharing}`);
			return 'h264';
		});
		engine.startDeviceScreenShare = vi.fn(async (options: {resolution: unknown}, publishOptions: unknown) => {
			started.push({resolution: options.resolution, publishOptions});
		});
		engine.replaceActiveDeviceScreenShare = vi.fn(async () => true);
	});

	afterEach(() => {
		vi.restoreAllMocks();
		ScreenShareDelivery.endShare();
		ActiveScreenShareSource.clear();
		selfStream = true;
		gpuReport = null;
		gpuReportLoad = Promise.resolve(null);
	});

	it('loads the delivery memory before it ranks a codec', async () => {
		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);
		expect(order).toEqual(['refresh:true', 'codec:true']);
		expect(ScreenShareDelivery.isSharing).toBe(true);
	});

	it('publishes the target the settings push builds', async () => {
		await startConfiguredDeviceScreenShare('camera-1');
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
		expect(ScreenShareDelivery.snapshot().share?.target).toEqual(target);
	});

	it('clears the recorded source dimensions for a capture device', async () => {
		ActiveScreenShareSource.setPublishedSource('display', 'screen:1');
		ActiveScreenShareSource.setSourceDimensions({width: 3840, height: 2160});
		await startConfiguredDeviceScreenShare('camera-1');
		expect(ActiveScreenShareSource.getPublishedSource()).toBe('device');
		expect(ActiveScreenShareSource.getSourceDimensions()).toBeNull();
	});

	it('ends the delivery share and the capture source when the capture never starts', async () => {
		selfStream = false;
		ActiveScreenShareSource.setPublishedSource('display', 'screen:1');
		ActiveScreenShareSource.setSourceDimensions({width: 3840, height: 2160});
		engine.startDeviceScreenShare = vi.fn(async () => {
			throw new Error('capture failed');
		});
		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(false);
		expect(ScreenShareDelivery.isSharing).toBe(false);
		expect(ScreenShareDelivery.snapshot().share).toBeNull();
		expect(ActiveScreenShareSource.getPublishedSource()).toBeNull();
		expect(ActiveScreenShareSource.getSourceDimensions()).toBeNull();
	});

	it('keeps the share counters across a source switch', async () => {
		await startConfiguredDeviceScreenShare('camera-1');
		ScreenShareDelivery.counters.switches = 1;
		ScreenShareDelivery.counters.probes = 2;
		const startedLevels = ScreenShareDelivery.snapshot().share?.levels;
		storedResolution = 'medium';
		expect(await switchConfiguredDeviceScreenShare('camera-2')).toBe(true);
		expect(ScreenShareDelivery.counters.switches).toBe(1);
		expect(ScreenShareDelivery.counters.probes).toBe(2);
		expect(ScreenShareDelivery.snapshot().share?.target).toEqual(resolveConfiguredScreenShareTarget('device', null));
		const switchedLevels = resolveConfiguredScreenShareLadder('device', null).levels;
		expect(switchedLevels).not.toEqual(startedLevels);
		expect(ScreenShareDelivery.snapshot().share?.levels).toEqual(switchedLevels);
	});

	it('builds the ladder from the share the start flow asked for', async () => {
		storedResolution = 'source';
		expect(await startConfiguredDeviceScreenShare('camera-1')).toBe(true);
		const device = resolveConfiguredScreenShareLadder('device', null);
		expect(device.target).not.toEqual(resolveConfiguredScreenShareTarget('display', null));
		expect(ScreenShareDelivery.snapshot().share?.target).toEqual(device.target);
		expect(ScreenShareDelivery.snapshot().share?.levels).toEqual(device.levels);
		expect(started).toEqual([
			{
				resolution: buildConfiguredScreenShareOptions({
					target: device.target,
					sourceDimensions: null,
					includeAudio: false,
					videoCodec: 'h264',
				}).captureOptions.resolution,
				publishOptions: buildConfiguredScreenShareOptions({
					target: device.target,
					sourceDimensions: null,
					includeAudio: false,
					videoCodec: 'h264',
				}).publishOptions,
			},
		]);
	});

	it('restores the live share when a switch does not take', async () => {
		await startConfiguredDeviceScreenShare('camera-1');
		ScreenShareDelivery.counters.switches = 1;
		ScreenShareDelivery.setNotice({kind: 'apply-failed'});
		const before = ScreenShareDelivery.snapshot();
		engine.replaceActiveDeviceScreenShare = vi.fn(async () => false);
		storedResolution = 'medium';
		expect(await switchConfiguredDeviceScreenShare('camera-2')).toBe(false);
		expect(ScreenShareDelivery.snapshot().share?.target).toEqual(before.share?.target);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'apply-failed'});
		expect(ScreenShareDelivery.counters.switches).toBe(1);
	});

	it('waits for the GPU report before it commits the delivery header', async () => {
		let reportLoaded: (() => void) | undefined;
		gpuReportLoad = new Promise<void>((resolve) => {
			reportLoaded = resolve;
		});
		const starting = startConfiguredDeviceScreenShare('camera-1');
		await new Promise((resolve) => setTimeout(resolve, 0));
		expect(ScreenShareDelivery.isSharing).toBe(false);
		gpuReport = {gpuLabel: 'proof-gpu'};
		reportLoaded?.();
		expect(await starting).toBe(true);
		expect(ScreenShareDelivery.snapshot().share?.header.gpuKey).toBe('proof-gpu');
	});

	it('keys the delivery memory on the browser, the encoder and the GPU', () => {
		vi.spyOn(VoiceSettings, 'getScreenShareEncoderMode').mockReturnValue('software');
		const original = Object.getOwnPropertyDescriptor(navigator, 'userAgent');
		Object.defineProperty(navigator, 'userAgent', {
			value: 'Mozilla/5.0 (Macintosh) AppleWebKit/537.36 Chrome/141.0.0.0 Safari/537.36',
			configurable: true,
		});
		expect(buildScreenShareDeliveryHeader()).toEqual({
			chromiumMajor: 141,
			gpuKey: 'none',
			encoderMode: 'software',
			hardwareAccelerationDisabled: false,
		});
		gpuReport = {gpuLabel: 'header-gpu'};
		expect(buildScreenShareDeliveryHeader().gpuKey).toBe('header-gpu');
		Object.defineProperty(navigator, 'userAgent', {value: 'Mozilla/5.0 (Macintosh) Gecko/20100101 Firefox/141.0'});
		expect(buildScreenShareDeliveryHeader().chromiumMajor).toBeNull();
		if (original === undefined) Reflect.deleteProperty(navigator, 'userAgent');
		else Object.defineProperty(navigator, 'userAgent', original);
	});

	it('keeps the delivery share only while the operation holds', () => {
		expect(resolveScreenShareDeliveryOutcome(true, true)).toBe('keep');
		expect(resolveScreenShareDeliveryOutcome(true, false)).toBe('keep');
		expect(resolveScreenShareDeliveryOutcome(false, true)).toBe('restore');
		expect(resolveScreenShareDeliveryOutcome(false, false)).toBe('end');
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
		ScreenShareDelivery.endShare();
		ActiveScreenShareSource.clear();
		AppStorage.removeItem('ScreenShareDeliveryMemoryV1');
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
		ScreenShareDelivery.endShare();
		ActiveScreenShareSource.clear();
		displayShareEnvironment = 'web';
		Reflect.deleteProperty(electronHost, 'electron');
	});

	it('builds the ladder from the picked source rather than the one still recorded', async () => {
		ActiveScreenShareSource.setPublishedSource('display', 'screen:0');
		ActiveScreenShareSource.setSourceDimensions({width: 3840, height: 2160});
		const picked = {width: 1280, height: 720};
		const pickedLadder = resolveConfiguredScreenShareLadder('display', picked);
		const staleLadder = resolveConfiguredScreenShareLadder('display', {width: 3840, height: 2160});
		expect(pickedLadder.target).not.toEqual(staleLadder.target);
		expect(await startConfiguredDisplayScreenShare('screen:1', {sourceDimensions: picked})).toBe(true);
		expect(ScreenShareDelivery.snapshot().share?.target).toEqual(pickedLadder.target);
		expect(ScreenShareDelivery.snapshot().share?.levels).toEqual(pickedLadder.levels);
	});

	it('rebuilds the ladder from the newly picked source on a switch', async () => {
		const first = {width: 1280, height: 720};
		const second = {width: 3840, height: 2160};
		expect(await startConfiguredDisplayScreenShare('screen:1', {sourceDimensions: first})).toBe(true);
		ActiveScreenShareSource.setSourceDimensions(first);
		const secondLadder = resolveConfiguredScreenShareLadder('display', second);
		expect(secondLadder.target).not.toEqual(resolveConfiguredScreenShareLadder('display', first).target);
		expect(await switchConfiguredDisplayScreenShare('screen:2', {sourceDimensions: second})).toBe(true);
		expect(ScreenShareDelivery.snapshot().share?.target).toEqual(secondLadder.target);
		expect(ScreenShareDelivery.snapshot().share?.levels).toEqual(secondLadder.levels);
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
		replaceActiveDeviceScreenShare: ReturnType<typeof vi.fn>;
		updateActiveScreenShareSettings: ReturnType<typeof vi.fn>;
	};

	beforeEach(() => {
		selfStream = true;
		ScreenShareDelivery.endShare();
		ActiveScreenShareSource.clear();
		AppStorage.removeItem('ScreenShareDeliveryMemoryV1');
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
		ScreenShareDelivery.endShare();
		ActiveScreenShareSource.clear();
	});

	it('waits behind an in-flight source switch', async () => {
		const order: Array<string> = [];
		let releaseSwitch: (() => void) | undefined;
		let switchStarted: (() => void) | undefined;
		const switchRunning = new Promise<void>((resolve) => {
			switchStarted = resolve;
		});
		const gate = new Promise<void>((resolve) => {
			releaseSwitch = resolve;
		});
		engine.startDeviceScreenShare = vi.fn(async () => undefined);
		engine.replaceActiveDeviceScreenShare = vi.fn(async () => {
			order.push('switch:start');
			switchStarted?.();
			await gate;
			order.push('switch:end');
			return true;
		});
		engine.updateActiveScreenShareSettings = vi.fn(async () => {
			order.push('push');
		});
		await startConfiguredDeviceScreenShare('camera-1');
		const switching = switchConfiguredDeviceScreenShare('camera-2');
		await switchRunning;
		const pushing = pushActiveStreamSettings('device', 'desktop-custom');
		await Promise.resolve();
		expect(order).toEqual(['switch:start']);
		releaseSwitch?.();
		expect(await switching).toBe(true);
		await pushing;
		expect(order).toEqual(['switch:start', 'switch:end', 'push']);
	});

	it('puts the committed ladder back when the live share refuses the push', async () => {
		engine.startDeviceScreenShare = vi.fn(async () => undefined);
		engine.updateActiveScreenShareSettings = vi.fn(async () => false);
		await startConfiguredDeviceScreenShare('camera-1');
		const before = ScreenShareDelivery.snapshot();
		ScreenShareDelivery.setNotice({kind: 'apply-failed'});
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue('low_480p');
		const retargeted = resolveConfiguredScreenShareLadder('device', null);
		expect(retargeted.target).not.toEqual(before.share?.target);
		await pushActiveStreamSettings('device', 'desktop-custom');
		expect(engine.updateActiveScreenShareSettings).toHaveBeenCalledTimes(1);
		expect(ScreenShareDelivery.snapshot().share?.target).toEqual(before.share?.target);
		expect(ScreenShareDelivery.snapshot().share?.levels).toEqual(before.share?.levels);
		expect(ScreenShareDelivery.notice).toEqual({kind: 'apply-failed'});
	});

	it('keeps the retargeted ladder when the live share takes the push', async () => {
		engine.startDeviceScreenShare = vi.fn(async () => undefined);
		engine.updateActiveScreenShareSettings = vi.fn(async () => true);
		await startConfiguredDeviceScreenShare('camera-1');
		vi.spyOn(VoiceSettings, 'getScreenshareResolution').mockReturnValue('low_480p');
		const retargeted = resolveConfiguredScreenShareLadder('device', null);
		await pushActiveStreamSettings('device', 'desktop-custom');
		expect(ScreenShareDelivery.snapshot().share?.target).toEqual(retargeted.target);
		expect(ScreenShareDelivery.snapshot().share?.levels).toEqual(retargeted.levels);
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
		ScreenShareDelivery.counters.switches = 1;
		await pushActiveStreamSettings('device', 'desktop-custom');
		expect(published).toHaveLength(2);
		expect(published[1]).toEqual(published[0]);
		expect(ScreenShareDelivery.counters.switches).toBe(1);
	});
});

const {VideoTab} = await import('@app/features/user/components/modals/tabs/UserVideoTab');

describe('the video settings tab of a live share', () => {
	const mountedTabs: Array<{root: ReturnType<typeof createRoot>; container: HTMLElement}> = [];
	const storedBeforeTab = {
		mode: VoiceSettings.getStreamingMode(),
		resolution: VoiceSettings.getScreenshareResolution(),
		frameRate: VoiceSettings.getVideoFrameRate(),
	};
	const renderVideoTab = async () => {
		const container = document.createElement('div');
		document.body.append(container);
		const root = createRoot(container);
		mountedTabs.push({root, container});
		await act(async () => {
			root.render(createElement(VideoTab, {voiceSettings: VoiceSettings, hasPremium: true}));
		});
		return container;
	};
	const comboboxOf = (container: HTMLElement, dataFlx: string) => {
		const node = container.querySelector(`[data-combobox="${dataFlx}"]`);
		if (node == null) throw new Error(`no ${dataFlx} combobox`);
		return {value: node.getAttribute('data-value'), options: node.getAttribute('data-options')};
	};
	const rowOf = (container: HTMLElement, dataFlx: string) =>
		container.querySelector(`[data-flx="${dataFlx}"]`)?.textContent ?? null;
	const rowOrder = (container: HTMLElement, dataFlxList: ReadonlyArray<string>) =>
		Array.from(container.querySelectorAll(dataFlxList.map((dataFlx) => `[data-flx="${dataFlx}"]`).join(','))).map(
			(node) => node.getAttribute('data-flx'),
		);
	const pickOption = async (container: HTMLElement, dataFlx: string, option: string) => {
		const node = container.querySelector(`[data-combobox="${dataFlx}"] [data-option="${option}"]`);
		if (node == null) throw new Error(`no ${option} option in ${dataFlx}`);
		await act(async () => {
			(node as HTMLButtonElement).click();
		});
	};
	const startLiveDeviceShare = () => {
		ActiveScreenShareSource.setPublishedSource('device', 'camera-1');
		const target = resolveConfiguredScreenShareTarget('device', null);
		const level = resolveScreenShareLevels(target, null)[1];
		ScreenShareDelivery.restore({
			share: {
				header: buildScreenShareDeliveryHeader(),
				...resolveConfiguredScreenShareLadder('device', null),
				hardware: {},
				adaptive: true,
			},
			notice: {kind: 'device-framerate', width: level.width, height: level.height, frameRate: level.frameRate},
			plan: {
				target,
				codec: 'vp9',
				level,
				levelIndex: 1,
				deliveredFrameRate: level.frameRate,
				sentWidth: level.width,
				sentHeight: level.height,
				sourceFrameRate: null,
				holdingShort: false,
			},
			counters: {switches: 0, probes: 0, layerings: 0, probeBlocked: false, toastShown: false},
			verdicts: {},
			multiLayer: false,
		});
	};

	beforeEach(() => {
		menuEntitled = true;
		VoiceSettings.updateSettings({streamingMode: 'custom', screenshareResolution: 'source', videoFrameRate: 30});
	});

	afterEach(async () => {
		for (const entry of mountedTabs) {
			await act(async () => {
				entry.root.unmount();
			});
			entry.container.remove();
		}
		mountedTabs.length = 0;
		ScreenShareDelivery.endShare();
		ActiveScreenShareSource.clear();
		VoiceSettings.updateSettings({
			streamingMode: storedBeforeTab.mode,
			screenshareResolution: storedBeforeTab.resolution,
			videoFrameRate: storedBeforeTab.frameRate,
		});
		settingsUpdate.mockClear();
		vi.restoreAllMocks();
	});

	const recordLiveDeviceShare = (adaptive: boolean) => {
		ActiveScreenShareSource.setPublishedSource('device', 'camera-1');
		const target = resolveConfiguredScreenShareTarget('device', null);
		const level = resolveScreenShareLevels(target, null)[1];
		ScreenShareDelivery.endShare();
		ScreenShareDelivery.beginShare({
			header: buildScreenShareDeliveryHeader(),
			...resolveConfiguredScreenShareLadder('device', null),
			hardware: {},
			adaptive,
		});
		ScreenShareDelivery.record(
			'vp9',
			{
				kind: 'ok',
				decision: {kind: 'none'},
				notice: null,
				noticeChanged: false,
				memoryWrite: null,
				shortCause: null,
				levelIndex: 1,
				warmingUp: false,
			},
			{
				level,
				levelIndex: 1,
				deliveredFrameRate: level.frameRate,
				sentWidth: level.width,
				sentHeight: level.height,
				sourceFrameRate: null,
				notice: null,
				holdingShort: false,
			},
		);
	};

	it('reports what a live share is sending to the sharer only in the treatment arm', async () => {
		recordLiveDeviceShare(true);
		expect(rowOf(await renderVideoTab(), 'user.video-tab.sending-note')).toBe('Set to send 1440p at 15 FPS with VP9');
		recordLiveDeviceShare(false);
		expect(ScreenShareDelivery.plan).toBeNull();
		expect(rowOf(await renderVideoTab(), 'user.video-tab.sending-note')).toBeNull();
	});

	it('offers a live capture device only what it can send and agrees with the live plan', async () => {
		startLiveDeviceShare();
		const container = await renderVideoTab();
		expect(comboboxOf(container, 'user.video-tab.select.screenshare-resolution-change')).toEqual({
			value: 'ultra',
			options: '480p,720p,1080p,1440p',
		});
		expect(rowOf(container, 'user.video-tab.sending-note')).toBe('Set to send 1440p at 15 FPS with VP9');
	});

	it('explains the step down beside the row that reports it', async () => {
		startLiveDeviceShare();
		const container = await renderVideoTab();
		expect(rowOf(container, 'user.video-tab.delivery-notice')).toBe(
			'Your device could not keep up, so your stream now sends 15 FPS to keep 1440p sharp.',
		);
	});

	it('puts the delivery notice after the row it explains', async () => {
		startLiveDeviceShare();
		const container = await renderVideoTab();
		expect(rowOrder(container, ['user.video-tab.sending-note', 'user.video-tab.delivery-notice'])).toEqual([
			'user.video-tab.sending-note',
			'user.video-tab.delivery-notice',
		]);
	});

	it('names the preset the user picked and says what a capture device does with it', async () => {
		VoiceSettings.updateSettings({streamingMode: 'screenshare', screenshareResolution: 'medium', videoFrameRate: 30});
		startLiveDeviceShare();
		const container = await renderVideoTab();
		expect(VoiceSettings.getStreamingMode()).toBe('screenshare');
		expect(rowOf(container, 'user.video-tab.preset-note')).toBe(
			'Set by the Screen share preset. Picking a value here switches to Custom.',
		);
		expect(rowOf(container, 'user.video-tab.preset-context-note')).toBe('Capture devices use the Gaming preset.');
	});

	it('keeps the capture device note off a display share', async () => {
		VoiceSettings.updateSettings({streamingMode: 'screenshare', screenshareResolution: 'medium', videoFrameRate: 30});
		const container = await renderVideoTab();
		expect(rowOf(container, 'user.video-tab.preset-note')).toBe(
			'Set by the Screen share preset. Picking a value here switches to Custom.',
		);
		expect(rowOf(container, 'user.video-tab.preset-context-note')).toBeNull();
	});

	it('sends a tab pick through the picker instead of storing the raw value', async () => {
		VoiceSettings.updateSettings({streamingMode: 'screenshare', screenshareResolution: 'medium', videoFrameRate: 30});
		startLiveDeviceShare();
		const container = await renderVideoTab();
		await pickOption(container, 'user.video-tab.select.screenshare-resolution-change', 'high');
		expect(settingsUpdate).toHaveBeenCalledWith({
			streamingMode: 'custom',
			screenshareResolution: 'high',
			videoFrameRate: 60,
		});
		settingsUpdate.mockClear();
		await pickOption(container, 'user.video-tab.select.frame-rate-change', '15');
		expect(settingsUpdate).toHaveBeenCalledWith({
			streamingMode: 'custom',
			screenshareResolution: 'ultra',
			videoFrameRate: 15,
		});
	});

	it('shows the stored display quality when no share is live', async () => {
		const container = await renderVideoTab();
		expect(comboboxOf(container, 'user.video-tab.select.screenshare-resolution-change')).toEqual({
			value: 'source',
			options: '480p,720p,1080p,1440p,Source',
		});
		expect(rowOf(container, 'user.video-tab.sending-note')).toBeNull();
		expect(rowOf(container, 'user.video-tab.delivery-notice')).toBeNull();
	});
});

const {beginScreenShareDelivery} = await import('@app/features/voice/engine/voice_screen_share_manager/shared');
const {default: ExperimentAssignments} = await import('@app/features/experiment/state/ExperimentAssignments');
const {http} = await import('@app/features/platform/transport/RestTransport');
const {INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT} = await import(
	'@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas'
);

describe('the screen share delivery experiment assignment', () => {
	const assign = (enabled: boolean, userTargeted: boolean) => {
		ExperimentAssignments.response = {
			poll_interval_seconds: 300,
			poll_jitter_percent: 15,
			assignments: {
				screen_share_delivery: {enabled, config_version: 1, user_targeted: userTargeted, source: null},
			},
		};
	};

	afterEach(() => {
		ScreenShareDelivery.endShare();
		ExperimentAssignments.reset();
		vi.restoreAllMocks();
	});

	const startShare = async (enabled: boolean, userTargeted: boolean) => {
		ScreenShareDelivery.endShare();
		assign(enabled, userTargeted);
		await beginScreenShareDelivery(resolveConfiguredScreenShareLadder('display', {width: 1920, height: 1080}));
		return ScreenShareDelivery.adaptive;
	};

	it('takes the arm from the assignment when the share starts', async () => {
		expect(await startShare(true, false)).toBe(false);
		expect(await startShare(true, true)).toBe(true);
	});

	it('holds the control arm for an answer that never names the experiment', async () => {
		ScreenShareDelivery.endShare();
		ExperimentAssignments.reset();
		vi.spyOn(http, 'get').mockResolvedValue({
			ok: true,
			status: 200,
			statusText: '',
			headers: {},
			body: JSON.parse('{"poll_interval_seconds":600,"poll_jitter_percent":20,"assignments":{}}'),
		});
		const answered = new Promise<void>((resolve) => {
			const unsubscribe = ExperimentAssignments.subscribe(() => {
				unsubscribe();
				resolve();
			});
		});
		ExperimentAssignments.start();
		await answered;
		expect(ExperimentAssignments.response.poll_interval_seconds).toBe(600);
		await beginScreenShareDelivery(resolveConfiguredScreenShareLadder('display', {width: 1920, height: 1080}));
		expect(ScreenShareDelivery.adaptive).toBe(false);
	});

	it('holds the control arm for an answer that names only another experiment', async () => {
		ScreenShareDelivery.endShare();
		ExperimentAssignments.response = {
			poll_interval_seconds: 300,
			poll_jitter_percent: 15,
			assignments: {
				voice_noise_suppression: {...INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT, enabled: true, user_targeted: true},
			},
		};
		await beginScreenShareDelivery(resolveConfiguredScreenShareLadder('display', {width: 1920, height: 1080}));
		expect(ScreenShareDelivery.adaptive).toBe(false);
	});

	it('holds the control arm for an experiment that is switched off', async () => {
		expect(await startShare(false, true)).toBe(false);
		expect(await startShare(false, false)).toBe(false);
	});

	it('freezes that arm for the life of the share', async () => {
		expect(await startShare(true, false)).toBe(false);
		assign(true, true);
		expect(ScreenShareDelivery.adaptive).toBe(false);
		ScreenShareDelivery.retarget(resolveConfiguredScreenShareLadder('display', {width: 1280, height: 720}));
		expect(ScreenShareDelivery.adaptive).toBe(false);
	});
});

const {ActiveScreenShareMenu} = await import('@app/features/voice/components/ActiveScreenShareMenu');

describe('the active screen share menu in each experiment arm', () => {
	const mountedMenus: Array<{root: ReturnType<typeof createRoot>; container: HTMLElement}> = [];

	afterEach(() => {
		for (const mounted of mountedMenus.splice(0)) {
			act(() => {
				mounted.root.unmount();
			});
			mounted.container.remove();
		}
		ScreenShareDelivery.endShare();
	});

	const renderMenu = async (adaptive: boolean) => {
		const target = resolveConfiguredScreenShareTarget('device', null);
		const level = resolveScreenShareLevels(target, null)[0];
		ScreenShareDelivery.restore({
			share: {
				header: buildScreenShareDeliveryHeader(),
				...resolveConfiguredScreenShareLadder('device', null),
				hardware: {},
				adaptive,
			},
			notice: null,
			plan: {
				target,
				codec: 'vp9',
				level,
				levelIndex: 0,
				deliveredFrameRate: level.frameRate,
				sentWidth: level.width,
				sentHeight: level.height,
				sourceFrameRate: null,
				holdingShort: false,
			},
			counters: {switches: 0, probes: 0, layerings: 0, probeBlocked: false, toastShown: false},
			verdicts: {},
			multiLayer: false,
		});
		const container = document.createElement('div');
		document.body.append(container);
		const root = createRoot(container);
		mountedMenus.push({root, container});
		await act(async () => {
			root.render(
				createElement(ActiveScreenShareMenu, {
					onClose: () => undefined,
					displayShareEnvironment: 'desktop-custom',
					shareContext: 'device',
					shareContextResolved: true,
					showLiveSettings: false,
				}),
			);
		});
		return container;
	};

	it('reports what the share is sending only in the treatment arm', async () => {
		const treatment = await renderMenu(true);
		expect(treatment.querySelector('[data-flx="voice.active-screen-share-menu.status-sending"]')?.textContent).toBe(
			'Set to send 1440p at 60 FPS with VP9',
		);
		expect(treatment.textContent).toContain('Stop streaming');
		expect(treatment.textContent).toContain('Change stream');
	});

	it('leaves the control arm with the actions the menu had before the rollout', async () => {
		const control = await renderMenu(false);
		expect(control.querySelector('[data-flx="voice.active-screen-share-menu.status-sending"]')).toBeNull();
		expect(control.querySelector('[data-flx="voice.active-screen-share-menu.status-notice"]')).toBeNull();
		expect(control.textContent).not.toContain('Try full quality again');
		expect(control.textContent).toBe('Stop streamingChange stream');
	});
});
