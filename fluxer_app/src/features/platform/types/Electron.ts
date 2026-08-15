// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceEngineV2BridgeApi} from '@fluxer/voice_engine_v2/bridge';
import type {
	AuthenticationResponseJSON,
	PublicKeyCredentialCreationOptionsJSON,
	PublicKeyCredentialRequestOptionsJSON,
	RegistrationResponseJSON,
} from '@simplewebauthn/browser';

export interface DesktopInfo {
	version: string;
	channel: 'stable' | 'canary';
	arch: string;
	hardwareArch: string;
	runningUnderRosetta: boolean;
	os: NodeJS.Platform;
	osVersion: string;
	systemVersion?: string;
	electronVersion: string;
	chromeVersion: string;
	nodeVersion: string;
	waylandSession: boolean;
	portable: boolean;
	flatpak: boolean;
	flatpakAppId: string | null;
	chromiumRuntime: ChromiumRuntimeInfo;
}

export interface ChromiumRuntimeInfo {
	enableFeatures: Array<string>;
	disableFeatures: Array<string>;
	switches: Array<string>;
}

export interface GpuDeviceInfo {
	active: boolean;
	vendorId: number;
	deviceId: number;
	vendorName?: string;
	deviceString?: string;
	driverVendor?: string;
	driverVersion?: string;
	dedicatedVideoMemory?: number;
	sharedSystemMemory?: number;
	subsystemVendorId?: number;
	subsystemDeviceId?: number;
	registryId?: string;
	adapterLuid?: string;
	pciPath?: string;
	integrated?: boolean;
	removable?: boolean;
	headless?: boolean;
	source?: 'metal' | 'dxgi' | 'linux-sysfs' | 'electron';
}

export interface GpuInfo {
	devices: ReadonlyArray<GpuDeviceInfo>;
	glRenderer?: string;
	glVendor?: string;
	machineModelName?: string;
	machineModelVersion?: string;
	nativeSource?: 'metal' | 'dxgi' | 'linux-sysfs';
}

export interface DesktopWindowBehaviorSettings {
	showTrayIcon: boolean;
	minimizeToTray: boolean;
	closeToTray: boolean;
	useNativeTitleBar: boolean;
	activeUseNativeTitleBar: boolean;
	rememberWindowState: boolean;
	allowTransparency: boolean;
	activeAllowTransparency: boolean;
	smoothScrolling: boolean;
	activeSmoothScrolling: boolean;
	middleClickAutoscroll: boolean;
	activeMiddleClickAutoscroll: boolean;
	firstClickPassThroughWhenUnfocused: boolean;
}

export interface ThemeLocalFileReference {
	id: string;
	name: string;
	path: string;
	mimeType: string;
	size: number;
}

export interface ThemeLocalFileReadResult {
	path: string;
	dataUrl?: string;
	error?: string;
}

export interface ThemeDirectoryCssFile {
	fileName: string;
	path: string;
	css: string;
}

export type VoiceBackgroundMediaKind = 'static' | 'animated' | 'video';

export interface VoiceBackgroundMediaCacheRequest {
	id: string;
	mimeType: string;
	fileName?: string;
	data: ArrayBuffer;
}

export interface VoiceBackgroundMediaCacheResult {
	path: string;
	mediaKind: VoiceBackgroundMediaKind;
}

export interface VoiceBackgroundMediaReadResult {
	path: string;
	mediaKind: VoiceBackgroundMediaKind;
	dataUrl: string;
}

export interface DesktopVoiceDebugEventSinkEntry {
	sequence: number;
	line: string;
}

export type TrayPresenceStatus = 'online' | 'idle' | 'dnd' | 'invisible';

export interface TrayRuntimeStatePayload {
	voiceConnected: boolean;
	voiceChannelLabel: string | null;
	selfMute: boolean;
	selfDeaf: boolean;
	presenceStatus: TrayPresenceStatus | null;
	buildInfo: string | null;
}

export type TrayActionPayload =
	| {
			action: 'set-status';
			status: TrayPresenceStatus;
	  }
	| {
			action: 'toggle-mute';
	  }
	| {
			action: 'toggle-deafen';
	  }
	| {
			action: 'disconnect-voice';
	  }
	| {
			action: 'check-for-updates';
	  };
export type UpdaterContext = 'user' | 'background' | 'focus';
export type UpdaterDownloadFormat = 'setup' | 'dmg' | 'zip' | 'appimage' | 'deb' | 'rpm' | 'tar_gz';
export interface UpdaterDownloadOption {
	format: UpdaterDownloadFormat;
	label: string;
	url: string;
	suggestedName?: string;
	sha256?: string | null;
}
export type UpdaterEvent =
	| {
			type: 'checking';
			context: UpdaterContext;
	  }
	| {
			type: 'available';
			context: UpdaterContext;
			version: string | null;
			downloadSize?: number | null;
			downloadStarted: boolean;
			downloadUrl?: string;
			downloadOptions?: Array<UpdaterDownloadOption>;
	  }
	| {
			type: 'not-available';
			context: UpdaterContext;
	  }
	| {
			type: 'downloaded';
			context: UpdaterContext;
			version: string | null;
	  }
	| {
			type: 'progress';
			context: UpdaterContext;
			percent: number;
			transferred: number;
			total: number;
			bytesPerSecond: number;
	  }
	| {
			type: 'error';
			context: UpdaterContext;
			message: string;
			phase?: 'check' | 'download' | 'install';
	  }
	| {
			type: 'unsupported';
			context: UpdaterContext;
			reason: 'platform' | 'unpackaged' | 'managed-package';
			downloadUrl?: string;
	  };

export interface DownloadFileOptions {
	url: string;
	defaultPath: string;
}

export interface DownloadFileResult {
	success: boolean;
	canceled?: boolean;
	path?: string;
	error?: string;
}

export type ClipboardWriteFileMediaType = 'image' | 'gif' | 'video' | 'audio';

export interface ClipboardWriteFileOptions {
	url: string;
	suggestedName?: string;
	mediaType: ClipboardWriteFileMediaType;
}

export interface ClipboardWriteFileResult {
	success: boolean;
	path?: string;
	error?: string;
}

export type MediaAccessType = 'microphone' | 'camera' | 'screen' | 'audio-capture';
export type MediaAccessStatus = 'granted' | 'denied' | 'not-determined' | 'restricted' | 'unknown';
export type InputMonitoringPermissionStatus = 'granted' | 'denied' | 'not-determined' | 'unsupported';

export interface DesktopSource {
	id: string;
	name: string;
	thumbnailDataUrl?: string;
	appIconDataUrl?: string;
	display_id?: string;
	nativeWidth?: number;
	nativeHeight?: number;
	isOwnWindow?: boolean;
}

export interface DisplayMediaRequestInfo {
	audioRequested: boolean;
	videoRequested: boolean;
	supportsLoopbackAudio?: boolean;
}

export interface NotificationOptions {
	id?: string;
	title: string;
	subtitle?: string;
	body: string;
	icon?: string;
	url?: string;
}

export interface NotificationResult {
	id: string;
}

export interface StreamerModeCaptureProcess {
	name: string;
	pid?: number;
}

export interface StreamerModeCaptureAppStatus {
	detected: boolean;
	processes: Array<StreamerModeCaptureProcess>;
}

export interface CpuInfo {
	model: string;
	speed: number;
	cores: number;
	physicalCores: number;
}

export interface ProcessMetrics {
	cpu: {
		percentCPUUsage: number;
	};
	memory: {
		workingSetSize: number;
		peakWorkingSetSize: number;
		privateBytes?: number;
	};
	pid: number;
	type: string;
	name?: string;
}

export interface AppMetricsSnapshot {
	cpuInfo: CpuInfo;
	processes: Array<ProcessMetrics>;
	totalMemoryMB: number;
	freeMemoryMB: number;
}

export interface ElectronAPI {
	platform: NodeJS.Platform;
	getDesktopInfo: () => Promise<DesktopInfo>;
	getGpuInfo?: () => Promise<GpuInfo>;
	getAppMetrics?: () => Promise<AppMetricsSnapshot>;
	getSystemIdleTimeMs?: () => Promise<number>;
	getDesktopWindowBehaviorSettings: () => Promise<DesktopWindowBehaviorSettings>;
	setDesktopWindowBehaviorSettings: (
		settings: Partial<DesktopWindowBehaviorSettings>,
	) => Promise<DesktopWindowBehaviorSettings>;
	getDesktopWindowBehaviorPendingRestart: () => Promise<boolean>;
	desktopAppRelaunch: () => Promise<void>;
	pickThemeLocalFiles: () => Promise<Array<ThemeLocalFileReference>>;
	readThemeLocalFiles: (paths: Array<string>) => Promise<Array<ThemeLocalFileReadResult>>;
	clearThemeLocalFiles: () => Promise<void>;
	importThemeDirectory: () => Promise<Array<ThemeDirectoryCssFile>>;
	cacheVoiceBackgroundMedia: (options: VoiceBackgroundMediaCacheRequest) => Promise<VoiceBackgroundMediaCacheResult>;
	resolveVoiceBackgroundMedia: (id: string) => Promise<VoiceBackgroundMediaCacheResult | null>;
	readVoiceBackgroundMedia: (id: string) => Promise<VoiceBackgroundMediaReadResult | null>;
	deleteVoiceBackgroundMedia: (id: string) => Promise<void>;
	onUpdaterEvent: (callback: (event: UpdaterEvent) => void) => () => void;
	updaterCheck: (context: UpdaterContext) => Promise<void>;
	updaterDownload: (context: UpdaterContext) => Promise<void>;
	updaterInstall: () => Promise<void>;
	windowMinimize: () => void;
	windowMaximize: () => void;
	windowClose: () => void;
	windowIsMaximized: () => Promise<boolean>;
	focusThemeStudioPopout: () => Promise<boolean>;
	closeThemeStudioPopout: () => Promise<boolean>;
	popoutSetAlwaysOnTop?: (key: string, flag: boolean) => Promise<boolean>;
	popoutFocus?: (key: string) => Promise<boolean>;
	openVoiceDebugEventSinkPopout?: (entries: Array<DesktopVoiceDebugEventSinkEntry>) => Promise<void>;
	appendVoiceDebugEventSinkEntries?: (entries: Array<DesktopVoiceDebugEventSinkEntry>) => void;
	setVoiceDebugEventSinkStatsHtml?: (html: string) => void;
	onWindowMaximizeChange: (callback: (maximized: boolean) => void) => () => void;
	openExternal: (url: string) => Promise<void>;
	clipboardWriteText: (text: string) => Promise<void>;
	clipboardReadText: () => Promise<string>;
	clipboardWriteFile: (options: ClipboardWriteFileOptions) => Promise<ClipboardWriteFileResult>;
	pasteFromClipboard: () => Promise<void>;
	onDeepLink: (callback: (url: string) => void) => () => void;
	getInitialDeepLink: () => Promise<string | null>;
	onRpcNavigate: (callback: (path: string) => void) => () => void;
	autostartEnable: () => Promise<void>;
	autostartDisable: () => Promise<void>;
	autostartIsEnabled: () => Promise<boolean>;
	autostartIsInitialized: () => Promise<boolean>;
	autostartMarkInitialized: () => Promise<void>;
	checkMediaAccess: (type: MediaAccessType) => Promise<MediaAccessStatus>;
	requestMediaAccess: (type: MediaAccessType) => Promise<boolean>;
	openMediaAccessSettings: (type: MediaAccessType) => Promise<void>;
	openInputMonitoringSettings: () => Promise<void>;
	getInputMonitoringPermissionStatus?: () => Promise<InputMonitoringPermissionStatus>;
	requestInputMonitoringPermission?: () => Promise<InputMonitoringPermissionStatus>;
	getScreenRecordingPermissionStatus?: () => Promise<InputMonitoringPermissionStatus>;
	requestScreenRecordingPermission?: () => Promise<InputMonitoringPermissionStatus>;
	downloadFile: (url: string, defaultPath: string) => Promise<DownloadFileResult>;
	toggleDevTools: () => void;
	showNotification: (options: NotificationOptions) => Promise<NotificationResult>;
	shouldPlayNotificationSound?: () => Promise<boolean>;
	getStreamerModeCaptureAppStatus?: () => Promise<StreamerModeCaptureAppStatus>;
	closeNotification: (id: string) => void;
	closeNotifications: (ids: Array<string>) => void;
	onNotificationClick: (callback: (id: string, url?: string) => void) => () => void;
	setBadgeCount: (count: number) => void;
	getBadgeCount: () => Promise<number>;
	bounceDock: (type?: 'critical' | 'informational') => number;
	cancelBounceDock: (id: number) => void;
	setNativeLocale: (locale: string, strings: Record<string, string>) => void;
	flashFrame: (persistent?: boolean) => void;
	stopFlashFrame: () => void;
	setTaskbarProgress: (fraction: number, mode?: 'normal' | 'indeterminate' | 'error' | 'paused' | 'none') => void;
	onJumpListNewDm: (callback: () => void) => () => void;
	setZoomFactor: (factor: number) => void;
	getZoomFactor: () => Promise<number>;
	getAccessibilitySupportEnabled: () => Promise<boolean>;
	onAccessibilitySupportChanged: (callback: (enabled: boolean) => void) => () => void;
	onZoomIn: (callback: () => void) => () => void;
	onZoomOut: (callback: () => void) => () => void;
	onZoomReset: (callback: () => void) => () => void;
	onOpenSettings: (callback: () => void) => () => void;
	setTrayRuntimeState: (state: Partial<TrayRuntimeStatePayload>) => void;
	onTrayAction: (callback: (payload: TrayActionPayload) => void) => () => void;
	globalKeyHookStart: () => Promise<boolean>;
	globalKeyHookStop: () => Promise<void>;
	globalKeyHookIsRunning: () => Promise<boolean>;
	checkInputMonitoringAccess: () => Promise<boolean>;
	globalKeyHookRegister: (options: GlobalKeyHookRegisterOptions) => Promise<void>;
	globalKeyHookUnregister: (id: string) => Promise<void>;
	globalKeyHookUnregisterAll: () => Promise<void>;
	linuxEvdevStatus: () => Promise<{
		supported: boolean;
		hasAccess: boolean;
		canPrompt: boolean;
		sandboxed: boolean;
		username: string | null;
		totalEventDevices: number;
		readableEventDevices: number;
		inInputGroup: boolean;
	}>;
	linuxEvdevGrantAccess: () => Promise<{
		success: boolean;
		needsRelogin: boolean;
		error?: string;
	}>;
	onGlobalKeyEvent: (callback: (event: GlobalKeyEvent) => void) => () => void;
	onGlobalMouseEvent: (callback: (event: GlobalMouseEvent) => void) => () => void;
	onGlobalKeybindTriggered: (callback: (event: GlobalKeybindTriggeredEvent) => void) => () => void;
	spellcheckGetState: () => Promise<SpellcheckState>;
	spellcheckSetState: (state: Partial<SpellcheckState>) => Promise<SpellcheckState>;
	spellcheckGetAvailableLanguages: () => Promise<Array<string>>;
	spellcheckGetBundledDictionaries?: () => Promise<Array<SpellcheckBundledDictionary>>;
	spellcheckSuggest?: (word: string) => Promise<Array<string>>;
	onSpellcheckStateChanged: (callback: (state: SpellcheckState) => void) => () => void;
	onSpellcheckEngineResolved?: (callback: (info: SpellcheckResolvedEngineInfo) => void) => () => void;
	onTextareaContextMenu: (callback: (params: TextareaContextMenuParams) => void) => () => void;
	spellcheckReplaceMisspelling: (replacement: string) => Promise<void>;
	spellcheckAddWordToDictionary: (word: string) => Promise<void>;
	passkeyIsSupported: () => Promise<boolean>;
	passkeyAuthenticate: (
		options: PublicKeyCredentialRequestOptionsJSON,
		requestContext?: {pin?: string},
	) => Promise<AuthenticationResponseJSON>;
	passkeyRegister: (
		options: PublicKeyCredentialCreationOptionsJSON,
		requestContext?: {pin?: string},
	) => Promise<RegistrationResponseJSON>;
	getDesktopSources: (
		types: Array<'screen' | 'window'>,
		requestId?: string,
		options?: {listOnly?: boolean},
	) => Promise<Array<DesktopSource>>;
	onDisplayMediaRequested?: (callback: (requestId: string, info: DisplayMediaRequestInfo) => void) => () => void;
	onDisplayMediaPortalEmpty?: (callback: (requestId: string) => void) => () => void;
	selectDisplayMediaSource: (requestId: string, sourceId: string | null, withAudio: boolean) => void;
	virtmic: VirtmicApi;
	nativeAudio: NativeAudioApi;
	voiceEngine?: VoiceEngineV2BridgeApi;
}

export type VirtmicUnavailableReason =
	| 'not-linux'
	| 'addon-not-installed'
	| 'load-failed'
	| 'no-pipewire'
	| 'disabled-by-launch'
	| 'glibcxx-too-old';
export type VirtmicBackend = 'pipewire';

export interface VirtmicAvailability {
	available: boolean;
	reason?: VirtmicUnavailableReason;
	backend?: VirtmicBackend;
}

export type VirtmicNode = Record<string, string>;

export interface VirtmicLinkOptions {
	ignoreDevices?: boolean;
	ignoreInputMedia?: boolean;
	ignoreVirtual?: boolean;
	workaround?: boolean;
}

export interface VirtmicSystemLinkOptions extends VirtmicLinkOptions {
	onlySpeakers?: boolean;
	onlyDefaultSpeakers?: boolean;
}

export interface VirtmicApi {
	getAvailability: () => Promise<VirtmicAvailability>;
	listTargets: (options?: {granular?: boolean}) => Promise<{
		ok: boolean;
		targets?: Array<VirtmicNode>;
		availability: VirtmicAvailability;
	}>;
	startInclude: (include: Array<VirtmicNode>, options?: VirtmicLinkOptions) => Promise<boolean>;
	startSystem: (exclude: Array<VirtmicNode>, options?: VirtmicSystemLinkOptions) => Promise<boolean>;
	resolveWindowPid: (sourceId: string) => Promise<number | null>;
	stop: () => Promise<void>;
}

export type NativeAudioBackend = 'macos-sck' | 'macos-coreaudio' | 'windows-wasapi-loopback' | 'linux-pipewire';

export interface NativeAudioAvailability {
	available: boolean;
	backend?: NativeAudioBackend;
	capabilities?: {
		process: boolean;
		system: boolean;
		systemExcludesSelf?: boolean;
		processInclude?: boolean;
		processExclude?: boolean;
		sessionMixer?: boolean;
		systemLoopbackMode?: 'process-exclude' | 'session-mixer' | 'unavailable';
	};
	reason?:
		| 'unsupported-platform'
		| 'addon-not-installed'
		| 'load-failed'
		| 'os-version-too-old'
		| 'permission-denied'
		| 'no-pipewire'
		| 'disabled-by-launch';
	detail?: string;
}

export interface NativeAudioApplication {
	pid: number;
	identifier: string;
	name: string;
	audible?: boolean;
}

export interface NativeAudioStartOptions {
	targetPid?: number;
	includeProcessTree?: boolean;
	macBackend?: 'sck' | 'coreaudio' | 'auto';
	macCaptureScope?: 'process' | 'system';
	winCaptureScope?: 'process' | 'system' | 'session-mixer';
	linuxRule?: VirtmicLinkOptions & {
		include?: Array<VirtmicNode>;
		exclude?: Array<VirtmicNode>;
		onlySpeakers?: boolean;
		onlyDefaultSpeakers?: boolean;
	};
}

export interface NativeAudioStartResult {
	captureId: string;
	sampleRate: number;
	channels: number;
}

export interface NativeAudioFrameMessage {
	captureId: string;
	sampleRate: number;
	channels: number;
	timestampUs: number;
	samples: ArrayBuffer;
}

export type NativeAudioEndReason = 'stopped' | 'target-exited' | 'addon-error';

export interface NativeAudioEndMessage {
	captureId: string;
	reason: NativeAudioEndReason;
	detail?: string;
}

export interface NativeAudioApi {
	getAvailability: () => Promise<NativeAudioAvailability>;
	listAudibleApplications: () => Promise<Array<NativeAudioApplication>>;
	resolveAudioRootPidForSource: (sourceId: string) => Promise<number | null>;
	start: (options: NativeAudioStartOptions) => Promise<NativeAudioStartResult>;
	stop: (captureId: string) => Promise<void>;
	onFrame: (callback: (message: NativeAudioFrameMessage) => void) => () => void;
	onEnd: (callback: (message: NativeAudioEndMessage) => void) => () => void;
}

export interface GlobalKeyHookRegisterOptions {
	id: string;
	description?: string;
	keycode?: number;
	keyName?: string;
	physicalKeyName?: string;
	mouseButton?: number;
	ctrl?: boolean;
	alt?: boolean;
	shift?: boolean;
	meta?: boolean;
}

export interface GlobalKeyEvent {
	type: 'keydown' | 'keyup';
	keycode: number;
	keyName: string;
	backend?: 'evdev' | 'native' | null;
	altKey: boolean;
	ctrlKey: boolean;
	shiftKey: boolean;
	metaKey: boolean;
}

export interface GlobalMouseEvent {
	type: 'mousedown' | 'mouseup';
	button: number;
	altKey: boolean;
	ctrlKey: boolean;
	shiftKey: boolean;
	metaKey: boolean;
}

export interface GlobalKeybindTriggeredEvent {
	id: string;
	type: 'keydown' | 'keyup';
}

export type SpellcheckEngine = 'auto' | 'hunspell' | 'system';

export interface SpellcheckState {
	enabled: boolean;
	engine: SpellcheckEngine;
	autoDetect: boolean;
	languages: Array<string>;
	personalDictionary: Array<string>;
}

export interface SpellcheckBundledDictionary {
	tag: string;
	package: string;
	displayName: string;
	nativeName: string;
}

export interface SpellcheckResolvedEngineInfo {
	mode: 'hunspell' | 'system' | 'off';
	hunspellLangs: Array<string>;
	systemLangs: Array<string>;
}

export interface TextareaContextMenuParams {
	misspelledWord?: string;
	suggestions?: Array<string>;
	editFlags: {
		canUndo: boolean;
		canRedo: boolean;
		canCut: boolean;
		canCopy: boolean;
		canPaste: boolean;
		canSelectAll: boolean;
	};
	x: number;
	y: number;
}
