// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AppMetricsSnapshot,
	ChromiumRuntimeInfo,
	ClipboardWriteFileOptions,
	ClipboardWriteFileResult,
	DesktopTroubleshootingSettings,
	DesktopVoiceDebugEventSinkEntry,
	DisplayMediaPortalSurfacePreference,
	GlobalKeybindTriggeredEvent,
	GlobalKeyEvent,
	GlobalKeyHookRegisterOptions,
	GlobalMouseEvent,
	GlobalShortcutCaptureEvent,
	GlobalShortcutEvent,
	GlobalShortcutsBackend,
	GlobalShortcutsPortalShortcut,
	GlobalShortcutsPortalState,
	GlobalShortcutsSyncPayload,
	GpuInfo,
	InputMonitoringPermissionStatus,
	LinuxAppearanceSnapshot,
	MediaAccessStatus,
	MediaAccessType,
	NativeAudioApplication,
	NativeAudioAvailability,
	NativeAudioEndMessage,
	NativeAudioFrameMessage,
	NativeAudioStartOptions,
	NativeAudioStartResult,
	NativeScreenCaptureLifecycleMessage,
	SetDesktopTroubleshootingDisableHardwareAccelerationOptions,
	SpellcheckBundledDictionary,
	SpellcheckEngine,
	SpellcheckResolvedEngineInfo,
	StreamerModeCaptureAppStatus,
	StreamingPriorityDiagnostics,
	ThemeDirectoryCssFile,
	ThemeLinkedFileChange,
	ThemeLocalFileReadResult,
	ThemeLocalFileReference,
	TrayPresenceStatus,
	TrayRuntimeStatePayload,
	UpdaterDownloadOption,
	VirtmicBackend,
	VirtmicNode,
	VoiceBackgroundMediaCacheRequest,
	VoiceBackgroundMediaCacheResult,
	VoiceBackgroundMediaReadResult,
} from '@fluxer/desktop_ipc/src/DesktopBridgeTypes';

export type * from '@fluxer/desktop_ipc/src/DesktopBridgeTypes';

import type {BuildChannel} from '@electron/common/BuildChannel';
import type {DesktopAccountStorageAPI} from '@fluxer/desktop_ipc/src/AccountContract';
import type {DesktopHandoffAPI} from '@fluxer/desktop_ipc/src/BrowserHandoffContract';
import type {DesktopCapabilityManifest} from '@fluxer/desktop_ipc/src/CapabilityManifest';
import type {NativeGatewayTransportAPI} from '@fluxer/desktop_ipc/src/GatewayTransportContract';
import type {DesktopKnownInstanceStorageAPI} from '@fluxer/desktop_ipc/src/KnownInstanceContract';
import type {DesktopModuleAPI, DesktopUpdateAPI} from '@fluxer/desktop_ipc/src/ModuleContract';
import type {DesktopStorageAPI} from '@fluxer/desktop_ipc/src/StorageContract';
import type {VoiceEngineV2BridgeHardwareEncoderApi} from '@fluxer/voice_engine_v2/bridge';
import type {
	AuthenticationResponseJSON,
	PublicKeyCredentialCreationOptionsJSON,
	PublicKeyCredentialRequestOptionsJSON,
	RegistrationResponseJSON,
} from '@simplewebauthn/browser';

export interface DesktopInfo {
	version: string;
	channel: BuildChannel;
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

export interface DesktopWindowBehaviorSettings {
	showTrayIcon: boolean;
	minimizeToTray: boolean;
	closeToTray: boolean;
	startMinimized: boolean;
	useNativeTitleBar: boolean;
	activeUseNativeTitleBar: boolean;
	rememberWindowState: boolean;
	allowTransparency: boolean;
	activeAllowTransparency: boolean;
	smoothScrolling: boolean;
	activeSmoothScrolling: boolean;
	middleClickAutoscroll: boolean;
	activeMiddleClickAutoscroll: boolean;
}

export type UpdaterContext = 'user' | 'background' | 'focus';

export type UpdaterEvent =
	| {
			type: 'checking';
			context: UpdaterContext;
	  }
	| {
			type: 'available';
			context: UpdaterContext;
			version: string | null;
			downloadUrl?: string;
			downloadOptions?: Array<UpdaterDownloadOption>;
	  }
	| {
			type: 'not-available';
			context: UpdaterContext;
	  }
	| {
			type: 'error';
			context: UpdaterContext;
			message: string;
	  }
	| {
			type: 'unsupported';
			context: UpdaterContext;
			reason: 'platform' | 'unpackaged' | 'managed-package';
			downloadUrl?: string;
	  };

export interface DownloadFileResult {
	success: boolean;
	canceled?: boolean;
	checksumMismatch?: boolean;
	path?: string;
	error?: string;
}

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

export interface GetDesktopSourcesOptions {
	listOnly?: boolean;
}

export type VirtmicUnavailableReason =
	| 'not-linux'
	| 'addon-not-installed'
	| 'load-failed'
	| 'no-pipewire'
	| 'disabled-by-launch';

export interface VirtmicAvailability {
	available: boolean;
	reason?: VirtmicUnavailableReason;
	backend?: VirtmicBackend;
}

export interface VirtmicApi {
	getAvailability: () => Promise<VirtmicAvailability>;
	listTargets: (options?: {granular?: boolean}) => Promise<{
		ok: boolean;
		targets?: Array<VirtmicNode>;
		availability: VirtmicAvailability;
	}>;
	stop: () => Promise<void>;
}

export interface NativeAudioApi {
	getAvailability: () => Promise<NativeAudioAvailability>;
	listAudibleApplications: () => Promise<Array<NativeAudioApplication>>;
	resolveAudioRootPidForSource: (sourceId: string) => Promise<number | null>;
	start: (options: NativeAudioStartOptions) => Promise<NativeAudioStartResult>;
	setRule: (captureId: string, linuxRule: NonNullable<NativeAudioStartOptions['linuxRule']>) => Promise<boolean>;
	stop: (captureId: string) => Promise<void>;
	onFrame: (callback: (message: NativeAudioFrameMessage) => void) => () => void;
	onEnd: (callback: (message: NativeAudioEndMessage) => void) => () => void;
}

export interface NativeScreenCaptureApi {
	onLifecycleEvent: (callback: (message: NativeScreenCaptureLifecycleMessage) => void) => () => void;
}

export interface DisplayMediaRequestInfo {
	audioRequested: boolean;
	videoRequested: boolean;
	supportsLoopbackAudio: boolean;
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

export interface ElectronAPI {
	platform: NodeJS.Platform;
	buildChannel: BuildChannel;
	localDevelopmentInstanceUrl: string | null;
	offlineBuild?: boolean;
	getDesktopInfo: () => Promise<DesktopInfo>;
	getGpuInfo: () => Promise<GpuInfo>;
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
	pickThemeLinkedFiles: (options?: {multiple?: boolean}) => Promise<Array<ThemeDirectoryCssFile>>;
	watchThemeLinkedFiles: (paths: Array<string>) => Promise<void>;
	onThemeLinkedFileChange: (callback: (change: ThemeLinkedFileChange) => void) => () => void;
	cacheVoiceBackgroundMedia: (options: VoiceBackgroundMediaCacheRequest) => Promise<VoiceBackgroundMediaCacheResult>;
	readVoiceBackgroundMedia: (id: string) => Promise<VoiceBackgroundMediaReadResult | null>;
	deleteVoiceBackgroundMedia: (id: string) => Promise<void>;
	getDesktopTroubleshootingSettings: () => Promise<DesktopTroubleshootingSettings>;
	setDesktopDisableHardwareAcceleration: (
		options: SetDesktopTroubleshootingDisableHardwareAccelerationOptions,
	) => Promise<DesktopTroubleshootingSettings>;
	onUpdaterEvent: (callback: (event: UpdaterEvent) => void) => () => void;
	updaterCheck: (context: UpdaterContext) => Promise<void>;
	windowMinimize: () => void;
	windowMaximize: () => void;
	windowClose: () => void;
	windowIsMaximized: () => Promise<boolean>;
	focusThemeStudioPopout: () => Promise<boolean>;

	popoutSetAlwaysOnTop: (key: string, flag: boolean) => Promise<boolean>;
	popoutFocus: (key: string) => Promise<boolean>;
	openVoiceDebugEventSinkPopout: (entries: Array<DesktopVoiceDebugEventSinkEntry>) => Promise<void>;
	appendVoiceDebugEventSinkEntries: (entries: Array<DesktopVoiceDebugEventSinkEntry>) => void;
	setVoiceDebugEventSinkStatsHtml: (html: string) => void;
	onWindowLiveResizeChange: (callback: (resizing: boolean) => void) => () => void;
	onWindowMaximizeChange: (callback: (maximized: boolean) => void) => () => void;
	openExternal: (url: string) => Promise<void>;
	clipboardWriteText: (text: string) => Promise<void>;
	clipboardReadText: () => Promise<string>;
	clipboardWriteFile: (options: ClipboardWriteFileOptions) => Promise<ClipboardWriteFileResult>;
	pasteFromClipboard: () => Promise<void>;
	onDeepLink: (callback: (url: string) => void) => () => void;
	getInitialDeepLink: () => Promise<string | null>;
	autostartEnable: () => Promise<void>;
	autostartDisable: () => Promise<void>;
	autostartIsEnabled: () => Promise<boolean>;
	autostartIsInitialized: () => Promise<boolean>;
	autostartMarkInitialized: () => Promise<void>;
	checkMediaAccess: (type: MediaAccessType) => Promise<MediaAccessStatus>;
	requestMediaAccess: (type: MediaAccessType) => Promise<boolean>;
	openMediaAccessSettings: (type: MediaAccessType) => Promise<void>;
	openInputMonitoringSettings: () => Promise<void>;
	getInputMonitoringPermissionStatus: () => Promise<InputMonitoringPermissionStatus>;
	requestInputMonitoringPermission: () => Promise<InputMonitoringPermissionStatus>;
	getScreenRecordingPermissionStatus: () => Promise<InputMonitoringPermissionStatus>;
	requestScreenRecordingPermission: () => Promise<InputMonitoringPermissionStatus>;
	downloadFile: (url: string, defaultPath: string, sha256?: string | null) => Promise<DownloadFileResult>;
	showNotification: (options: NotificationOptions) => Promise<NotificationResult>;
	shouldPlayNotificationSound: () => Promise<boolean>;
	getStreamerModeCaptureAppStatus: () => Promise<StreamerModeCaptureAppStatus>;
	closeNotification: (id: string) => void;
	closeNotifications: (ids: Array<string>) => void;
	onNotificationClick: (callback: (id: string, url?: string) => void) => () => void;
	setBadgeCount: (count: number) => void;
	setNativeLocale: (locale: string, strings: Record<string, string>) => void;
	flashFrame: (persistent?: boolean) => void;
	stopFlashFrame: () => void;
	setTaskbarProgress: (fraction: number, mode?: 'normal' | 'indeterminate' | 'error' | 'paused' | 'none') => void;
	onJumpListNewDm: (callback: () => void) => () => void;
	setZoomFactor: (factor: number) => void;
	getAccessibilitySupportEnabled: () => Promise<boolean>;
	onAccessibilitySupportChanged: (callback: (enabled: boolean) => void) => () => void;
	getLinuxAppearance: () => Promise<LinuxAppearanceSnapshot>;
	onLinuxAppearanceChanged: (callback: (snapshot: LinuxAppearanceSnapshot) => void) => () => void;
	onZoomIn: (callback: () => void) => () => void;
	onZoomOut: (callback: () => void) => () => void;
	onZoomReset: (callback: () => void) => () => void;
	onOpenSettings: (callback: () => void) => () => void;
	setTrayRuntimeState: (state: Partial<TrayRuntimeStatePayload>) => void;
	acquireStreamingPriority: () => void;
	releaseStreamingPriority: () => void;
	getStreamingPriorityDiagnostics: () => Promise<StreamingPriorityDiagnostics>;
	onTrayAction: (callback: (action: TrayActionPayload) => void) => () => void;
	globalKeyHookStart: () => Promise<boolean>;
	globalKeyHookStop: () => Promise<void>;
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

	onGlobalKeyEvent: (callback: (event: GlobalKeyEvent) => void) => () => void;
	onGlobalMouseEvent: (callback: (event: GlobalMouseEvent) => void) => () => void;
	onGlobalKeybindTriggered: (callback: (event: GlobalKeybindTriggeredEvent) => void) => () => void;
	globalShortcuts: GlobalShortcutsApi;
	spellcheckSetState: (state: Partial<SpellcheckState>) => Promise<SpellcheckState>;
	spellcheckGetAvailableLanguages: () => Promise<Array<string>>;
	spellcheckGetBundledDictionaries: () => Promise<Array<SpellcheckBundledDictionary>>;
	onSpellcheckStateChanged: (callback: (state: SpellcheckState) => void) => () => void;
	onSpellcheckEngineResolved: (callback: (info: SpellcheckResolvedEngineInfo) => void) => () => void;
	onTextareaContextMenu: (callback: (params: TextareaContextMenuParams) => void) => () => void;
	spellcheckReplaceMisspelling: (replacement: string) => Promise<void>;
	spellcheckAddWordToDictionary: (word: string) => Promise<void>;
	passkeyIsSupported: () => Promise<boolean>;
	passkeyAuthenticate: (
		options: PublicKeyCredentialRequestOptionsJSON,
		requestContext?: {pin?: string; instanceKey?: string},
	) => Promise<AuthenticationResponseJSON>;
	passkeyRegister: (
		options: PublicKeyCredentialCreationOptionsJSON,
		requestContext?: {pin?: string; instanceKey?: string},
	) => Promise<RegistrationResponseJSON>;
	virtmic: VirtmicApi;
	nativeAudio: NativeAudioApi;
	nativeScreenCapture: NativeScreenCaptureApi;
	voiceEngine: VoiceEngineV2BridgeHardwareEncoderApi;
	getDesktopSources: (
		types: Array<'screen' | 'window'>,
		requestId?: string,
		options?: GetDesktopSourcesOptions,
	) => Promise<Array<DesktopSource>>;
	getSystemIdleTimeMs: () => Promise<number>;
	onDisplayMediaRequested?: (callback: (requestId: string, info: DisplayMediaRequestInfo) => void) => () => void;
	onDisplayMediaPortalEmpty?: (callback: (requestId: string) => void) => () => void;
	selectDisplayMediaSource: (requestId: string, sourceId: string | null, withAudio: boolean) => void;
	setDisplayMediaPortalPreference: (preference: DisplayMediaPortalSurfacePreference) => Promise<void>;
	getAppMetrics?: () => Promise<AppMetricsSnapshot>;
	capabilities: DesktopCapabilityManifest;
	desktopModules: DesktopModuleAPI;
	desktopUpdate: DesktopUpdateAPI;
	reportLastRoute: (routePath: string) => void;
	notifyFirstContentPainted: () => void;
	desktopAccounts: DesktopAccountStorageAPI;
	desktopStorage: DesktopStorageAPI;
	desktopKnownInstances: DesktopKnownInstanceStorageAPI;
	desktopHandoff: DesktopHandoffAPI;
	nativeGatewayTransport: NativeGatewayTransportAPI;
}

export interface GlobalShortcutsPortalStatus {
	state: GlobalShortcutsPortalState;
	version: number | null;
	canConfigure: boolean;
	recovering: boolean;
	canRecheck: boolean;
	portalAppId: string | null;
	shortcuts: Array<GlobalShortcutsPortalShortcut>;
	error: string | null;
}

export type LinuxSessionType = 'wayland' | 'x11' | 'unknown';

export type LinuxDesktopKind = 'kde' | 'gnome' | 'hyprland' | 'other';

export interface GlobalShortcutsLinuxStatus {
	session: LinuxSessionType;
	sandbox: 'flatpak' | 'none';
	desktop: LinuxDesktopKind;
	portal: GlobalShortcutsPortalStatus | null;
	directInput: {available: boolean; enabled: boolean; locked: boolean};
}

export interface GlobalShortcutsStatus {
	backend: GlobalShortcutsBackend;
	platform: 'linux' | 'windows' | 'macos';
	linux: GlobalShortcutsLinuxStatus | null;
	hooksActive: boolean;
	hookError: 'permission' | 'start-failed' | null;
	supportsMouseButtons: boolean;
	supportsModifierOnly: boolean;
}

export interface GlobalShortcutsApi {
	sync: (payload: GlobalShortcutsSyncPayload) => Promise<void>;
	setPaused: (paused: boolean) => Promise<void>;
	getStatus: () => Promise<GlobalShortcutsStatus>;
	onStatus: (callback: (status: GlobalShortcutsStatus) => void) => () => void;
	onEvent: (callback: (event: GlobalShortcutEvent) => void) => () => void;
	setUp: () => Promise<GlobalShortcutsStatus>;
	recheck: () => Promise<GlobalShortcutsStatus>;
	configure: () => Promise<void>;
	setDirectInputEnabled: (enabled: boolean) => Promise<GlobalShortcutsStatus>;
	startCapture: () => Promise<number | null>;
	stopCapture: (captureId: number) => Promise<void>;
	onCapture: (callback: (event: GlobalShortcutCaptureEvent) => void) => () => void;
}

export interface SpellcheckState {
	enabled: boolean;
	engine: SpellcheckEngine;
	autoDetect: boolean;
	languages: Array<string>;
	personalDictionary: Array<string>;
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

declare global {
	interface Window {
		electron?: ElectronAPI;
	}
}
