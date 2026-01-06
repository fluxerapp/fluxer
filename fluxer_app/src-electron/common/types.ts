/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

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
}

export type UpdaterContext = 'user' | 'background' | 'focus';

export type UpdaterEvent =
	| {type: 'checking'; context: UpdaterContext}
	| {type: 'available'; context: UpdaterContext; version: string | null}
	| {type: 'not-available'; context: UpdaterContext}
	| {type: 'downloaded'; context: UpdaterContext; version: string | null}
	| {
			type: 'progress';
			context: UpdaterContext;
			percent: number;
			transferred: number;
			total: number;
			bytesPerSecond: number;
	  }
	| {type: 'error'; context: UpdaterContext; message: string};

export interface DownloadFileOptions {
	url: string;
	defaultPath: string;
}

export interface DownloadFileResult {
	success: boolean;
	path?: string;
	error?: string;
}

export interface GlobalShortcutOptions {
	accelerator: string;
	id: string;
}

export type MediaAccessType = 'microphone' | 'camera' | 'screen';
export type MediaAccessStatus = 'granted' | 'denied' | 'not-determined' | 'restricted' | 'unknown';

export interface DesktopSource {
	id: string;
	name: string;
	thumbnailDataUrl: string;
	appIconDataUrl?: string;
	display_id?: string;
}

export interface DisplayMediaRequestInfo {
	audioRequested: boolean;
	videoRequested: boolean;
}

export type AccessibilityStatus = boolean;

export interface NotificationOptions {
	title: string;
	body: string;
	icon?: string;
	url?: string;
}

export interface NotificationResult {
	id: string;
}

export interface ElectronAPI {
	platform: NodeJS.Platform;
	getDesktopInfo: () => Promise<DesktopInfo>;
	getWsProxyUrl: () => string | null;
	getApiProxyUrl: () => string | null;
	getMediaProxyUrl: () => string | null;

	onUpdaterEvent: (callback: (event: UpdaterEvent) => void) => () => void;
	updaterCheck: (context: UpdaterContext) => Promise<void>;
	updaterInstall: () => Promise<void>;

	windowMinimize: () => void;
	windowMaximize: () => void;
	windowClose: () => void;
	windowIsMaximized: () => Promise<boolean>;
	onWindowMaximizeChange: (callback: (maximized: boolean) => void) => () => void;

	openExternal: (url: string) => Promise<void>;

	clipboardWriteText: (text: string) => Promise<void>;
	clipboardReadText: () => Promise<string>;

	onDeepLink: (callback: (url: string) => void) => () => void;
	getInitialDeepLink: () => Promise<string | null>;

	onRpcNavigate: (callback: (path: string) => void) => () => void;

	registerGlobalShortcut: (accelerator: string, id: string) => Promise<boolean>;
	unregisterGlobalShortcut: (accelerator: string) => Promise<void>;
	unregisterAllGlobalShortcuts: () => Promise<void>;
	onGlobalShortcut: (callback: (id: string) => void) => () => void;

	autostartEnable: () => Promise<void>;
	autostartDisable: () => Promise<void>;
	autostartIsEnabled: () => Promise<boolean>;
	autostartIsInitialized: () => Promise<boolean>;
	autostartMarkInitialized: () => Promise<void>;

	checkMediaAccess: (type: MediaAccessType) => Promise<MediaAccessStatus>;
	requestMediaAccess: (type: MediaAccessType) => Promise<boolean>;
	openMediaAccessSettings: (type: MediaAccessType) => Promise<void>;

	checkAccessibility: (prompt: boolean) => Promise<boolean>;
	openAccessibilitySettings: () => Promise<void>;
	openInputMonitoringSettings: () => Promise<void>;

	downloadFile: (url: string, defaultPath: string) => Promise<DownloadFileResult>;

	toggleDevTools: () => void;

	showNotification: (options: NotificationOptions) => Promise<NotificationResult>;
	closeNotification: (id: string) => void;
	closeNotifications: (ids: Array<string>) => void;
	onNotificationClick: (callback: (id: string, url?: string) => void) => () => void;

	setBadgeCount: (count: number) => void;
	getBadgeCount: () => Promise<number>;

	bounceDock: (type?: 'critical' | 'informational') => number;
	cancelBounceDock: (id: number) => void;

	setZoomFactor: (factor: number) => void;
	getZoomFactor: () => Promise<number>;

	onZoomIn: (callback: () => void) => () => void;
	onZoomOut: (callback: () => void) => () => void;
	onZoomReset: (callback: () => void) => () => void;

	onOpenSettings: (callback: () => void) => () => void;

	globalKeyHookStart: () => Promise<boolean>;
	globalKeyHookStop: () => Promise<void>;
	globalKeyHookIsRunning: () => Promise<boolean>;
	checkInputMonitoringAccess: () => Promise<boolean>;
	globalKeyHookRegister: (options: GlobalKeyHookRegisterOptions) => Promise<void>;
	globalKeyHookUnregister: (id: string) => Promise<void>;
	globalKeyHookUnregisterAll: () => Promise<void>;
	onGlobalKeyEvent: (callback: (event: GlobalKeyEvent) => void) => () => void;
	onGlobalMouseEvent: (callback: (event: GlobalMouseEvent) => void) => () => void;
	onGlobalKeybindTriggered: (callback: (event: GlobalKeybindTriggeredEvent) => void) => () => void;

	spellcheckGetState: () => Promise<SpellcheckState>;
	spellcheckSetState: (state: Partial<SpellcheckState>) => Promise<SpellcheckState>;
	spellcheckGetAvailableLanguages: () => Promise<Array<string>>;
	spellcheckOpenLanguageSettings: () => Promise<boolean>;
	onSpellcheckStateChanged: (callback: (state: SpellcheckState) => void) => () => void;
	onTextareaContextMenu: (callback: (params: TextareaContextMenuParams) => void) => () => void;
	spellcheckReplaceMisspelling: (replacement: string) => Promise<void>;
	spellcheckAddWordToDictionary: (word: string) => Promise<void>;

	passkeyIsSupported: () => Promise<boolean>;
	passkeyAuthenticate: (options: PublicKeyCredentialRequestOptionsJSON) => Promise<AuthenticationResponseJSON>;
	passkeyRegister: (options: PublicKeyCredentialCreationOptionsJSON) => Promise<RegistrationResponseJSON>;

	getDesktopSources: (types: Array<'screen' | 'window'>) => Promise<Array<DesktopSource>>;
	onDisplayMediaRequested?: (callback: (requestId: string, info: DisplayMediaRequestInfo) => void) => () => void;
	selectDisplayMediaSource: (requestId: string, sourceId: string | null, withAudio: boolean) => void;
}

export interface GlobalKeyHookRegisterOptions {
	id: string;
	keycode?: number;
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
	altKey: boolean;
	ctrlKey: boolean;
	shiftKey: boolean;
	metaKey: boolean;
}

export interface GlobalMouseEvent {
	type: 'mousedown' | 'mouseup';
	button: number;
}

export interface GlobalKeybindTriggeredEvent {
	id: string;
	type: 'keydown' | 'keyup';
}

export interface SpellcheckState {
	enabled: boolean;
	languages: Array<string>;
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
