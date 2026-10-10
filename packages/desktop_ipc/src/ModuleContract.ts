// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_MODULE_CHANNELS = Object.freeze({
	ensure: 'desktop-modules:ensure',
	confirmLaunch: 'desktop-modules:confirm-launch',
} as const);

export const DESKTOP_UPDATE_CHANNELS = Object.freeze({
	state: 'desktop-update:state',
	start: 'desktop-update:start',
} as const);

export const DESKTOP_UPDATE_EVENTS = Object.freeze({
	stateChanged: 'desktop-update:state-changed',
} as const);

const DESKTOP_MODULE_NAME_PATTERN = /^[a-z][a-z0-9_]{0,63}$/u;
export const DESKTOP_RENDERER_MODULE_NAME = 'fluxer_renderer';
export const DESKTOP_CAMERA_EFFECTS_MODULE_NAME = 'fluxer_camera_effects';
export const DESKTOP_SOURCEMAP_MODULE_NAME = 'fluxer_sourcemaps';

export const DESKTOP_FONT_MODULE_NAMES = Object.freeze({
	sc: 'fluxer_fonts_sc',
	tc: 'fluxer_fonts_tc',
	jp: 'fluxer_fonts_jp',
	kr: 'fluxer_fonts_kr',
} as const);

export type DesktopFontScript = keyof typeof DESKTOP_FONT_MODULE_NAMES;

export const DesktopModuleEnsureStatus = Object.freeze({
	INSTALLED: 'installed',
	ALREADY_INSTALLED: 'already-installed',
	UNAVAILABLE: 'unavailable',
	DISABLED: 'disabled',
} as const);

export type DesktopModuleEnsureStatus = (typeof DesktopModuleEnsureStatus)[keyof typeof DesktopModuleEnsureStatus];

export interface DesktopModuleEnsureResult {
	readonly module: string;
	readonly status: DesktopModuleEnsureStatus;
}

export interface DesktopPendingModuleUpdate {
	readonly modules: ReadonlyArray<string>;
}

export interface DesktopModuleAPI {
	ensure: (moduleName: string) => Promise<DesktopModuleEnsureResult>;
	pendingUpdate?: () => Promise<DesktopPendingModuleUpdate | null>;
	applyPendingUpdate?: () => Promise<boolean>;
	onPendingUpdateChanged?: (listener: (pending: DesktopPendingModuleUpdate | null) => void) => () => void;
	confirmLaunch?: () => Promise<void>;
}

export interface DesktopUpdateState {
	readonly available: boolean;
	readonly updating?: boolean;
}

export interface DesktopUpdateAPI {
	state: () => Promise<DesktopUpdateState>;
	start: () => Promise<void>;
	onStateChanged: (listener: (state: DesktopUpdateState) => void) => () => void;
}

export function isDesktopModuleName(value: unknown): value is string {
	return typeof value === 'string' && DESKTOP_MODULE_NAME_PATTERN.test(value);
}
