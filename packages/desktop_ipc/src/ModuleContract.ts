// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_MODULE_CHANNELS = Object.freeze({
	ensure: 'desktop-modules:ensure',
	pendingUpdate: 'desktop-modules:pending-update',
	applyPendingUpdate: 'desktop-modules:apply-pending-update',
	confirmLaunch: 'desktop-modules:confirm-launch',
} as const);

export const DESKTOP_MODULE_EVENTS = Object.freeze({
	pendingUpdateChanged: 'desktop-modules:pending-update-changed',
} as const);

const DESKTOP_MODULE_NAME_PATTERN = /^[a-z][a-z0-9_]{0,63}$/u;
export const DESKTOP_GRAMMAR_MODULE_NAME = 'fluxer_grammars';
export const DESKTOP_TWEMOJI_MODULE_NAME = 'fluxer_twemoji';
export const DESKTOP_DEEP_FILTER_MODULE_NAME = 'fluxer_deepfilter';
export const DESKTOP_EMOJI_SPRITES_MODULE_NAME = 'fluxer_emoji_sprites';
export const DESKTOP_EXTRAS_MODULE_NAME = 'fluxer_extras';
export const DESKTOP_SOURCEMAP_MODULE_NAME = 'fluxer_sourcemaps';

export const DESKTOP_FONT_MODULE_NAMES = Object.freeze({
	'non-latin': 'fluxer_fonts_nonlatin',
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

export function isDesktopModuleName(value: unknown): value is string {
	return typeof value === 'string' && DESKTOP_MODULE_NAME_PATTERN.test(value);
}
