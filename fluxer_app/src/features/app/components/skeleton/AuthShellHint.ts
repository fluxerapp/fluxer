// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AUTH_SHELL_HINT_STORAGE_KEY,
	readRawStorageItem,
	writeRawStorageItem,
} from '@app/features/platform/state/PrebootMirror';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getRemScaleForDocument} from '@app/features/theme/layout/RemFromPx';
import {isObjectLike} from '@fluxer/record_utils/src/RecordGuards';
import {useEffect} from 'react';

const logger = new Logger('AuthShellHint');
const AUTH_SHELL_HINT_VERSION = 2;

export const AuthShellHintKind = Object.freeze({
	CARD: 'card',
	FULL: 'full',
} as const);

export type AuthShellHintKind = (typeof AuthShellHintKind)[keyof typeof AuthShellHintKind];

interface AuthShellHintEntry {
	readonly t: number;
	readonly mo: number;
	readonly k: AuthShellHintKind;
	readonly h: number;
	readonly w: number;
	readonly s: 0 | 1;
}

interface AuthShellHintMeasurement {
	readonly pathname: string;
	readonly mobile: boolean;
	readonly kind: AuthShellHintKind;
	readonly heightPx: number;
	readonly widthPx: number;
	readonly split: boolean;
}

const MAX_CATEGORIES = 12;
const MAX_MEASURED_PX = 4000;
const SETTLE_MS = 400;
const LOADING_SELECTOR =
	'[data-flx="auth.flow.auth-loading-state.loading-container"],[data-flx^="auth.flow.auth-shell-loading-state."]';
const NARROW_BRAND_META_SELECTOR = 'meta[name="fluxer-auth-card"][content="narrow"]';
const LOGIN_PATHS = new Set(['/', '/app', '/login', '/bookmarks', '/mentions', '/notifications', '/you']);
const LINK_PAGE_PATTERN = /^\/(invite|gift|theme)\/[^/]+(\/login)?\/?$/u;
const ACCOUNT_PAGE_PATTERN = /^\/(forgot|reset|verify|authorize-ip|wasntme)(\/|$)/u;
const HANDOFF_PAGE_PATTERN = /^\/channels\/@(start|checkout)(\/|$)/u;
const capturedCategories = new Set<string>();

export function resolveAuthShellCategory(pathname: string): string | null {
	if (pathname === '/register') {
		return 'register';
	}
	if (HANDOFF_PAGE_PATTERN.test(pathname)) {
		return null;
	}
	if (LOGIN_PATHS.has(pathname) || pathname.startsWith('/channels/')) {
		return 'login';
	}
	const linkPage = LINK_PAGE_PATTERN.exec(pathname);
	if (linkPage != null) {
		return linkPage[2] == null ? linkPage[1] : `${linkPage[1]}-login`;
	}
	const accountPage = ACCOUNT_PAGE_PATTERN.exec(pathname);
	return accountPage == null ? null : accountPage[1];
}

function isEntry(entry: unknown): entry is AuthShellHintEntry {
	if (!isObjectLike(entry)) {
		return false;
	}
	return (
		typeof entry.t === 'number' &&
		typeof entry.h === 'number' &&
		typeof entry.w === 'number' &&
		(entry.mo === 0 || entry.mo === 1) &&
		(entry.s === 0 || entry.s === 1) &&
		(entry.k === AuthShellHintKind.CARD || entry.k === AuthShellHintKind.FULL)
	);
}

function readAuthShellHintEntries(): Map<string, AuthShellHintEntry> {
	const entries = new Map<string, AuthShellHintEntry>();
	const raw = readRawStorageItem(AUTH_SHELL_HINT_STORAGE_KEY);
	if (raw == null) {
		return entries;
	}
	try {
		const record: unknown = JSON.parse(raw);
		if (!isObjectLike(record)) {
			return entries;
		}
		if (record.v !== AUTH_SHELL_HINT_VERSION || record.c == null || typeof record.c !== 'object') {
			return entries;
		}
		for (const [category, entry] of Object.entries(record.c)) {
			if (isEntry(entry)) {
				entries.set(category, entry);
			}
		}
	} catch (error) {
		if (!(error instanceof SyntaxError)) {
			logger.warn('Failed to read auth shell hints', error);
		}
		return entries;
	}
	return entries;
}

export interface AuthShellPlaceholder {
	readonly category: string;
	readonly kind: AuthShellHintKind;
	readonly heightPx: number;
	readonly widthPx: number;
	readonly rows: number;
	readonly split: boolean;
	readonly source: 'hint' | 'default';
}

interface AuthShellPlaceholderEnvironment {
	readonly mobile: boolean;
	readonly native: boolean;
	readonly passkeyLikely: boolean;
	readonly wide: boolean;
	readonly narrowBrand: boolean;
}

const LOGIN_CARD_HEIGHT_PX = 214;
const LOGIN_MOBILE_HEIGHT_PX = 132;
const LOGIN_PASSKEY_ROW_PX = 46;
const REGISTER_CARD_HEIGHT_PX = 674;
const REGISTER_MOBILE_HEIGHT_PX = 592;
const GENERIC_CARD_HEIGHT_PX = 402;
const GENERIC_MOBILE_HEIGHT_PX = 320;
const SPLIT_CARD_MIN_HEIGHT_PX = 448;
export const BRAND_ROW_CARD_PX = 44;
export const BRAND_ROW_MOBILE_PX = 64;
const MIN_HINT_HEIGHT_PX = 48;
const MAX_HINT_WIDTH_PX = 1200;

function resolveDefaultAuthShellPlaceholder(
	category: string,
	environment: AuthShellPlaceholderEnvironment,
): AuthShellPlaceholder {
	const {mobile} = environment;
	if (category === 'login' && !mobile && environment.native) {
		return {
			category,
			kind: AuthShellHintKind.FULL,
			heightPx: 0,
			widthPx: 0,
			rows: -1,
			split: false,
			source: 'default',
		};
	}
	const split = !mobile && environment.wide && !environment.narrowBrand;
	const brandedHeightPx = (contentHeightPx: number): number => {
		if (mobile) {
			return contentHeightPx + BRAND_ROW_MOBILE_PX;
		}
		return split ? Math.max(contentHeightPx, SPLIT_CARD_MIN_HEIGHT_PX) : contentHeightPx + BRAND_ROW_CARD_PX;
	};
	if (category === 'login') {
		const passkeyRow = environment.passkeyLikely ? LOGIN_PASSKEY_ROW_PX : 0;
		return {
			category,
			kind: AuthShellHintKind.CARD,
			heightPx: brandedHeightPx((mobile ? LOGIN_MOBILE_HEIGHT_PX : LOGIN_CARD_HEIGHT_PX) + passkeyRow),
			widthPx: 0,
			rows: environment.passkeyLikely ? 3 : 2,
			split,
			source: 'default',
		};
	}
	let heightPx = GENERIC_CARD_HEIGHT_PX;
	if (category === 'register') {
		heightPx = mobile ? REGISTER_MOBILE_HEIGHT_PX : REGISTER_CARD_HEIGHT_PX;
	} else if (mobile) {
		heightPx = GENERIC_MOBILE_HEIGHT_PX;
	}
	return {
		category,
		kind: AuthShellHintKind.CARD,
		heightPx: brandedHeightPx(heightPx),
		widthPx: 0,
		rows: -1,
		split,
		source: 'default',
	};
}

function clampPlaceholderPx(value: number, min: number, max: number): number {
	return Math.min(max, Math.max(min, value));
}

export function resolveAuthShellPlaceholder(
	pathname: string,
	environment: AuthShellPlaceholderEnvironment,
): AuthShellPlaceholder | null {
	const category = resolveAuthShellCategory(pathname);
	if (category == null) {
		return null;
	}
	const entry = readAuthShellHintEntries().get(category);
	if (entry != null && entry.mo === (environment.mobile ? 1 : 0) && entry.h > 0) {
		const kind = entry.k;
		const heightPx = clampPlaceholderPx(entry.h, MIN_HINT_HEIGHT_PX, MAX_MEASURED_PX);
		const widthPx = clampPlaceholderPx(entry.w, 0, MAX_HINT_WIDTH_PX);
		let rows = -1;
		if (category === 'login') {
			rows = environment.passkeyLikely ? 3 : 2;
		}
		return {
			category,
			kind,
			heightPx,
			widthPx,
			rows,
			split: entry.s === 1 && !environment.mobile && environment.wide,
			source: 'hint',
		};
	}
	return resolveDefaultAuthShellPlaceholder(category, environment);
}

function isPasskeyLikelyForDocumentHost(host: string, publicKeyCredentialAvailable: boolean): boolean {
	return publicKeyCredentialAvailable && host.split('.').length >= 2 && !/^[\d.]+$/u.test(host) && !host.includes(':');
}

export function readAuthShellPlaceholderEnvironment(mobile: boolean): AuthShellPlaceholderEnvironment {
	const publicKeyCredential = window.PublicKeyCredential;
	return {
		mobile,
		native: document.documentElement.classList.contains('platform-native'),
		passkeyLikely: isPasskeyLikelyForDocumentHost(window.location.hostname, typeof publicKeyCredential === 'function'),
		wide: typeof window.matchMedia === 'function' && window.matchMedia('(min-width: 55rem)').matches,
		narrowBrand: document.querySelector(NARROW_BRAND_META_SELECTOR) !== null,
	};
}

function clampMeasuredPx(value: number): number {
	if (!Number.isFinite(value)) {
		return 0;
	}
	return Math.min(MAX_MEASURED_PX, Math.max(0, Math.round(value)));
}

function recordAuthShellHint(measurement: AuthShellHintMeasurement): void {
	const now = Date.now();
	const category = resolveAuthShellCategory(measurement.pathname);
	const heightPx = clampMeasuredPx(measurement.heightPx);
	if (category == null || heightPx <= 0) {
		return;
	}
	const entries = readAuthShellHintEntries();
	entries.delete(category);
	entries.set(category, {
		t: now,
		mo: measurement.mobile ? 1 : 0,
		k: measurement.kind,
		h: heightPx,
		w: clampMeasuredPx(measurement.widthPx),
		s: measurement.split ? 1 : 0,
	});
	const kept = Array.from(entries.entries())
		.sort(([, left], [, right]) => right.t - left.t)
		.slice(0, MAX_CATEGORIES);
	writeRawStorageItem(
		AUTH_SHELL_HINT_STORAGE_KEY,
		JSON.stringify({v: AUTH_SHELL_HINT_VERSION, c: Object.fromEntries(kept)}),
	);
}

interface AuthShellHintCaptureOptions {
	readonly enabled: boolean;
	readonly pathname: string;
	readonly mobile: boolean;
	readonly kind: AuthShellHintKind;
	readonly split: boolean;
	readonly resolveTarget: () => HTMLElement | null;
}

export function useAuthShellHintCapture({
	enabled,
	pathname,
	mobile,
	kind,
	split,
	resolveTarget,
}: AuthShellHintCaptureOptions): void {
	useEffect(() => {
		const category = resolveAuthShellCategory(pathname);
		if (!enabled || category == null || capturedCategories.has(category)) {
			return;
		}
		const target = resolveTarget();
		if (target == null) {
			return;
		}
		let timer: ReturnType<typeof setTimeout> | null = null;
		const measure = (): void => {
			timer = null;
			if (target.querySelector(LOADING_SELECTOR) != null || !target.isConnected) {
				return;
			}
			const remScale = getRemScaleForDocument(target.ownerDocument);
			const heightPx = target.offsetHeight / remScale;
			if (!(heightPx > 0)) {
				return;
			}
			recordAuthShellHint({pathname, mobile, kind, heightPx, widthPx: target.offsetWidth / remScale, split});
			capturedCategories.add(category);
			stop();
		};
		const schedule = (): void => {
			if (timer != null) {
				clearTimeout(timer);
			}
			timer = setTimeout(measure, SETTLE_MS);
		};
		const resizeObserver = typeof ResizeObserver === 'undefined' ? null : new ResizeObserver(schedule);
		const mutationObserver = new MutationObserver(schedule);
		const stop = (): void => {
			resizeObserver?.disconnect();
			mutationObserver.disconnect();
			if (timer != null) {
				clearTimeout(timer);
				timer = null;
			}
		};
		resizeObserver?.observe(target);
		mutationObserver.observe(target, {childList: true, subtree: true});
		schedule();
		return stop;
	}, [enabled, pathname, mobile, kind, split, resolveTarget]);
}

const CARD_PADDING_MIN_REM = 32;
const CARD_PADDING_VIEWPORT_RATIO = 0.04;
const CARD_PADDING_MAX_REM = 44;
const CARD_BORDER_PX = 2;
const TITLE_BLOCK_PX = 40;
const ROW_PITCH_PX = 46;
const MAX_ROWS = 12;

export function resolveAuthShellRowCount(
	placeholder: AuthShellPlaceholder,
	mobile: boolean,
	viewportWidthPx: number,
	remScale: number,
): number {
	if (placeholder.rows > 0) {
		return placeholder.rows;
	}
	let paddingPx = BRAND_ROW_MOBILE_PX;
	if (!mobile) {
		paddingPx =
			(Math.max(
				CARD_PADDING_MIN_REM * remScale,
				Math.min(CARD_PADDING_VIEWPORT_RATIO * viewportWidthPx, CARD_PADDING_MAX_REM * remScale),
			) *
				2) /
				remScale +
			CARD_BORDER_PX +
			(placeholder.split ? 0 : BRAND_ROW_CARD_PX);
	}
	return Math.max(
		1,
		Math.min(MAX_ROWS, Math.floor((placeholder.heightPx - paddingPx - TITLE_BLOCK_PX) / ROW_PITCH_PX)),
	);
}
