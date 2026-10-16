// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AUTH_SHELL_HINT_STORAGE_KEY,
	readRawStorageItem,
	writeRawStorageItem,
} from '@app/features/platform/state/PrebootMirror';
import {getRemScaleForDocument} from '@app/features/theme/layout/RemFromPx';
import {useEffect} from 'react';

export const AUTH_SHELL_HINT_VERSION = 1;

export const AuthShellHintKind = Object.freeze({
	CARD: 'card',
	FULL: 'full',
} as const);

export type AuthShellHintKind = (typeof AuthShellHintKind)[keyof typeof AuthShellHintKind];

export interface AuthShellHintEntry {
	readonly t: number;
	readonly mo: number;
	readonly k: AuthShellHintKind;
	readonly h: number;
	readonly w: number;
}

export interface AuthShellHintMeasurement {
	readonly pathname: string;
	readonly mobile: boolean;
	readonly kind: AuthShellHintKind;
	readonly heightPx: number;
	readonly widthPx: number;
}

const MAX_CATEGORIES = 12;
const MAX_MEASURED_PX = 4000;
const SETTLE_MS = 400;
const LOADING_SELECTOR =
	'[data-flx="auth.flow.auth-loading-state.loading-container"],[data-flx^="auth.flow.auth-shell-loading-state."]';
const LOGIN_PATHS = new Set(['/', '/app', '/login', '/bookmarks', '/mentions', '/notifications', '/you']);
const LINK_PAGE_PATTERN = /^\/(invite|gift|theme)\/[^/]+(\/login)?\/?$/u;
const ACCOUNT_PAGE_PATTERN = /^\/(forgot|reset|verify|authorize-ip|wasntme)(\/|$)/u;
const capturedCategories = new Set<string>();

export function resolveAuthShellCategory(pathname: string): string | null {
	if (pathname === '/register') {
		return 'register';
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

function isEntry(value: unknown): value is AuthShellHintEntry {
	if (value == null || typeof value !== 'object') {
		return false;
	}
	const entry = value as Record<string, unknown>;
	return (
		typeof entry.t === 'number' &&
		typeof entry.h === 'number' &&
		typeof entry.w === 'number' &&
		(entry.mo === 0 || entry.mo === 1) &&
		(entry.k === AuthShellHintKind.CARD || entry.k === AuthShellHintKind.FULL)
	);
}

export function readAuthShellHintEntries(): Map<string, AuthShellHintEntry> {
	const entries = new Map<string, AuthShellHintEntry>();
	const raw = readRawStorageItem(AUTH_SHELL_HINT_STORAGE_KEY);
	if (raw == null) {
		return entries;
	}
	try {
		const parsed: unknown = JSON.parse(raw);
		if (parsed == null || typeof parsed !== 'object') {
			return entries;
		}
		const record = parsed as {v?: unknown; c?: unknown};
		if (record.v !== AUTH_SHELL_HINT_VERSION || record.c == null || typeof record.c !== 'object') {
			return entries;
		}
		for (const [category, entry] of Object.entries(record.c)) {
			if (isEntry(entry)) {
				entries.set(category, entry);
			}
		}
	} catch {
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
	readonly source: 'hint' | 'default';
}

export interface AuthShellPlaceholderEnvironment {
	readonly mobile: boolean;
	readonly native: boolean;
	readonly passkeyLikely: boolean;
}

const LOGIN_CARD_HEIGHT_PX = 214;
const LOGIN_MOBILE_HEIGHT_PX = 132;
const LOGIN_PASSKEY_ROW_PX = 46;
const REGISTER_CARD_HEIGHT_PX = 674;
const REGISTER_MOBILE_HEIGHT_PX = 592;
const GENERIC_CARD_HEIGHT_PX = 402;
const GENERIC_MOBILE_HEIGHT_PX = 320;
const MIN_HINT_HEIGHT_PX = 48;
const MAX_HINT_WIDTH_PX = 1200;

export function resolveDefaultAuthShellPlaceholder(
	category: string,
	environment: AuthShellPlaceholderEnvironment,
): AuthShellPlaceholder {
	const {mobile} = environment;
	if (category === 'login' && !mobile && environment.native) {
		return {category, kind: AuthShellHintKind.FULL, heightPx: 0, widthPx: 0, rows: -1, source: 'default'};
	}
	if (category === 'login') {
		const passkeyRow = environment.passkeyLikely ? LOGIN_PASSKEY_ROW_PX : 0;
		return {
			category,
			kind: AuthShellHintKind.CARD,
			heightPx: (mobile ? LOGIN_MOBILE_HEIGHT_PX : LOGIN_CARD_HEIGHT_PX) + passkeyRow,
			widthPx: 0,
			rows: environment.passkeyLikely ? 3 : 2,
			source: 'default',
		};
	}
	const heightPx =
		category === 'register'
			? mobile
				? REGISTER_MOBILE_HEIGHT_PX
				: REGISTER_CARD_HEIGHT_PX
			: mobile
				? GENERIC_MOBILE_HEIGHT_PX
				: GENERIC_CARD_HEIGHT_PX;
	return {category, kind: AuthShellHintKind.CARD, heightPx, widthPx: 0, rows: -1, source: 'default'};
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
		return {
			category,
			kind: entry.k,
			heightPx: clampPlaceholderPx(entry.h, MIN_HINT_HEIGHT_PX, MAX_MEASURED_PX),
			widthPx: clampPlaceholderPx(entry.w, 0, MAX_HINT_WIDTH_PX),
			rows: -1,
			source: 'hint',
		};
	}
	return resolveDefaultAuthShellPlaceholder(category, environment);
}

export function isPasskeyLikelyForDocumentHost(host: string, publicKeyCredentialAvailable: boolean): boolean {
	return publicKeyCredentialAvailable && host.split('.').length >= 2 && !/^[\d.]+$/u.test(host) && !host.includes(':');
}

export function readAuthShellPlaceholderEnvironment(mobile: boolean): AuthShellPlaceholderEnvironment {
	const publicKeyCredential = (window as {PublicKeyCredential?: unknown}).PublicKeyCredential;
	return {
		mobile,
		native: document.documentElement.classList.contains('platform-native'),
		passkeyLikely: isPasskeyLikelyForDocumentHost(window.location.hostname, typeof publicKeyCredential === 'function'),
	};
}

function clampMeasuredPx(value: number): number {
	if (!Number.isFinite(value)) {
		return 0;
	}
	return Math.min(MAX_MEASURED_PX, Math.max(0, Math.round(value)));
}

export function recordAuthShellHint(measurement: AuthShellHintMeasurement, now = Date.now()): void {
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
	readonly resolveTarget: () => HTMLElement | null;
}

export function useAuthShellHintCapture({
	enabled,
	pathname,
	mobile,
	kind,
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
			recordAuthShellHint({pathname, mobile, kind, heightPx, widthPx: target.offsetWidth / remScale});
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
	}, [enabled, pathname, mobile, kind, resolveTarget]);
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
	const paddingPx = mobile
		? 0
		: (Math.max(
				CARD_PADDING_MIN_REM * remScale,
				Math.min(CARD_PADDING_VIEWPORT_RATIO * viewportWidthPx, CARD_PADDING_MAX_REM * remScale),
			) *
				2) /
				remScale +
			CARD_BORDER_PX;
	return Math.max(
		1,
		Math.min(MAX_ROWS, Math.floor((placeholder.heightPx - paddingPx - TITLE_BLOCK_PX) / ROW_PITCH_PX)),
	);
}
