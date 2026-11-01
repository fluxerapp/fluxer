// SPDX-License-Identifier: AGPL-3.0-or-later

import AttachmentUrlRefresher from '@app/features/messaging/state/AttachmentUrlRefresher';
import {
	ATTACHMENT_URL_REFRESH_MARGIN_MS,
	readAttachmentUrlSignature,
} from '@app/features/messaging/utils/AttachmentCdnUrl';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {clearRemScaleCache} from '@app/features/theme/layout/RemFromPx';
import type {ThemeLibraryAsset, ThemeLibraryLocalFileReference} from '@app/features/theme/state/ThemeLibrary';
import {
	gateThemeCss,
	signGatedCss,
	type ThemeCssGatePolicy,
	type ThemeCssGateResult,
} from '@app/features/theme/utils/ThemeCssUrlGate';
import {resolveThemeCssReferences} from '@app/features/theme/utils/ThemeCssUtils';
import {useEffect, useLayoutEffect, useRef} from 'react';

export const CUSTOM_THEME_STYLE_ELEMENT_ID = 'fluxer-custom-theme-style';
export const INSTANCE_CUSTOM_CSS_ELEMENT_ID = 'fluxer-instance-custom-css';
const logger = new Logger('useCustomThemeStyle');
const SHEET_PREPARE_WAIT_MS = 1_500;
const UNSIGNED_URL_RETRY_MS = 300_000;
const SIGNING_TIMEOUT_RETRY_MS = 5_000;
const MAX_TIMER_DELAY_MS = 2_147_483_647;
const USER_STYLE_ELEMENT_IDS: ReadonlySet<string> = new Set([
	CUSTOM_THEME_STYLE_ELEMENT_ID,
	INSTANCE_CUSTOM_CSS_ELEMENT_ID,
]);

export function excludeUserStylesheets(node: Element): boolean {
	return USER_STYLE_ELEMENT_IDS.has(node.id);
}

interface CustomThemeStyleOptions {
	enabledThemeCss: string | null | undefined;
	customThemeCss: string | null | undefined;
	themeLibraryAssets: ReadonlyArray<ThemeLibraryAsset>;
	themeLibraryLocalFiles: ReadonlyArray<ThemeLibraryLocalFileReference>;
	themeLibraryRevision: number;
	mediaEndpoint: string;
	onMissingReferences: (references: ReadonlyArray<string>) => void;
	onBlockedUrl: (url: string) => void;
	onCssRejected: (rejected: boolean) => void;
}

export function useInstanceCustomCss(instanceCss: string | null, gatedOrigins: ReadonlyArray<string> | null): void {
	const gatedOriginsKey = gatedOrigins?.join(' ') ?? null;
	useLayoutEffect(() => {
		const css = gateInstanceCss(instanceCss?.trim() ?? '', gatedOriginsKey);
		const existing = document.getElementById(INSTANCE_CUSTOM_CSS_ELEMENT_ID);
		if (!css) {
			if (existing) {
				existing.remove();
				clearRemScaleCache();
			}
			return;
		}
		const styleElement = existing ?? document.createElement('style');
		styleElement.id = INSTANCE_CUSTOM_CSS_ELEMENT_ID;
		styleElement.textContent = css;
		if (!existing) {
			document.head.insertBefore(styleElement, document.getElementById(CUSTOM_THEME_STYLE_ELEMENT_ID));
		}
		clearRemScaleCache();
	}, [instanceCss, gatedOriginsKey]);
}

function gateInstanceCss(css: string, gatedOriginsKey: string | null): string {
	if (!css || gatedOriginsKey === null) return css;
	try {
		const policy = {
			mediaEndpoint: '',
			objectUrls: new Set<string>(),
			instanceOrigins: new Set(gatedOriginsKey.split(' ')),
		};
		return gateThemeCss(css, policy).css;
	} catch (error) {
		logger.error('Failed to check instance custom CSS', error);
		return '';
	}
}

interface ThemeCssRequest {
	css: string;
	assets: ReadonlyArray<ThemeLibraryAsset>;
	localFiles: ReadonlyArray<ThemeLibraryLocalFileReference>;
	mediaEndpoint: string;
	onMissingReferences: (references: ReadonlyArray<string>) => void;
	onBlockedUrl: (url: string) => void;
	onCssRejected: (rejected: boolean) => void;
}

interface GatedThemeSheet {
	css: string;
	policy: ThemeCssGatePolicy;
	keptMediaUrls: ReadonlyArray<string>;
	signedUrls: ReadonlyMap<string, string>;
}

interface SignedKeptUrls {
	signedUrls: ReadonlyMap<string, string>;
	timedOut: boolean;
}

interface ThemeCssApplier {
	pending: ThemeCssRequest | null;
	running: boolean;
	disposed: boolean;
	objectUrls: Array<string>;
	sheet: GatedThemeSheet | null;
	resignDue: boolean;
	resignAtMs: number | null;
	resignTimer: ReturnType<typeof setTimeout> | null;
}

function revokeObjectUrls(objectUrls: ReadonlyArray<string>): void {
	for (const objectUrl of objectUrls) {
		URL.revokeObjectURL(objectUrl);
	}
}

function writeThemeStyle(applier: ThemeCssApplier, css: string, objectUrls: Array<string>): void {
	const existing = document.getElementById(CUSTOM_THEME_STYLE_ELEMENT_ID);
	if (!css) {
		existing?.remove();
	} else if (existing?.textContent !== css) {
		const styleElement = existing ?? document.createElement('style');
		styleElement.id = CUSTOM_THEME_STYLE_ELEMENT_ID;
		styleElement.textContent = css;
		if (!existing) document.head.appendChild(styleElement);
	}
	if (objectUrls !== applier.objectUrls) {
		revokeObjectUrls(applier.objectUrls);
		applier.objectUrls = objectUrls;
	}
	clearRemScaleCache();
}

function clearResignSchedule(applier: ThemeCssApplier): void {
	if (applier.resignTimer !== null) clearTimeout(applier.resignTimer);
	applier.resignTimer = null;
	applier.resignAtMs = null;
	applier.resignDue = false;
}

function clearThemeStyle(applier: ThemeCssApplier): void {
	clearResignSchedule(applier);
	applier.sheet = null;
	writeThemeStyle(applier, '', []);
}

function requestResign(applier: ThemeCssApplier): void {
	if (applier.disposed || applier.sheet === null) return;
	applier.resignDue = true;
	if (!applier.running) void drainThemeCssRequests(applier);
}

function scheduleResign(applier: ThemeCssApplier, urls: ReadonlyArray<string>, retryMs: number): void {
	clearResignSchedule(applier);
	if (urls.length === 0) return;
	const nowMs = Date.now();
	let resignAtMs = nowMs + MAX_TIMER_DELAY_MS;
	for (const url of urls) {
		const signature = readAttachmentUrlSignature(url);
		const dueAtMs = signature ? signature.expiresAtMs - ATTACHMENT_URL_REFRESH_MARGIN_MS : nowMs;
		resignAtMs = Math.min(resignAtMs, dueAtMs > nowMs ? dueAtMs : nowMs + retryMs);
	}
	applier.resignAtMs = resignAtMs;
	applier.resignTimer = setTimeout(() => requestResign(applier), resignAtMs - nowMs);
}

function resignIfOverdue(applier: ThemeCssApplier): void {
	if (applier.resignAtMs !== null && Date.now() >= applier.resignAtMs) requestResign(applier);
}

function waitAtMost<T>(work: Promise<T>, fallback: T): Promise<T> {
	return new Promise((resolve) => {
		const timer = setTimeout(() => resolve(fallback), SHEET_PREPARE_WAIT_MS);
		void work.then((value) => {
			clearTimeout(timer);
			resolve(value);
		});
	});
}

async function signKeptUrls(
	urls: ReadonlyArray<string>,
	lastSignedUrls: ReadonlyMap<string, string>,
): Promise<SignedKeptUrls> {
	let timedOut = false;
	const signed = await Promise.all(
		urls.map(async (url) => {
			const refreshed = await waitAtMost<string | null>(AttachmentUrlRefresher.refresh(url), null);
			if (refreshed === null) timedOut = true;
			const isNewer = refreshed !== null && refreshed !== url;
			return [url, isNewer ? refreshed : (lastSignedUrls.get(url) ?? url)] as const;
		}),
	);
	return {signedUrls: new Map(signed), timedOut};
}

function preloadImage(url: string): Promise<void> {
	return new Promise((resolve) => {
		const image = new Image();
		image.onload = () => resolve();
		image.onerror = () => resolve();
		image.src = url;
	});
}

function isRequestStale(applier: ThemeCssApplier): boolean {
	return applier.disposed || applier.pending !== null;
}

async function publishSheet(
	applier: ThemeCssApplier,
	sheet: GatedThemeSheet,
	objectUrls: Array<string>,
): Promise<ThemeCssGateResult | null> {
	const {signedUrls, timedOut} = await signKeptUrls(sheet.keptMediaUrls, applier.sheet?.signedUrls ?? new Map());
	if (isRequestStale(applier)) return null;
	const final = signGatedCss(sheet.css, sheet.policy, signedUrls);
	if (final.rejected) return final;
	await waitAtMost(Promise.all(final.keptMediaUrls.map(preloadImage)), []);
	if (isRequestStale(applier)) return null;
	applier.sheet = {...sheet, signedUrls};
	writeThemeStyle(applier, final.css, objectUrls);
	scheduleResign(applier, final.keptMediaUrls, timedOut ? SIGNING_TIMEOUT_RETRY_MS : UNSIGNED_URL_RETRY_MS);
	return final;
}

async function applyThemeCssRequest(applier: ThemeCssApplier, request: ThemeCssRequest): Promise<void> {
	let objectUrls: Array<string> = [];
	try {
		const resolved = await resolveThemeCssReferences(request.css, request.assets, request.localFiles);
		objectUrls = resolved.objectUrls;
		if (isRequestStale(applier)) {
			revokeObjectUrls(objectUrls);
			return;
		}
		const policy = {mediaEndpoint: request.mediaEndpoint, objectUrls: new Set(objectUrls)};
		const gated = gateThemeCss(resolved.css, policy);
		for (const url of gated.blocked) request.onBlockedUrl(url);
		const final = gated.rejected
			? gated
			: await publishSheet(
					applier,
					{css: gated.css, policy, keptMediaUrls: gated.keptMediaUrls, signedUrls: new Map()},
					objectUrls,
				);
		if (final === null) {
			revokeObjectUrls(objectUrls);
			return;
		}
		if (final.rejected) {
			logger.error('Custom theme CSS did not settle after filtering and was not applied');
			revokeObjectUrls(objectUrls);
			if (!isRequestStale(applier)) clearThemeStyle(applier);
		}
		request.onCssRejected(final.rejected);
		request.onMissingReferences([...resolved.missingAssetReferences, ...resolved.missingLocalFileReferences]);
		if (resolved.missingAssetReferences.length > 0 || resolved.missingLocalFileReferences.length > 0) {
			logger.warn('Custom theme CSS has unresolved file references', {
				assets: resolved.missingAssetReferences,
				localFiles: resolved.missingLocalFileReferences,
			});
		}
	} catch (error) {
		logger.error('Failed to prepare custom theme CSS', error);
		revokeObjectUrls(objectUrls);
		if (!isRequestStale(applier)) {
			clearThemeStyle(applier);
			request.onCssRejected(true);
		}
	}
}

async function resignAppliedSheet(applier: ThemeCssApplier): Promise<void> {
	const sheet = applier.sheet;
	if (sheet === null) return;
	try {
		const final = await publishSheet(applier, sheet, applier.objectUrls);
		if (final?.rejected) clearThemeStyle(applier);
	} catch (error) {
		logger.error('Failed to refresh custom theme CSS links', error);
		clearThemeStyle(applier);
	}
}

async function drainThemeCssRequests(applier: ThemeCssApplier): Promise<void> {
	applier.running = true;
	while (!applier.disposed && (applier.pending || applier.resignDue)) {
		const request = applier.pending;
		applier.pending = null;
		if (request === null) {
			applier.resignDue = false;
			await resignAppliedSheet(applier);
			continue;
		}
		if (!request.css) {
			clearThemeStyle(applier);
			request.onCssRejected(false);
			request.onMissingReferences([]);
			continue;
		}
		await applyThemeCssRequest(applier, request);
	}
	applier.running = false;
}

export function useCustomThemeStyle({
	enabledThemeCss,
	customThemeCss,
	themeLibraryAssets,
	themeLibraryLocalFiles,
	themeLibraryRevision,
	mediaEndpoint,
	onMissingReferences,
	onBlockedUrl,
	onCssRejected,
}: CustomThemeStyleOptions): void {
	const applierRef = useRef<ThemeCssApplier>({
		pending: null,
		running: false,
		disposed: false,
		objectUrls: [],
		sheet: null,
		resignDue: false,
		resignAtMs: null,
		resignTimer: null,
	});
	const requestedCssRef = useRef('');
	useEffect(() => {
		const handleViolation = (event: SecurityPolicyViolationEvent): void => {
			const url = event.blockedURI;
			if (url && requestedCssRef.current.includes(url)) onBlockedUrl(url);
		};
		document.addEventListener('securitypolicyviolation', handleViolation);
		return () => document.removeEventListener('securitypolicyviolation', handleViolation);
	}, [onBlockedUrl]);
	useEffect(() => {
		const applier = applierRef.current;
		applier.disposed = false;
		const handleWake = (): void => resignIfOverdue(applier);
		window.addEventListener('focus', handleWake);
		window.addEventListener('online', handleWake);
		return () => {
			window.removeEventListener('focus', handleWake);
			window.removeEventListener('online', handleWake);
			applier.disposed = true;
			applier.pending = null;
			clearResignSchedule(applier);
			applier.sheet = null;
			revokeObjectUrls(applier.objectUrls);
			applier.objectUrls = [];
		};
	}, []);
	useLayoutEffect(() => {
		const applier = applierRef.current;
		const css = [enabledThemeCss, customThemeCss]
			.map((value) => value?.trim() ?? '')
			.filter(Boolean)
			.join('\n\n');
		requestedCssRef.current = css;
		applier.pending = {
			css,
			assets: themeLibraryAssets,
			localFiles: themeLibraryLocalFiles,
			mediaEndpoint,
			onMissingReferences,
			onBlockedUrl,
			onCssRejected,
		};
		if (!css) clearThemeStyle(applier);
		if (!applier.running) void drainThemeCssRequests(applier);
	}, [
		customThemeCss,
		enabledThemeCss,
		themeLibraryAssets,
		themeLibraryLocalFiles,
		themeLibraryRevision,
		mediaEndpoint,
		onMissingReferences,
		onBlockedUrl,
		onCssRejected,
	]);
}
