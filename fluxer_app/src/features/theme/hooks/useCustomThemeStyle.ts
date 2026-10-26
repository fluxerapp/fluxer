// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {clearRemScaleCache} from '@app/features/theme/layout/RemFromPx';
import type {ThemeLibraryAsset, ThemeLibraryLocalFileReference} from '@app/features/theme/state/ThemeLibrary';
import {resolveThemeCssReferences} from '@app/features/theme/utils/ThemeCssUtils';
import {useEffect, useRef} from 'react';

export const CUSTOM_THEME_STYLE_ELEMENT_ID = 'fluxer-custom-theme-style';
export const INSTANCE_CUSTOM_CSS_ELEMENT_ID = 'fluxer-instance-custom-css';
const logger = new Logger('useCustomThemeStyle');
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
	onMissingReferences: (references: ReadonlyArray<string>) => void;
	onBlockedUrl: (url: string) => void;
}

export function useInstanceCustomCss(instanceCss: string | null): void {
	useEffect(() => {
		const css = instanceCss?.trim() ?? '';
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
	}, [instanceCss]);
}

interface ThemeCssRequest {
	css: string;
	assets: ReadonlyArray<ThemeLibraryAsset>;
	localFiles: ReadonlyArray<ThemeLibraryLocalFileReference>;
	onMissingReferences: (references: ReadonlyArray<string>) => void;
}

interface ThemeCssApplier {
	pending: ThemeCssRequest | null;
	running: boolean;
	disposed: boolean;
	objectUrls: Array<string>;
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
	} else {
		const styleElement = existing ?? document.createElement('style');
		styleElement.id = CUSTOM_THEME_STYLE_ELEMENT_ID;
		styleElement.textContent = css;
		if (!existing) document.head.appendChild(styleElement);
	}
	revokeObjectUrls(applier.objectUrls);
	applier.objectUrls = objectUrls;
	clearRemScaleCache();
}

async function drainThemeCssRequests(applier: ThemeCssApplier): Promise<void> {
	applier.running = true;
	while (applier.pending && !applier.disposed) {
		const request = applier.pending;
		applier.pending = null;
		if (!request.css) {
			writeThemeStyle(applier, '', []);
			request.onMissingReferences([]);
			continue;
		}
		try {
			const resolved = await resolveThemeCssReferences(request.css, request.assets, request.localFiles);
			if (applier.disposed || applier.pending) {
				revokeObjectUrls(resolved.objectUrls);
				continue;
			}
			writeThemeStyle(applier, resolved.css, resolved.objectUrls);
			request.onMissingReferences([...resolved.missingAssetReferences, ...resolved.missingLocalFileReferences]);
			if (resolved.missingAssetReferences.length > 0 || resolved.missingLocalFileReferences.length > 0) {
				logger.warn('Custom theme CSS has unresolved file references', {
					assets: resolved.missingAssetReferences,
					localFiles: resolved.missingLocalFileReferences,
				});
			}
		} catch (error) {
			logger.error('Failed to resolve custom theme CSS references', error);
			if (!applier.disposed && !applier.pending) writeThemeStyle(applier, request.css, []);
		}
	}
	applier.running = false;
}

export function useCustomThemeStyle({
	enabledThemeCss,
	customThemeCss,
	themeLibraryAssets,
	themeLibraryLocalFiles,
	themeLibraryRevision,
	onMissingReferences,
	onBlockedUrl,
}: CustomThemeStyleOptions): void {
	const applierRef = useRef<ThemeCssApplier>({pending: null, running: false, disposed: false, objectUrls: []});
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
		return () => {
			applier.disposed = true;
			applier.pending = null;
			revokeObjectUrls(applier.objectUrls);
			applier.objectUrls = [];
		};
	}, []);
	useEffect(() => {
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
			onMissingReferences,
		};
		if (!applier.running) void drainThemeCssRequests(applier);
	}, [
		customThemeCss,
		enabledThemeCss,
		themeLibraryAssets,
		themeLibraryLocalFiles,
		themeLibraryRevision,
		onMissingReferences,
	]);
}
