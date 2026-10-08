// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ThemeLibraryAsset, ThemeLibraryLocalFileReference} from '@app/features/theme/state/ThemeLibrary';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';

export interface ThemeMetadata {
	name: string;
	description: string;
	author: string;
	version: string;
	tags: Array<string>;
}

export interface ThemeCssResolution {
	css: string;
	objectUrls: Array<string>;
	missingAssetReferences: Array<string>;
	missingLocalFileReferences: Array<string>;
}

export interface ThemeExportPayload {
	version: 1;
	exportedAt: string;
	themes: Array<{
		id: string;
		name: string;
		description: string;
		author: string;
		version: string;
		tags: Array<string>;
		css: string;
		fileName: string;
		createdAt: number;
		updatedAt: number;
	}>;
	assets: Array<{
		id: string;
		name: string;
		mimeType: string;
		size: number;
		dataUrl?: string;
		desktopPath?: string;
		createdAt: number;
		updatedAt: number;
	}>;
}

const THEME_ASSET_REFERENCE_PREFIX = 'fluxer-theme-asset';
const THEME_LOCAL_FILE_REFERENCE_PREFIX = 'fluxer-local-file';
const HEADER_COMMENT_PATTERN = /^\s*\/\*([\s\S]*?)\*\//;
const METADATA_LINE_PATTERN =
	/^\s*(?:\*\s*)?(?:@?([a-zA-Z][a-zA-Z0-9_-]*)|([a-zA-Z][a-zA-Z0-9 _-]*))\s*[:=]\s*(.*?)\s*$/;
const AT_RULE_METADATA_PATTERN = /^\s*(?:\*\s*)?@([a-zA-Z][a-zA-Z0-9_-]*)\s+(.*?)\s*$/;
const ASSET_FUNCTION_PATTERN = /fluxer-theme-asset\(\s*(['"])(.*?)\1\s*\)/g;
const ASSET_URL_PATTERN = /url\(\s*(['"]?)fluxer-theme-asset:\/\/([^'")\s]+)\1\s*\)/g;
const LOCAL_FILE_FUNCTION_PATTERN = /fluxer-local-file\(\s*(['"])(.*?)\1\s*\)/g;
const LOCAL_FILE_URL_PATTERN = /url\(\s*(['"]?)fluxer-local-file:\/\/([^'")]+)\1\s*\)/g;

function normalizeMetadataKey(key: string): keyof ThemeMetadata | null {
	const normalized = key
		.trim()
		.toLowerCase()
		.replace(/[\s_-]+/g, '');
	switch (normalized) {
		case 'name':
		case 'title':
			return 'name';
		case 'description':
		case 'desc':
		case 'summary':
			return 'description';
		case 'author':
		case 'authors':
			return 'author';
		case 'version':
		case 'ver':
			return 'version';
		case 'tags':
		case 'tag':
			return 'tags';
		default:
			return null;
	}
}

function parseTags(value: string): Array<string> {
	return value
		.split(',')
		.map((tag) => tag.trim())
		.filter((tag) => tag.length > 0);
}

function stripCommentDecoration(line: string): string {
	return line.replace(/^\s*\*\s?/, '').trim();
}

export function parseThemeMetadata(css: string, fallbackName: string): ThemeMetadata {
	const metadata: ThemeMetadata = {
		name: fallbackName,
		description: '',
		author: '',
		version: '',
		tags: [],
	};
	const headerMatch = HEADER_COMMENT_PATTERN.exec(css);
	if (!headerMatch) {
		return metadata;
	}
	const header = headerMatch[1] ?? '';
	for (const rawLine of header.split(/\r?\n/)) {
		const line = stripCommentDecoration(rawLine);
		if (!line) continue;
		const keyValueMatch = METADATA_LINE_PATTERN.exec(line);
		const atRuleMatch = keyValueMatch ? null : AT_RULE_METADATA_PATTERN.exec(line);
		const rawKey = keyValueMatch?.[1] ?? keyValueMatch?.[2] ?? atRuleMatch?.[1];
		const rawValue = keyValueMatch?.[3] ?? atRuleMatch?.[2];
		if (!rawKey || rawValue === undefined) continue;
		const key = normalizeMetadataKey(rawKey);
		if (!key) continue;
		if (key === 'tags') {
			metadata.tags = parseTags(rawValue);
		} else {
			metadata[key] = rawValue.trim();
		}
	}
	return metadata;
}

function buildThemeCssHeader(metadata: ThemeMetadata): string {
	const lines = ['/**', ` * @name ${metadata.name || 'Untitled theme'}`];
	if (metadata.description.trim()) {
		lines.push(` * @description ${metadata.description.trim()}`);
	}
	if (metadata.author.trim()) {
		lines.push(` * @author ${metadata.author.trim()}`);
	}
	if (metadata.version.trim()) {
		lines.push(` * @version ${metadata.version.trim()}`);
	}
	if (metadata.tags.length > 0) {
		lines.push(` * @tags ${metadata.tags.join(', ')}`);
	}
	lines.push(' */');
	return `${lines.join('\n')}\n\n`;
}

export function upsertThemeCssHeader(css: string, metadata: ThemeMetadata): string {
	const header = buildThemeCssHeader(metadata);
	return HEADER_COMMENT_PATTERN.test(css) ? css.replace(HEADER_COMMENT_PATTERN, header.trimEnd()) : `${header}${css}`;
}

export function createThemeAssetReference(nameOrId: string): string {
	return `${THEME_ASSET_REFERENCE_PREFIX}("${nameOrId.replace(/"/g, '\\"')}")`;
}

export function createThemeLocalFileReference(path: string): string {
	return `${THEME_LOCAL_FILE_REFERENCE_PREFIX}("${path.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}")`;
}

function cssUrl(value: string): string {
	return `url("${value.replace(/"/g, '\\"')}")`;
}

function unescapeCssString(value: string): string {
	return value.replace(/\\(["'\\])/g, '$1');
}

function decodeReferenceValue(value: string): string {
	const unescaped = unescapeCssString(value);
	try {
		return decodeURIComponent(unescaped);
	} catch {
		return unescaped;
	}
}

function blobToDataUrl(blob: Blob): Promise<string> {
	return new Promise((resolve, reject) => {
		const reader = new FileReader();
		reader.onerror = () => reject(reader.error ?? new Error('Failed to read theme asset blob'));
		reader.onload = () => resolve(String(reader.result ?? ''));
		reader.readAsDataURL(blob);
	});
}

function getAssetReferenceMap(assets: ReadonlyArray<ThemeLibraryAsset>): Map<string, ThemeLibraryAsset> {
	const map = new Map<string, ThemeLibraryAsset>();
	for (const asset of assets) {
		map.set(asset.id, asset);
		map.set(asset.name, asset);
		map.set(asset.name.toLowerCase(), asset);
	}
	return map;
}

function getLocalFileReferenceMap(localFiles: ReadonlyArray<ThemeLibraryLocalFileReference>): Map<string, string> {
	const map = new Map<string, string>();
	for (const file of localFiles) {
		map.set(file.path, file.path);
		map.set(file.id, file.path);
		map.set(file.name, file.path);
		map.set(file.name.toLowerCase(), file.path);
	}
	return map;
}

async function resolveAssetUrls(
	css: string,
	assets: ReadonlyArray<ThemeLibraryAsset>,
): Promise<{
	css: string;
	objectUrls: Array<string>;
	missing: Array<string>;
}> {
	const assetMap = getAssetReferenceMap(assets);
	const objectUrls: Array<string> = [];
	const urlCache = new Map<string, string>();
	const missing = new Set<string>();
	const resolveAsset = (rawReference: string): string => {
		const reference = decodeReferenceValue(rawReference);
		const asset = assetMap.get(reference) ?? assetMap.get(reference.toLowerCase());
		if (!asset?.data) {
			missing.add(reference);
			return cssUrl(`about:blank#missing-theme-asset-${encodeURIComponent(reference)}`);
		}
		const cached = urlCache.get(asset.id);
		if (cached) {
			return cssUrl(cached);
		}
		const objectUrl = URL.createObjectURL(asset.data);
		urlCache.set(asset.id, objectUrl);
		objectUrls.push(objectUrl);
		return cssUrl(objectUrl);
	};
	const nextCss = css
		.replace(ASSET_FUNCTION_PATTERN, (_match, _quote: string, reference: string) => resolveAsset(reference))
		.replace(ASSET_URL_PATTERN, (_match, _quote: string, reference: string) => resolveAsset(reference));
	return {css: nextCss, objectUrls, missing: [...missing]};
}

async function resolveLocalFileUrls(
	css: string,
	localFiles: ReadonlyArray<ThemeLibraryLocalFileReference>,
): Promise<{
	css: string;
	missing: Array<string>;
}> {
	const referenceMap = getLocalFileReferenceMap(localFiles);
	const requestedReferences = new Set<string>();
	const missing = new Set<string>();
	const collectReference = (_match: string, _quote: string, rawReference: string): string => {
		const reference = decodeReferenceValue(rawReference);
		const path = referenceMap.get(reference) ?? referenceMap.get(reference.toLowerCase());
		if (path) {
			requestedReferences.add(path);
		} else {
			missing.add(reference);
		}
		return _match;
	};
	css.replace(LOCAL_FILE_FUNCTION_PATTERN, collectReference).replace(LOCAL_FILE_URL_PATTERN, collectReference);
	const electronApi = getElectronAPI();
	const dataByPath = new Map<string, string>();
	if (electronApi?.readThemeLocalFiles && requestedReferences.size > 0) {
		const files = await electronApi.readThemeLocalFiles([...requestedReferences]);
		for (const file of files) {
			if (file.dataUrl) {
				dataByPath.set(file.path, file.dataUrl);
			} else {
				missing.add(file.path);
			}
		}
	} else {
		for (const path of requestedReferences) {
			missing.add(path);
		}
	}
	const resolveLocalFile = (rawReference: string): string => {
		const reference = decodeReferenceValue(rawReference);
		const path = referenceMap.get(reference) ?? referenceMap.get(reference.toLowerCase());
		if (!path) {
			return cssUrl(`about:blank#missing-theme-local-file-${encodeURIComponent(reference)}`);
		}
		const dataUrl = dataByPath.get(path);
		if (!dataUrl) {
			return cssUrl(`about:blank#missing-theme-local-file-${encodeURIComponent(path)}`);
		}
		return cssUrl(dataUrl);
	};
	const nextCss = css
		.replace(LOCAL_FILE_FUNCTION_PATTERN, (_match, _quote: string, reference: string) => resolveLocalFile(reference))
		.replace(LOCAL_FILE_URL_PATTERN, (_match, _quote: string, reference: string) => resolveLocalFile(reference));
	return {css: nextCss, missing: [...missing]};
}

export async function resolveThemeCssReferences(
	css: string,
	assets: ReadonlyArray<ThemeLibraryAsset>,
	localFiles: ReadonlyArray<ThemeLibraryLocalFileReference>,
): Promise<ThemeCssResolution> {
	const assetResolution = await resolveAssetUrls(css, assets);
	const localFileResolution = await resolveLocalFileUrls(assetResolution.css, localFiles);
	return {
		css: localFileResolution.css,
		objectUrls: assetResolution.objectUrls,
		missingAssetReferences: assetResolution.missing,
		missingLocalFileReferences: localFileResolution.missing,
	};
}

export async function assetToExportDataUrl(asset: ThemeLibraryAsset): Promise<string | undefined> {
	if (!asset.data) {
		return undefined;
	}
	return blobToDataUrl(asset.data);
}
