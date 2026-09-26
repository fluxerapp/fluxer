// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import type {Dirent} from 'node:fs';
import fs from 'node:fs/promises';
import {createRequire} from 'node:module';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
import {createChildLogger} from '@electron/common/Logger';
import type {
	ClipboardWriteFileMediaType,
	ClipboardWriteFileOptions,
	ClipboardWriteFileResult,
} from '@electron/common/Types';
import {downloadFile} from '@electron/main/FileDownloads';
import {app, ClipboardItem, clipboard} from 'electron';

const requireModule = createRequire(import.meta.url);

type MacClipboardModule = {
	writeFileReferenceToClipboard: ((filePath: string) => Promise<void>) | null;
	loadError: Error | null;
};
type WinClipboardModule = {
	writeFileReferenceToClipboard: ((filePath: string) => Promise<void>) | null;
	loadError: Error | null;
};

let macClipboardModule: MacClipboardModule | null | undefined;
let winClipboardModule: WinClipboardModule | null | undefined;

function loadMacClipboardModule(): MacClipboardModule | null {
	if (process.platform !== 'darwin') {
		throw new Error(`@fluxer/mac-clipboard is only loadable on darwin, got ${process.platform}`);
	}
	let mod: MacClipboardModule;
	try {
		mod = requireModule('@fluxer/mac-clipboard') as MacClipboardModule;
	} catch (error) {
		logger.warn('@fluxer/mac-clipboard failed to load; file clipboard writes will use the text fallback', {error});
		return null;
	}
	if (typeof mod.writeFileReferenceToClipboard !== 'function') {
		logger.warn('@fluxer/mac-clipboard unavailable; file clipboard writes will use the text fallback', {
			loadError: mod.loadError,
		});
		return null;
	}
	return mod;
}

function loadWinClipboardModule(): WinClipboardModule {
	if (process.platform !== 'win32') {
		throw new Error(`@fluxer/win-clipboard is only loadable on win32, got ${process.platform}`);
	}
	let mod: WinClipboardModule;
	try {
		mod = requireModule('@fluxer/win-clipboard') as WinClipboardModule;
	} catch (error) {
		throw new Error(
			`@fluxer/win-clipboard failed to load on Windows — this is a packaging bug, not a runtime fallback case. ` +
				`Original error: ${error instanceof Error ? error.message : String(error)}`,
		);
	}
	if (!mod.writeFileReferenceToClipboard) {
		throw new Error(
			`@fluxer/win-clipboard loaded but exports no writeFileReferenceToClipboard. ` +
				`Underlying loadError: ${mod.loadError ? mod.loadError.message : '<none>'}`,
		);
	}
	return mod;
}

const logger = createChildLogger('MediaClipboard');
const CACHE_DIR_NAME = 'clipboard-media';
const WINDOWS_FILE_NAME_W_FORMAT = 'electron application/osclipboard;format="FileNameW"';
const GNOME_COPIED_FILES_FORMAT = 'electron application/osclipboard;format="x-special/gnome-copied-files"';
const MAX_CACHE_AGE_MS = 24 * 60 * 60 * 1000;
const MAX_CACHE_ITEMS = 25;
const CACHE_SLOW_TRIM_INTERVAL_MS = 30 * 60 * 1000;
const CACHE_BLUR_TRIM_DELAY_MS = 10 * 1000;

let cacheTrimTimer: NodeJS.Timeout | null = null;
let cacheTrimInFlight: Promise<void> | null = null;

function getCacheDir(): string {
	return path.join(app.getPath('userData'), CACHE_DIR_NAME);
}

function defaultExtensionFor(mediaType: ClipboardWriteFileMediaType): string {
	switch (mediaType) {
		case 'gif':
			return '.gif';
		case 'image':
			return '.png';
		case 'video':
			return '.mp4';
		case 'audio':
			return '.mp3';
	}
}

function sanitizeFilename(rawName: string | undefined, mediaType: ClipboardWriteFileMediaType): string {
	const fallback = `media${defaultExtensionFor(mediaType)}`;
	const rawBaseName = rawName?.trim().split(/[\\/]/).filter(Boolean).pop() ?? fallback;
	// biome-ignore lint/suspicious/noControlCharactersInRegex: stripping control chars is the whole point of this sanitizer.
	const withoutControlChars = rawBaseName.replace(/[\u0000-\u001f<>:"/\\|?*]+/g, '_').trim();
	let filename = withoutControlChars || fallback;
	if (/^(con|prn|aux|nul|com[1-9]|lpt[1-9])(?:\.|$)/i.test(filename)) {
		filename = `_${filename}`;
	}
	const extension = path.extname(filename);
	if (!extension) {
		filename = `${filename}${defaultExtensionFor(mediaType)}`;
	}
	if (filename.length > 160) {
		const ext = path.extname(filename);
		const base = filename.slice(0, Math.max(1, 160 - ext.length));
		filename = `${base}${ext}`;
	}
	return filename;
}

function isClipboardWriteFileMediaType(value: unknown): value is ClipboardWriteFileMediaType {
	return value === 'image' || value === 'gif' || value === 'video' || value === 'audio';
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object' && !Array.isArray(value);
}

export function parseClipboardWriteFileOptions(value: unknown): ClipboardWriteFileOptions {
	if (!isRecord(value)) {
		throw new Error('Invalid clipboard file payload');
	}
	const {url, suggestedName, mediaType} = value;
	if (typeof url !== 'string' || !url) {
		throw new Error('Clipboard file URL is required');
	}
	if (suggestedName !== undefined && typeof suggestedName !== 'string') {
		throw new Error('Clipboard file suggested name must be a string');
	}
	if (!isClipboardWriteFileMediaType(mediaType)) {
		throw new Error('Clipboard file media type is invalid');
	}
	return {url, suggestedName, mediaType};
}

async function writeMacFileReference(filePath: string): Promise<void> {
	if (macClipboardModule === undefined) {
		macClipboardModule = loadMacClipboardModule();
	}
	if (macClipboardModule?.writeFileReferenceToClipboard) {
		await macClipboardModule.writeFileReferenceToClipboard(filePath);
		return;
	}
	const fileUrl = pathToFileURL(filePath).toString();
	await clipboard.writeText(fileUrl);
}

async function writeWindowsFileReference(filePath: string): Promise<void> {
	if (winClipboardModule === undefined) {
		winClipboardModule = loadWinClipboardModule();
	}
	if (winClipboardModule?.writeFileReferenceToClipboard) {
		try {
			await winClipboardModule.writeFileReferenceToClipboard(filePath);
			return;
		} catch (error) {
			logger.warn('Native win-clipboard write failed; falling back to FileNameW buffer', {error});
		}
	}
	await clipboard.write([
		new ClipboardItem({
			[WINDOWS_FILE_NAME_W_FORMAT]: new Blob([Buffer.concat([Buffer.from(filePath, 'ucs2'), Buffer.from([0, 0])])]),
		}),
	]);
}

async function writeLinuxFileReference(filePath: string): Promise<void> {
	const fileUrl = pathToFileURL(filePath).toString();
	const desktop = `${process.env.XDG_CURRENT_DESKTOP ?? ''} ${process.env.DESKTOP_SESSION ?? ''}`.toLowerCase();
	const isGnomeLike =
		desktop.includes('gnome') || desktop.includes('unity') || desktop.includes('cinnamon') || desktop.includes('xfce');
	if (isGnomeLike) {
		await clipboard.write([new ClipboardItem({[GNOME_COPIED_FILES_FORMAT]: `copy\n${fileUrl}\n`})]);
		return;
	}
	await clipboard.write([new ClipboardItem({'text/uri-list': `${fileUrl}\n`})]);
}

async function writeFileReferenceToClipboard(filePath: string): Promise<void> {
	switch (process.platform) {
		case 'darwin':
			await writeMacFileReference(filePath);
			return;
		case 'win32':
			await writeWindowsFileReference(filePath);
			return;
		case 'linux':
			await writeLinuxFileReference(filePath);
			return;
		default:
			throw new Error(`Unsupported clipboard file platform: ${process.platform}`);
	}
}

async function trimClipboardMediaCache(): Promise<void> {
	const cacheDir = getCacheDir();
	let entries: Array<Dirent>;
	try {
		entries = await fs.readdir(cacheDir, {withFileTypes: true});
	} catch {
		return;
	}
	const now = Date.now();
	const items = await Promise.all(
		entries
			.filter((entry) => entry.isDirectory())
			.map(async (entry) => {
				const itemPath = path.join(cacheDir, entry.name);
				try {
					const stat = await fs.stat(itemPath);
					return {path: itemPath, mtimeMs: stat.mtimeMs};
				} catch {
					return {path: itemPath, mtimeMs: 0};
				}
			}),
	);
	const sorted = items.sort((a, b) => b.mtimeMs - a.mtimeMs);
	const expired = sorted.filter((item, index) => now - item.mtimeMs > MAX_CACHE_AGE_MS || index >= MAX_CACHE_ITEMS);
	await Promise.all(expired.map((item) => fs.rm(item.path, {recursive: true, force: true})));
}

function trimClipboardMediaCacheSoon(delayMs: number): void {
	if (cacheTrimTimer !== null) {
		clearTimeout(cacheTrimTimer);
	}
	cacheTrimTimer = setTimeout(() => {
		cacheTrimTimer = null;
		if (cacheTrimInFlight !== null) return;
		cacheTrimInFlight = trimClipboardMediaCache()
			.catch((error) => {
				logger.warn('Failed to trim clipboard media cache', {error});
			})
			.finally(() => {
				cacheTrimInFlight = null;
			});
	}, delayMs);
	cacheTrimTimer.unref?.();
}

let clipboardMediaCacheEvictionInitialized = false;

function initializeClipboardMediaCacheEviction(): void {
	if (clipboardMediaCacheEvictionInitialized) {
		return;
	}
	clipboardMediaCacheEvictionInitialized = true;
	const startSlowTrim = (): void => {
		const timer = setInterval(() => {
			trimClipboardMediaCacheSoon(0);
		}, CACHE_SLOW_TRIM_INTERVAL_MS);
		timer.unref?.();
		trimClipboardMediaCacheSoon(0);
	};
	if (app.isReady()) {
		startSlowTrim();
	} else {
		app.once('ready', startSlowTrim);
	}
	app.on('browser-window-blur', () => {
		trimClipboardMediaCacheSoon(CACHE_BLUR_TRIM_DELAY_MS);
	});
}

initializeClipboardMediaCacheEviction();

export async function copyRemoteFileToClipboard(options: ClipboardWriteFileOptions): Promise<ClipboardWriteFileResult> {
	const cacheDir = getCacheDir();
	const itemDir = path.join(cacheDir, `${Date.now()}-${crypto.randomUUID()}`);
	const filename = sanitizeFilename(options.suggestedName, options.mediaType);
	const filePath = path.join(itemDir, filename);
	try {
		await fs.mkdir(itemDir, {recursive: true});
		await downloadFile(options.url, filePath);
		await writeFileReferenceToClipboard(filePath);
		trimClipboardMediaCacheSoon(0);
		return {success: true, path: filePath};
	} catch (error) {
		await fs.rm(itemDir, {recursive: true, force: true}).catch(() => undefined);
		logger.warn('Failed to copy remote file to clipboard', {error});
		return {success: false, error: 'Failed to copy file to clipboard'};
	}
}
