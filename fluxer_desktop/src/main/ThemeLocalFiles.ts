// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {
	addAllowedThemeLocalFiles,
	clearAllowedThemeLocalFiles,
	getAllowedThemeLocalFiles,
} from '@electron/common/DesktopConfig';
import {createChildLogger} from '@electron/common/Logger';
import type {
	ThemeDirectoryCssFile,
	ThemeLinkedFileChange,
	ThemeLocalFileReadResult,
	ThemeLocalFileReference,
} from '@electron/common/Types';
import {t} from '@electron/main/MainI18n';
import {requirePrivilegedRendererDocumentSender} from '@electron/main/PrivilegedRendererDocuments';
import {createThemeLinkedFileWatchSet, readThemeCssFile} from '@electron/main/ThemeLinkedFileWatch';
import {BrowserWindow, dialog, ipcMain, type WebContents} from 'electron';

const logger = createChildLogger('ThemeLocalFiles');
const THEME_LOCAL_FILE_MAX_BYTES = 50 * 1024 * 1024;
const THEME_DIRECTORY_MAX_CSS_FILES = 200;
const THEME_LINKED_FILES_MAX_WATCHED = 500;

interface ThemeLinkedFileWatchSubscription {
	watchSet: ReturnType<typeof createThemeLinkedFileWatchSet>;
	emit: (change: ThemeLinkedFileChange) => void;
	dispose: () => void;
}

const linkedFileWatchSets = new Map<number, ThemeLinkedFileWatchSubscription>();

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object';
}

function getLinkedFileWatchSubscription(sender: WebContents): ThemeLinkedFileWatchSubscription {
	const existing = linkedFileWatchSets.get(sender.id);
	if (existing) return existing;
	const senderId = sender.id;
	const emit = (change: ThemeLinkedFileChange): void => {
		if (!sender.isDestroyed()) {
			sender.send('theme-linked-file-changed', change);
		}
	};
	const watchSet = createThemeLinkedFileWatchSet(emit);
	let disposed = false;
	const onNavigation = (_event: Electron.Event, _url: string, isSameDoc: boolean, isMainFrame: boolean): void => {
		if (isMainFrame && !isSameDoc) dispose();
	};
	const dispose = (): void => {
		if (disposed) return;
		disposed = true;
		watchSet.dispose();
		linkedFileWatchSets.delete(senderId);
		sender.removeListener('destroyed', dispose);
		sender.removeListener('did-start-navigation', onNavigation);
	};
	sender.once('destroyed', dispose);
	sender.on('did-start-navigation', onNavigation);
	const subscription = {watchSet, emit, dispose};
	linkedFileWatchSets.set(senderId, subscription);
	return subscription;
}

function getMimeType(filePath: string): string {
	switch (path.extname(filePath).toLowerCase()) {
		case '.css':
			return 'text/css';
		case '.woff':
			return 'font/woff';
		case '.woff2':
			return 'font/woff2';
		case '.ttf':
			return 'font/ttf';
		case '.otf':
			return 'font/otf';
		case '.png':
			return 'image/png';
		case '.jpg':
		case '.jpeg':
			return 'image/jpeg';
		case '.gif':
			return 'image/gif';
		case '.webp':
			return 'image/webp';
		case '.avif':
			return 'image/avif';
		case '.svg':
			return 'image/svg+xml';
		case '.mp4':
			return 'video/mp4';
		case '.webm':
			return 'video/webm';
		case '.mp3':
			return 'audio/mpeg';
		case '.ogg':
			return 'audio/ogg';
		default:
			return 'application/octet-stream';
	}
}

function createThemeLocalFileReference(filePath: string, size: number): ThemeLocalFileReference {
	return {
		id: Buffer.from(filePath).toString('hex'),
		name: path.basename(filePath),
		path: filePath,
		mimeType: getMimeType(filePath),
		size,
	};
}

async function collectCssFilesFromDirectory(directoryPath: string): Promise<Array<string>> {
	const files: Array<string> = [];
	const visit = async (currentPath: string): Promise<void> => {
		if (files.length >= THEME_DIRECTORY_MAX_CSS_FILES) return;
		const entries = await fs.promises.readdir(currentPath, {withFileTypes: true});
		for (const entry of entries) {
			if (files.length >= THEME_DIRECTORY_MAX_CSS_FILES) return;
			const entryPath = path.join(currentPath, entry.name);
			if (entry.isDirectory()) {
				await visit(entryPath);
			} else if (entry.isFile() && entry.name.toLowerCase().endsWith('.css')) {
				files.push(entryPath);
			}
		}
	};
	await visit(directoryPath);
	return files;
}

function showThemeOpenDialog(
	parent: BrowserWindow | null,
	options: Electron.OpenDialogOptions,
): Promise<Electron.OpenDialogReturnValue> {
	if (parent) {
		return dialog.showOpenDialog(parent, options);
	}
	return dialog.showOpenDialog(options);
}

export function registerThemeLocalFileHandlers(getMainWindow: () => BrowserWindow | null): void {
	ipcMain.handle('theme-local-files-pick', async (event): Promise<Array<ThemeLocalFileReference>> => {
		const parent = BrowserWindow.fromWebContents(event.sender) ?? getMainWindow();
		const result = await showThemeOpenDialog(parent, {
			title: t('desktop.themes.addLocalFiles'),
			properties: ['openFile', 'multiSelections'],
		});
		if (result.canceled) {
			return [];
		}
		const references: Array<ThemeLocalFileReference> = [];
		for (const filePath of result.filePaths) {
			try {
				const stats = await fs.promises.stat(filePath);
				if (!stats.isFile() || stats.size > THEME_LOCAL_FILE_MAX_BYTES) continue;
				references.push(createThemeLocalFileReference(filePath, stats.size));
			} catch (error) {
				logger.warn('Failed to inspect selected theme file', {filePath, error});
			}
		}
		addAllowedThemeLocalFiles(references.map((reference) => reference.path));
		return references;
	});
	ipcMain.handle(
		'theme-local-files-read',
		async (_event, payload: unknown): Promise<Array<ThemeLocalFileReadResult>> => {
			if (!Array.isArray(payload)) {
				return [];
			}
			const allowed = new Set(getAllowedThemeLocalFiles().map((entry) => path.resolve(entry)));
			const paths = payload.filter((item): item is string => typeof item === 'string');
			const results: Array<ThemeLocalFileReadResult> = [];
			for (const filePath of paths) {
				if (!allowed.has(path.resolve(filePath))) {
					results.push({path: filePath, error: 'not_allowed'});
					continue;
				}
				try {
					const stats = await fs.promises.stat(filePath);
					if (!stats.isFile()) {
						results.push({path: filePath, error: 'not_file'});
						continue;
					}
					if (stats.size > THEME_LOCAL_FILE_MAX_BYTES) {
						results.push({path: filePath, error: 'too_large'});
						continue;
					}
					const data = await fs.promises.readFile(filePath);
					results.push({
						path: filePath,
						dataUrl: `data:${getMimeType(filePath)};base64,${data.toString('base64')}`,
					});
				} catch (error) {
					logger.warn('Failed to read theme local file', {filePath, error});
					results.push({path: filePath, error: 'read_failed'});
				}
			}
			return results;
		},
	);
	ipcMain.handle('theme-local-files-clear', (): void => {
		clearAllowedThemeLocalFiles();
	});
	ipcMain.handle('theme-directory-import', async (event): Promise<Array<ThemeDirectoryCssFile>> => {
		const parent = BrowserWindow.fromWebContents(event.sender) ?? getMainWindow();
		const result = await showThemeOpenDialog(parent, {
			title: t('desktop.themes.importFolder'),
			properties: ['openDirectory'],
		});
		if (result.canceled || result.filePaths.length === 0) {
			return [];
		}
		let directoryPath: string;
		try {
			directoryPath = await fs.promises.realpath(result.filePaths[0]);
		} catch (error) {
			logger.warn('Failed to resolve selected theme directory', {directoryPath: result.filePaths[0], error});
			return [];
		}
		const cssFiles = await collectCssFilesFromDirectory(directoryPath);
		const themes: Array<ThemeDirectoryCssFile> = [];
		for (const filePath of cssFiles) {
			const result = await readThemeCssFile(filePath);
			if (result.css === undefined) {
				logger.warn('Failed to import CSS file from theme directory', {filePath, error: result.error});
				continue;
			}
			themes.push({
				fileName: path.relative(directoryPath, filePath) || path.basename(filePath),
				path: filePath,
				css: result.css,
			});
		}
		addAllowedThemeLocalFiles(themes.map((theme) => theme.path));
		return themes;
	});
	ipcMain.handle('theme-linked-files-pick', async (event, options: unknown): Promise<Array<ThemeDirectoryCssFile>> => {
		requirePrivilegedRendererDocumentSender(event, 'theme-linked-files-pick');
		const multiple = !(isRecord(options) && options.multiple === false);
		const parent = BrowserWindow.fromWebContents(event.sender) ?? getMainWindow();
		const result = await showThemeOpenDialog(parent, {
			title: t(multiple ? 'desktop.themes.importCss' : 'desktop.themes.linkFile'),
			filters: [{name: t('desktop.themes.cssFilesFilter'), extensions: ['css']}],
			properties: multiple ? ['openFile', 'multiSelections'] : ['openFile'],
		});
		if (result.canceled) {
			return [];
		}
		const files: Array<ThemeDirectoryCssFile> = [];
		for (const selectedPath of result.filePaths) {
			let filePath: string;
			try {
				filePath = await fs.promises.realpath(selectedPath);
			} catch (error) {
				logger.warn('Failed to resolve selected theme CSS file', {filePath: selectedPath, error});
				continue;
			}
			const read = await readThemeCssFile(filePath);
			if (read.css === undefined) {
				logger.warn('Failed to read selected theme CSS file', {filePath, error: read.error});
				continue;
			}
			files.push({fileName: path.basename(filePath), path: filePath, css: read.css});
		}
		addAllowedThemeLocalFiles(files.map((file) => file.path));
		return files;
	});
	ipcMain.handle('theme-linked-files-watch', (event, payload: unknown): void => {
		requirePrivilegedRendererDocumentSender(event, 'theme-linked-files-watch');
		const requested = Array.isArray(payload)
			? [...new Set(payload.filter((item): item is string => typeof item === 'string'))]
			: [];
		const paths = requested.slice(0, THEME_LINKED_FILES_MAX_WATCHED);
		const overflow = requested.slice(THEME_LINKED_FILES_MAX_WATCHED);
		if (paths.length === 0) {
			linkedFileWatchSets.get(event.sender.id)?.dispose();
			return;
		}
		const allowedFiles = new Set(getAllowedThemeLocalFiles().map((entry) => path.resolve(entry)));
		const allowed = paths.filter((filePath) => allowedFiles.has(path.resolve(filePath)));
		const subscription = getLinkedFileWatchSubscription(event.sender);
		for (const filePath of paths) {
			if (!allowedFiles.has(path.resolve(filePath))) {
				subscription.emit({path: filePath, error: 'not_allowed'});
			}
		}
		for (const filePath of overflow) {
			subscription.emit({path: filePath, error: 'too_many'});
		}
		if (allowed.length === 0) {
			subscription.dispose();
			return;
		}
		subscription.watchSet.replace(allowed);
	});
}
