import {isObjectLike as isRecord} from '@fluxer/record_utils/src/RecordGuards';
// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {
	addAllowedThemeLocalFiles,
	addThemeFolder,
	canAddThemeFolder,
	clearAllowedThemeLocalFiles,
	getAllowedThemeLocalFiles,
	getThemeFolders,
	removeThemeFolder,
	THEME_FOLDERS_MAX,
} from '@electron/common/DesktopConfig';
import {createChildLogger} from '@electron/common/Logger';
import type {
	ThemeDirectoryCssFile,
	ThemeFolderChange,
	ThemeFolderImportResult,
	ThemeLinkedFileChange,
	ThemeLocalFileReadResult,
	ThemeLocalFileReference,
} from '@electron/common/Types';
import {t} from '@electron/main/MainI18n';
import {requirePrivilegedRendererDocumentSender} from '@electron/main/PrivilegedRendererDocuments';
import {
	createThemeFolderWatch,
	listThemeFolderCssFiles,
	resolveThemeFolderFile,
	type ThemeFolderWatch,
} from '@electron/main/ThemeFolderWatch';
import {createThemeLinkedFileWatchSet, readThemeCssFile} from '@electron/main/ThemeLinkedFileWatch';
import {app, BrowserWindow, dialog, ipcMain, type WebContents} from 'electron';

const logger = createChildLogger('ThemeLocalFiles');
const THEME_LOCAL_FILE_MAX_BYTES = 50 * 1024 * 1024;
const THEME_LINKED_FILES_MAX_WATCHED = 500;

interface ThemeLinkedFileWatchSubscription {
	watchSet: ReturnType<typeof createThemeLinkedFileWatchSet>;
	emit: (change: ThemeLinkedFileChange) => void;
	dispose: () => void;
}

const linkedFileWatchSets = new Map<number, ThemeLinkedFileWatchSubscription>();

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
	let removeTeardownListeners = (): void => {};
	const dispose = (): void => {
		if (disposed) return;
		disposed = true;
		watchSet.dispose();
		linkedFileWatchSets.delete(senderId);
		removeTeardownListeners();
	};
	removeTeardownListeners = disposeOnSenderTeardown(sender, dispose);
	const subscription = {watchSet, emit, dispose};
	linkedFileWatchSets.set(senderId, subscription);
	return subscription;
}

interface ThemeFolderWatchSubscription {
	watches: Map<string, ThemeFolderWatch>;
	emit: (change: ThemeFolderChange) => void;
	dispose: () => void;
}

const folderWatchSubscriptions = new Map<number, ThemeFolderWatchSubscription>();

function disposeOnSenderTeardown(sender: WebContents, dispose: () => void): () => void {
	const onNavigation = (_event: Electron.Event, _url: string, isSameDoc: boolean, isMainFrame: boolean): void => {
		if (isMainFrame && !isSameDoc) dispose();
	};
	sender.once('destroyed', dispose);
	sender.on('did-start-navigation', onNavigation);
	return () => {
		sender.removeListener('destroyed', dispose);
		sender.removeListener('did-start-navigation', onNavigation);
	};
}

function getFolderWatchSubscription(sender: WebContents): ThemeFolderWatchSubscription {
	const existing = folderWatchSubscriptions.get(sender.id);
	if (existing) return existing;
	const senderId = sender.id;
	const watches = new Map<string, ThemeFolderWatch>();
	const emit = (change: ThemeFolderChange): void => {
		if (!sender.isDestroyed()) sender.send('theme-folder-changed', change);
	};
	let disposed = false;
	let removeTeardownListeners = (): void => {};
	const dispose = (): void => {
		if (disposed) return;
		disposed = true;
		for (const watch of watches.values()) watch.dispose();
		watches.clear();
		folderWatchSubscriptions.delete(senderId);
		removeTeardownListeners();
	};
	removeTeardownListeners = disposeOnSenderTeardown(sender, dispose);
	const subscription = {watches, emit, dispose};
	folderWatchSubscriptions.set(senderId, subscription);
	return subscription;
}

function stopWatchingThemeFolder(folder: string): void {
	for (const subscription of folderWatchSubscriptions.values()) {
		subscription.watches.get(folder)?.dispose();
		subscription.watches.delete(folder);
	}
}

const THEME_FOLDER_FILE_FALLBACK_MIME_TYPE = 'application/octet-stream';

async function resolveReadableThemeFile(filePath: string, allowedFiles: ReadonlySet<string>): Promise<string | null> {
	if (allowedFiles.has(path.resolve(filePath))) return filePath;
	const folderFile = await resolveThemeFolderFile(filePath, getThemeFolders());
	if (!folderFile || getMimeType(folderFile) === THEME_FOLDER_FILE_FALLBACK_MIME_TYPE) return null;
	return folderFile;
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
			return THEME_FOLDER_FILE_FALLBACK_MIME_TYPE;
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
		async (event, payload: unknown): Promise<Array<ThemeLocalFileReadResult>> => {
			requirePrivilegedRendererDocumentSender(event, 'theme-local-files-read');
			if (!Array.isArray(payload)) {
				return [];
			}
			const allowed = new Set(getAllowedThemeLocalFiles().map((entry) => path.resolve(entry)));
			const paths = payload.filter((item): item is string => typeof item === 'string');
			const results: Array<ThemeLocalFileReadResult> = [];
			for (const filePath of paths) {
				const readablePath = await resolveReadableThemeFile(filePath, allowed);
				if (readablePath === null) {
					results.push({path: filePath, error: 'not_allowed'});
					continue;
				}
				try {
					const stats = await fs.promises.stat(readablePath);
					if (!stats.isFile()) {
						results.push({path: filePath, error: 'not_file'});
						continue;
					}
					if (stats.size > THEME_LOCAL_FILE_MAX_BYTES) {
						results.push({path: filePath, error: 'too_large'});
						continue;
					}
					const data = await fs.promises.readFile(readablePath);
					results.push({
						path: filePath,
						dataUrl: `data:${getMimeType(readablePath)};base64,${data.toString('base64')}`,
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
		for (const folder of getThemeFolders()) stopWatchingThemeFolder(folder);
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
		const {files: cssFiles} = await listThemeFolderCssFiles(directoryPath);
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
	ipcMain.handle('theme-folder-import', async (event): Promise<ThemeFolderImportResult | null> => {
		requirePrivilegedRendererDocumentSender(event, 'theme-folder-import');
		const parent = BrowserWindow.fromWebContents(event.sender) ?? getMainWindow();
		const result = await showThemeOpenDialog(parent, {
			title: t('desktop.themes.importFolder'),
			properties: ['openDirectory'],
		});
		if (result.canceled || result.filePaths.length === 0) return null;
		let folder: string;
		try {
			folder = await fs.promises.realpath(result.filePaths[0]);
		} catch (error) {
			logger.warn('Failed to resolve selected theme folder', {folder: result.filePaths[0], error});
			return null;
		}
		if (!canAddThemeFolder(folder)) {
			logger.warn('Refused to watch another theme folder because the limit is reached', {folder});
			return {error: 'folder_limit', limit: THEME_FOLDERS_MAX};
		}
		let listing: Awaited<ReturnType<typeof listThemeFolderCssFiles>>;
		try {
			listing = await listThemeFolderCssFiles(folder);
		} catch (error) {
			logger.warn('Failed to list the selected theme folder', {folder, error});
			return {error: 'unreadable'};
		}
		const files: Array<ThemeDirectoryCssFile> = [];
		for (const filePath of listing.files) {
			const read = await readThemeCssFile(filePath);
			if (read.css === undefined) continue;
			files.push({fileName: path.relative(folder, filePath) || path.basename(filePath), path: filePath, css: read.css});
		}
		addThemeFolder(folder);
		return {folder, files, truncated: listing.truncated};
	});
	ipcMain.handle('theme-folders-watch', (event, payload: unknown): void => {
		requirePrivilegedRendererDocumentSender(event, 'theme-folders-watch');
		const requested = Array.isArray(payload)
			? [...new Set(payload.filter((item): item is string => typeof item === 'string'))]
			: [];
		if (requested.length === 0) {
			folderWatchSubscriptions.get(event.sender.id)?.dispose();
			return;
		}
		const allowedFolders = new Set(getThemeFolders());
		const subscription = getFolderWatchSubscription(event.sender);
		for (const [folder, watch] of subscription.watches) {
			if (requested.includes(folder) && allowedFolders.has(folder)) continue;
			watch.dispose();
			subscription.watches.delete(folder);
		}
		for (const folder of requested) {
			if (!allowedFolders.has(folder)) {
				subscription.emit({
					folder,
					status: 'not_allowed',
					truncated: false,
					snapshot: false,
					added: [],
					changed: [],
					removed: [],
					renamed: [],
				});
				continue;
			}
			if (!subscription.watches.has(folder)) {
				subscription.watches.set(folder, createThemeFolderWatch(folder, subscription.emit));
			}
		}
	});
	ipcMain.handle('theme-folder-remove', (event, payload: unknown): void => {
		requirePrivilegedRendererDocumentSender(event, 'theme-folder-remove');
		if (typeof payload !== 'string') return;
		removeThemeFolder(payload);
		stopWatchingThemeFolder(payload);
	});
	app.on('will-quit', () => {
		for (const subscription of [...folderWatchSubscriptions.values()]) subscription.dispose();
		for (const subscription of [...linkedFileWatchSets.values()]) subscription.dispose();
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
