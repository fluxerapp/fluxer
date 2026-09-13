// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {
	addAllowedThemeLocalFiles,
	clearAllowedThemeLocalFiles,
	getAllowedThemeLocalFiles,
} from '@electron/common/DesktopConfig';
import {createChildLogger} from '@electron/common/Logger';
import type {ThemeDirectoryCssFile, ThemeLocalFileReadResult, ThemeLocalFileReference} from '@electron/common/Types';
import {t} from '@electron/main/MainI18n';
import {BrowserWindow, dialog, ipcMain} from 'electron';

const logger = createChildLogger('ThemeLocalFiles');
const THEME_LOCAL_FILE_MAX_BYTES = 50 * 1024 * 1024;
const THEME_DIRECTORY_CSS_MAX_BYTES = 1024 * 1024;
const THEME_DIRECTORY_MAX_CSS_FILES = 200;

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
		const directoryPath = result.filePaths[0];
		const cssFiles = await collectCssFilesFromDirectory(directoryPath);
		const themes: Array<ThemeDirectoryCssFile> = [];
		for (const filePath of cssFiles) {
			try {
				const stats = await fs.promises.stat(filePath);
				if (!stats.isFile() || stats.size > THEME_DIRECTORY_CSS_MAX_BYTES) continue;
				const css = await fs.promises.readFile(filePath, 'utf8');
				themes.push({
					fileName: path.relative(directoryPath, filePath) || path.basename(filePath),
					path: filePath,
					css,
				});
			} catch (error) {
				logger.warn('Failed to import CSS file from theme directory', {filePath, error});
			}
		}
		return themes;
	});
}
