// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	assetToExportDataUrl,
	parseThemeMetadata,
	type ThemeExportPayload,
} from '@app/features/theme/utils/ThemeCssUtils';
import {
	clearThemeLibraryAssets,
	clearThemeLibraryLocalFiles,
	clearThemeLibraryMeta,
	clearThemeLibraryThemes,
	deleteThemeLibraryAsset,
	deleteThemeLibraryLocalFile,
	deleteThemeLibraryTheme,
	getEnabledThemeIds,
	listThemeLibraryAssets,
	listThemeLibraryLocalFiles,
	listThemeLibraryThemes,
	saveThemeLibraryAsset,
	saveThemeLibraryLocalFile,
	saveThemeLibraryTheme,
	setEnabledThemeIds,
	updateThemeLibraryThemes,
} from '@app/features/theme/utils/ThemeLibraryDb';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {ThemeLinkedFileError} from '@app/types/electron.d';
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {compareShallow, makeAutoObservable, reaction, runInAction} from 'mobx';

const logger = new Logger('ThemeLibrary');
const DUPLICATE_THEME_NAME_DESCRIPTOR = msg({
	message: '{themeName} Copy',
	comment:
		'Name given to the copy created by the Duplicate button in the Theme Studio theme library. {themeName} is the name of the theme being duplicated.',
});

export type ThemeLibraryThemeSource = 'quick_css' | 'css_file' | 'desktop_directory' | 'shared_theme' | 'import';

export type ThemeLinkedFileStatus = 'live' | ThemeLinkedFileError;

export interface ThemeLibraryTheme {
	id: string;
	name: string;
	description: string;
	author: string;
	version: string;
	tags: Array<string>;
	css: string;
	fileName: string;
	source: ThemeLibraryThemeSource;
	linkedPath?: string;
	createdAt: number;
	updatedAt: number;
}

export interface ThemeLibraryAsset {
	id: string;
	name: string;
	mimeType: string;
	size: number;
	data?: Blob;
	desktopPath?: string;
	createdAt: number;
	updatedAt: number;
}

export interface ThemeLibraryLocalFileReference {
	id: string;
	name: string;
	path: string;
	mimeType: string;
	size: number;
	createdAt: number;
	updatedAt: number;
}

export interface ThemeDirectoryCssFile {
	fileName: string;
	path: string;
	css: string;
}

function createThemeLibraryId(prefix: string): string {
	const cryptoApi = globalThis.crypto;
	if (cryptoApi?.randomUUID) {
		return `${prefix}-${cryptoApi.randomUUID()}`;
	}
	return `${prefix}-${Date.now().toString(36)}-${Math.random().toString(36).slice(2)}`;
}

function sanitizeFileName(name: string): string {
	const normalized = name.trim().replace(/[\\/:*?"<>|]+/g, '-');
	return normalized.length > 0 ? normalized : 'theme.css';
}

function now(): number {
	return Date.now();
}

function getFallbackThemeName(fileName: string): string {
	return fileName
		.replace(/\.css$/i, '')
		.replace(/[-_]+/g, ' ')
		.trim()
		.replace(/\s+/g, ' ')
		.replace(/^./, (letter) => letter.toUpperCase());
}

function readFileText(file: File): Promise<string> {
	return new Promise((resolve, reject) => {
		const reader = new FileReader();
		reader.onerror = () => reject(reader.error ?? new Error(`Failed to read ${file.name}`));
		reader.onload = () => resolve(String(reader.result ?? ''));
		reader.readAsText(file);
	});
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object';
}

async function dataUrlToBlob(dataUrl: string): Promise<Blob> {
	const response = await fetch(dataUrl);
	return await response.blob();
}

function themeSortValue(theme: ThemeLibraryTheme): string {
	return `${theme.name.toLowerCase()}\u0000${theme.updatedAt}`;
}

class ThemeLibrary {
	themes: Array<ThemeLibraryTheme> = [];
	assets: Array<ThemeLibraryAsset> = [];
	localFiles: Array<ThemeLibraryLocalFileReference> = [];
	enabledThemeIds: Array<string> = [];
	isHydrated = false;
	loadFailed = false;
	isBusy = false;
	revision = 0;
	linkedFileStatus = new Map<string, ThemeLinkedFileStatus>();
	focusedThemeId: string | null = null;
	private initPromise: Promise<void> | null = null;
	private linkedFileCss = new Map<string, string>();
	private linkedThemesChanged: (() => void) | null = null;

	constructor() {
		makeAutoObservable<this, 'linkedFileCss' | 'linkedThemesChanged'>(
			this,
			{linkedFileCss: false, linkedThemesChanged: false},
			{autoBind: true},
		);
	}

	get enabledThemes(): Array<ThemeLibraryTheme> {
		const byId = new Map(this.themes.map((theme) => [theme.id, theme]));
		return this.enabledThemeIds.map((id) => byId.get(id)).filter((theme): theme is ThemeLibraryTheme => theme != null);
	}

	get activeThemeCss(): string {
		return this.enabledThemes
			.map((theme) => theme.css.trim())
			.filter(Boolean)
			.join('\n\n');
	}

	get watchedLinkedPaths(): Array<string> {
		const paths = new Set<string>();
		for (const theme of this.enabledThemes) {
			if (theme.linkedPath) paths.add(theme.linkedPath);
		}
		const focusedTheme = this.focusedThemeId
			? this.themes.find((theme) => theme.id === this.focusedThemeId)
			: undefined;
		if (focusedTheme?.linkedPath) paths.add(focusedTheme.linkedPath);
		return [...paths].sort();
	}

	get canWatchLinkedFiles(): boolean {
		const electronApi = getElectronAPI();
		return Boolean(electronApi?.watchThemeLinkedFiles && electronApi.onThemeLinkedFileChange);
	}

	setFocusedThemeId(id: string | null): void {
		this.focusedThemeId = id;
	}

	async init(): Promise<void> {
		if (this.initPromise) {
			return this.initPromise;
		}
		this.initPromise = this.load();
		return this.initPromise;
	}

	async reload(): Promise<void> {
		const nextLoad = this.load();
		this.initPromise = nextLoad;
		return nextLoad;
	}

	private async load(): Promise<void> {
		try {
			const [themes, assets, localFiles, enabledThemeIds] = await Promise.all([
				listThemeLibraryThemes(),
				listThemeLibraryAssets(),
				listThemeLibraryLocalFiles(),
				getEnabledThemeIds(),
			]);
			runInAction(() => {
				this.themes = themes.sort((a, b) => themeSortValue(a).localeCompare(themeSortValue(b)));
				this.assets = assets.sort((a, b) => a.name.localeCompare(b.name));
				this.localFiles = localFiles.sort((a, b) => a.name.localeCompare(b.name));
				this.enabledThemeIds = enabledThemeIds.filter((id) => themes.some((theme) => theme.id === id));
				this.isHydrated = true;
				this.loadFailed = false;
				this.revision += 1;
			});
			this.refreshLinkedThemes();
		} catch (error) {
			logger.error('Failed to hydrate theme library', error);
			runInAction(() => {
				this.loadFailed = true;
			});
		}
	}

	private async mutate<T>(operation: () => Promise<T>): Promise<T> {
		runInAction(() => {
			this.isBusy = true;
		});
		try {
			return await operation();
		} finally {
			runInAction(() => {
				this.isBusy = false;
				this.revision += 1;
			});
		}
	}

	createThemeFromCss(
		css: string,
		fileName: string,
		source: ThemeLibraryThemeSource,
		options?: {
			id?: string;
			createdAt?: number;
			updatedAt?: number;
		},
	): ThemeLibraryTheme {
		const metadata = parseThemeMetadata(css, getFallbackThemeName(fileName));
		const timestamp = now();
		return {
			id: options?.id ?? createThemeLibraryId('theme'),
			name: metadata.name,
			description: metadata.description,
			author: metadata.author,
			version: metadata.version,
			tags: metadata.tags,
			css,
			fileName: sanitizeFileName(fileName),
			source,
			createdAt: options?.createdAt ?? timestamp,
			updatedAt: options?.updatedAt ?? timestamp,
		};
	}

	async saveTheme(theme: ThemeLibraryTheme): Promise<void> {
		await this.mutate(async () => {
			await saveThemeLibraryTheme(theme);
			runInAction(() => {
				const existingIndex = this.themes.findIndex((item) => item.id === theme.id);
				if (existingIndex >= 0) {
					this.themes.splice(existingIndex, 1, theme);
				} else {
					this.themes.push(theme);
				}
				this.themes.sort((a, b) => themeSortValue(a).localeCompare(themeSortValue(b)));
			});
		});
	}

	async saveCssAsTheme(css: string, name: string): Promise<ThemeLibraryTheme> {
		const fileName = `${sanitizeFileName(name || 'quick-css').replace(/\.css$/i, '')}.css`;
		const theme = {
			...this.createThemeFromCss(css, fileName, 'quick_css'),
			name: name.trim() || 'Quick CSS',
		};
		await this.saveTheme(theme);
		return theme;
	}

	async importCssFiles(files: ReadonlyArray<File>): Promise<Array<ThemeLibraryTheme>> {
		return await this.mutate(async () => {
			const imported: Array<ThemeLibraryTheme> = [];
			for (const file of files) {
				if (!file.name.toLowerCase().endsWith('.css')) continue;
				const css = await readFileText(file);
				const theme = this.createThemeFromCss(css, file.name, 'css_file');
				await saveThemeLibraryTheme(theme);
				imported.push(theme);
			}
			runInAction(() => {
				this.themes.push(...imported);
				this.themes.sort((a, b) => themeSortValue(a).localeCompare(themeSortValue(b)));
			});
			return imported;
		});
	}

	async importDesktopCssFiles(): Promise<Array<ThemeLibraryTheme>> {
		const pickThemeLinkedFiles = getElectronAPI()?.pickThemeLinkedFiles;
		if (!pickThemeLinkedFiles) {
			return [];
		}
		return await this.mutate(async () =>
			this.upsertDesktopCssFiles(await pickThemeLinkedFiles({multiple: true}), 'css_file'),
		);
	}

	async importDesktopThemeDirectory(): Promise<Array<ThemeLibraryTheme>> {
		const electronApi = getElectronAPI();
		const importThemeDirectory = electronApi?.importThemeDirectory;
		if (!importThemeDirectory) {
			return [];
		}
		return await this.mutate(async () => this.upsertDesktopCssFiles(await importThemeDirectory(), 'desktop_directory'));
	}

	private async upsertDesktopCssFiles(
		files: ReadonlyArray<ThemeDirectoryCssFile>,
		source: ThemeLibraryThemeSource,
	): Promise<Array<ThemeLibraryTheme>> {
		const link = this.canWatchLinkedFiles;
		const imported: Array<ThemeLibraryTheme> = [];
		for (const file of files) {
			const linkedThemes = link ? this.themes.filter((theme) => theme.linkedPath === file.path) : [];
			const targets: Array<ThemeLibraryTheme | undefined> = linkedThemes.length > 0 ? linkedThemes : [undefined];
			for (const existing of targets) {
				const theme: ThemeLibraryTheme = {
					...this.createThemeFromCss(
						file.css,
						file.fileName,
						existing?.source ?? source,
						existing ? {id: existing.id, createdAt: existing.createdAt} : undefined,
					),
					...(link ? {linkedPath: file.path} : {}),
				};
				await saveThemeLibraryTheme(theme);
				imported.push(theme);
			}
		}
		runInAction(() => {
			for (const theme of imported) {
				const existingIndex = this.themes.findIndex((item) => item.id === theme.id);
				if (existingIndex >= 0) {
					this.themes.splice(existingIndex, 1, theme);
				} else {
					this.themes.push(theme);
				}
			}
			this.themes.sort((a, b) => themeSortValue(a).localeCompare(themeSortValue(b)));
		});
		return imported;
	}

	async linkThemeToDesktopFile(id: string): Promise<ThemeLibraryTheme | null> {
		const pickThemeLinkedFiles = getElectronAPI()?.pickThemeLinkedFiles;
		if (!pickThemeLinkedFiles) {
			return null;
		}
		const [file] = await pickThemeLinkedFiles({multiple: false});
		const existing = this.themes.find((theme) => theme.id === id);
		if (!file || !existing) {
			return null;
		}
		const linkedTheme = this.themes.find((theme) => theme.id !== id && theme.linkedPath === file.path);
		if (linkedTheme) {
			return linkedTheme;
		}
		const theme: ThemeLibraryTheme = {
			...this.createThemeFromCss(file.css, file.fileName, existing.source, {id, createdAt: existing.createdAt}),
			linkedPath: file.path,
		};
		await this.saveTheme(theme);
		return theme;
	}

	async unlinkTheme(id: string): Promise<void> {
		await this.mutate(async () => {
			const result = await updateThemeLibraryThemes(
				(theme) => theme.id === id && theme.linkedPath !== undefined,
				(theme) => ({...theme, linkedPath: undefined, updatedAt: now()}),
			);
			this.replaceStoredThemes(result.themes);
		});
	}

	startLinkedFileSync(options?: {onThemesChanged?: () => void}): () => void {
		const electronApi = getElectronAPI();
		const watchThemeLinkedFiles = electronApi?.watchThemeLinkedFiles;
		const onThemeLinkedFileChange = electronApi?.onThemeLinkedFileChange;
		if (!watchThemeLinkedFiles || !onThemeLinkedFileChange) {
			return () => {};
		}
		this.linkedThemesChanged = options?.onThemesChanged ?? null;
		const watch = (paths: Array<string>): void => {
			void watchThemeLinkedFiles(paths).catch((error) => {
				logger.warn('Failed to update linked theme file watch', error);
				runInAction(() => {
					const watched = new Set(this.watchedLinkedPaths);
					for (const path of paths) {
						if (watched.has(path)) this.linkedFileStatus.set(path, 'read_failed');
					}
				});
			});
		};
		const unsubscribe = onThemeLinkedFileChange((change) => {
			if (!this.watchedLinkedPaths.includes(change.path)) return;
			runInAction(() => {
				this.linkedFileStatus.set(change.path, change.css === undefined ? (change.error ?? 'read_failed') : 'live');
			});
			if (change.css === undefined) return;
			this.linkedFileCss.set(change.path, change.css);
			this.syncLinkedFileCss(change.path);
		});
		const disposeReaction = reaction(
			() => this.watchedLinkedPaths,
			(paths) => {
				const watched = new Set(paths);
				for (const path of [...this.linkedFileStatus.keys()]) {
					if (!watched.has(path)) this.linkedFileStatus.delete(path);
				}
				for (const path of [...this.linkedFileCss.keys()]) {
					if (!watched.has(path)) this.linkedFileCss.delete(path);
				}
				watch(paths);
			},
			{equals: compareShallow, fireImmediately: true},
		);
		return () => {
			disposeReaction();
			unsubscribe();
			this.linkedFileCss.clear();
			this.linkedThemesChanged = null;
			watch([]);
		};
	}

	private syncLinkedFileCss(path: string): void {
		void this.applyLinkedFileCss(path).then(
			(written) => {
				if (written) this.linkedThemesChanged?.();
			},
			(error) => logger.warn('Failed to apply linked theme file', error),
		);
	}

	private async applyLinkedFileCss(path: string): Promise<boolean> {
		const result = await updateThemeLibraryThemes(
			(theme) => theme.linkedPath === path,
			(theme) => {
				const css = this.linkedFileCss.get(path);
				if (css === undefined || theme.css === css) return null;
				const metadata = parseThemeMetadata(css, theme.name);
				return {
					...theme,
					name: metadata.name,
					description: metadata.description,
					author: metadata.author,
					version: metadata.version,
					tags: metadata.tags,
					css,
					updatedAt: now(),
				};
			},
		);
		this.replaceStoredThemes(result.themes);
		return result.written;
	}

	private refreshLinkedThemes(): void {
		const paths = new Set(this.linkedFileCss.keys());
		if (paths.size === 0) return;
		void updateThemeLibraryThemes(
			(theme) => theme.linkedPath !== undefined && paths.has(theme.linkedPath),
			() => null,
		).then(
			(result) => this.replaceStoredThemes(result.themes),
			(error) => logger.warn('Failed to refresh linked themes', error),
		);
	}

	private replaceStoredThemes(themes: ReadonlyArray<ThemeLibraryTheme>): void {
		runInAction(() => {
			let changed = false;
			for (const theme of themes) {
				const existingIndex = this.themes.findIndex((item) => item.id === theme.id);
				if (existingIndex < 0) continue;
				const existing = this.themes[existingIndex];
				if (
					existing.updatedAt === theme.updatedAt &&
					existing.css === theme.css &&
					existing.linkedPath === theme.linkedPath
				) {
					continue;
				}
				this.themes.splice(existingIndex, 1, theme);
				changed = true;
			}
			if (!changed) return;
			this.themes.sort((a, b) => themeSortValue(a).localeCompare(themeSortValue(b)));
			this.revision += 1;
		});
	}

	async updateThemeDetails(
		id: string,
		details: Partial<Pick<ThemeLibraryTheme, 'name' | 'description' | 'author' | 'version' | 'tags' | 'css'>>,
	): Promise<void> {
		const existing = this.themes.find((theme) => theme.id === id);
		if (!existing) return;
		const nextTheme: ThemeLibraryTheme = {
			...existing,
			...details,
			updatedAt: now(),
		};
		await this.saveTheme(nextTheme);
	}

	async duplicateTheme(id: string): Promise<ThemeLibraryTheme | null> {
		const existing = this.themes.find((theme) => theme.id === id);
		if (!existing) return null;
		const duplicate: ThemeLibraryTheme = {
			...existing,
			id: createThemeLibraryId('theme'),
			name: i18n._(DUPLICATE_THEME_NAME_DESCRIPTOR, {themeName: existing.name}),
			fileName: sanitizeFileName(existing.fileName.replace(/\.css$/i, '-copy.css')),
			source: 'import',
			linkedPath: undefined,
			createdAt: now(),
			updatedAt: now(),
		};
		await this.saveTheme(duplicate);
		return duplicate;
	}

	async deleteTheme(id: string): Promise<void> {
		await this.mutate(async () => {
			await deleteThemeLibraryTheme(id);
			const nextEnabledThemeIds = this.enabledThemeIds.filter((themeId) => themeId !== id);
			await setEnabledThemeIds(nextEnabledThemeIds);
			runInAction(() => {
				this.themes = this.themes.filter((theme) => theme.id !== id);
				this.enabledThemeIds = nextEnabledThemeIds;
			});
		});
	}

	async setThemeEnabled(id: string, enabled: boolean): Promise<void> {
		await this.mutate(async () => {
			const next = enabled
				? [...this.enabledThemeIds.filter((themeId) => themeId !== id), id]
				: this.enabledThemeIds.filter((themeId) => themeId !== id);
			await setEnabledThemeIds(next);
			runInAction(() => {
				this.enabledThemeIds = next;
			});
		});
	}

	async clearEnabledThemes(): Promise<void> {
		await this.mutate(async () => {
			await setEnabledThemeIds([]);
			runInAction(() => {
				this.enabledThemeIds = [];
			});
		});
	}

	async uploadAssets(files: ReadonlyArray<File>): Promise<Array<ThemeLibraryAsset>> {
		return await this.mutate(async () => {
			const assets: Array<ThemeLibraryAsset> = [];
			for (const file of files) {
				const timestamp = now();
				const asset: ThemeLibraryAsset = {
					id: createThemeLibraryId('asset'),
					name: file.name,
					mimeType: file.type || 'application/octet-stream',
					size: file.size,
					data: file,
					createdAt: timestamp,
					updatedAt: timestamp,
				};
				await saveThemeLibraryAsset(asset);
				assets.push(asset);
			}
			runInAction(() => {
				this.assets.push(...assets);
				this.assets.sort((a, b) => a.name.localeCompare(b.name));
			});
			return assets;
		});
	}

	async addDesktopLocalFiles(): Promise<Array<ThemeLibraryLocalFileReference>> {
		const electronApi = getElectronAPI();
		const pickThemeLocalFiles = electronApi?.pickThemeLocalFiles;
		if (!pickThemeLocalFiles) {
			return [];
		}
		return await this.mutate(async () => {
			const pickedFiles = await pickThemeLocalFiles();
			const files: Array<ThemeLibraryLocalFileReference> = pickedFiles.map((file) => {
				const existing = this.localFiles.find((item) => item.path === file.path);
				const timestamp = now();
				return {
					id: existing?.id ?? createThemeLibraryId('local-file'),
					name: file.name,
					path: file.path,
					mimeType: file.mimeType,
					size: file.size,
					createdAt: existing?.createdAt ?? timestamp,
					updatedAt: timestamp,
				};
			});
			for (const file of files) {
				await saveThemeLibraryLocalFile(file);
			}
			runInAction(() => {
				const nextByPath = new Map(this.localFiles.map((file) => [file.path, file]));
				for (const file of files) {
					nextByPath.set(file.path, file);
				}
				this.localFiles = [...nextByPath.values()].sort((a, b) => a.name.localeCompare(b.name));
			});
			return files;
		});
	}

	async deleteAsset(id: string): Promise<void> {
		await this.mutate(async () => {
			await deleteThemeLibraryAsset(id);
			runInAction(() => {
				this.assets = this.assets.filter((asset) => asset.id !== id);
			});
		});
	}

	async deleteLocalFile(id: string): Promise<void> {
		await this.mutate(async () => {
			await deleteThemeLibraryLocalFile(id);
			runInAction(() => {
				this.localFiles = this.localFiles.filter((file) => file.id !== id);
			});
		});
	}

	async resetLibrary(): Promise<void> {
		await this.mutate(async () => {
			const electronApi = getElectronAPI();
			await electronApi?.clearThemeLocalFiles?.();
			await Promise.all([
				clearThemeLibraryThemes(),
				clearThemeLibraryAssets(),
				clearThemeLibraryLocalFiles(),
				clearThemeLibraryMeta(),
			]);
			runInAction(() => {
				this.themes = [];
				this.assets = [];
				this.localFiles = [];
				this.enabledThemeIds = [];
			});
		});
	}

	async buildExportPayload(themeIds?: ReadonlyArray<string>): Promise<ThemeExportPayload> {
		const selectedThemes =
			themeIds && themeIds.length > 0 ? this.themes.filter((theme) => themeIds.includes(theme.id)) : this.themes;
		const assets = await Promise.all(
			this.assets.map(async (asset) => ({
				id: asset.id,
				name: asset.name,
				mimeType: asset.mimeType,
				size: asset.size,
				dataUrl: await assetToExportDataUrl(asset),
				desktopPath: asset.desktopPath,
				createdAt: asset.createdAt,
				updatedAt: asset.updatedAt,
			})),
		);
		return {
			version: 1,
			exportedAt: new Date().toISOString(),
			themes: selectedThemes.map((theme) => ({
				id: theme.id,
				name: theme.name,
				description: theme.description,
				author: theme.author,
				version: theme.version,
				tags: theme.tags,
				css: theme.css,
				fileName: theme.fileName,
				createdAt: theme.createdAt,
				updatedAt: theme.updatedAt,
			})),
			assets,
		};
	}

	async importExportPayload(payload: unknown): Promise<{
		themes: number;
		assets: number;
	}> {
		if (!isRecord(payload) || payload.version !== 1 || !Array.isArray(payload.themes)) {
			throw new Error('Unsupported theme library export');
		}
		const payloadThemes = payload.themes;
		const payloadAssets = payload.assets;
		return await this.mutate(async () => {
			const importedThemes: Array<ThemeLibraryTheme> = [];
			for (const item of payloadThemes) {
				if (!isRecord(item) || typeof item.css !== 'string') continue;
				const fileName = typeof item.fileName === 'string' ? item.fileName : 'imported-theme.css';
				const theme = this.createThemeFromCss(item.css, fileName, 'import', {
					createdAt: typeof item.createdAt === 'number' ? item.createdAt : undefined,
					updatedAt: typeof item.updatedAt === 'number' ? item.updatedAt : undefined,
				});
				if (typeof item.name === 'string' && item.name.trim()) theme.name = item.name.trim();
				if (typeof item.description === 'string') theme.description = item.description;
				if (typeof item.author === 'string') theme.author = item.author;
				if (typeof item.version === 'string') theme.version = item.version;
				if (Array.isArray(item.tags)) theme.tags = item.tags.filter((tag): tag is string => typeof tag === 'string');
				await saveThemeLibraryTheme(theme);
				importedThemes.push(theme);
			}
			const importedAssets: Array<ThemeLibraryAsset> = [];
			if (Array.isArray(payloadAssets)) {
				for (const item of payloadAssets) {
					if (!isRecord(item) || typeof item.name !== 'string' || typeof item.mimeType !== 'string') continue;
					const timestamp = now();
					const data =
						typeof item.dataUrl === 'string' && item.dataUrl.startsWith('data:')
							? await dataUrlToBlob(item.dataUrl)
							: undefined;
					const asset: ThemeLibraryAsset = {
						id: createThemeLibraryId('asset'),
						name: item.name,
						mimeType: item.mimeType,
						size: typeof item.size === 'number' ? item.size : (data?.size ?? 0),
						data,
						desktopPath: typeof item.desktopPath === 'string' ? item.desktopPath : undefined,
						createdAt: typeof item.createdAt === 'number' ? item.createdAt : timestamp,
						updatedAt: typeof item.updatedAt === 'number' ? item.updatedAt : timestamp,
					};
					await saveThemeLibraryAsset(asset);
					importedAssets.push(asset);
				}
			}
			runInAction(() => {
				this.themes.push(...importedThemes);
				this.themes.sort((a, b) => themeSortValue(a).localeCompare(themeSortValue(b)));
				this.assets.push(...importedAssets);
				this.assets.sort((a, b) => a.name.localeCompare(b.name));
			});
			return {themes: importedThemes.length, assets: importedAssets.length};
		});
	}
}

export default new ThemeLibrary();
