// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {randomUuid} from '@app/features/platform/utils/RandomUuid';
import type {ThemeTarget} from '@app/features/theme/state/ThemeEditTarget';
import {
	assetToExportDataUrl,
	parseThemeMetadata,
	type ThemeExportPayload,
} from '@app/features/theme/utils/ThemeCssUtils';
import {
	applyThemeLibraryThemeChanges,
	clearThemeLibraryAssets,
	clearThemeLibraryLocalFiles,
	clearThemeLibraryMeta,
	clearThemeLibraryThemes,
	deleteThemeLibraryAsset,
	deleteThemeLibraryLocalFile,
	deleteThemeLibraryTheme,
	getAccountEnabledThemeIds,
	getEnabledThemeIds,
	getThemeLibraryFolders,
	listThemeLibraryAssets,
	listThemeLibraryLocalFiles,
	listThemeLibraryThemes,
	saveThemeLibraryAsset,
	saveThemeLibraryLocalFile,
	saveThemeLibraryTheme,
	setAccountEnabledThemeIds,
	setEnabledThemeIds,
	setThemeLibraryFolders,
	updateThemeLibraryThemes,
} from '@app/features/theme/utils/ThemeLibraryDb';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {readFileText} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import type {
	ThemeDirectoryCssFile,
	ThemeFolderChange,
	ThemeFolderImportError,
	ThemeFolderStatus,
	ThemeLinkedFileError,
} from '@app/types/electron.d';
import {isObjectLike as isRecord} from '@fluxer/record_utils/src/RecordGuards';
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {compareShallow, makeAutoObservable, reaction, runInAction} from 'mobx';

const logger = new Logger('ThemeLibrary');

export class ThemeFolderImportFailedError extends Error {
	constructor(readonly reason: ThemeFolderImportError) {
		super(`Theme folder import failed: ${reason.error}`);
		this.name = 'ThemeFolderImportFailedError';
	}
}
const DUPLICATE_THEME_NAME_DESCRIPTOR = msg({
	message: '{themeName} Copy',
	comment:
		'Name given to the copy created by the "Duplicate" menu item in Settings > Themes. {themeName} is the name of the theme being duplicated.',
});

type ThemeLibraryThemeSource = 'quick_css' | 'css_file' | 'desktop_directory' | 'shared_theme' | 'import';

type ThemeLinkedFileStatus = 'live' | ThemeLinkedFileError;

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
	folderPath?: string;
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

export interface ThemeLibraryFolder {
	path: string;
	status: ThemeFolderStatus;
	truncated: boolean;
}

interface ThemeFolderFileChanges {
	upserts: ReadonlyArray<ThemeDirectoryCssFile>;
	renamed: ReadonlyArray<{from: string; to: string; fileName: string}>;
	removedPaths: ReadonlyArray<string>;
	snapshot: boolean;
}

function createThemeLibraryId(prefix: string): string {
	return `${prefix}-${randomUuid()}`;
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

async function dataUrlToBlob(dataUrl: string): Promise<Blob> {
	const response = await fetch(dataUrl);
	return await response.blob();
}

const THEME_LIBRARY_STORAGE_KEYS: ReadonlySet<string> = new Set([
	AppStorageKey.THEME_LIBRARY_THEMES,
	AppStorageKey.THEME_LIBRARY_LOCAL_FILES,
	AppStorageKey.THEME_LIBRARY_ENABLED_IDS,
	AppStorageKey.THEME_LIBRARY_FOLDERS,
]);

export function getThemeLabel(theme: ThemeLibraryTheme): string {
	return theme.name || theme.fileName;
}

function themeSortValue(theme: ThemeLibraryTheme): string {
	return `${theme.name.toLowerCase()}\u0000${theme.updatedAt}`;
}

class ThemeLibrary {
	themes: Array<ThemeLibraryTheme> = [];
	assets: Array<ThemeLibraryAsset> = [];
	localFiles: Array<ThemeLibraryLocalFileReference> = [];
	installationEnabledIds: Array<string> = [];
	accountEnabledIds: Array<string> = getAccountEnabledThemeIds();
	isHydrated = false;
	loadFailed = false;
	isBusy = false;
	revision = 0;
	linkedFileStatus = new Map<string, ThemeLinkedFileStatus>();
	folderPaths: Array<string> = getThemeLibraryFolders();
	folderStatus = new Map<string, {status: ThemeFolderStatus; truncated: boolean}>();
	focusedThemeId: string | null = null;
	private initPromise: Promise<void> | null = null;
	private loadGeneration = 0;
	private storageSyncStarted = false;
	private linkedFileCss = new Map<string, string>();
	private folderQueue: Promise<void> = Promise.resolve();

	constructor() {
		makeAutoObservable<this, 'linkedFileCss' | 'loadGeneration' | 'storageSyncStarted' | 'folderQueue'>(
			this,
			{linkedFileCss: false, loadGeneration: false, storageSyncStarted: false, folderQueue: false},
			{autoBind: true},
		);
	}

	get folders(): Array<ThemeLibraryFolder> {
		return this.folderPaths.map((path) => ({
			path,
			status: this.folderStatus.get(path)?.status ?? 'live',
			truncated: this.folderStatus.get(path)?.truncated ?? false,
		}));
	}

	get canWatchFolders(): boolean {
		return getElectronAPI()?.themeFolderWatch === true;
	}

	isWatchedFolderTheme(theme: ThemeLibraryTheme): boolean {
		return theme.folderPath !== undefined && this.canWatchFolders && this.folderPaths.includes(theme.folderPath);
	}

	enabledIdsFor(target: ThemeTarget): Array<string> {
		const ids = target === 'desktop' ? this.installationEnabledIds : this.accountEnabledIds;
		return ids.filter((id) => this.has(id));
	}

	has(id: string): boolean {
		return this.themes.some((theme) => theme.id === id);
	}

	themeCssFor(themeIds: ReadonlyArray<string>, preview: {id: string; css: string} | null = null): string {
		const byId = new Map(this.themes.map((theme) => [theme.id, theme]));
		return themeIds
			.map((id) => (id === preview?.id ? preview.css : (byId.get(id)?.css ?? '')).trim())
			.filter(Boolean)
			.join('\n\n');
	}

	get watchedLinkedPaths(): Array<string> {
		const paths = new Set<string>();
		const enabledIds = new Set([...this.installationEnabledIds, ...this.accountEnabledIds]);
		for (const theme of this.themes) {
			if (theme.linkedPath && enabledIds.has(theme.id) && !this.isWatchedFolderTheme(theme)) {
				paths.add(theme.linkedPath);
			}
		}
		const focusedTheme = this.focusedThemeId
			? this.themes.find((theme) => theme.id === this.focusedThemeId)
			: undefined;
		if (focusedTheme?.linkedPath && !this.isWatchedFolderTheme(focusedTheme)) paths.add(focusedTheme.linkedPath);
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

	private followExternalStorageChanges(): void {
		if (this.storageSyncStarted) return;
		this.storageSyncStarted = true;
		AppStorage.subscribe(
			(event) => {
				if (event.key === null || THEME_LIBRARY_STORAGE_KEYS.has(event.key)) {
					void this.reload();
				}
			},
			{source: 'external'},
		);
	}

	private async load(): Promise<void> {
		this.followExternalStorageChanges();
		const generation = ++this.loadGeneration;
		try {
			const [themes, assets, localFiles, enabledThemeIds] = await Promise.all([
				listThemeLibraryThemes(),
				listThemeLibraryAssets(),
				listThemeLibraryLocalFiles(),
				getEnabledThemeIds(),
			]);
			if (generation !== this.loadGeneration) {
				return;
			}
			runInAction(() => {
				this.themes = themes.sort((a, b) => themeSortValue(a).localeCompare(themeSortValue(b)));
				this.assets = assets.sort((a, b) => a.name.localeCompare(b.name));
				this.localFiles = localFiles.sort((a, b) => a.name.localeCompare(b.name));
				this.installationEnabledIds = enabledThemeIds;
				this.folderPaths = getThemeLibraryFolders();
				this.isHydrated = true;
				this.loadFailed = false;
				this.revision += 1;
			});
			this.refreshLinkedThemes();
		} catch (error) {
			if (generation !== this.loadGeneration) {
				return;
			}
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
		const importThemeFolder = electronApi?.importThemeFolder;
		if (this.canWatchFolders && importThemeFolder) {
			const result = await importThemeFolder();
			if (!result) return [];
			if ('error' in result) throw new ThemeFolderImportFailedError(result);
			runInAction(() => {
				if (!this.folderPaths.includes(result.folder)) this.folderPaths = [...this.folderPaths, result.folder];
				this.folderStatus.set(result.folder, {status: 'live', truncated: result.truncated});
			});
			setThemeLibraryFolders(this.folderPaths);
			return await this.enqueueFolderWork(() =>
				this.applyFolderFiles(result.folder, {upserts: result.files, renamed: [], removedPaths: [], snapshot: true}),
			);
		}
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
				(theme) => ({...theme, linkedPath: undefined, folderPath: undefined, updatedAt: now()}),
			);
			this.replaceStoredThemes(result.themes);
		});
	}

	async stopWatchingFolder(folder: string): Promise<void> {
		await getElectronAPI()?.removeThemeFolder?.(folder);
		runInAction(() => {
			this.folderPaths = this.folderPaths.filter((path) => path !== folder);
			this.folderStatus.delete(folder);
		});
		setThemeLibraryFolders(this.folderPaths);
		await this.enqueueFolderWork(async () => {
			const removed = new Set(this.themes.filter((theme) => theme.folderPath === folder).map((theme) => theme.id));
			await this.commitThemeChanges([], removed);
		});
	}

	startFolderSync(): () => void {
		const electronApi = getElectronAPI();
		const watchThemeFolders = electronApi?.watchThemeFolders;
		const onThemeFolderChange = electronApi?.onThemeFolderChange;
		if (!this.canWatchFolders || !watchThemeFolders || !onThemeFolderChange) {
			return () => {};
		}
		const unsubscribe = onThemeFolderChange((change) => {
			if (!this.folderPaths.includes(change.folder)) return;
			void this.enqueueFolderWork(() => this.applyFolderChange(change));
		});
		const disposeReaction = reaction(
			() => [...this.folderPaths],
			(paths) => {
				void watchThemeFolders(paths).catch((error) => logger.warn('Failed to update theme folder watch', error));
			},
			{equals: compareShallow, fireImmediately: true},
		);
		return () => {
			disposeReaction();
			unsubscribe();
			void watchThemeFolders([]).catch(() => {});
		};
	}

	private enqueueFolderWork<T>(work: () => Promise<T>): Promise<T> {
		const run = this.folderQueue.then(async () => {
			await this.init();
			return await work();
		});
		this.folderQueue = run.then(
			() => {},
			(error) => logger.warn('Failed to apply theme folder changes', error),
		);
		return run;
	}

	private async applyFolderChange(change: ThemeFolderChange): Promise<void> {
		runInAction(() => {
			this.folderStatus.set(change.folder, {status: change.status, truncated: change.truncated});
		});
		if (change.status !== 'live') return;
		await this.applyFolderFiles(change.folder, {
			upserts: [...change.added, ...change.changed],
			renamed: change.renamed,
			removedPaths: change.removed,
			snapshot: change.snapshot,
		});
	}

	private folderTheme(
		folder: string,
		file: ThemeDirectoryCssFile,
		existing: ThemeLibraryTheme | undefined,
	): ThemeLibraryTheme {
		return {
			...this.createThemeFromCss(
				file.css,
				file.fileName,
				'desktop_directory',
				existing ? {id: existing.id, createdAt: existing.createdAt} : undefined,
			),
			linkedPath: file.path,
			folderPath: folder,
		};
	}

	private async applyFolderFiles(folder: string, changes: ThemeFolderFileChanges): Promise<Array<ThemeLibraryTheme>> {
		const byPath = new Map<string, ThemeLibraryTheme>();
		const linkedOutsideFolder = new Set<string>();
		for (const theme of this.themes) {
			if (!theme.linkedPath) continue;
			if (theme.folderPath === folder) byPath.set(theme.linkedPath, theme);
			else linkedOutsideFolder.add(theme.linkedPath);
		}
		const upserts: Array<ThemeLibraryTheme> = [];
		const removed = new Set<string>();
		for (const {from, to, fileName} of changes.renamed) {
			const existing = byPath.get(from);
			if (!existing) continue;
			const renamed = {...existing, linkedPath: to, fileName, folderPath: folder, updatedAt: now()};
			byPath.delete(from);
			byPath.set(to, renamed);
			upserts.push(renamed);
		}
		const matched = new Set<string>();
		const newFiles: Array<ThemeDirectoryCssFile> = [];
		for (const file of changes.upserts) {
			const existing = byPath.get(file.path);
			if (!existing) {
				if (!linkedOutsideFolder.has(file.path)) newFiles.push(file);
				continue;
			}
			matched.add(existing.id);
			if (existing.css === file.css && existing.folderPath === folder) continue;
			upserts.push(this.folderTheme(folder, file, existing));
		}
		for (const path of changes.removedPaths) {
			const existing = byPath.get(path);
			if (existing?.folderPath === folder) removed.add(existing.id);
		}
		const leftovers = changes.snapshot
			? this.themes.filter(
					(theme) =>
						theme.folderPath === folder && !matched.has(theme.id) && !upserts.some((upsert) => upsert.id === theme.id),
				)
			: [];
		for (const file of newFiles) {
			const renamedFrom = leftovers.find((theme) => theme.css === file.css);
			if (renamedFrom) leftovers.splice(leftovers.indexOf(renamedFrom), 1);
			upserts.push(this.folderTheme(folder, file, renamedFrom));
		}
		for (const theme of leftovers) removed.add(theme.id);
		await this.commitThemeChanges(upserts, removed);
		return upserts;
	}

	private async commitThemeChanges(
		upserts: ReadonlyArray<ThemeLibraryTheme>,
		removed: ReadonlySet<string>,
	): Promise<void> {
		if (upserts.length === 0 && removed.size === 0) return;
		await this.mutate(async () => {
			await applyThemeLibraryThemeChanges(upserts, removed);
			runInAction(() => {
				const upsertIds = new Set(upserts.map((theme) => theme.id));
				this.themes = [
					...this.themes.filter((theme) => !upsertIds.has(theme.id) && !removed.has(theme.id)),
					...upserts,
				].sort((a, b) => themeSortValue(a).localeCompare(themeSortValue(b)));
			});
		});
	}

	startLinkedFileSync(): () => void {
		const electronApi = getElectronAPI();
		const watchThemeLinkedFiles = electronApi?.watchThemeLinkedFiles;
		const onThemeLinkedFileChange = electronApi?.onThemeLinkedFileChange;
		if (!watchThemeLinkedFiles || !onThemeLinkedFileChange) {
			return () => {};
		}
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
			watch([]);
		};
	}

	private syncLinkedFileCss(path: string): void {
		void this.applyLinkedFileCss(path).catch((error) => logger.warn('Failed to apply linked theme file', error));
	}

	private async applyLinkedFileCss(path: string): Promise<void> {
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
			folderPath: undefined,
			createdAt: now(),
			updatedAt: now(),
		};
		await this.saveTheme(duplicate);
		return duplicate;
	}

	async deleteTheme(id: string): Promise<void> {
		await this.mutate(async () => {
			await deleteThemeLibraryTheme(id);
			runInAction(() => {
				this.themes = this.themes.filter((theme) => theme.id !== id);
			});
		});
	}

	loadAccountEnabledIds(): void {
		this.accountEnabledIds = getAccountEnabledThemeIds();
	}

	async setThemeEnabled(id: string, enabled: boolean, target: ThemeTarget): Promise<void> {
		await this.mutate(async () => {
			const current = target === 'desktop' ? this.installationEnabledIds : this.accountEnabledIds;
			const next = enabled
				? [...current.filter((themeId) => themeId !== id), id]
				: current.filter((themeId) => themeId !== id);
			await this.writeEnabledIds(next, target);
		});
	}

	async moveEnabledTheme(id: string, offset: -1 | 1, target: ThemeTarget): Promise<void> {
		await this.mutate(async () => {
			const enabledIds = this.enabledIdsFor(target);
			const index = enabledIds.indexOf(id);
			const neighbor = index < 0 ? undefined : enabledIds[index + offset];
			if (neighbor === undefined) return;
			const current = target === 'desktop' ? this.installationEnabledIds : this.accountEnabledIds;
			const next = current.map((themeId) => {
				if (themeId === id) {
					return neighbor;
				}
				if (themeId === neighbor) {
					return id;
				}
				return themeId;
			});
			await this.writeEnabledIds(next, target);
		});
	}

	private async writeEnabledIds(next: Array<string>, target: ThemeTarget): Promise<void> {
		if (target === 'desktop') {
			await setEnabledThemeIds(next);
		} else {
			setAccountEnabledThemeIds(next);
		}
		runInAction(() => {
			if (target === 'desktop') {
				this.installationEnabledIds = next;
			} else {
				this.accountEnabledIds = next;
			}
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
			setThemeLibraryFolders([]);
			runInAction(() => {
				this.themes = [];
				this.assets = [];
				this.localFiles = [];
				this.installationEnabledIds = [];
				this.folderPaths = [];
				this.folderStatus.clear();
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
