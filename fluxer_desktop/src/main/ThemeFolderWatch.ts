// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import type {ThemeDirectoryCssFile, ThemeFolderChange, ThemeFolderStatus} from '@electron/common/Types';
import {readThemeCssFile} from '@electron/main/ThemeLinkedFileWatch';

const THEME_FOLDER_MAX_CSS_FILES = 200;
const SKIPPED_DIRECTORY_NAMES: ReadonlySet<string> = new Set(['node_modules']);

interface ThemeFolderEntry {
	mtimeMs: number;
	size: number;
	hash: string;
}

interface ThemeFolderListing {
	files: Array<string>;
	truncated: boolean;
}

interface ThemeFolderScanDiff {
	added: Array<string>;
	changed: Array<string>;
	removed: Array<string>;
	renamed: Array<{from: string; to: string}>;
}

export interface ThemeFolderWatchOptions {
	forcePolling?: boolean;
	pollIntervalMs?: number;
	debounceMs?: number;
	reconcileIntervalMs?: number;
}

export interface ThemeFolderWatch {
	dispose: () => void;
}

function isMissingError(error: unknown): boolean {
	const code = (error as NodeJS.ErrnoException | null)?.code;
	return code === 'ENOENT' || code === 'ENOTDIR';
}

export async function listThemeFolderCssFiles(folder: string): Promise<ThemeFolderListing> {
	const files: Array<string> = [];
	let truncated = false;
	const visit = async (directory: string, root: boolean): Promise<void> => {
		let entries: Array<fs.Dirent>;
		try {
			entries = await fs.promises.readdir(directory, {withFileTypes: true});
		} catch (error) {
			if (root) throw error;
			return;
		}
		entries.sort((a, b) => a.name.localeCompare(b.name));
		for (const entry of entries) {
			if (truncated) return;
			const entryPath = path.join(directory, entry.name);
			if (entry.isDirectory()) {
				if (entry.name.startsWith('.') || SKIPPED_DIRECTORY_NAMES.has(entry.name)) continue;
				await visit(entryPath, false);
			} else if (entry.isFile() && entry.name.toLowerCase().endsWith('.css')) {
				if (files.length === THEME_FOLDER_MAX_CSS_FILES) {
					truncated = true;
					return;
				}
				files.push(entryPath);
			}
		}
	};
	await visit(folder, true);
	return {files, truncated};
}

function diffThemeFolderScan(
	previous: ReadonlyMap<string, {hash: string}>,
	next: ReadonlyMap<string, {hash: string}>,
): ThemeFolderScanDiff {
	const added: Array<string> = [];
	const changed: Array<string> = [];
	const removed: Array<string> = [];
	for (const [filePath, entry] of next) {
		const before = previous.get(filePath);
		if (!before) added.push(filePath);
		else if (before.hash !== entry.hash) changed.push(filePath);
	}
	for (const filePath of previous.keys()) {
		if (!next.has(filePath)) removed.push(filePath);
	}
	const renamed: Array<{from: string; to: string}> = [];
	for (const from of [...removed]) {
		const hash = previous.get(from)?.hash;
		const to = added.find((candidate) => next.get(candidate)?.hash === hash);
		if (to === undefined) continue;
		renamed.push({from, to});
		removed.splice(removed.indexOf(from), 1);
		added.splice(added.indexOf(to), 1);
	}
	return {added, changed, removed, renamed};
}

function isFlatpakDocumentPortalPath(folder: string): boolean {
	const runtimeDir = process.env.XDG_RUNTIME_DIR;
	if (runtimeDir && folder.startsWith(`${path.join(runtimeDir, 'doc')}${path.sep}`)) return true;
	return /^\/run\/user\/\d+\/doc\//.test(folder);
}

export async function resolveThemeFolderFile(filePath: string, folders: ReadonlyArray<string>): Promise<string | null> {
	if (folders.length === 0) return null;
	let realPath: string;
	try {
		realPath = await fs.promises.realpath(path.resolve(filePath));
	} catch {
		return null;
	}
	for (const folder of folders) {
		const relative = path.relative(folder, realPath);
		if (relative === '' || relative === '..' || relative.startsWith(`..${path.sep}`) || path.isAbsolute(relative)) {
			continue;
		}
		const directories = relative.split(path.sep).slice(0, -1);
		if (directories.some((name) => name.startsWith('.') || SKIPPED_DIRECTORY_NAMES.has(name))) continue;
		return realPath;
	}
	return null;
}

function hashCss(css: string): string {
	return createHash('sha1').update(css).digest('hex');
}

function toCssFile(folder: string, filePath: string, css: string): ThemeDirectoryCssFile {
	return {fileName: path.relative(folder, filePath) || path.basename(filePath), path: filePath, css};
}

export function createThemeFolderWatch(
	folder: string,
	emit: (change: ThemeFolderChange) => void,
	{
		forcePolling = isFlatpakDocumentPortalPath(folder),
		pollIntervalMs = 2000,
		debounceMs = 150,
		reconcileIntervalMs = 30_000,
	}: ThemeFolderWatchOptions = {},
): ThemeFolderWatch {
	let entries = new Map<string, ThemeFolderEntry>();
	let snapshotSent = false;
	let status: ThemeFolderStatus = 'live';
	let truncated = false;
	let polling = forcePolling;
	let watcher: fs.FSWatcher | null = null;
	let debounceTimer: NodeJS.Timeout | null = null;
	let intervalTimer: NodeJS.Timeout | null = null;
	let scanning = false;
	let rerun = false;
	let disposed = false;

	const clearIntervalTimer = (): void => {
		if (intervalTimer !== null) clearInterval(intervalTimer);
		intervalTimer = null;
	};

	const closeWatcher = (): void => {
		watcher?.close();
		watcher = null;
	};

	const startInterval = (intervalMs: number): void => {
		clearIntervalTimer();
		intervalTimer = setInterval(() => void scan(), intervalMs);
		intervalTimer.unref?.();
	};

	const scheduleScan = (): void => {
		if (debounceTimer !== null) clearTimeout(debounceTimer);
		debounceTimer = setTimeout(() => {
			debounceTimer = null;
			void scan();
		}, debounceMs);
		debounceTimer.unref?.();
	};

	const startWatching = (): void => {
		if (polling) {
			startInterval(pollIntervalMs);
			return;
		}
		if (watcher) return;
		try {
			watcher = fs.watch(folder, {recursive: true, persistent: false}, scheduleScan);
		} catch {
			polling = true;
			startInterval(pollIntervalMs);
			return;
		}
		watcher.on('error', () => {
			closeWatcher();
			void fs.promises.stat(folder).then(
				() => {
					if (disposed) return;
					polling = true;
					startInterval(pollIntervalMs);
				},
				() => void scan(),
			);
		});
		startInterval(reconcileIntervalMs);
	};

	const markMissing = (): void => {
		closeWatcher();
		startInterval(pollIntervalMs);
		if (status === 'missing') return;
		status = 'missing';
		emit({folder, status, truncated, snapshot: false, added: [], changed: [], removed: [], renamed: []});
	};

	const readEntries = async (
		files: ReadonlyArray<string>,
	): Promise<{next: Map<string, ThemeFolderEntry>; css: Map<string, string>}> => {
		const next = new Map<string, ThemeFolderEntry>();
		const css = new Map<string, string>();
		for (const filePath of files) {
			const previous = entries.get(filePath);
			let stats: fs.Stats;
			try {
				stats = await fs.promises.stat(filePath);
			} catch {
				continue;
			}
			if (previous && previous.mtimeMs === stats.mtimeMs && previous.size === stats.size && snapshotSent) {
				next.set(filePath, previous);
				continue;
			}
			const read = await readThemeCssFile(filePath);
			if (read.css === undefined) {
				if (previous) next.set(filePath, previous);
				continue;
			}
			next.set(filePath, {mtimeMs: stats.mtimeMs, size: stats.size, hash: hashCss(read.css)});
			css.set(filePath, read.css);
		}
		return {next, css};
	};

	const readCss = async (filePath: string, known: ReadonlyMap<string, string>): Promise<string | null> => {
		const cached = known.get(filePath);
		if (cached !== undefined) return cached;
		const read = await readThemeCssFile(filePath);
		return read.css ?? null;
	};

	const runScan = async (): Promise<void> => {
		let listing: ThemeFolderListing;
		try {
			listing = await listThemeFolderCssFiles(folder);
		} catch (error) {
			if (isMissingError(error)) {
				markMissing();
				return;
			}
			throw error;
		}
		if (disposed) return;
		const wasMissing = status === 'missing';
		const {next, css} = await readEntries(listing.files);
		if (disposed) return;
		const diff = diffThemeFolderScan(entries, next);
		const truncatedChanged = listing.truncated !== truncated;
		entries = next;
		truncated = listing.truncated;
		status = 'live';
		if (wasMissing) clearIntervalTimer();
		startWatching();
		if (!snapshotSent) {
			snapshotSent = true;
			const added: Array<ThemeDirectoryCssFile> = [];
			for (const filePath of next.keys()) {
				const text = await readCss(filePath, css);
				if (text !== null) added.push(toCssFile(folder, filePath, text));
			}
			if (disposed) return;
			emit({folder, status, truncated, snapshot: true, added, changed: [], removed: [], renamed: []});
			return;
		}
		const nothingChanged =
			diff.added.length === 0 && diff.changed.length === 0 && diff.removed.length === 0 && diff.renamed.length === 0;
		if (nothingChanged && !truncatedChanged && !wasMissing) return;
		const added: Array<ThemeDirectoryCssFile> = [];
		const changed: Array<ThemeDirectoryCssFile> = [];
		for (const filePath of diff.added) {
			const text = await readCss(filePath, css);
			if (text !== null) added.push(toCssFile(folder, filePath, text));
		}
		for (const filePath of diff.changed) {
			const text = await readCss(filePath, css);
			if (text !== null) changed.push(toCssFile(folder, filePath, text));
		}
		if (disposed) return;
		emit({
			folder,
			status,
			truncated,
			snapshot: false,
			added,
			changed,
			removed: diff.removed,
			renamed: diff.renamed.map(({from, to}) => ({from, to, fileName: path.relative(folder, to)})),
		});
	};

	const scan = async (): Promise<void> => {
		if (disposed) return;
		if (scanning) {
			rerun = true;
			return;
		}
		scanning = true;
		try {
			do {
				rerun = false;
				await runScan().catch(() => markMissing());
			} while (rerun && !disposed);
		} finally {
			scanning = false;
		}
	};

	void scan();

	return {
		dispose() {
			disposed = true;
			closeWatcher();
			clearIntervalTimer();
			if (debounceTimer !== null) clearTimeout(debounceTimer);
			debounceTimer = null;
		},
	};
}
