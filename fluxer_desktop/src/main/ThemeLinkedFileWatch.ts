// SPDX-License-Identifier: AGPL-3.0-or-later

import {Buffer} from 'node:buffer';
import fs from 'node:fs';
import type {ThemeLinkedFileChange} from '@electron/common/Types';

const THEME_CSS_MAX_BYTES = 1024 * 1024;

interface ThemeLinkedFileWatchEntry {
	listener: () => void;
	generation: number;
}

export async function readThemeCssFile(filePath: string): Promise<ThemeLinkedFileChange> {
	let handle: fs.promises.FileHandle | null = null;
	try {
		const linkStats = await fs.promises.lstat(filePath);
		if (!linkStats.isFile()) {
			return {path: filePath, error: 'not_file'};
		}
		handle = await fs.promises.open(filePath, fs.constants.O_RDONLY | (fs.constants.O_NOFOLLOW ?? 0));
		const stats = await handle.stat();
		if (!stats.isFile()) {
			return {path: filePath, error: 'not_file'};
		}
		if (stats.size > THEME_CSS_MAX_BYTES) {
			return {path: filePath, error: 'too_large'};
		}
		const buffer = Buffer.alloc(THEME_CSS_MAX_BYTES + 1);
		let bytesRead = 0;
		while (bytesRead < buffer.length) {
			const result = await handle.read(buffer, bytesRead, buffer.length - bytesRead, bytesRead);
			if (result.bytesRead === 0) break;
			bytesRead += result.bytesRead;
		}
		if (bytesRead > THEME_CSS_MAX_BYTES) {
			return {path: filePath, error: 'too_large'};
		}
		return {path: filePath, css: buffer.toString('utf8', 0, bytesRead).replace(/^\uFEFF/, '')};
	} catch (error) {
		const code = (error as NodeJS.ErrnoException | null)?.code;
		if (code === 'ENOENT') {
			return {path: filePath, error: 'missing'};
		}
		if (code === 'ELOOP') {
			return {path: filePath, error: 'not_file'};
		}
		return {path: filePath, error: 'read_failed'};
	} finally {
		await handle?.close().catch(() => {});
	}
}

export function createThemeLinkedFileWatchSet(
	emit: (change: ThemeLinkedFileChange) => void,
	intervalMs = 250,
): {replace: (paths: ReadonlyArray<string>) => void; dispose: () => void} {
	const entries = new Map<string, ThemeLinkedFileWatchEntry>();
	const read = (filePath: string): void => {
		const entry = entries.get(filePath);
		if (!entry) return;
		entry.generation += 1;
		const generation = entry.generation;
		void readThemeCssFile(filePath).then((change) => {
			const current = entries.get(filePath);
			if (current !== entry || current.generation !== generation) return;
			emit(change);
		});
	};
	const unwatch = (filePath: string): void => {
		const entry = entries.get(filePath);
		if (!entry) return;
		fs.unwatchFile(filePath, entry.listener);
		entries.delete(filePath);
	};
	return {
		replace(paths) {
			const next = new Set(paths);
			for (const filePath of [...entries.keys()]) {
				if (!next.has(filePath)) unwatch(filePath);
			}
			for (const filePath of next) {
				if (entries.has(filePath)) continue;
				const entry: ThemeLinkedFileWatchEntry = {listener: () => read(filePath), generation: 0};
				entries.set(filePath, entry);
				fs.watchFile(filePath, {interval: intervalMs, persistent: false}, entry.listener);
				read(filePath);
			}
		},
		dispose() {
			for (const filePath of [...entries.keys()]) unwatch(filePath);
		},
	};
}
