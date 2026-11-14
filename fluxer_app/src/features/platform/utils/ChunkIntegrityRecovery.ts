// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {isBrowserOffline, isLazyModuleLoadError} from '@app/features/platform/utils/LazyModuleLoader';

type ChunkId = string | number;

interface ChunkRuntime {
	e: (chunkId: ChunkId) => Promise<unknown>;
	u: (chunkId: ChunkId) => string;
	k?: (chunkId: ChunkId) => string;
	p: string;
	sriHashes?: Record<string, string>;
	sriCssHashes?: Record<string, string>;
}

declare const __webpack_require__: ChunkRuntime;

const MAX_RECOVERY_ATTEMPTS = 2;

const logger = new Logger('ChunkIntegrity');
const reportedFiles = new Set<string>();
const recoveryAttempts = new Map<string, number>();

let installed = false;

function withRecoveryQuery(url: string, attempt: number): string {
	if (attempt === 0) {
		return url;
	}
	const separator = url.includes('?') ? '&' : '?';
	return `${url}${separator}r=${attempt}-${Math.random().toString(36).slice(2, 10)}`;
}

function reportFailedFile(url: string): void {
	if (reportedFiles.has(url)) {
		return;
	}
	reportedFiles.add(url);
	logger.warn('A file failed to load or failed its integrity check. Fetching a fresh copy.', url);
}

async function refreshCachedCopy(url: string, integrity: string): Promise<void> {
	try {
		const response = await fetch(url, {cache: 'reload', mode: 'cors', credentials: 'omit', integrity});
		await response.arrayBuffer();
	} catch {}
}

function chunkFiles(
	runtime: ChunkRuntime,
	baseJsUrl: ChunkRuntime['u'],
	baseCssUrl: ChunkRuntime['k'],
	chunkId: ChunkId,
) {
	const key = String(chunkId);
	const files: Array<{url: string; integrity: string}> = [];
	const jsIntegrity = runtime.sriHashes?.[key];
	if (jsIntegrity) {
		files.push({url: runtime.p + baseJsUrl(chunkId), integrity: jsIntegrity});
	}
	const cssIntegrity = runtime.sriCssHashes?.[key];
	if (cssIntegrity && baseCssUrl) {
		files.push({url: runtime.p + baseCssUrl(chunkId), integrity: cssIntegrity});
	}
	return files;
}

export function installChunkIntegrityRecovery(): void {
	if (installed || typeof __webpack_require__ === 'undefined') {
		return;
	}
	const runtime = __webpack_require__;
	if (runtime.sriHashes == null) {
		return;
	}
	installed = true;
	const loadChunk = runtime.e;
	const baseJsUrl = runtime.u;
	const baseCssUrl = runtime.k;
	const attemptOf = (chunkId: ChunkId) => recoveryAttempts.get(String(chunkId)) ?? 0;
	runtime.u = (chunkId) => withRecoveryQuery(baseJsUrl(chunkId), attemptOf(chunkId));
	if (baseCssUrl) {
		runtime.k = (chunkId) => withRecoveryQuery(baseCssUrl(chunkId), attemptOf(chunkId));
	}
	runtime.e = async (chunkId) => {
		for (;;) {
			try {
				return await loadChunk(chunkId);
			} catch (error) {
				const attempt = attemptOf(chunkId);
				if (attempt >= MAX_RECOVERY_ATTEMPTS || !isLazyModuleLoadError(error) || isBrowserOffline()) {
					throw error;
				}
				const files = chunkFiles(runtime, baseJsUrl, baseCssUrl, chunkId);
				const request = (error as {request?: unknown}).request;
				if (typeof request === 'string' && request !== '') {
					reportFailedFile(request);
				} else {
					for (const file of files) {
						reportFailedFile(file.url);
					}
				}
				await Promise.all(files.map((file) => refreshCachedCopy(file.url, file.integrity)));
				recoveryAttempts.set(String(chunkId), attempt + 1);
			}
		}
	};
}
