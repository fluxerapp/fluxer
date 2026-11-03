// SPDX-License-Identifier: AGPL-3.0-or-later

import {Buffer} from 'node:buffer';
import fs from 'node:fs/promises';
import path from 'node:path';
import {writeFileAtomically} from '@electron/main/AtomicFile';
import {
	type DesktopModuleManifestTarget,
	type DesktopModuleUpdateManifest,
	parseModuleUpdateManifest,
} from '@electron/main/ModuleManifest';
import type {ModuleStore} from '@electron/main/ModuleStore';
import {type ModuleVersion, parseModuleVersion} from '@electron/main/ModuleVersion';

const CHANNEL_MANIFEST_CACHE_FILE_NAME = 'manifest.json';
const MODULE_MANIFEST_TIMEOUT_MS = 15000;
const MODULE_MANIFEST_MEMO_TTL_MS = 60000;
const MODULE_MANIFEST_MAX_BYTES = 1024 * 1024;
const SHELL_FEED_MAX_BYTES = 64 * 1024;
const MODULE_CACHE_FILE_MODE = 0o644;

export interface ModuleManifestDocument {
	readonly bytes: Buffer;
	readonly etag: string | null;
	readonly fromCache: boolean;
}

interface ModuleManifestRepositoryOptions {
	readonly store: ModuleStore;
	readonly target: DesktopModuleManifestTarget;
	readonly packageOrigin: string;
	readonly fetch: typeof globalThis.fetch;
	readonly now: () => number;
	readonly onResponseCancellationError: (message: string, error: unknown) => void;
}

export class ModuleManifestFetchError extends Error {
	public readonly status: number | null;

	public constructor(message: string, status: number | null, options?: ErrorOptions) {
		super(message, options);
		this.name = 'ModuleManifestFetchError';
		this.status = status;
	}
}

function isMissingFileError(error: unknown): boolean {
	return (error as NodeJS.ErrnoException).code === 'ENOENT';
}

async function readBoundedCacheFile(filePath: string, limit: number, description: string): Promise<Buffer | null> {
	let file: fs.FileHandle;
	try {
		file = await fs.open(filePath, 'r');
	} catch (error) {
		if (isMissingFileError(error)) {
			return null;
		}
		throw error;
	}
	try {
		const metadata = await file.stat();
		if (!metadata.isFile()) {
			throw new ModuleManifestFetchError(`${description} cache path is not a file`, null);
		}
		if (metadata.size > limit) {
			throw new ModuleManifestFetchError(`${description} cache is larger than ${limit} bytes`, null);
		}
		const buffer = Buffer.allocUnsafe(limit + 1);
		let offset = 0;
		while (offset < buffer.byteLength) {
			const {bytesRead} = await file.read(buffer, offset, buffer.byteLength - offset, offset);
			if (bytesRead === 0) {
				break;
			}
			offset += bytesRead;
		}
		if (offset > limit) {
			throw new ModuleManifestFetchError(`${description} cache is larger than ${limit} bytes`, null);
		}
		return buffer.subarray(0, offset);
	} finally {
		await file.close();
	}
}

export class ModuleManifestRepository {
	private readonly store: ModuleStore;
	private readonly target: DesktopModuleManifestTarget;
	private readonly packageOrigin: string;
	private readonly fetchImplementation: typeof globalThis.fetch;
	private readonly now: () => number;
	private readonly onResponseCancellationError: (message: string, error: unknown) => void;
	private readonly manifestCachePath: string;
	private memo: {readonly expiresAt: number; readonly document: ModuleManifestDocument} | null = null;
	private inFlight: Promise<ModuleManifestDocument> | null = null;
	private memoEpoch = 0;
	private validated: ModuleManifestDocument | null = null;
	private shellFeed: {readonly etag: string; readonly version: ModuleVersion} | null = null;

	public constructor(options: ModuleManifestRepositoryOptions) {
		this.store = options.store;
		this.target = options.target;
		this.packageOrigin = options.packageOrigin;
		this.fetchImplementation = options.fetch;
		this.now = options.now;
		this.onResponseCancellationError = options.onResponseCancellationError;
		this.manifestCachePath = path.join(this.store.root, CHANNEL_MANIFEST_CACHE_FILE_NAME);
	}

	public parse(document: ModuleManifestDocument): DesktopModuleUpdateManifest {
		return parseModuleUpdateManifest(document.bytes, this.target);
	}

	public async fetchLatest(): Promise<ModuleManifestDocument> {
		const memoEpoch = this.memoEpoch;
		const document = await this.readLatestDocument();
		if (memoEpoch === this.memoEpoch) {
			this.memo = {expiresAt: this.now() + MODULE_MANIFEST_MEMO_TTL_MS, document};
		}
		return document;
	}

	public async resolveMemoizedDocument(): Promise<ModuleManifestDocument> {
		const memo = this.memo;
		if (memo != null && memo.expiresAt > this.now()) {
			return memo.document;
		}
		const request = this.inFlight ?? this.fetchLatest();
		this.inFlight = request;
		try {
			return await request;
		} finally {
			if (this.inFlight === request) {
				this.inFlight = null;
			}
		}
	}

	public clearMemo(): void {
		this.memo = null;
		this.inFlight = null;
		this.memoEpoch += 1;
	}

	public async readCached(): Promise<ModuleManifestDocument | null> {
		const bytes = await readBoundedCacheFile(this.manifestCachePath, MODULE_MANIFEST_MAX_BYTES, 'manifest');
		if (bytes == null) {
			return null;
		}
		return {
			bytes,
			etag: this.store.getState().last_manifest_etag,
			fromCache: true,
		};
	}

	public async persist(document: ModuleManifestDocument): Promise<void> {
		if (document.fromCache) {
			return;
		}
		await writeFileAtomically(this.manifestCachePath, document.bytes, MODULE_CACHE_FILE_MODE);
	}

	public async fetchShellLatestVersion(): Promise<ModuleVersion> {
		const url = this.shellFeedUrl();
		const known = this.shellFeed;
		const headers: Record<string, string> = {accept: 'application/json'};
		if (known != null) {
			headers['if-none-match'] = known.etag;
		}
		let response: Response;
		try {
			response = await this.fetchImplementation(url, {
				headers,
				signal: AbortSignal.timeout(MODULE_MANIFEST_TIMEOUT_MS),
			});
		} catch (error) {
			throw new ModuleManifestFetchError(`failed to reach ${url}`, null, {cause: error});
		}
		if (response.status === 304 && known != null) {
			return known.version;
		}
		if (!response.ok) {
			throw new ModuleManifestFetchError(`shell feed request returned ${response.status}`, response.status);
		}
		const bytes = await this.readBoundedBody(response, SHELL_FEED_MAX_BYTES, 'shell feed');
		const payload = JSON.parse(bytes.toString('utf8')) as {version?: unknown};
		if (typeof payload.version !== 'string') {
			throw new ModuleManifestFetchError('shell feed is missing its version', response.status);
		}
		const version = parseModuleVersion(payload.version, 'shell feed version');
		const etag = response.headers.get('etag');
		this.shellFeed = etag == null ? null : {etag, version};
		return version;
	}

	private async readLatestDocument(): Promise<ModuleManifestDocument> {
		const validated = this.validated;
		if (validated != null) {
			return (await this.requestManifest(validated.etag)) ?? validated;
		}
		const first = await this.requestManifest(this.store.getState().last_manifest_etag);
		if (first != null) {
			return first;
		}
		const cached = await this.readCached();
		if (cached != null) {
			return cached;
		}
		const refetched = await this.requestManifest(null);
		if (refetched == null) {
			throw new ModuleManifestFetchError('manifest returned 304 without a conditional request', 304);
		}
		return refetched;
	}

	private feedUrl(fileName: string): string {
		return new URL(
			`${this.packageOrigin}/desktop/${this.target.releaseChannel}/${this.target.platform}/${this.target.arch}/${fileName}`,
		).toString();
	}

	private manifestUrl(): string {
		return this.feedUrl('modules.json');
	}

	private shellFeedUrl(): string {
		return this.feedUrl('latest.json');
	}

	private async requestManifest(ifNoneMatch: string | null): Promise<ModuleManifestDocument | null> {
		const url = this.manifestUrl();
		const headers: Record<string, string> = {accept: 'application/json'};
		if (ifNoneMatch != null) {
			headers['if-none-match'] = ifNoneMatch;
		}
		let response: Response;
		try {
			response = await this.fetchImplementation(url, {
				headers,
				signal: AbortSignal.timeout(MODULE_MANIFEST_TIMEOUT_MS),
			});
		} catch (error) {
			throw new ModuleManifestFetchError(`failed to reach ${url}`, null, {cause: error});
		}
		if (response.status === 304) {
			return null;
		}
		if (!response.ok) {
			throw new ModuleManifestFetchError(`manifest request returned ${response.status}`, response.status);
		}
		const bytes = await this.readBoundedBody(response, MODULE_MANIFEST_MAX_BYTES, 'manifest');
		parseModuleUpdateManifest(bytes, this.target);
		const document = {bytes, etag: response.headers.get('etag'), fromCache: false};
		this.validated = document.etag == null ? null : document;
		return document;
	}

	private async readBoundedBody(response: Response, limit: number, description: string): Promise<Buffer> {
		const declared = Number.parseInt(response.headers.get('content-length') ?? '', 10);
		if (Number.isSafeInteger(declared) && declared > limit) {
			throw new ModuleManifestFetchError(`${description} is larger than ${limit} bytes`, response.status);
		}
		if (response.body == null) {
			throw new ModuleManifestFetchError(`${description} response had no body`, response.status);
		}
		const reader = response.body.getReader();
		const chunks: Array<Uint8Array> = [];
		let received = 0;
		try {
			for (;;) {
				const chunk = await reader.read();
				if (chunk.done) {
					break;
				}
				received += chunk.value.byteLength;
				if (received > limit) {
					throw new ModuleManifestFetchError(`${description} is larger than ${limit} bytes`, response.status);
				}
				chunks.push(chunk.value);
			}
		} finally {
			try {
				await reader.cancel();
			} catch (error) {
				this.onResponseCancellationError(`failed to cancel ${description} response stream`, error);
			}
		}
		return Buffer.concat(chunks.map((chunk) => Buffer.from(chunk.buffer, chunk.byteOffset, chunk.byteLength)));
	}
}
