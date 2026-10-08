// SPDX-License-Identifier: AGPL-3.0-or-later

import {Buffer} from 'node:buffer';
import fs from 'node:fs';
import path from 'node:path';
import {Readable} from 'node:stream';
import {isServedLocalAppExtension, localAppCacheControl, localAppContentType} from '@electron/main/LocalAppMime';
import {isLocalAppURL} from '@electron/main/LocalAppURL';
import {app} from 'electron';

const INDEX_HTML = 'index.html';
const RENDERER_DIRECTORY_SEGMENTS: ReadonlyArray<string> = Object.freeze(['dist', 'renderer']);
const NOT_FOUND_ERROR_CODE = 'ENOENT';
const LOCAL_APP_STREAMED_RESPONSES_IN_FLIGHT_MAX = 512;
const LOCAL_APP_BUFFERED_RESPONSE_MAX_BYTES = 256 * 1024;
const LOCAL_APP_BUFFERED_RESPONSE_BUDGET_BYTES = 64 * 1024 * 1024;

export const LocalAppFileResolution = Object.freeze({
	FILE: 'file',
	NOT_FOUND: 'not-found',
	BLOCKED: 'blocked',
} as const);

export type LocalAppFileResolution = (typeof LocalAppFileResolution)[keyof typeof LocalAppFileResolution];

export const LocalAppFileBlockReason = Object.freeze({
	INVALID_ORIGIN: 'invalid-origin',
	INVALID_PATH: 'invalid-path',
	PATH_ESCAPE: 'path-escape',
} as const);

export type LocalAppFileBlockReason = (typeof LocalAppFileBlockReason)[keyof typeof LocalAppFileBlockReason];

export interface LocalAppFile {
	readonly type: typeof LocalAppFileResolution.FILE;
	readonly filePath: string;
	readonly size: number;
	readonly cacheControl: string;
	readonly contentType: string;
}

export type LocalAppResolvedFile =
	| LocalAppFile
	| {readonly type: typeof LocalAppFileResolution.NOT_FOUND}
	| {readonly type: typeof LocalAppFileResolution.BLOCKED; readonly reason: LocalAppFileBlockReason};

interface DesktopLocalAppFilesOptions {
	readonly rendererRoot?: string;
}

interface LocalAppModuleIndex {
	readonly root: string;
	readonly files: ReadonlyMap<string, string>;
}

interface MissingLocalRendererIndexErrorContext {
	readonly root: string;
}

class MissingLocalRendererIndexError extends Error {
	constructor({root}: MissingLocalRendererIndexErrorContext) {
		super(`Local renderer root is missing ${INDEX_HTML}: ${root}`);
		this.name = 'MissingLocalRendererIndexError';
	}
}

class LocalRendererFileChangedDuringOpenError extends Error {
	constructor() {
		super('Local renderer file changed while it was being opened');
		this.name = 'LocalRendererFileChangedDuringOpenError';
	}
}

interface LocalRendererFileTooLargeErrorContext {
	readonly filePath: string;
	readonly maxBytes: number;
}

class LocalRendererFileTooLargeError extends RangeError {
	constructor({filePath, maxBytes}: LocalRendererFileTooLargeErrorContext) {
		super(`Local renderer file exceeds ${maxBytes} bytes: ${filePath}`);
		this.name = 'LocalRendererFileTooLargeError';
	}
}

class LocalRendererRewrittenFileTooLargeError extends RangeError {
	constructor(actualBytes: number, maxBytes: number) {
		super(`Rewritten local renderer file is ${actualBytes} bytes and exceeds the ${maxBytes} byte limit`);
		this.name = 'LocalRendererRewrittenFileTooLargeError';
	}
}

class InvalidLocalRendererFileSizeError extends RangeError {
	constructor() {
		super('Local renderer file has an invalid size');
		this.name = 'InvalidLocalRendererFileSizeError';
	}
}

class InvalidLocalRendererReservationError extends RangeError {
	constructor(amount: number) {
		super(`Local renderer resource reservation is not a positive integer: ${amount}`);
		this.name = 'InvalidLocalRendererReservationError';
	}
}

class LocalRendererFileResponseStateInvariantError extends Error {
	constructor() {
		super('Local renderer response ownership was transferred more than once');
		this.name = 'LocalRendererFileResponseStateInvariantError';
	}
}

class LocalAppResourcePermit {
	private releaseResource: (() => void) | null;

	constructor(releaseResource: () => void) {
		this.releaseResource = releaseResource;
	}

	release(): void {
		const releaseResource = this.releaseResource;
		if (releaseResource == null) {
			return;
		}
		this.releaseResource = null;
		releaseResource();
	}
}

interface LocalAppResourceWaiter {
	readonly amount: number;
	readonly admit: (permit: LocalAppResourcePermit) => void;
}

export class LocalAppResourceBudget {
	private used = 0;
	private draining = false;
	private readonly limit: number;
	private readonly waiting: Array<LocalAppResourceWaiter> = [];

	constructor(limit: number) {
		this.limit = limit;
	}

	get inUse(): number {
		return this.used;
	}

	get waitingCount(): number {
		return this.waiting.length;
	}

	acquire(amount: number, signal: AbortSignal): Promise<LocalAppResourcePermit> {
		if (!Number.isSafeInteger(amount) || amount <= 0) {
			return Promise.reject(new InvalidLocalRendererReservationError(amount));
		}
		if (signal.aborted) {
			return Promise.reject(signal.reason);
		}
		const required = Math.min(amount, this.limit);
		if (this.waiting.length === 0 && required <= this.limit - this.used) {
			return Promise.resolve(this.grant(required));
		}
		return new Promise<LocalAppResourcePermit>((resolve, reject) => {
			let waiter: LocalAppResourceWaiter;
			const onAbort = (): void => {
				const index = this.waiting.indexOf(waiter);
				if (index === -1) {
					return;
				}
				this.waiting.splice(index, 1);
				reject(signal.reason);
				this.drain();
			};
			const admit = (permit: LocalAppResourcePermit): void => {
				signal.removeEventListener('abort', onAbort);
				if (signal.aborted) {
					permit.release();
					reject(signal.reason);
					return;
				}
				resolve(permit);
			};
			waiter = {amount: required, admit};
			this.waiting.push(waiter);
			signal.addEventListener('abort', onAbort, {once: true});
		});
	}

	private grant(amount: number): LocalAppResourcePermit {
		this.used += amount;
		return new LocalAppResourcePermit(() => {
			this.used -= amount;
			this.drain();
		});
	}

	private drain(): void {
		if (this.draining) {
			return;
		}
		this.draining = true;
		try {
			while (this.waiting.length > 0) {
				const waiter = this.waiting[0];
				if (waiter == null || waiter.amount > this.limit - this.used) {
					return;
				}
				this.waiting.shift();
				waiter.admit(this.grant(waiter.amount));
			}
		} finally {
			this.draining = false;
		}
	}
}

class LocalAppFileResponseReservation {
	private permits: ReadonlyArray<LocalAppResourcePermit> | null;

	constructor(permits: ReadonlyArray<LocalAppResourcePermit>) {
		this.permits = permits;
	}

	release(): void {
		const permits = this.permits;
		if (permits == null) {
			return;
		}
		this.permits = null;
		for (const permit of permits) {
			permit.release();
		}
	}

	transfer(): LocalAppFileResponseReservation {
		if (this.permits == null) {
			throw new LocalRendererFileResponseStateInvariantError();
		}
		const transferred = new LocalAppFileResponseReservation(this.permits);
		this.permits = null;
		return transferred;
	}
}

export interface LocalAppPreparedResponseBody {
	readonly body: ReadableStream<Uint8Array> | null;
	readonly byteLength: number;
}

export interface LocalAppBufferedResponseFile {
	readonly contents: Buffer;
	prepareResponseBody(body: string, includeBody: boolean): LocalAppPreparedResponseBody;
	release(): void;
}

class BufferedLocalAppResponseFile implements LocalAppBufferedResponseFile {
	readonly contents: Buffer;
	private readonly maxBytes: number;
	private readonly reservation: LocalAppFileResponseReservation;
	private readonly signal: AbortSignal;

	constructor(contents: Buffer, maxBytes: number, reservation: LocalAppFileResponseReservation, signal: AbortSignal) {
		this.contents = contents;
		this.maxBytes = maxBytes;
		this.reservation = reservation;
		this.signal = signal;
	}

	prepareResponseBody(body: string, includeBody: boolean): LocalAppPreparedResponseBody {
		const byteLength = Buffer.byteLength(body);
		if (byteLength > this.maxBytes) {
			throw new LocalRendererRewrittenFileTooLargeError(byteLength, this.maxBytes);
		}
		if (!includeBody) {
			return {body: null, byteLength};
		}
		const bytes = Buffer.from(body);
		return {
			body: singleChunkResponseBody(bytes, this.reservation.transfer(), this.signal),
			byteLength,
		};
	}

	release(): void {
		this.reservation.release();
	}
}

export class DesktopLocalAppFiles {
	private readonly configuredRendererRoot: string | null;
	private readonly streamSlots = new LocalAppResourceBudget(LOCAL_APP_STREAMED_RESPONSES_IN_FLIGHT_MAX);
	private readonly bufferedBytes = new LocalAppResourceBudget(LOCAL_APP_BUFFERED_RESPONSE_BUDGET_BYTES);
	private rendererRootInitialization: Promise<string | null> | null = null;
	private moduleIndex: LocalAppModuleIndex | null = null;

	constructor(options?: DesktopLocalAppFilesOptions) {
		this.configuredRendererRoot = options?.rendererRoot ?? null;
	}

	setModuleIndex(moduleIndex: LocalAppModuleIndex | null): void {
		this.moduleIndex = moduleIndex;
	}

	async resolve(requestURL: string): Promise<LocalAppResolvedFile> {
		if (!isLocalAppURL(requestURL)) {
			return {type: LocalAppFileResolution.BLOCKED, reason: LocalAppFileBlockReason.INVALID_ORIGIN};
		}
		const relativePath = getSafeRelativePath(new URL(requestURL).pathname);
		if (relativePath == null) {
			return {type: LocalAppFileResolution.BLOCKED, reason: LocalAppFileBlockReason.INVALID_PATH};
		}
		const moduleFile = await this.resolveModuleFile(relativePath);
		if (moduleFile != null) {
			return moduleFile;
		}
		const root = await this.resolveRendererRoot();
		if (root == null && (this.moduleIndex?.files.size ?? 0) === 0) {
			throw new MissingLocalRendererIndexError({root: this.rendererRootPath()});
		}
		if (root != null) {
			const requestedPath = path.resolve(root, relativePath);
			if (!isPathInsideRoot(root, requestedPath)) {
				return {type: LocalAppFileResolution.BLOCKED, reason: LocalAppFileBlockReason.PATH_ESCAPE};
			}
			const requestedFile = await statFile(requestedPath);
			if (requestedFile != null) {
				return describeFile(requestedPath, requestedFile.size);
			}
		}
		if (isServedLocalAppExtension(relativePath)) {
			return {type: LocalAppFileResolution.NOT_FOUND};
		}
		const moduleIndexFile = await this.resolveModuleFile(INDEX_HTML);
		if (moduleIndexFile != null) {
			return moduleIndexFile;
		}
		if (root == null) {
			return {type: LocalAppFileResolution.NOT_FOUND};
		}
		const indexPath = path.join(root, INDEX_HTML);
		const indexFile = await statFile(indexPath);
		if (indexFile == null) {
			throw new MissingLocalRendererIndexError({root});
		}
		return describeFile(indexPath, indexFile.size);
	}

	async readForResponse(
		filePath: string,
		maxBytes: number,
		signal: AbortSignal,
	): Promise<LocalAppBufferedResponseFile> {
		signal.throwIfAborted();
		const contents = await readLocalRendererFile(filePath, maxBytes);
		return new BufferedLocalAppResponseFile(contents, maxBytes, new LocalAppFileResponseReservation([]), signal);
	}

	async openStream(filePath: string, expectedSize: number, signal: AbortSignal): Promise<ReadableStream<Uint8Array>> {
		signal.throwIfAborted();
		if (expectedSize <= LOCAL_APP_BUFFERED_RESPONSE_MAX_BYTES) {
			return await this.openBufferedBody(filePath, expectedSize, signal);
		}
		const responseSlot = await this.streamSlots.acquire(1, signal);
		let file: fs.promises.FileHandle | null = null;
		try {
			signal.throwIfAborted();
			file = await fs.promises.open(filePath, readOnlyOpenFlags());
			const stats = await file.stat();
			if (!stats.isFile() || stats.size !== expectedSize) {
				throw new LocalRendererFileChangedDuringOpenError();
			}
			const stream = Readable.toWeb(file.createReadStream({signal})) as unknown as ReadableStream<Uint8Array>;
			file = null;
			return holdReservationForStream(stream, new LocalAppFileResponseReservation([responseSlot]), signal);
		} catch (error) {
			let failure = error;
			if (file != null) {
				try {
					await file.close();
				} catch (closeError) {
					failure = new AggregateError([error, closeError], 'Local renderer file open and cleanup failed');
				}
			}
			responseSlot.release();
			throw failure;
		}
	}

	isIndexFile(filePath: string): boolean {
		return path.basename(filePath) === INDEX_HTML;
	}

	private async openBufferedBody(
		filePath: string,
		expectedSize: number,
		signal: AbortSignal,
	): Promise<ReadableStream<Uint8Array>> {
		const bufferPermit = await this.bufferedBytes.acquire(Math.max(1, expectedSize), signal);
		try {
			signal.throwIfAborted();
			const contents = await readExactLocalRendererFile(filePath, expectedSize);
			return singleChunkResponseBody(contents, new LocalAppFileResponseReservation([bufferPermit]), signal);
		} catch (error) {
			bufferPermit.release();
			throw error;
		}
	}

	private async resolveModuleFile(relativePath: string): Promise<LocalAppResolvedFile | null> {
		const moduleIndex = this.moduleIndex;
		if (moduleIndex == null) {
			return null;
		}
		const modulePath = moduleIndex.files.get(getModuleIndexKey(relativePath));
		if (modulePath == null) {
			return null;
		}
		const requestedPath = path.resolve(modulePath);
		if (!isPathInsideRoot(moduleIndex.root, requestedPath)) {
			return {type: LocalAppFileResolution.BLOCKED, reason: LocalAppFileBlockReason.PATH_ESCAPE};
		}
		const requestedFile = await statFile(requestedPath);
		if (requestedFile == null) {
			return null;
		}
		return describeFile(requestedPath, requestedFile.size);
	}

	private rendererRootPath(): string {
		return this.configuredRendererRoot ?? path.join(app.getAppPath(), ...RENDERER_DIRECTORY_SEGMENTS);
	}

	private resolveRendererRoot(): Promise<string | null> {
		this.rendererRootInitialization ??= resolveOfflineRendererRoot(this.rendererRootPath()).catch((error: unknown) => {
			this.rendererRootInitialization = null;
			throw error;
		});
		return this.rendererRootInitialization;
	}
}

async function readLocalRendererFile(filePath: string, maxBytes: number): Promise<Buffer> {
	const file = await fs.promises.open(filePath, readOnlyOpenFlags());
	try {
		const stats = await file.stat();
		if (!stats.isFile()) {
			throw new LocalRendererFileChangedDuringOpenError();
		}
		if (!Number.isSafeInteger(stats.size) || stats.size < 0) {
			throw new InvalidLocalRendererFileSizeError();
		}
		if (stats.size > maxBytes) {
			throw new LocalRendererFileTooLargeError({filePath, maxBytes});
		}
		return await readFileContents(file, stats.size);
	} finally {
		await file.close();
	}
}

async function readExactLocalRendererFile(filePath: string, expectedSize: number): Promise<Buffer> {
	const file = await fs.promises.open(filePath, readOnlyOpenFlags());
	try {
		const stats = await file.stat();
		if (!stats.isFile() || stats.size !== expectedSize) {
			throw new LocalRendererFileChangedDuringOpenError();
		}
		return await readFileContents(file, expectedSize);
	} finally {
		await file.close();
	}
}

async function readFileContents(file: fs.promises.FileHandle, size: number): Promise<Buffer> {
	const contents = Buffer.alloc(size);
	let offset = 0;
	while (offset < size) {
		const {bytesRead} = await file.read(contents, offset, size - offset, offset);
		if (bytesRead === 0) {
			throw new LocalRendererFileChangedDuringOpenError();
		}
		offset += bytesRead;
	}
	return contents;
}

function singleChunkResponseBody(
	bytes: Uint8Array,
	reservation: LocalAppFileResponseReservation,
	signal: AbortSignal,
): ReadableStream<Uint8Array> {
	let enqueued = false;
	let settled = false;
	let controller: ReadableStreamDefaultController<Uint8Array> | null = null;
	const settle = (): void => {
		if (settled) {
			return;
		}
		settled = true;
		signal.removeEventListener('abort', onAbort);
		reservation.release();
	};
	const onAbort = (): void => {
		if (settled) {
			return;
		}
		try {
			controller?.error(signal.reason);
		} finally {
			settle();
		}
	};
	return new ReadableStream<Uint8Array>({
		start(streamController) {
			controller = streamController;
			if (signal.aborted) {
				onAbort();
				return;
			}
			signal.addEventListener('abort', onAbort, {once: true});
		},
		pull(streamController) {
			if (settled) {
				return;
			}
			try {
				if (!enqueued) {
					enqueued = true;
					streamController.enqueue(bytes);
					return;
				}
				streamController.close();
			} catch (error) {
				settle();
				throw error;
			}
			settle();
		},
		cancel() {
			settle();
		},
	});
}

function holdReservationForStream(
	stream: ReadableStream<Uint8Array>,
	reservation: LocalAppFileResponseReservation,
	signal: AbortSignal,
): ReadableStream<Uint8Array> {
	const reader = stream.getReader();
	let settled = false;
	let controller: ReadableStreamDefaultController<Uint8Array> | null = null;
	const settle = (): void => {
		if (settled) {
			return;
		}
		settled = true;
		signal.removeEventListener('abort', onAbort);
		reservation.release();
	};
	const onAbort = (): void => {
		if (settled) {
			return;
		}
		try {
			controller?.error(signal.reason);
		} finally {
			settle();
		}
	};
	return new ReadableStream<Uint8Array>({
		start(streamController) {
			controller = streamController;
			if (signal.aborted) {
				onAbort();
				return;
			}
			signal.addEventListener('abort', onAbort, {once: true});
		},
		async pull(streamController) {
			if (settled) {
				return;
			}
			try {
				const chunk = await reader.read();
				if (settled) {
					return;
				}
				if (chunk.done) {
					try {
						streamController.close();
					} finally {
						settle();
					}
					return;
				}
				streamController.enqueue(chunk.value);
			} catch (error) {
				if (!settled) {
					try {
						streamController.error(error);
					} finally {
						settle();
					}
				}
			}
		},
		async cancel(reason) {
			try {
				await reader.cancel(reason);
			} finally {
				settle();
			}
		},
	});
}

function describeFile(filePath: string, size: number): LocalAppFile {
	return {
		type: LocalAppFileResolution.FILE,
		filePath,
		size,
		cacheControl: localAppCacheControl(filePath),
		contentType: localAppContentType(filePath),
	};
}

function getModuleIndexKey(relativePath: string): string {
	return path.sep === '/' ? relativePath : relativePath.split(path.sep).join('/');
}

function readOnlyOpenFlags(): number {
	const noFollow = typeof fs.constants.O_NOFOLLOW === 'number' ? fs.constants.O_NOFOLLOW : 0;
	return fs.constants.O_RDONLY | noFollow;
}

function getSafeRelativePath(rawPathname: string): string | null {
	let decodedPathname: string;
	try {
		decodedPathname = decodeURIComponent(rawPathname);
	} catch {
		return null;
	}
	if (decodedPathname.includes('\0') || decodedPathname.includes('\\')) {
		return null;
	}
	const strippedPathname = decodedPathname.replace(/^\/+/u, '');
	if (strippedPathname.length === 0) {
		return INDEX_HTML;
	}
	const normalized = path.normalize(strippedPathname);
	if (normalized === '.' || normalized === '..' || normalized.startsWith(`..${path.sep}`)) {
		return null;
	}
	if (path.isAbsolute(normalized)) {
		return null;
	}
	return normalized;
}

async function resolveOfflineRendererRoot(root: string): Promise<string | null> {
	const indexFile = await statFile(path.join(root, INDEX_HTML));
	return indexFile == null ? null : root;
}

async function statFile(filePath: string): Promise<fs.Stats | null> {
	let stats: fs.Stats;
	try {
		stats = await fs.promises.lstat(filePath);
	} catch (error) {
		if (isNotFoundError(error)) {
			return null;
		}
		throw error;
	}
	if (!stats.isFile() || stats.isSymbolicLink()) {
		return null;
	}
	if (!Number.isSafeInteger(stats.size) || stats.size < 0) {
		throw new InvalidLocalRendererFileSizeError();
	}
	return stats;
}

function isNotFoundError(error: unknown): boolean {
	if (typeof error !== 'object' || error === null || !('code' in error)) {
		return false;
	}
	return error.code === NOT_FOUND_ERROR_CODE;
}

function isPathInsideRoot(root: string, candidate: string): boolean {
	const relative = path.relative(root, candidate);
	return relative === '' || (!relative.startsWith('..') && !path.isAbsolute(relative));
}
