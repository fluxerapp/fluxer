// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ModulePackageHashMismatchError,
	ModulePackageIOError,
	ModulePackageMalformedError,
} from '@electron/main/ModulePackage';
import {
	type ModulePackageDownload,
	type ModuleStore,
	ModuleStoreDownloadHashMismatchError,
} from '@electron/main/ModuleStore';
import type {ModulePlanItem} from '@electron/main/ModuleUpdatePlanner';

const MODULE_PACKAGE_HASH_MISMATCH_RETRIES = 1;
const MODULE_PACKAGE_HEADER_TIMEOUT_MS = 30000;
export const MODULE_PACKAGE_STALL_TIMEOUT_MS = 30000;
const MODULE_PACKAGE_PROGRESS_INTERVAL_MS = 250;
const TRANSIENT_HTTP_STATUS_FLOOR = 500;
const TRANSIENT_PACKAGE_HTTP_STATUSES: ReadonlySet<number> = new Set([408, 425, 429]);
const CAUSE_CHAIN_LIMIT = 8;
const MODULE_LOOPBACK_HOSTS: ReadonlySet<string> = new Set(['127.0.0.1', 'localhost']);
const DETERMINISTIC_PACKAGE_IO_CODES: ReadonlySet<string> = new Set(['EEXIST', 'EISDIR', 'ENOTDIR']);
const HTTP_PARTIAL_CONTENT = 206;
const HTTP_RANGE_NOT_SATISFIABLE = 416;
const CONTENT_RANGE_PATTERN = /^bytes (\d+)-(\d+)\/(\d+|\*)$/u;

export const ModulePackageInstallPhase = Object.freeze({
	DOWNLOADING: 'downloading',
	INSTALLING: 'installing',
} as const);

export type ModulePackageInstallPhase = (typeof ModulePackageInstallPhase)[keyof typeof ModulePackageInstallPhase];

interface ModulePackageInstallProgress {
	readonly phase: ModulePackageInstallPhase;
	readonly current: number;
	readonly total: number;
	readonly moduleName: string;
	readonly progress: number;
	readonly receivedBytes: number;
	readonly totalBytes: number;
}

export const ModulePackageInstallerReportType = Object.freeze({
	HASH_MISMATCH: 'hash-mismatch',
	PACKAGE_MISSING: 'package-missing',
	PACKAGE_REJECTED: 'package-rejected',
	NETWORK_ERROR: 'network-error',
} as const);

export type ModulePackageInstallerReport =
	| {
			readonly type: typeof ModulePackageInstallerReportType.HASH_MISMATCH;
			readonly module: string;
			readonly message: string;
			readonly error: unknown;
	  }
	| {
			readonly type: typeof ModulePackageInstallerReportType.PACKAGE_MISSING;
			readonly module: string;
			readonly message: string;
			readonly error: unknown;
	  }
	| {
			readonly type: typeof ModulePackageInstallerReportType.PACKAGE_REJECTED;
			readonly module: string;
			readonly message: string;
			readonly error: unknown;
	  }
	| {
			readonly type: typeof ModulePackageInstallerReportType.NETWORK_ERROR;
			readonly module: string;
			readonly message: string;
			readonly error: unknown;
	  };

interface ModulePackageInstallerOptions {
	readonly store: ModuleStore;
	readonly packageOrigin: string;
	readonly fetch: typeof globalThis.fetch;
	readonly onProgress: (progress: ModulePackageInstallProgress) => void;
	readonly now?: () => number;
	readonly report: (report: ModulePackageInstallerReport) => void;
}

export class ModulePackageFetchError extends Error {
	public readonly module: string;
	public readonly status: number | null;

	public constructor(moduleName: string, status: number | null, message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'ModulePackageFetchError';
		this.module = moduleName;
		this.status = status;
	}
}

export class ModulePackageStallError extends ModulePackageFetchError {
	public constructor(moduleName: string, stallMs: number) {
		super(moduleName, null, `package download stalled for ${stallMs}ms`);
		this.name = 'ModulePackageStallError';
	}
}

export function isModulePackageStall(error: unknown): boolean {
	return findCause(error, ModulePackageStallError) != null;
}

class ModulePackageUrlPolicyError extends Error {
	public readonly module: string;

	public constructor(moduleName: string, url: string) {
		super(`module package url is not https: ${url}`);
		this.name = 'ModulePackageUrlPolicyError';
		this.module = moduleName;
	}
}

function errorMessage(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

export function describeErrorChain(error: unknown): string {
	const messages: Array<string> = [];
	let cursor: unknown = error;
	for (let depth = 0; depth < CAUSE_CHAIN_LIMIT && cursor != null; depth += 1) {
		const message = errorMessage(cursor);
		if (message.length > 0 && !messages.includes(message)) {
			messages.push(message);
		}
		cursor = (cursor as {cause?: unknown}).cause;
	}
	return messages.join(': ');
}

export function isTransientServerStatus(status: number | null): boolean {
	return status == null || status >= TRANSIENT_HTTP_STATUS_FLOOR || TRANSIENT_PACKAGE_HTTP_STATUSES.has(status);
}

function findCause<T>(error: unknown, kind: abstract new (...args: Array<never>) => T): T | null {
	let cursor: unknown = error;
	for (let depth = 0; depth < CAUSE_CHAIN_LIMIT && cursor != null; depth += 1) {
		if (cursor instanceof kind) {
			return cursor;
		}
		cursor = (cursor as {cause?: unknown}).cause;
	}
	return null;
}

function isPackageHashMismatch(error: unknown): boolean {
	return (
		findCause(error, ModuleStoreDownloadHashMismatchError) != null ||
		findCause(error, ModulePackageHashMismatchError) != null
	);
}

function isDeterministicPackageIOFailure(error: unknown): boolean {
	const failure = findCause(error, ModulePackageIOError);
	if (failure == null) {
		return false;
	}
	const code = (failure.cause as {code?: unknown} | null | undefined)?.code;
	return typeof code === 'string' && DETERMINISTIC_PACKAGE_IO_CODES.has(code);
}

export function isDeterministicPackageFailure(error: unknown): boolean {
	if (
		isPackageHashMismatch(error) ||
		findCause(error, ModulePackageMalformedError) != null ||
		findCause(error, ModulePackageUrlPolicyError) != null ||
		isDeterministicPackageIOFailure(error)
	) {
		return true;
	}
	const fetchError = findCause(error, ModulePackageFetchError);
	return fetchError != null && !isTransientServerStatus(fetchError.status);
}

export class ModulePackageInstaller {
	private readonly store: ModuleStore;
	private readonly packageOrigin: string;
	private readonly fetchImplementation: typeof globalThis.fetch;
	private readonly onProgress: (progress: ModulePackageInstallProgress) => void;
	private readonly now: () => number;
	private readonly reporter: (report: ModulePackageInstallerReport) => void;

	public constructor(options: ModulePackageInstallerOptions) {
		this.store = options.store;
		this.packageOrigin = options.packageOrigin;
		this.fetchImplementation = options.fetch;
		this.onProgress = options.onProgress;
		this.now = options.now ?? Date.now;
		this.reporter = options.report;
	}

	public async install(item: ModulePlanItem, current: number, total: number): Promise<void> {
		for (let attempt = 0; ; attempt += 1) {
			try {
				await this.store.installModule({
					module: item.module,
					sha256: item.entry.sha256,
					download: (resumeFrom) => this.openPackageStream(item, current, total, resumeFrom),
				});
				this.onProgress({
					phase: ModulePackageInstallPhase.INSTALLING,
					current,
					total,
					moduleName: item.module,
					progress: Math.round((current / total) * 100),
					receivedBytes: item.entry.bytes,
					totalBytes: item.entry.bytes,
				});
				return;
			} catch (error) {
				if (isPackageHashMismatch(error)) {
					this.reporter({
						type: ModulePackageInstallerReportType.HASH_MISMATCH,
						module: item.module,
						message: errorMessage(error),
						error,
					});
				} else if (findCause(error, ModulePackageFetchError)?.status != null) {
					this.reporter({
						type: ModulePackageInstallerReportType.PACKAGE_MISSING,
						module: item.module,
						message: errorMessage(error),
						error,
					});
				} else if (isDeterministicPackageFailure(error)) {
					this.reporter({
						type: ModulePackageInstallerReportType.PACKAGE_REJECTED,
						module: item.module,
						message: describeErrorChain(error),
						error,
					});
				}
				if (attempt >= MODULE_PACKAGE_HASH_MISMATCH_RETRIES || !isPackageHashMismatch(error)) {
					throw error;
				}
			}
		}
	}

	private async openPackageStream(
		item: ModulePlanItem,
		current: number,
		total: number,
		resumeFrom: number,
	): Promise<ModulePackageDownload> {
		const url = this.requirePackageUrl(item);
		const offset = resumeFrom > 0 && resumeFrom <= item.entry.bytes ? resumeFrom : 0;
		this.onProgress({
			phase: ModulePackageInstallPhase.DOWNLOADING,
			current,
			total,
			moduleName: item.module,
			progress: item.entry.bytes === 0 ? 0 : Math.floor((offset / item.entry.bytes) * 100),
			receivedBytes: offset,
			totalBytes: item.entry.bytes,
		});
		if (offset > 0 && offset === item.entry.bytes) {
			return {offset, chunks: []};
		}
		const controller = new AbortController();
		const response = await this.requestPackage(item, url, controller, offset);
		const resumed = offset > 0 && response.status === HTTP_PARTIAL_CONTENT;
		if (resumed && parseContentRangeStart(response.headers.get('content-range')) !== offset) {
			const failure = new ModulePackageFetchError(
				item.module,
				null,
				`package response resumed at an unexpected range: ${response.headers.get('content-range') ?? 'none'}`,
			);
			await this.cancelFailedPackageResponse(response, controller, failure);
			return this.openPackageStream(item, current, total, 0);
		}
		if (offset > 0 && response.status === HTTP_RANGE_NOT_SATISFIABLE) {
			const failure = new ModulePackageFetchError(item.module, response.status, 'package range was not satisfiable');
			await this.cancelFailedPackageResponse(response, controller, failure);
			return this.openPackageStream(item, current, total, 0);
		}
		if (!response.ok) {
			const failure = new ModulePackageFetchError(
				item.module,
				response.status,
				`package request returned ${response.status}`,
			);
			await this.cancelFailedPackageResponse(response, controller, failure);
			throw failure;
		}
		if (response.body == null) {
			const failure = new ModulePackageFetchError(item.module, response.status, 'package response had no body');
			await this.cancelFailedPackageResponse(response, controller, failure);
			throw failure;
		}
		const start = resumed ? offset : 0;
		return {offset: start, chunks: this.readPackageBody(response.body, controller, start, item, current, total)};
	}

	private async requestPackage(
		item: ModulePlanItem,
		url: string,
		controller: AbortController,
		offset: number,
	): Promise<Response> {
		const headerTimeoutError = new ModulePackageFetchError(
			item.module,
			null,
			`package response headers exceeded ${MODULE_PACKAGE_HEADER_TIMEOUT_MS}ms`,
		);
		const headerTimeout = setTimeout(() => controller.abort(headerTimeoutError), MODULE_PACKAGE_HEADER_TIMEOUT_MS);
		const headers: Record<string, string> = {accept: 'application/octet-stream'};
		if (offset > 0) {
			headers['range'] = `bytes=${offset}-`;
		}
		try {
			return await this.fetchImplementation(url, {signal: controller.signal, headers});
		} catch (error) {
			const failure =
				controller.signal.reason instanceof ModulePackageFetchError
					? controller.signal.reason
					: new ModulePackageFetchError(item.module, null, `failed to reach ${url}`, {cause: error});
			controller.abort(failure);
			throw failure;
		} finally {
			clearTimeout(headerTimeout);
		}
	}

	private async cancelFailedPackageResponse(
		response: Response,
		controller: AbortController,
		failure: ModulePackageFetchError,
	): Promise<void> {
		controller.abort(failure);
		if (response.body == null) {
			return;
		}
		try {
			await response.body.cancel(failure);
		} catch (error) {
			this.reporter({
				type: ModulePackageInstallerReportType.NETWORK_ERROR,
				module: failure.module,
				message: `failed to cancel package response stream for ${failure.module}`,
				error,
			});
		}
	}

	private async *readPackageBody(
		body: ReadableStream<Uint8Array>,
		controller: AbortController,
		start: number,
		item: ModulePlanItem,
		current: number,
		total: number,
	): AsyncGenerator<Uint8Array> {
		let reader: ReadableStreamDefaultReader<Uint8Array> | null = null;
		let received = start;
		let reported = item.entry.bytes === 0 ? 0 : Math.floor((start / item.entry.bytes) * 100);
		let reportedAt = this.now();
		let completed = false;
		let transferFailure: unknown = null;
		let stall: NodeJS.Timeout | null = null;
		const arm = (): void => {
			stall = setTimeout(() => {
				controller.abort(new ModulePackageStallError(item.module, MODULE_PACKAGE_STALL_TIMEOUT_MS));
			}, MODULE_PACKAGE_STALL_TIMEOUT_MS);
		};
		const disarm = (): void => {
			if (stall != null) {
				clearTimeout(stall);
				stall = null;
			}
		};
		try {
			reader = body.getReader();
			for (;;) {
				arm();
				const chunk = await reader.read();
				disarm();
				if (chunk.done) {
					completed = true;
					break;
				}
				received += chunk.value.byteLength;
				if (received > item.entry.bytes) {
					throw new ModulePackageFetchError(
						item.module,
						null,
						`package body exceeded the declared ${item.entry.bytes} bytes`,
					);
				}
				const percent = item.entry.bytes === 0 ? 100 : Math.floor((received / item.entry.bytes) * 100);
				const at = this.now();
				if (percent > reported || at - reportedAt >= MODULE_PACKAGE_PROGRESS_INTERVAL_MS) {
					reported = percent;
					reportedAt = at;
					this.onProgress({
						phase: ModulePackageInstallPhase.DOWNLOADING,
						current,
						total,
						moduleName: item.module,
						progress: percent,
						receivedBytes: received,
						totalBytes: item.entry.bytes,
					});
				}
				yield chunk.value;
			}
		} catch (error) {
			transferFailure =
				controller.signal.reason instanceof ModulePackageFetchError
					? controller.signal.reason
					: error instanceof ModulePackageFetchError
						? error
						: new ModulePackageFetchError(item.module, null, `package download failed for ${item.module}`, {
								cause: error,
							});
			throw transferFailure;
		} finally {
			disarm();
			if (completed && reader != null) {
				reader.releaseLock();
			} else {
				controller.abort(transferFailure ?? undefined);
				try {
					if (reader == null) {
						await body.cancel(transferFailure);
					} else {
						await reader.cancel(transferFailure);
					}
				} catch (error) {
					this.reporter({
						type: ModulePackageInstallerReportType.NETWORK_ERROR,
						module: item.module,
						message: `failed to cancel package response stream for ${item.module}`,
						error,
					});
				}
			}
		}
	}

	private requirePackageUrl(item: ModulePlanItem): string {
		const url = new URL(item.entry.url);
		if (url.protocol === 'https:') {
			return url.toString();
		}
		const base = new URL(this.packageOrigin);
		if (url.protocol === 'http:' && url.origin === base.origin && MODULE_LOOPBACK_HOSTS.has(base.hostname)) {
			return url.toString();
		}
		throw new ModulePackageUrlPolicyError(item.module, item.entry.url);
	}
}

function parseContentRangeStart(value: string | null): number | null {
	if (value == null) {
		return null;
	}
	const match = CONTENT_RANGE_PATTERN.exec(value.trim());
	if (match == null) {
		return null;
	}
	const start = Number(match[1]);
	return Number.isSafeInteger(start) ? start : null;
}
