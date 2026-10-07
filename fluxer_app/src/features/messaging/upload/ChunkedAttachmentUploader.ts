// SPDX-License-Identifier: AGPL-3.0-or-later

import {wrapDesktopLocalUploadURL} from '@app/features/messaging/utils/DesktopResourceUrl';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ExponentialBackoff} from '@app/features/platform/utils/RetryScheduler';

const logger = new Logger('ChunkedAttachmentUploader');
const MAX_CONCURRENT_PARTS = 4;
const PART_MAX_ATTEMPTS = 4;
const PART_RETRY_MIN_DELAY_MS = 500;
const PART_RETRY_MAX_DELAY_MS = 8000;
const PART_RETRY_HARD_CAP_MS = 30_000;

export interface ChunkedUploadPart {
	partNumber: number;
	uploadUrl: string;
}

export interface ChunkedUploadPlan {
	file: File;
	contentType: string;
	partSize: number;
	parts: Array<ChunkedUploadPart>;
}

export interface ChunkedUploadHooks {
	signal: AbortSignal;
	onProgress?: (uploadedBytes: number, totalBytes: number) => void;
}

function isAbortError(error: unknown): boolean {
	return error instanceof DOMException && error.name === 'AbortError';
}

function isTransientError(error: unknown): boolean {
	if (error instanceof DOMException && error.name === 'TimeoutError') return true;
	if (error instanceof HttpError) {
		const status = error.status;
		if (typeof status !== 'number') return true;
		return status === 408 || status === 425 || status === 429 || status >= 500;
	}
	if (error instanceof Error && error.message === 'Network error during request') return true;
	return false;
}

function parseRetryAfterMs(error: unknown): number | null {
	if (!(error instanceof HttpError)) return null;
	const raw = error.responseHeaders['retry-after'] ?? error.responseHeaders['Retry-After'];
	if (!raw) return null;
	const seconds = Number(raw);
	if (Number.isFinite(seconds) && seconds >= 0) {
		return Math.min(seconds * 1000, PART_RETRY_HARD_CAP_MS);
	}
	const dateMs = Date.parse(raw);
	if (!Number.isNaN(dateMs)) {
		return Math.max(0, Math.min(dateMs - Date.now(), PART_RETRY_HARD_CAP_MS));
	}
	return null;
}

interface PartAttemptResult {
	partNumber: number;
}

class ConcurrencyGate {
	private active = 0;
	private waiters: Array<() => void> = [];

	constructor(private readonly limit: number) {}

	async acquire(): Promise<void> {
		if (this.active < this.limit) {
			this.active++;
			return;
		}
		await new Promise<void>((resolve) => this.waiters.push(resolve));
		this.active++;
	}

	release(): void {
		this.active--;
		const next = this.waiters.shift();
		if (next) next();
	}
}

export async function uploadFileInChunks(plan: ChunkedUploadPlan, hooks: ChunkedUploadHooks): Promise<void> {
	const {file, contentType, partSize, parts} = plan;
	const {signal, onProgress} = hooks;
	if (signal.aborted) {
		throw new DOMException('Upload aborted', 'AbortError');
	}
	const totalBytes = file.size;
	const loadedBytesByPart = new Array<number>(parts.length).fill(0);
	const reportProgress = () => {
		if (!onProgress) return;
		let sum = 0;
		for (const bytes of loadedBytesByPart) sum += bytes;
		onProgress(Math.min(sum, totalBytes), totalBytes);
	};
	const gate = new ConcurrencyGate(MAX_CONCURRENT_PARTS);
	let firstError: unknown;
	const groupAbort = new AbortController();
	const externalAbortHandler = () => groupAbort.abort();
	if (signal.aborted) {
		groupAbort.abort();
	} else {
		signal.addEventListener('abort', externalAbortHandler, {once: true});
	}
	try {
		await Promise.all(
			parts.map(async (part, index): Promise<PartAttemptResult> => {
				const offset = index * partSize;
				const end = Math.min(offset + partSize, file.size);
				const chunk = file.slice(offset, end);
				await gate.acquire();
				try {
					if (groupAbort.signal.aborted) {
						throw new DOMException('Upload aborted', 'AbortError');
					}
					const backoff = new ExponentialBackoff({
						minDelay: PART_RETRY_MIN_DELAY_MS,
						maxDelay: PART_RETRY_MAX_DELAY_MS,
						maxNumOfAttempts: PART_MAX_ATTEMPTS,
						jitter: true,
					});
					let attempt = 0;
					while (true) {
						attempt++;
						try {
							await http.put(wrapDesktopLocalUploadURL(part.uploadUrl), {
								body: chunk,
								auth: 'none',
								headers: {
									'Content-Type': contentType,
								},
								signal: groupAbort.signal,
								onProgress: (event) => {
									const loaded = Math.min(chunk.size, event.loaded);
									if (loaded > loadedBytesByPart[index]) {
										loadedBytesByPart[index] = loaded;
										reportProgress();
									}
								},
							});
							loadedBytesByPart[index] = chunk.size;
							reportProgress();
							return {partNumber: part.partNumber};
						} catch (error) {
							if (isAbortError(error)) {
								throw error;
							}
							if (attempt >= PART_MAX_ATTEMPTS || !isTransientError(error)) {
								logger.warn(
									`Part ${part.partNumber} failed after ${attempt} ${attempt === 1 ? 'attempt' : 'attempts'}; giving up`,
									error,
								);
								throw error;
							}
							const backoffDelay = backoff.next();
							const retryAfter = parseRetryAfterMs(error);
							const delay = retryAfter !== null ? Math.max(retryAfter, backoffDelay) : backoffDelay;
							logger.info(
								`Part ${part.partNumber} attempt ${attempt} failed, retrying in ${Math.round(delay)}ms${
									retryAfter !== null ? ' (server hint)' : ''
								}`,
								error,
							);
							loadedBytesByPart[index] = 0;
							reportProgress();
							await sleepOrAbort(delay, groupAbort.signal);
						}
					}
				} catch (error) {
					if (!firstError) {
						firstError = error;
						if (!isAbortError(error)) {
							groupAbort.abort();
						}
					}
					throw error;
				} finally {
					gate.release();
				}
			}),
		);
	} finally {
		signal.removeEventListener('abort', externalAbortHandler);
	}
	if (firstError) {
		throw firstError;
	}
}

function sleepOrAbort(ms: number, signal: AbortSignal): Promise<void> {
	return new Promise((resolve, reject) => {
		if (signal.aborted) {
			reject(new DOMException('Upload aborted', 'AbortError'));
			return;
		}
		const timer = window.setTimeout(() => {
			signal.removeEventListener('abort', onAbort);
			resolve();
		}, ms);
		const onAbort = () => {
			window.clearTimeout(timer);
			reject(new DOMException('Upload aborted', 'AbortError'));
		};
		signal.addEventListener('abort', onAbort, {once: true});
	});
}
