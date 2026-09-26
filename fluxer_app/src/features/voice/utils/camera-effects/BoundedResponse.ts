// SPDX-License-Identifier: AGPL-3.0-or-later

export interface BoundedResponseBodyOptions {
	readonly maximumBytes: number;
	readonly maximumChunks: number;
	readonly description: string;
}

export interface ReadBoundedResponseBodyRequest extends BoundedResponseBodyOptions {
	readonly response: Response;
}

export interface ResponseDeadlineOptions<T> {
	readonly timeoutMilliseconds: number;
	readonly description: string;
	readonly signal: AbortSignal | null;
	readonly operation: (signal: AbortSignal) => Promise<T>;
}

export interface CancellableResponseBody {
	readonly body?: ReadableStream<Uint8Array> | null;
}

export interface CancelResponseBodyAndThrowRequest {
	readonly description: string;
	readonly error: unknown;
	readonly response: CancellableResponseBody;
}

export const BoundedResponseBodyLimit = Object.freeze({
	BYTES: 'bytes',
	CHUNKS: 'chunks',
} as const);

export type BoundedResponseBodyLimit = (typeof BoundedResponseBodyLimit)[keyof typeof BoundedResponseBodyLimit];

export class BoundedResponseBodyLimitError extends Error {
	readonly description: string;
	readonly limit: BoundedResponseBodyLimit;
	readonly maximum: number;

	constructor(description: string, limit: BoundedResponseBodyLimit, maximum: number) {
		super(`${description} exceeds ${maximum.toString()} ${limit}`);
		this.name = 'BoundedResponseBodyLimitError';
		this.description = description;
		this.limit = limit;
		this.maximum = maximum;
	}
}

class ResponseDeadlineExceededError extends Error {
	constructor(description: string, timeoutMilliseconds: number) {
		super(`${description} timed out after ${timeoutMilliseconds.toString()} ms`);
		this.name = 'ResponseDeadlineExceededError';
	}
}

interface BoundedResponseChunks {
	readonly chunks: Array<Uint8Array>;
	readonly totalBytes: number;
}

function responseChunkForStorage(chunk: Uint8Array): Uint8Array {
	const copy = new Uint8Array(chunk.byteLength);
	copy.set(chunk);
	return copy;
}

async function readBoundedResponseChunks(request: ReadBoundedResponseBodyRequest): Promise<BoundedResponseChunks> {
	if (request.response.body == null) {
		return {chunks: [], totalBytes: 0};
	}
	const reader = request.response.body.getReader();
	const chunks: Array<Uint8Array> = [];
	let totalBytes = 0;
	try {
		for (;;) {
			const result = await reader.read();
			if (result.done) {
				break;
			}
			if (chunks.length >= request.maximumChunks) {
				throw new BoundedResponseBodyLimitError(
					request.description,
					BoundedResponseBodyLimit.CHUNKS,
					request.maximumChunks,
				);
			}
			totalBytes += result.value.byteLength;
			if (!Number.isSafeInteger(totalBytes) || totalBytes > request.maximumBytes) {
				throw new BoundedResponseBodyLimitError(
					request.description,
					BoundedResponseBodyLimit.BYTES,
					request.maximumBytes,
				);
			}
			chunks.push(responseChunkForStorage(result.value));
		}
	} catch (error) {
		const failures: Array<unknown> = [error];
		try {
			await reader.cancel(error);
		} catch (cancelError) {
			failures.push(cancelError);
		}
		if (failures.length > 1) {
			throw new AggregateError(failures, `${request.description} read and cancellation failed`);
		}
		throw error;
	} finally {
		try {
			reader.releaseLock();
		} catch {}
	}
	return {chunks, totalBytes};
}

export async function readBoundedResponseBytes(request: ReadBoundedResponseBodyRequest): Promise<Uint8Array> {
	const {chunks, totalBytes} = await readBoundedResponseChunks(request);
	const bytes = new Uint8Array(totalBytes);
	let offset = 0;
	for (const chunk of chunks) {
		bytes.set(chunk, offset);
		offset += chunk.byteLength;
	}
	return bytes;
}

export async function readBoundedResponseArrayBuffer(request: ReadBoundedResponseBodyRequest): Promise<ArrayBuffer> {
	const bytes = await readBoundedResponseBytes(request);
	const buffer = new ArrayBuffer(bytes.byteLength);
	new Uint8Array(buffer).set(bytes);
	return buffer;
}

export async function readBoundedResponseBlob(request: ReadBoundedResponseBodyRequest): Promise<Blob> {
	const {chunks} = await readBoundedResponseChunks(request);
	const contentType = request.response.headers.get('content-type') ?? '';
	return new Blob(chunks as Array<BlobPart>, {type: contentType});
}

export async function cancelResponseBodyAndThrow({
	description,
	error,
	response,
}: CancelResponseBodyAndThrowRequest): Promise<never> {
	if (response.body != null) {
		try {
			await response.body.cancel(error);
		} catch (cancelError) {
			throw new AggregateError([error, cancelError], `${description} rejection and cancellation failed`);
		}
	}
	throw error;
}

export async function runWithResponseDeadline<T>(options: ResponseDeadlineOptions<T>): Promise<T> {
	const upstreamSignal = options.signal;
	if (upstreamSignal?.aborted) {
		throw upstreamSignal.reason ?? new Error(`${options.description} was aborted upstream`);
	}
	const controller = new AbortController();
	const abortFromUpstream = (): void => {
		controller.abort(upstreamSignal?.reason ?? new Error(`${options.description} was aborted upstream`));
	};
	if (upstreamSignal != null) {
		upstreamSignal.addEventListener('abort', abortFromUpstream, {once: true});
	}
	const timer = setTimeout(() => {
		controller.abort(new ResponseDeadlineExceededError(options.description, options.timeoutMilliseconds));
	}, options.timeoutMilliseconds);
	try {
		return await options.operation(controller.signal);
	} finally {
		clearTimeout(timer);
		if (upstreamSignal != null) {
			upstreamSignal.removeEventListener('abort', abortFromUpstream);
		}
	}
}
