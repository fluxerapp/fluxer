// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';
import * as FetchUtils from '../FetchUtils';

function createStream(chunks: Array<string>): ReadableStream<Uint8Array> {
	const encoder = new TextEncoder();
	let index = 0;
	return new ReadableStream<Uint8Array>({
		pull(controller) {
			if (index >= chunks.length) {
				controller.close();
				return;
			}
			controller.enqueue(encoder.encode(chunks[index]));
			index += 1;
		},
	});
}

describe('streamToBufferWithLimit', () => {
	it('returns the full response body when it is within the limit', async () => {
		const body = await FetchUtils.streamToBufferWithLimit(createStream(['hello', ' world']), {
			maxBytes: 32,
			description: 'Test response',
		});
		expect(new TextDecoder().decode(body)).toBe('hello world');
	});
	it('rejects when the declared content-length exceeds the configured limit', async () => {
		await expect(
			FetchUtils.streamToBufferWithLimit(createStream(['hello']), {
				maxBytes: 4,
				headers: new Headers({'Content-Length': '5'}),
				description: 'Test response',
			}),
		).rejects.toBeInstanceOf(FetchUtils.ResponseBodyTooLargeError);
	});
	it('rejects when the streamed body grows beyond the configured limit', async () => {
		await expect(
			FetchUtils.streamToBufferWithLimit(createStream(['hello', ' world']), {
				maxBytes: 5,
				description: 'Test response',
			}),
		).rejects.toBeInstanceOf(FetchUtils.ResponseBodyTooLargeError);
	});
	it('rejects an abort during a pending read instead of returning a partial response', async () => {
		const pendingRead = Promise.withResolvers<void>();
		const abort = new AbortController();
		const stream = new ReadableStream<Uint8Array>(
			{
				start(controller) {
					controller.enqueue(new TextEncoder().encode('partial'));
				},
				pull() {
					pendingRead.resolve();
				},
			},
			{highWaterMark: 0},
		);
		const result = FetchUtils.streamToBufferWithLimit(stream, {maxBytes: 32, signal: abort.signal});
		await pendingRead.promise;
		abort.abort();
		await expect(result).rejects.toMatchObject({name: 'AbortError'});
		expect(stream.locked).toBe(false);
	});

	it('rejects an oversized response without waiting for cancellation to finish', async () => {
		vi.useFakeTimers();
		const cancelled = Promise.withResolvers<void>();
		const cleanup = Promise.withResolvers<void>();
		const stream = new ReadableStream<Uint8Array>({
			start(controller) {
				controller.enqueue(new Uint8Array(5));
			},
			cancel() {
				cancelled.resolve();
				return cleanup.promise;
			},
		});
		const settled = vi.fn();
		const result = FetchUtils.streamToBufferWithLimit(stream, {maxBytes: 4}).then(settled, settled);
		try {
			await cancelled.promise;
			await vi.advanceTimersByTimeAsync(0);
			expect(settled).toHaveBeenCalledExactlyOnceWith(expect.any(FetchUtils.ResponseBodyTooLargeError));
			expect(stream.locked).toBe(false);
		} finally {
			cleanup.resolve();
			await result;
			vi.useRealTimers();
		}
	});
});
