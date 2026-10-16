// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {constants, createZstdCompress, type ZstdCompress} from 'node:zlib';
import {GatewayCompression, isGatewayCompressionError} from '@app/features/gateway/transport/GatewayCompression';
import {initSync} from '@pkgs/libfluxcore/libfluxcore';
import {beforeAll, describe, expect, it} from 'vitest';

const wasmBytes = readFileSync(new URL('../../../../pkgs/libfluxcore/libfluxcore_bg.wasm', import.meta.url));
const STREAM_MESSAGE_COUNT = 10_000;

describe('GatewayCompression', () => {
	beforeAll(() => {
		initSync({module: wasmBytes});
	});

	it('passes uncompressed payloads through as text', async () => {
		const compression = new GatewayCompression('none');
		const payload = new TextEncoder().encode('{"op":11}');

		await expect(compression.decompress(payload.buffer)).resolves.toBe('{"op":11}');
	});

	it('classifies invalid zstd as a gateway compression error', async () => {
		const compression = new GatewayCompression('zstd-stream');

		try {
			await compression.decompress(new Uint8Array([0x6e, 0x6f, 0x70, 0x65]).buffer);
			expect.fail('expected zstd decompression to fail');
		} catch (error) {
			expect(isGatewayCompressionError(error)).toBe(true);
			if (isGatewayCompressionError(error)) {
				expect(error.compression).toBe('zstd-stream');
			}
		}
	});

	it('decodes 10,000 flushed zstd stream gateway JSON messages', async () => {
		const encoder = createZstdCompress({
			params: {
				[constants.ZSTD_c_compressionLevel]: 3,
			},
		});
		const compression = new GatewayCompression('zstd-stream');
		const messages: Array<{compressed: Uint8Array; payload: string}> = [];

		try {
			for (let seq = 1; seq <= STREAM_MESSAGE_COUNT; seq++) {
				const payload = gatewayJsonPayload(seq);
				const compressed = await compressAndFlushChunk(encoder, payload);
				expect(compressed.byteLength).toBeGreaterThan(0);
				messages.push({compressed, payload});
			}

			const decodedMessages = await Promise.all(
				messages.map(({compressed}) => compression.decompress(toArrayBuffer(compressed))),
			);
			for (let index = 0; index < messages.length; index++) {
				const decoded = decodedMessages[index];
				const payload = messages[index].payload;
				const parsed = JSON.parse(decoded) as {s: number; d: {id: string}};
				const seq = index + 1;

				expect(decoded).toBe(payload);
				expect(parsed.s).toBe(seq);
				expect(parsed.d.id).toBe(String(seq));
			}
		} finally {
			compression.destroy();
			encoder.destroy();
		}
	}, 30_000);

	it('round-trips 10,000 outbound messages through the wasm encoder into a peer decoder', async () => {
		const client = new GatewayCompression('zstd-stream');
		const server = new GatewayCompression('zstd-stream');
		await client.warmup();

		try {
			for (let seq = 1; seq <= STREAM_MESSAGE_COUNT; seq++) {
				const payload = gatewayJsonPayload(seq);
				const compressed = client.compress(payload);
				expect(compressed.byteLength).toBeGreaterThan(0);

				const decoded = await server.decompress(toArrayBuffer(compressed));
				expect(decoded).toBe(payload);
			}
		} finally {
			client.destroy();
			server.destroy();
		}
	}, 30_000);
});

function gatewayJsonPayload(seq: number): string {
	return JSON.stringify({
		op: 0,
		t: 'MESSAGE_CREATE',
		s: seq,
		d: {
			id: String(seq),
			channel_id: '1497639278555484216',
			guild_id: '1427764661718740994',
			author: {
				id: '1042',
				username: 'canary',
				bot: false,
			},
			content: `gateway zstd stream stress payload ${seq}`,
			mentions: [],
			attachments: [],
			embeds: [],
			flags: seq % 8,
		},
	});
}

function compressAndFlushChunk(encoder: ZstdCompress, payload: string): Promise<Uint8Array> {
	return new Promise((resolve, reject) => {
		const chunks: Array<Buffer> = [];
		const cleanup = (): void => {
			encoder.off('data', onData);
			encoder.off('error', onError);
		};
		const onData = (chunk: Buffer): void => {
			chunks.push(Buffer.from(chunk));
		};
		const onError = (error: Error): void => {
			cleanup();
			reject(error);
		};

		encoder.on('data', onData);
		encoder.once('error', onError);
		encoder.write(Buffer.from(payload), (error?: Error | null) => {
			if (error) {
				onError(error);
				return;
			}
			encoder.flush(constants.ZSTD_e_flush, () => {
				cleanup();
				resolve(new Uint8Array(Buffer.concat(chunks)));
			});
		});
	});
}

function toArrayBuffer(bytes: Uint8Array): ArrayBuffer {
	const copy = new Uint8Array(bytes.byteLength);
	copy.set(bytes);
	return copy.buffer;
}
