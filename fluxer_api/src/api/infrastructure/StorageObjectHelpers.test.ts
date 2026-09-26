// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildProcessedMediaObject,
	stripNonJpegImageMetadataForUpload,
} from '@app/api/infrastructure/StorageObjectHelpers';
import {describe, expect, it} from 'vitest';

const PNG_SIGNATURE = new Uint8Array([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]);

function concatBytes(chunks: ReadonlyArray<Uint8Array>): Uint8Array {
	const totalLength = chunks.reduce((sum, chunk) => sum + chunk.length, 0);
	const out = new Uint8Array(totalLength);
	let offset = 0;
	for (const chunk of chunks) {
		out.set(chunk, offset);
		offset += chunk.length;
	}
	return out;
}

function textBytes(value: string): Uint8Array {
	const encoded = new TextEncoder().encode(value);
	const out = new Uint8Array(encoded.length);
	out.set(encoded);
	return out;
}

function chunk(type: string, data: Uint8Array<ArrayBufferLike> = new Uint8Array()): Uint8Array {
	const out = new Uint8Array(12 + data.length);
	out[0] = (data.length >>> 24) & 0xff;
	out[1] = (data.length >>> 16) & 0xff;
	out[2] = (data.length >>> 8) & 0xff;
	out[3] = data.length & 0xff;
	out.set(textBytes(type), 4);
	out.set(data, 8);
	return out;
}

function chunkNames(data: Uint8Array): Array<string> {
	const names: Array<string> = [];
	let offset = PNG_SIGNATURE.length;
	while (offset + 12 <= data.length) {
		const length =
			((data[offset]! << 24) | (data[offset + 1]! << 16) | (data[offset + 2]! << 8) | data[offset + 3]!) >>> 0;
		names.push(String.fromCharCode(data[offset + 4]!, data[offset + 5]!, data[offset + 6]!, data[offset + 7]!));
		offset += 12 + length;
	}
	return names;
}

function pngWithPrivateChunks(): Uint8Array {
	return concatBytes([
		PNG_SIGNATURE,
		chunk('IHDR', new Uint8Array(13)),
		chunk('tEXt', textBytes('GPS=1,2')),
		chunk('prIv', textBytes('private metadata')),
		chunk('IDAT', new Uint8Array([1, 2, 3])),
		chunk('IEND'),
	]);
}

describe('stripNonJpegImageMetadataForUpload', () => {
	it('strips PNG metadata and unknown ancillary chunks while preserving image chunks', async () => {
		const input = pngWithPrivateChunks();
		const stripped = await stripNonJpegImageMetadataForUpload(input, 'image/png');
		expect(stripped.contentType).toBe('image/png');
		expect(chunkNames(stripped.body)).toEqual(['IHDR', 'IDAT', 'IEND']);
	});
	it('preserves APNG animation chunks', async () => {
		const input = concatBytes([
			PNG_SIGNATURE,
			chunk('IHDR', new Uint8Array(13)),
			chunk('acTL', new Uint8Array(8)),
			chunk('fcTL', new Uint8Array(26)),
			chunk('tEXt', textBytes('camera=private')),
			chunk('fdAT', new Uint8Array([1, 2, 3])),
			chunk('IDAT', new Uint8Array([4, 5, 6])),
			chunk('IEND'),
		]);
		const stripped = await stripNonJpegImageMetadataForUpload(input, 'image/apng');
		expect(stripped.contentType).toBe('image/apng');
		expect(chunkNames(stripped.body)).toEqual(['IHDR', 'acTL', 'fcTL', 'fdAT', 'IDAT', 'IEND']);
	});
});

function riffChunk(type: string, data: Uint8Array): Uint8Array {
	const out = new Uint8Array(8 + data.length + (data.length & 1));
	out.set(textBytes(type), 0);
	new DataView(out.buffer).setUint32(4, data.length, true);
	out.set(data, 8);
	return out;
}

function webp(chunks: ReadonlyArray<Uint8Array>): Uint8Array {
	const body = concatBytes(chunks);
	const header = concatBytes([textBytes('RIFF'), new Uint8Array(4), textBytes('WEBP')]);
	new DataView(header.buffer).setUint32(4, 4 + body.length, true);
	return concatBytes([header, body]);
}

describe('stripNonJpegImageMetadataForUpload for WebP', () => {
	it('drops EXIF and XMP chunks without re-encoding frames', async () => {
		const vp8x = new Uint8Array(10);
		vp8x[0] = 0x02 | 0x08 | 0x04;
		const anmf = riffChunk('ANMF', new Uint8Array([9, 8, 7]));
		const input = webp([
			riffChunk('VP8X', vp8x),
			riffChunk('ANIM', new Uint8Array(6)),
			anmf,
			riffChunk('EXIF', textBytes('GPS=1,2')),
			riffChunk('XMP ', textBytes('private metadata')),
		]);
		const stripped = await stripNonJpegImageMetadataForUpload(input, 'image/webp');
		const expectedVp8x = new Uint8Array(10);
		expectedVp8x[0] = 0x02;
		expect(stripped.contentType).toBe('image/webp');
		expect(stripped.body).toEqual(webp([riffChunk('VP8X', expectedVp8x), riffChunk('ANIM', new Uint8Array(6)), anmf]));
	});
});

describe('buildProcessedMediaObject', () => {
	it('leaves non-media objects for plain copy', async () => {
		await expect(buildProcessedMediaObject(textBytes('plain text'), 'text/plain')).resolves.toBeNull();
	});
});
