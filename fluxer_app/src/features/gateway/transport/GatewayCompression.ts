// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	compressZstdStreamChunk,
	createZstdStreamDecoder,
	createZstdStreamEncoderSync,
	decompressZstdStreamChunk,
	ensureLibfluxcoreReady,
	freeZstdStreamDecoder,
	freeZstdStreamEncoder,
	isLibfluxcoreReady,
} from '@app/features/platform/utils/LibFluxcore';

export type CompressionType = 'none' | 'zstd-stream';

const ZSTD_STREAM_LEVEL = 3;

class GatewayCompressionError extends Error {
	readonly compression: CompressionType;
	readonly originalError: unknown;

	constructor(compression: CompressionType, originalError: unknown) {
		super(`Gateway ${compression} decompression failed`);
		this.name = 'GatewayCompressionError';
		this.compression = compression;
		this.originalError = originalError;
	}
}

export function isGatewayCompressionError(error: unknown): error is GatewayCompressionError {
	return error instanceof GatewayCompressionError;
}

export class GatewayCompression {
	private readonly type: CompressionType;
	private zstdStreamDecoder: number | null = null;
	private zstdStreamEncoder: number | null = null;
	private pendingStreamOperations = 0;
	private destroyed = false;
	private streamQueue: Promise<void> = Promise.resolve();

	constructor(type: CompressionType) {
		this.type = type;
	}

	canCompress(): boolean {
		return this.type === 'zstd-stream';
	}

	async warmup(): Promise<void> {
		if (!this.canCompress()) return;
		await ensureLibfluxcoreReady();
		if (this.destroyed) return;
		if (this.zstdStreamEncoder == null) {
			this.zstdStreamEncoder = createZstdStreamEncoderSync(ZSTD_STREAM_LEVEL);
		}
	}

	compress(text: string): Uint8Array {
		if (!this.canCompress()) {
			throw new GatewayCompressionError(this.type, new Error('Gateway compression is not enabled'));
		}
		if (this.destroyed) {
			throw new GatewayCompressionError(this.type, new Error('Gateway compression codec is destroyed'));
		}
		if (!isLibfluxcoreReady()) {
			throw new GatewayCompressionError(this.type, new Error('Gateway compression encoder is not ready'));
		}
		try {
			if (this.zstdStreamEncoder == null) {
				this.zstdStreamEncoder = createZstdStreamEncoderSync(ZSTD_STREAM_LEVEL);
			}
			return compressZstdStreamChunk(this.zstdStreamEncoder, new TextEncoder().encode(text));
		} catch (error) {
			throw new GatewayCompressionError(this.type, error);
		}
	}

	async decompress(data: ArrayBuffer): Promise<string> {
		const input = new Uint8Array(data);
		switch (this.type) {
			case 'none':
				return new TextDecoder().decode(input);
			case 'zstd-stream':
				return this.decompressZstdStream(input);
			default:
				throw new Error(`Unsupported compression type: ${this.type}`);
		}
	}

	private async decompressZstdStream(data: Uint8Array): Promise<string> {
		const operation = this.streamQueue.then(() => this.decompressZstdStreamLocked(data));
		this.streamQueue = operation.then(
			() => undefined,
			() => undefined,
		);
		return operation;
	}

	private async decompressZstdStreamLocked(data: Uint8Array): Promise<string> {
		this.pendingStreamOperations++;
		try {
			if (this.destroyed) {
				throw new Error('Gateway compression decoder is destroyed');
			}
			if (this.zstdStreamDecoder == null) {
				const decoder = await createZstdStreamDecoder();
				if (this.destroyed) {
					freeZstdStreamDecoder(decoder);
					throw new Error('Gateway compression decoder is destroyed');
				}
				this.zstdStreamDecoder = decoder;
			}
			const decompressed = await decompressZstdStreamChunk(this.zstdStreamDecoder, data);
			return new TextDecoder().decode(decompressed);
		} catch (error) {
			throw new GatewayCompressionError(this.type, error);
		} finally {
			this.pendingStreamOperations--;
			this.freeDestroyedStreamDecoder();
		}
	}

	destroy(): void {
		this.destroyed = true;
		this.freeDestroyedStreamDecoder();
		if (this.zstdStreamEncoder != null) {
			freeZstdStreamEncoder(this.zstdStreamEncoder);
			this.zstdStreamEncoder = null;
		}
	}

	private freeDestroyedStreamDecoder(): void {
		if (!this.destroyed || this.pendingStreamOperations > 0 || this.zstdStreamDecoder == null) return;
		freeZstdStreamDecoder(this.zstdStreamDecoder);
		this.zstdStreamDecoder = null;
	}
}
