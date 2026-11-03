// SPDX-License-Identifier: AGPL-3.0-or-later

import {constants, createZstdCompress, createZstdDecompress, type ZstdCompress, type ZstdDecompress} from 'node:zlib';

class ZstdStreamCodecError extends Error {
	constructor(message: string, options?: {cause?: unknown}) {
		super(message, options);
		this.name = 'ZstdStreamCodecError';
	}
}

type ZstdStream = ZstdCompress | ZstdDecompress;

class SerialStream {
	private readonly stream: ZstdStream;
	private readonly flushKind: number | null;
	private tail: Promise<unknown> = Promise.resolve();
	private chunks: Array<Buffer> = [];
	private pendingReject: ((error: Error) => void) | null = null;
	private failure: Error | null = null;

	constructor(stream: ZstdStream, flushKind: number | null) {
		this.stream = stream;
		this.flushKind = flushKind;
		stream.on('data', (chunk: Buffer) => {
			this.chunks.push(chunk);
		});
		stream.on('error', (error: Error) => {
			this.failure = new ZstdStreamCodecError(error.message, {cause: error});
			this.pendingReject?.(this.failure);
			this.pendingReject = null;
		});
	}

	process(input: Buffer): Promise<Buffer> {
		const run = this.tail.then(() => this.processNow(input));
		this.tail = run.catch(() => undefined);
		return run;
	}

	dispose(): void {
		this.failure ??= new ZstdStreamCodecError('The zstd stream is disposed');
		this.pendingReject?.(this.failure);
		this.pendingReject = null;
		this.stream.destroy();
	}

	private processNow(input: Buffer): Promise<Buffer> {
		if (this.failure !== null) {
			return Promise.reject(this.failure);
		}
		return new Promise<Buffer>((resolve, reject) => {
			this.chunks = [];
			this.pendingReject = reject;
			const done = (): void => {
				if (this.pendingReject !== reject) {
					return;
				}
				this.pendingReject = null;
				const output = Buffer.concat(this.chunks);
				this.chunks = [];
				resolve(output);
			};
			this.stream.write(input);
			if (this.flushKind === null) {
				this.stream.flush(done);
			} else {
				this.stream.flush(this.flushKind, done);
			}
		});
	}
}

export class ZstdStreamCodec {
	private readonly decoder = new SerialStream(createZstdDecompress(), null);
	private readonly encoder = new SerialStream(createZstdCompress(), constants.ZSTD_e_flush);
	private readonly textDecoder = new TextDecoder('utf-8', {fatal: true});

	async decompress(input: Buffer): Promise<string> {
		const output = await this.decoder.process(input);
		try {
			return this.textDecoder.decode(output);
		} catch (error) {
			throw new ZstdStreamCodecError('Decompressed gateway frame is not valid UTF-8', {cause: error});
		}
	}

	compress(text: string): Promise<Buffer> {
		return this.encoder.process(Buffer.from(text, 'utf8'));
	}

	dispose(): void {
		this.decoder.dispose();
		this.encoder.dispose();
	}
}
