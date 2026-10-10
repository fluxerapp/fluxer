// SPDX-License-Identifier: AGPL-3.0-or-later

import {createReadStream} from 'node:fs';
import {createConnection} from 'node:net';
import {Readable} from 'node:stream';
import type {VirusScanProviderResult} from '@pkgs/virus_scan/src/VirusScanProviderResult';

interface ClamAVConfig {
	host: string;
	port: number;
	streamChunkBytes?: number;
}

export class ClamAVProvider {
	private readonly streamChunkBytes: number;

	constructor(private config: ClamAVConfig) {
		this.streamChunkBytes = config.streamChunkBytes ?? 64 * 1024;
	}

	async scanFile(filePath: string): Promise<VirusScanProviderResult> {
		return this.scanReadable(createReadStream(filePath, {highWaterMark: this.streamChunkBytes}));
	}

	async scanBuffer(buffer: Buffer): Promise<VirusScanProviderResult> {
		return this.scanReadable(Readable.from(buffer));
	}

	private scanReadable(source: Readable): Promise<VirusScanProviderResult> {
		const chunkSize = this.streamChunkBytes;
		return new Promise((resolve, reject) => {
			const socket = createConnection(this.config.port, this.config.host);
			let response = '';
			let settled = false;
			const cleanup = (): void => {
				if (!socket.destroyed) {
					socket.destroy();
				}
				if (!source.destroyed) {
					source.destroy();
				}
			};
			const doReject = (error: Error): void => {
				if (settled) return;
				settled = true;
				cleanup();
				reject(error);
			};
			const doResolve = (result: VirusScanProviderResult): void => {
				if (settled) return;
				settled = true;
				cleanup();
				resolve(result);
			};
			socket.on('connect', () => {
				socket.write('zINSTREAM\0');
				let pendingTail: Buffer | null = null;
				const writeSized = (slice: Buffer): boolean => {
					const sizeBuffer = Buffer.alloc(4);
					sizeBuffer.writeUInt32BE(slice.length, 0);
					socket.write(sizeBuffer);
					return socket.write(slice);
				};
				const drain = (): Promise<void> => {
					return new Promise((res) => socket.once('drain', () => res()));
				};
				const onData = async (raw: unknown): Promise<void> => {
					try {
						const buf = raw instanceof Buffer ? raw : Buffer.from(raw as Uint8Array);
						const merged = pendingTail ? Buffer.concat([pendingTail, buf]) : buf;
						pendingTail = null;
						let offset = 0;
						while (offset < merged.length) {
							const remaining = merged.length - offset;
							const sliceLen = Math.min(chunkSize, remaining);
							const slice = merged.subarray(offset, offset + sliceLen);
							offset += sliceLen;
							if (!writeSized(slice)) {
								source.pause();
								await drain();
								source.resume();
							}
						}
					} catch (error) {
						doReject(new Error(`ClamAV write failed: ${error instanceof Error ? error.message : String(error)}`));
					}
				};
				source.on('data', (raw) => {
					void onData(raw);
				});
				source.once('end', () => {
					try {
						const endBuffer = Buffer.alloc(4);
						endBuffer.writeUInt32BE(0, 0);
						socket.write(endBuffer);
					} catch (error) {
						doReject(new Error(`ClamAV finalize failed: ${error instanceof Error ? error.message : String(error)}`));
					}
				});
				source.once('error', (error) => {
					doReject(new Error(`Source stream failed: ${error.message}`));
				});
			});
			socket.on('data', (data) => {
				response += data.toString();
			});
			socket.on('end', () => {
				const trimmedResponse = response.trim();
				if (trimmedResponse.includes('FOUND')) {
					const threatMatch = trimmedResponse.match(/:\s(.+)\sFOUND/);
					const threat = threatMatch ? threatMatch[1] : 'Virus detected';
					doResolve({
						isClean: false,
						threat,
					});
				} else if (trimmedResponse.includes('OK')) {
					doResolve({
						isClean: true,
					});
				} else {
					doReject(new Error(`Unexpected ClamAV response: ${trimmedResponse}`));
				}
			});
			socket.on('error', (error) => {
				doReject(new Error(`ClamAV connection failed: ${error.message}`));
			});
		});
	}
}
