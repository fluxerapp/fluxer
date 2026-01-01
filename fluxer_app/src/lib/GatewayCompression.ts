/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {decompressZstdFrame} from '~/lib/libfluxcore';

export type CompressionType = 'none' | 'zstd-stream' | (string & {});

export class GatewayDecompressor {
	private readonly type: CompressionType;

	constructor(type: CompressionType) {
		this.type = type;
	}

	async decompress(data: ArrayBuffer): Promise<string> {
		const input = new Uint8Array(data);

		switch (this.type) {
			case 'none':
				return new TextDecoder().decode(input);

			case 'zstd-stream':
				return this.decompressZstd(input);

			default:
				throw new Error(`Unsupported compression type: ${this.type}`);
		}
	}

	private async decompressZstd(data: Uint8Array): Promise<string> {
		const wasmDecoded = await decompressZstdFrame(data);
		if (!wasmDecoded) {
			throw new Error('Gateway zstd WASM not available');
		}
		const decompressed = wasmDecoded;
		return new TextDecoder().decode(decompressed);
	}

	destroy(): void {}
}

export function getPreferredCompression(): CompressionType {
	return 'zstd-stream';
}

export function isCompressionSupported(type: CompressionType): boolean {
	switch (type) {
		case 'none':
		case 'zstd-stream':
			return true;
		default:
			return false;
	}
}
