// SPDX-License-Identifier: AGPL-3.0-or-later

declare module 'gifenc' {
	export type GifPaletteColor = [number, number, number];
	export interface GifFrameOptions {
		palette?: Array<GifPaletteColor>;
		delay?: number;
		repeat?: number;
		transparent?: boolean;
		transparentIndex?: number;
		dispose?: number;
	}
	export interface GifEncoderInstance {
		writeFrame(index: Uint8Array, width: number, height: number, options?: GifFrameOptions): void;
		finish(): void;
		bytes(): Uint8Array;
		bytesView(): Uint8Array;
	}
	export function GIFEncoder(options?: {initialCapacity?: number}): GifEncoderInstance;
}
