// SPDX-License-Identifier: AGPL-3.0-or-later

declare module 'bidi-js' {
	interface EmbeddingLevels {
		levels: Uint8Array;
		paragraphs: Array<{start: number; end: number; level: number}>;
	}

	interface Bidi {
		getEmbeddingLevels(text: string, direction?: 'ltr' | 'rtl' | 'auto'): EmbeddingLevels;
		getReorderedString(text: string, levels: EmbeddingLevels, start?: number, end?: number): string;
	}

	export default function bidiFactory(): Bidi;
}
