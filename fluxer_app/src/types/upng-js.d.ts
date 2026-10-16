// SPDX-License-Identifier: AGPL-3.0-or-later

declare module 'upng-js' {
	export interface UpngImage {
		width: number;
		height: number;
	}
	const UPNG: {
		decode(buffer: ArrayBuffer, inflated?: Uint8Array): UpngImage;
		toRGBA8(image: UpngImage): Array<ArrayBuffer>;
	};
	export default UPNG;
}
