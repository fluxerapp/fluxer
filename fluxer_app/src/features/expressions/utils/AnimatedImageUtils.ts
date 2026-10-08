// SPDX-License-Identifier: AGPL-3.0-or-later

export type InspectedImageFormat = 'unknown' | 'png' | 'gif' | 'webp' | 'avif' | 'jpeg';

export interface InspectedImage {
	format: InspectedImageFormat;
	animated: boolean;
}

const SNIFFED_FORMATS: ReadonlyArray<InspectedImageFormat> = ['unknown', 'png', 'gif', 'webp', 'avif', 'jpeg'];

export async function inspectImageBytes(bytes: Uint8Array): Promise<InspectedImage> {
	try {
		const {detectAnimatedImage, sniffImageFormat} = await import('@app/features/platform/utils/LibFluxcore');
		const format = SNIFFED_FORMATS[await sniffImageFormat(bytes)] ?? 'unknown';
		if (format === 'unknown' || format === 'jpeg') {
			return {format, animated: false};
		}
		return {format, animated: await detectAnimatedImage(bytes)};
	} catch {
		return {format: 'unknown', animated: false};
	}
}

export async function inspectImageFile(file: File): Promise<InspectedImage> {
	try {
		return await inspectImageBytes(new Uint8Array(await file.arrayBuffer()));
	} catch {
		return {format: 'unknown', animated: false};
	}
}

export async function isAnimatedFile(file: File): Promise<boolean> {
	return (await inspectImageFile(file)).animated;
}
