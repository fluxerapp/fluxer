// SPDX-License-Identifier: AGPL-3.0-or-later

export interface FluxerImageDecoderInit {
	data: ReadableStream<Uint8Array> | ArrayBuffer | ArrayBufferView;
	type: string;
}

export interface FluxerImageDecoderDecodedFrame {
	image: VideoFrame;
	complete: boolean;
}

export interface FluxerImageDecoderTrack {
	animated: boolean;
	frameCount: number;
	repetitionCount?: number;
}

export interface FluxerImageDecoderInstance {
	decode(options?: {frameIndex?: number; completeFramesOnly?: boolean}): Promise<FluxerImageDecoderDecodedFrame>;
	tracks: {
		ready: Promise<void>;
		selectedTrack: FluxerImageDecoderTrack | null;
	};
	completed: Promise<void>;
	close(): void;
}

export interface FluxerImageDecoderConstructor {
	new (init: FluxerImageDecoderInit): FluxerImageDecoderInstance;
	isTypeSupported(type: string): Promise<boolean>;
}

export const AVIF_ALPHA_PROBE_BASE64 =
	'AAAAIGZ0eXBhdmlmAAAAAGF2aWZtaWYxbWlhZk1BMUIAAAGGbWV0YQAAAAAAAAAhaGRscgAAAAAAAAAAcGljdAAAAAAAAAAAAAAAAAAAAAAOcGl0bQAAAAAAAQAAACxpbG9jAAAAAEQAAAIAAQAAAAEAAAHAAAAAFAACAAAAAQAAAa4AAAASAAAAQmlpbmYAAAAAAAIAAAAaaW5mZQIAAAAAAQAAYXYwMUNvbG9yAAAAABppbmZlAgAAAAACAABhdjAxQWxwaGEAAAAAGmlyZWYAAAAAAAAADmF1eGwAAgABAAEAAADDaXBycAAAAJ1pcGNvAAAAFGlzcGUAAAAAAAAAAgAAAAIAAAAQcGl4aQAAAAADCAgIAAAADGF2MUOBAAwAAAAAE2NvbHJuY2x4AAEADQAGgAAAAA5waXhpAAAAAAEIAAAADGF2MUOBABwAAAAAOGF1eEMAAAAAdXJuOm1wZWc6bXBlZ0I6Y2ljcDpzeXN0ZW1zOmF1eGlsaWFyeTphbHBoYQAAAAAeaXBtYQAAAAAAAAACAAEEAQKDBAACBAEFhgcAAAAubWRhdBIACgQYADYVMggQAAAY4UJyEBIACgUYADYEIDIJEAAAAAfIJMZi';

export type Canvas2DContext = OffscreenCanvasRenderingContext2D | CanvasRenderingContext2D;

function isImageDecoderConstructor(value: unknown): value is FluxerImageDecoderConstructor {
	if (typeof value !== 'function') return false;
	const candidate = value as {isTypeSupported?: unknown};
	return typeof candidate.isTypeSupported === 'function';
}

export function getImageDecoderConstructor(): FluxerImageDecoderConstructor | null {
	const candidate: unknown = Reflect.get(globalThis, 'ImageDecoder');
	return isImageDecoderConstructor(candidate) ? candidate : null;
}

export function drawVideoFrameToCanvas(ctx: Canvas2DContext, image: VideoFrame, x = 0, y = 0): void {
	ctx.drawImage(image as VideoFrame & CanvasImageSource, x, y);
}

let animatedAvifSupport: Promise<boolean> | null = null;

function decodeBase64(value: string): Uint8Array {
	const binary = atob(value);
	const out = new Uint8Array(binary.length);
	for (let i = 0; i < binary.length; i++) out[i] = binary.charCodeAt(i);
	return out;
}

export function closeImageDecoder(decoder: FluxerImageDecoderInstance): void {
	try {
		decoder.close();
	} catch {}
}

async function probeAvifAlpha(ImageDecoderConstructor: FluxerImageDecoderConstructor): Promise<boolean> {
	const decoder = new ImageDecoderConstructor({data: decodeBase64(AVIF_ALPHA_PROBE_BASE64), type: 'image/avif'});
	try {
		const {image} = await decoder.decode({frameIndex: 0});
		try {
			const canvas = new OffscreenCanvas(image.displayWidth, image.displayHeight);
			const context = canvas.getContext('2d', {willReadFrequently: true});
			if (!context) return false;
			drawVideoFrameToCanvas(context, image);
			return context.getImageData(0, 0, 1, 1).data[3] === 0;
		} finally {
			image.close();
		}
	} catch {
		return false;
	} finally {
		closeImageDecoder(decoder);
	}
}

async function detectAnimatedAvifSupport(): Promise<boolean> {
	const ImageDecoderConstructor = getImageDecoderConstructor();
	if (!ImageDecoderConstructor || typeof OffscreenCanvas === 'undefined') return false;
	try {
		if (!(await ImageDecoderConstructor.isTypeSupported('image/avif'))) return false;
	} catch {
		return false;
	}
	return probeAvifAlpha(ImageDecoderConstructor);
}

export function canDecodeAnimatedAvif(): Promise<boolean> {
	animatedAvifSupport ??= detectAnimatedAvifSupport();
	return animatedAvifSupport;
}
