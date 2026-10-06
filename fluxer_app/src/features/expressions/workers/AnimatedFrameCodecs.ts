// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	apngDelayMs,
	assertSourceBudget,
	avifDelayMs,
	avifPlaysFromRepetitionCount,
	clampPlays,
	gifPlaysFromRepeat,
	gifRepeatFromPlays,
	MAX_SOURCE_CANVAS_PIXELS,
} from '@app/features/expressions/workers/AnimatedCropPlan';
import {CropPipelineError} from '@app/features/expressions/workers/AnimatedImageCropMessages';
import {
	canDecodeAnimatedAvif,
	closeImageDecoder,
	drawVideoFrameToCanvas,
	type FluxerImageDecoderDecodedFrame,
	getImageDecoderConstructor,
} from '@app/features/platform/utils/ImageDecoderInterop';
import {Unzlib} from 'fflate';
import {GIFEncoder, type GifPaletteColor} from 'gifenc';
import {decompressFrame, type ParsedGif, parseGIF} from 'gifuct-js';
import UPNG from 'upng-js';

export interface DecodedFrame {
	rgba: Uint8Array;
	delayMs: number;
}

export interface FrameSource {
	width: number;
	height: number;
	frameCount: number;
	plays: number;
	next(): DecodedFrame | null | Promise<DecodedFrame | null>;
	close(): void;
}

export interface GifFrameSink {
	add(rgba: Uint8Array, delayMs: number): boolean;
	finish(): Uint8Array;
	close(): void;
}

interface Rect {
	x: number;
	y: number;
	width: number;
	height: number;
}

type GifBlock = ParsedGif['frames'][number];
type GifImageBlock = Extract<GifBlock, {image: unknown}>;

const PNG_SIGNATURE = new Uint8Array([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]);
const GIF_EXTENSION_INTRODUCER = 0x21;
const GIF_APPLICATION_LABEL = 0xff;
const GIF_LOOP_APPLICATIONS = new Set(['NETSCAPE2.0', 'ANIMEXTS1.0']);
const GIF_MAX_DELAY_CS = 0xffff;
const GIF_TRANSPARENT_INDEX = 0;
const GIF_MAX_OPAQUE_COLORS = 255;
const GIF_MAX_LZW_CODE_SIZE = 11;
const GIF_VERSION_87_MINOR = 0x37;
const APNG_DISPOSE_BACKGROUND = 1;
const APNG_DISPOSE_PREVIOUS = 2;
const APNG_BLEND_OVER = 1;
const PNG_CHANNELS: Readonly<Record<number, number>> = {0: 1, 2: 3, 3: 1, 4: 2, 6: 4};
const ADAM7_PASSES: ReadonlyArray<readonly [number, number, number, number]> = [
	[0, 0, 8, 8],
	[4, 0, 8, 8],
	[0, 4, 4, 8],
	[2, 0, 4, 4],
	[0, 2, 2, 4],
	[1, 0, 2, 2],
	[0, 1, 1, 2],
];
const INFLATE_SLICE_BYTES = 16_384;
const INFLATE_SLACK_BYTES = 65_536;

function decodeFailed(message: string): CropPipelineError {
	return new CropPipelineError('decode_failed', message);
}

function toArrayBuffer(bytes: Uint8Array): ArrayBuffer {
	if (bytes.buffer instanceof ArrayBuffer && bytes.byteOffset === 0 && bytes.byteLength === bytes.buffer.byteLength) {
		return bytes.buffer;
	}
	return bytes.slice().buffer;
}

function clipRect(rect: Rect, canvasWidth: number, canvasHeight: number): Rect {
	const x = Math.min(rect.x, canvasWidth);
	const y = Math.min(rect.y, canvasHeight);
	return {
		x,
		y,
		width: Math.max(0, Math.min(rect.x + rect.width, canvasWidth) - x),
		height: Math.max(0, Math.min(rect.y + rect.height, canvasHeight) - y),
	};
}

function clearRect(canvas: Uint8Array, canvasWidth: number, rect: Rect): void {
	for (let row = 0; row < rect.height; row++) {
		const start = ((rect.y + row) * canvasWidth + rect.x) * 4;
		canvas.fill(0, start, start + rect.width * 4);
	}
}

function saveRect(canvas: Uint8Array, canvasWidth: number, rect: Rect): Uint8Array {
	const saved = new Uint8Array(rect.width * rect.height * 4);
	for (let row = 0; row < rect.height; row++) {
		const start = ((rect.y + row) * canvasWidth + rect.x) * 4;
		saved.set(canvas.subarray(start, start + rect.width * 4), row * rect.width * 4);
	}
	return saved;
}

function restoreRect(canvas: Uint8Array, canvasWidth: number, rect: Rect, saved: Uint8Array): void {
	for (let row = 0; row < rect.height; row++) {
		const start = ((rect.y + row) * canvasWidth + rect.x) * 4;
		canvas.set(saved.subarray(row * rect.width * 4, (row + 1) * rect.width * 4), start);
	}
}

function readGifRepeat(bytes: Uint8Array): number | null {
	let pos = 13;
	if (bytes[10] & 0x80) pos += 3 * (2 << (bytes[10] & 0x07));
	while (pos + 1 < bytes.length && bytes[pos] === GIF_EXTENSION_INTRODUCER) {
		const label = bytes[pos + 1];
		pos += 2;
		let isLoopApplication = false;
		if (label === GIF_APPLICATION_LABEL && pos < bytes.length) {
			const blockSize = bytes[pos];
			isLoopApplication = GIF_LOOP_APPLICATIONS.has(
				String.fromCharCode(...bytes.subarray(pos + 1, pos + 1 + blockSize)),
			);
			pos += 1 + blockSize;
		}
		while (pos < bytes.length && bytes[pos] !== 0) {
			const size = bytes[pos];
			if (isLoopApplication && size >= 3 && bytes[pos + 1] === 1 && pos + 3 < bytes.length) {
				return bytes[pos + 2] | (bytes[pos + 3] << 8);
			}
			pos += size + 1;
		}
		pos += 1;
	}
	return null;
}

export function openGifFrameSource(bytes: Uint8Array): FrameSource {
	if (bytes.length < 13 || bytes[0] !== 0x47 || bytes[1] !== 0x49 || bytes[2] !== 0x46) {
		throw decodeFailed('Not a GIF file');
	}
	const plays = gifPlaysFromRepeat(readGifRepeat(bytes));
	const parsed = parseGIF(toArrayBuffer(bytes));
	const frames = parsed.frames.filter((block): block is GifImageBlock => 'image' in block && block.image != null);
	if (frames.some((frame) => !(frame.image.data.minCodeSize <= GIF_MAX_LZW_CODE_SIZE))) {
		throw decodeFailed('GIF frame has an invalid LZW code size');
	}
	const screenWidth = bytes[6] | (bytes[7] << 8);
	const screenHeight = bytes[8] | (bytes[9] << 8);
	const first = frames[0]?.image.descriptor;
	const width = Math.max(screenWidth, first ? first.left + first.width : 0);
	const height = Math.max(screenHeight, first ? first.top + first.height : 0);
	if (
		first &&
		(screenWidth < first.width || screenHeight < first.height || bytes[4] === GIF_VERSION_87_MINOR) &&
		(width !== first.width || height !== first.height)
	) {
		throw decodeFailed('GIF logical screen does not fit its first frame');
	}
	let canvas: Uint8Array | null = null;
	let index = 0;
	let pendingDisposal: {rect: Rect; saved: Uint8Array | null} | null = null;

	return {
		width,
		height,
		frameCount: frames.length,
		plays,
		next() {
			if (index >= frames.length) return null;
			const frame = frames[index++];
			const descriptor = frame.image.descriptor;
			if (descriptor.width * descriptor.height > MAX_SOURCE_CANVAS_PIXELS) {
				throw new CropPipelineError(
					'budget_exceeded',
					`GIF frame ${descriptor.width}x${descriptor.height} is too large`,
				);
			}
			if (!canvas) {
				assertSourceBudget(width, height, frames.length);
				canvas = new Uint8Array(width * height * 4);
			}
			if (pendingDisposal) {
				if (pendingDisposal.saved) restoreRect(canvas, width, pendingDisposal.rect, pendingDisposal.saved);
				else clearRect(canvas, width, pendingDisposal.rect);
				pendingDisposal = null;
			}
			const decoded = decompressFrame(frame, parsed.gct, false);
			const rect = clipRect(
				{x: descriptor.left, y: descriptor.top, width: descriptor.width, height: descriptor.height},
				width,
				height,
			);
			const disposal = frame.gce?.extras.disposal ?? 0;
			if (disposal === 3) pendingDisposal = {rect, saved: saveRect(canvas, width, rect)};
			else if (disposal === 2) pendingDisposal = {rect, saved: null};
			const colorTable = decoded.colorTable ?? [];
			const transparentIndex = frame.gce?.extras.transparentColorGiven ? frame.gce.transparentColorIndex : -1;
			const pixels = decoded.pixels;
			for (let row = 0; row < rect.height; row++) {
				const sourceRow = (rect.y - descriptor.top + row) * descriptor.width + (rect.x - descriptor.left);
				let target = ((rect.y + row) * width + rect.x) * 4;
				for (let column = 0; column < rect.width; column++, target += 4) {
					const colorIndex = pixels[sourceRow + column];
					if (colorIndex === transparentIndex) continue;
					const color = colorTable[colorIndex];
					if (!color) continue;
					canvas[target] = color[0];
					canvas[target + 1] = color[1];
					canvas[target + 2] = color[2];
					canvas[target + 3] = 255;
				}
			}
			return {rgba: canvas, delayMs: (frame.gce?.delay ?? 0) * 10};
		},
		close() {
			canvas = null;
		},
	};
}

interface Range {
	start: number;
	end: number;
}

interface IsoBox extends Range {
	type: string;
}

interface ApngFrameChunks {
	rect: Rect;
	delayMs: number;
	dispose: number;
	blend: number;
	data: Array<Uint8Array>;
}

function readU32(bytes: Uint8Array, offset: number): number {
	return ((bytes[offset] << 24) | (bytes[offset + 1] << 16) | (bytes[offset + 2] << 8) | bytes[offset + 3]) >>> 0;
}

function readU16(bytes: Uint8Array, offset: number): number {
	return (bytes[offset] << 8) | bytes[offset + 1];
}

function writeChunk(out: Uint8Array, offset: number, type: string, parts: Array<Uint8Array>): number {
	const length = parts.reduce((sum, part) => sum + part.length, 0);
	new DataView(out.buffer, out.byteOffset).setUint32(offset, length);
	for (let i = 0; i < 4; i++) out[offset + 4 + i] = type.charCodeAt(i);
	let cursor = offset + 8;
	for (const part of parts) {
		out.set(part, cursor);
		cursor += part.length;
	}
	return cursor + 4;
}

function buildFrameHeaderPng(
	ihdr: Uint8Array,
	palette: Uint8Array | null,
	transparency: Uint8Array | null,
	rect: Rect,
): Uint8Array {
	const header = ihdr.slice();
	new DataView(header.buffer).setUint32(0, rect.width);
	new DataView(header.buffer).setUint32(4, rect.height);
	const chunks: Array<[string, Array<Uint8Array>]> = [['IHDR', [header]]];
	if (palette) chunks.push(['PLTE', [palette]]);
	if (transparency) chunks.push(['tRNS', [transparency]]);
	chunks.push(['IEND', []]);
	const size = chunks.reduce(
		(sum, [, parts]) => sum + 12 + parts.reduce((partSum, part) => partSum + part.length, 0),
		PNG_SIGNATURE.length,
	);
	const out = new Uint8Array(size);
	out.set(PNG_SIGNATURE, 0);
	let offset = PNG_SIGNATURE.length;
	for (const [type, parts] of chunks) offset = writeChunk(out, offset, type, parts);
	return out;
}

function filteredPngByteLength(ihdr: Uint8Array, width: number, height: number): number {
	const channels = PNG_CHANNELS[ihdr[9]];
	if (channels === undefined) throw decodeFailed(`PNG color type ${ihdr[9]} is not supported`);
	const bitsPerPixel = channels * ihdr[8];
	const rowBytes = (rowWidth: number) => 1 + Math.ceil((rowWidth * bitsPerPixel) / 8);
	if (ihdr[12] === 0) return height * rowBytes(width);
	let total = 0;
	for (const [column, row, columnStep, rowStep] of ADAM7_PASSES) {
		const passWidth = Math.ceil((width - column) / columnStep);
		const passHeight = Math.ceil((height - row) / rowStep);
		if (passWidth > 0 && passHeight > 0) total += passHeight * rowBytes(passWidth);
	}
	return total;
}

function inflateFrame(parts: Array<Uint8Array>, out: Uint8Array, frameIndex: number): Uint8Array {
	const filteredLength = out.length;
	const limit = filteredLength * 2 + INFLATE_SLACK_BYTES;
	let inflated = 0;
	const inflater = new Unzlib((chunk) => {
		if (inflated < filteredLength) out.set(chunk.subarray(0, filteredLength - inflated), inflated);
		inflated += chunk.length;
		if (inflated > limit) throw decodeFailed(`APNG frame ${frameIndex} inflates past its dimensions`);
	});
	for (const part of parts) {
		for (let offset = 0; offset < part.length && inflated < filteredLength; offset += INFLATE_SLICE_BYTES) {
			inflater.push(part.subarray(offset, offset + INFLATE_SLICE_BYTES));
		}
	}
	if (inflated < filteredLength) throw decodeFailed(`APNG frame ${frameIndex} is truncated`);
	return out;
}

function blendOver(canvas: Uint8Array, target: number, rgba: Uint8Array, source: number): void {
	const sourceAlpha = rgba[source + 3];
	if (sourceAlpha === 0) return;
	const destinationAlpha = canvas[target + 3];
	if (sourceAlpha === 255 || destinationAlpha === 0) {
		canvas.set(rgba.subarray(source, source + 4), target);
		return;
	}
	const destinationWeight = destinationAlpha * (255 - sourceAlpha);
	const outAlpha = sourceAlpha * 255 + destinationWeight;
	for (let channel = 0; channel < 3; channel++) {
		canvas[target + channel] = Math.round(
			(rgba[source + channel] * sourceAlpha * 255 + canvas[target + channel] * destinationWeight) / outAlpha,
		);
	}
	canvas[target + 3] = Math.round(outAlpha / 255);
}

export function openApngFrameSource(bytes: Uint8Array): FrameSource {
	for (let i = 0; i < PNG_SIGNATURE.length; i++) {
		if (bytes[i] !== PNG_SIGNATURE[i]) throw decodeFailed('Not a PNG file');
	}
	let ihdr: Uint8Array | null = null;
	let palette: Uint8Array | null = null;
	let transparency: Uint8Array | null = null;
	let plays = 0;
	let animated = false;
	const frames: Array<ApngFrameChunks> = [];
	let pos = PNG_SIGNATURE.length;
	for (;;) {
		if (pos + 8 > bytes.length) throw decodeFailed('PNG ended before IEND');
		const length = readU32(bytes, pos);
		const type = String.fromCharCode(bytes[pos + 4], bytes[pos + 5], bytes[pos + 6], bytes[pos + 7]);
		const dataStart = pos + 8;
		const dataEnd = dataStart + length;
		if (dataEnd + 4 > bytes.length) throw decodeFailed(`PNG chunk ${type} overruns the file`);
		const data = bytes.subarray(dataStart, dataEnd);
		pos = dataEnd + 4;
		if (type === 'IEND') break;
		if (type === 'IHDR' && length === 13) ihdr = data;
		else if (type === 'PLTE') palette = data;
		else if (type === 'tRNS') transparency = data;
		else if (type === 'acTL' && length >= 8) {
			animated = true;
			plays = clampPlays(readU32(data, 4));
		} else if (type === 'fcTL' && length >= 26) {
			frames.push({
				rect: {x: readU32(data, 12), y: readU32(data, 16), width: readU32(data, 4), height: readU32(data, 8)},
				delayMs: apngDelayMs(readU16(data, 20), readU16(data, 22)),
				dispose: data[24],
				blend: data[25],
				data: [],
			});
		} else if (type === 'IDAT' && frames.length === 1) frames[0].data.push(data);
		else if (type === 'fdAT' && frames.length > 0 && length > 4) frames[frames.length - 1].data.push(data.subarray(4));
	}
	if (!ihdr || !animated) throw decodeFailed('PNG is not animated');
	const header = ihdr;
	const width = readU32(header, 0);
	const height = readU32(header, 4);
	let canvas: Uint8Array | null = null;
	let scratch = new Uint8Array(0);
	let index = 0;
	let pendingDisposal: {rect: Rect; saved: Uint8Array | null} | null = null;

	return {
		width,
		height,
		frameCount: frames.length,
		plays,
		next() {
			if (index >= frames.length) return null;
			const frameIndex = index++;
			const frame = frames[frameIndex];
			const {rect} = frame;
			if (
				rect.width === 0 ||
				rect.height === 0 ||
				rect.x + rect.width > width ||
				rect.y + rect.height > height ||
				frame.data.length === 0
			) {
				throw decodeFailed(`APNG frame ${frameIndex} is outside the canvas or empty`);
			}
			if (!canvas) {
				assertSourceBudget(width, height, frames.length);
				canvas = new Uint8Array(width * height * 4);
			}
			if (pendingDisposal) {
				if (pendingDisposal.saved) restoreRect(canvas, width, pendingDisposal.rect, pendingDisposal.saved);
				else clearRect(canvas, width, pendingDisposal.rect);
				pendingDisposal = null;
			}
			const filteredLength = filteredPngByteLength(header, rect.width, rect.height);
			if (scratch.length < filteredLength) scratch = new Uint8Array(filteredLength);
			const inflated = inflateFrame(frame.data, scratch.subarray(0, filteredLength), frameIndex);
			const image = UPNG.decode(toArrayBuffer(buildFrameHeaderPng(header, palette, transparency, rect)), inflated);
			const rgba = new Uint8Array(UPNG.toRGBA8(image)[0]);
			if (rgba.length !== rect.width * rect.height * 4) throw decodeFailed(`APNG frame ${frameIndex} did not decode`);
			const dispose =
				frameIndex === 0 && frame.dispose === APNG_DISPOSE_PREVIOUS ? APNG_DISPOSE_BACKGROUND : frame.dispose;
			if (dispose === APNG_DISPOSE_PREVIOUS) pendingDisposal = {rect, saved: saveRect(canvas, width, rect)};
			else if (dispose === APNG_DISPOSE_BACKGROUND) pendingDisposal = {rect, saved: null};
			for (let row = 0; row < rect.height; row++) {
				const target = ((rect.y + row) * width + rect.x) * 4;
				const source = row * rect.width * 4;
				if (frame.blend === APNG_BLEND_OVER) {
					for (let column = 0; column < rect.width; column++) {
						blendOver(canvas, target + column * 4, rgba, source + column * 4);
					}
				} else {
					canvas.set(rgba.subarray(source, source + rect.width * 4), target);
				}
			}
			return {rgba: canvas, delayMs: frame.delayMs};
		},
		close() {
			canvas = null;
			scratch = new Uint8Array(0);
		},
	};
}

function readFourCc(bytes: Uint8Array, offset: number): string {
	return String.fromCharCode(bytes[offset], bytes[offset + 1], bytes[offset + 2], bytes[offset + 3]);
}

function isoChildren(bytes: Uint8Array, parent: Range): Array<IsoBox> {
	const boxes: Array<IsoBox> = [];
	let pos = parent.start;
	while (pos + 8 <= parent.end) {
		let size = readU32(bytes, pos);
		let headerSize = 8;
		if (size === 1) {
			if (pos + 16 > parent.end) break;
			size = readU32(bytes, pos + 8) * 2 ** 32 + readU32(bytes, pos + 12);
			headerSize = 16;
		} else if (size === 0) {
			size = parent.end - pos;
		}
		if (size < headerSize || pos + size > parent.end) break;
		boxes.push({type: readFourCc(bytes, pos + 4), start: pos + headerSize, end: pos + size});
		pos += size;
	}
	return boxes;
}

function isoChild(bytes: Uint8Array, parent: Range | undefined, type: string): IsoBox | undefined {
	return parent && isoChildren(bytes, parent).find((box) => box.type === type);
}

function readTrackDelaysMs(bytes: Uint8Array, trak: IsoBox, frameCount: number): Array<number> | null {
	const mdia = isoChild(bytes, trak, 'mdia');
	const mdhd = isoChild(bytes, mdia, 'mdhd');
	const stts = isoChild(bytes, isoChild(bytes, isoChild(bytes, mdia, 'minf'), 'stbl'), 'stts');
	if (!mdhd || !stts || stts.end - stts.start < 8) return null;
	const timescaleOffset = mdhd.start + (bytes[mdhd.start] === 1 ? 20 : 12);
	if (timescaleOffset + 4 > mdhd.end) return null;
	const timescale = readU32(bytes, timescaleOffset);
	const entryCount = readU32(bytes, stts.start + 4);
	if (timescale === 0 || stts.start + 8 + entryCount * 8 > stts.end) return null;
	const delays: Array<number> = [];
	for (let entry = 0; entry < entryCount; entry++) {
		const sampleCount = readU32(bytes, stts.start + 8 + entry * 8);
		const delayMs = (readU32(bytes, stts.start + 12 + entry * 8) * 1000) / timescale;
		for (let sample = 0; sample < sampleCount; sample++) {
			if (delays.length === frameCount) return null;
			delays.push(delayMs);
		}
	}
	return delays.length === frameCount ? delays : null;
}

function readAvifFrameDelaysMs(bytes: Uint8Array, frameCount: number): Array<number> | null {
	const moov = isoChild(bytes, {start: 0, end: bytes.length}, 'moov');
	if (!moov) return null;
	for (const trak of isoChildren(bytes, moov)) {
		if (trak.type !== 'trak') continue;
		const hdlr = isoChild(bytes, isoChild(bytes, trak, 'mdia'), 'hdlr');
		if (!hdlr || hdlr.end - hdlr.start < 12 || readFourCc(bytes, hdlr.start + 8) !== 'pict') continue;
		if (isoChild(bytes, isoChild(bytes, trak, 'tref'), 'auxl')) continue;
		return readTrackDelaysMs(bytes, trak, frameCount);
	}
	return null;
}

export async function openAvifFrameSource(bytes: Uint8Array): Promise<FrameSource> {
	const ImageDecoderConstructor = getImageDecoderConstructor();
	if (!ImageDecoderConstructor || !(await canDecodeAnimatedAvif())) {
		throw new CropPipelineError('animated_avif_unsupported', 'This browser cannot decode animated AVIF frames');
	}
	const decoder = new ImageDecoderConstructor({data: bytes, type: 'image/avif'});
	let pending: FluxerImageDecoderDecodedFrame | null = null;
	try {
		await Promise.all([decoder.tracks.ready, decoder.completed]);
		const track = decoder.tracks.selectedTrack;
		const frameCount = track?.frameCount ?? 0;
		if (frameCount === 0) throw decodeFailed('AVIF has no frames');
		pending = await decoder.decode({frameIndex: 0});
		const width = pending.image.displayWidth;
		const height = pending.image.displayHeight;
		assertSourceBudget(width, height, frameCount);
		const containerDelays = readAvifFrameDelaysMs(bytes, frameCount);
		let context: OffscreenCanvasRenderingContext2D | null = null;
		let index = 0;
		return {
			width,
			height,
			frameCount,
			plays: avifPlaysFromRepetitionCount(track?.repetitionCount),
			async next() {
				if (index >= frameCount) return null;
				const frameIndex = index++;
				const decoded = pending ?? (await decoder.decode({frameIndex}));
				pending = null;
				try {
					if (!context) {
						context = new OffscreenCanvas(width, height).getContext('2d', {willReadFrequently: true});
						if (!context) throw decodeFailed('OffscreenCanvas 2d context is unavailable');
					}
					context.clearRect(0, 0, width, height);
					drawVideoFrameToCanvas(context, decoded.image);
					const imageData = context.getImageData(0, 0, width, height);
					return {
						rgba: new Uint8Array(imageData.data.buffer, imageData.data.byteOffset, imageData.data.byteLength),
						delayMs: containerDelays?.[frameIndex] ?? avifDelayMs(decoded.image.duration),
					};
				} finally {
					decoded.image.close();
				}
			},
			close() {
				pending?.image.close();
				pending = null;
				closeImageDecoder(decoder);
			},
		};
	} catch (error) {
		pending?.image.close();
		closeImageDecoder(decoder);
		throw error;
	}
}

interface PendingGifFrame {
	index: Uint8Array;
	palette: Array<GifPaletteColor>;
	delayMs: number;
	dispose: number;
}

export function openGifFrameSink(width: number, height: number, plays: number, capBytes: number): GifFrameSink {
	const encoder = GIFEncoder();
	const pixelCount = width * height;
	let previous: Uint8Array | null = null;
	let pending: PendingGifFrame | null = null;
	let wroteFirst = false;

	function write(frame: PendingGifFrame): void {
		encoder.writeFrame(frame.index, width, height, {
			palette: frame.palette,
			delay: Math.min(frame.delayMs, GIF_MAX_DELAY_CS * 10),
			transparent: true,
			transparentIndex: GIF_TRANSPARENT_INDEX,
			dispose: frame.dispose,
			...(wroteFirst ? {} : {repeat: gifRepeatFromPlays(plays)}),
		});
		wroteFirst = true;
	}

	function needsFullFrame(rgba: Uint8Array): boolean {
		if (!previous) return true;
		for (let offset = 3; offset < rgba.length; offset += 4) {
			if (previous[offset] === 255 && rgba[offset] === 0) return true;
		}
		return false;
	}

	return {
		add(rgba, delayMs) {
			if (rgba.length !== pixelCount * 4) return false;
			for (let offset = 3; offset < rgba.length; offset += 4) {
				const alpha = rgba[offset];
				if (alpha !== 0 && alpha !== 255) return false;
			}
			const full = needsFullFrame(rgba);
			const index = new Uint8Array(pixelCount);
			const palette: Array<GifPaletteColor> = [[0, 0, 0]];
			const colors = new Map<number, number>();
			for (let pixel = 0, offset = 0; pixel < pixelCount; pixel++, offset += 4) {
				if (rgba[offset + 3] === 0) continue;
				if (
					!full &&
					previous &&
					previous[offset + 3] === 255 &&
					previous[offset] === rgba[offset] &&
					previous[offset + 1] === rgba[offset + 1] &&
					previous[offset + 2] === rgba[offset + 2]
				) {
					continue;
				}
				const key = (rgba[offset] << 16) | (rgba[offset + 1] << 8) | rgba[offset + 2];
				let colorIndex = colors.get(key);
				if (colorIndex === undefined) {
					if (palette.length > GIF_MAX_OPAQUE_COLORS) return false;
					colorIndex = palette.length;
					colors.set(key, colorIndex);
					palette.push([rgba[offset], rgba[offset + 1], rgba[offset + 2]]);
				}
				index[pixel] = colorIndex;
			}
			if (pending) {
				pending.dispose = full ? 2 : 1;
				write(pending);
				if (encoder.bytesView().length > capBytes) return false;
			}
			pending = {index, palette, delayMs, dispose: 1};
			if (previous) previous.set(rgba);
			else previous = rgba.slice();
			return true;
		},
		finish() {
			if (pending) write(pending);
			pending = null;
			encoder.finish();
			return encoder.bytes();
		},
		close() {
			pending = null;
			previous = null;
		},
	};
}
