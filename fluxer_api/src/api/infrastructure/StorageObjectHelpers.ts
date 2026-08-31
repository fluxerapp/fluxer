// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFile} from 'node:child_process';
import {createHash} from 'node:crypto';
import fs from 'node:fs';
import {promisify} from 'node:util';
import sharp from 'sharp';
import {temporaryFile} from 'tempy';
import {Logger} from '../Logger';
import {isJsonRecord, parseJsonArray} from '../utils/JsonBoundaryUtils';

const execFilePromise = promisify(execFile);

interface ProcessedMediaFile {
	filePath: string;
	contentType: string;
	contentHash: string;
	contentLength: number;
	width?: number;
	height?: number;
}

interface ProcessedMediaObject {
	body: Uint8Array;
	contentType: string;
	contentHash: string;
	contentLength: number;
	width?: number;
	height?: number;
}

interface StrippedMediaBuffer {
	body: Uint8Array;
	contentType: string;
	width?: number;
	height?: number;
}

function processedMediaObject(
	body: Uint8Array,
	contentType: string,
	dimensions?: {width: number; height: number},
): ProcessedMediaObject {
	return {
		body,
		contentType,
		contentHash: createHash('sha256').update(body).digest('hex'),
		contentLength: body.length,
		...dimensions,
	};
}

async function processJpegData(sourceData: Uint8Array, contentType: string): Promise<ProcessedMediaObject> {
	const inputPath = temporaryFile({extension: 'jpg'});
	const outputPath = temporaryFile({extension: 'jpg'});
	try {
		await fs.promises.writeFile(inputPath, sourceData);
		const metadata = await sharp(sourceData).metadata();
		const needsRotation = metadata.orientation != null && metadata.orientation > 1;
		let preserveIcc = true;
		if (needsRotation) {
			const sourceQuality = await getJpegQualityEstimate(inputPath);
			const chromaSubsampling = metadata.chromaSubsampling === '4:4:4' ? '4:4:4' : '4:2:0';
			const processedBuffer = await sharp(sourceData)
				.rotate()
				.jpeg({quality: sourceQuality ?? 95, chromaSubsampling})
				.toBuffer();
			await fs.promises.writeFile(outputPath, processedBuffer);
			preserveIcc = metadata.space !== 'cmyk';
		} else {
			await fs.promises.copyFile(inputPath, outputPath);
		}
		await stripJpegMetadata(outputPath, inputPath, {preserveIcc, rotated: needsRotation});
		const finalBuffer = await fs.promises.readFile(outputPath);
		const dimensions =
			metadata.width && metadata.height
				? metadata.orientation != null && metadata.orientation >= 5 && metadata.orientation <= 8
					? {width: metadata.height, height: metadata.width}
					: {width: metadata.width, height: metadata.height}
				: undefined;
		const processed = processedMediaObject(new Uint8Array(finalBuffer), contentType, dimensions);
		const cleanupErrors = await cleanupTempFiles([inputPath, outputPath]);
		if (cleanupErrors.length > 0) {
			throw new Error(
				`Failed to cleanup temporary files: ${cleanupErrors.map((e) => e.path).join(', ')}. This may indicate disk space or permission issues.`,
			);
		}
		return processed;
	} catch (error) {
		const cleanupErrors = await cleanupTempFiles([inputPath, outputPath]);
		if (cleanupErrors.length > 0) {
			Logger.error({cleanupErrors, originalError: error}, 'Failed to cleanup temp files after operation failure');
		}
		throw error;
	}
}

function normalizeContentType(contentType: string): string {
	return contentType.split(';', 1)[0]?.trim().toLowerCase() || 'application/octet-stream';
}

function isMediaContentType(contentType: string): boolean {
	return contentType.startsWith('image/') || contentType.startsWith('video/') || contentType.startsWith('audio/');
}

function isJpegContentType(contentType: string): boolean {
	return contentType.includes('jpeg') || contentType.includes('jpg');
}

function contentTypeToMediaExtension(contentType: string): string {
	if (contentType.includes('mp4') || contentType.includes('mpeg4')) return 'mp4';
	if (contentType.includes('webm')) return 'webm';
	if (contentType.includes('quicktime')) return 'mov';
	if (contentType.includes('x-matroska')) return 'mkv';
	if (contentType.includes('avi')) return 'avi';
	if (contentType.includes('flv')) return 'flv';
	if (contentType.includes('mp2t')) return 'ts';
	if (contentType.includes('mpeg')) return contentType.startsWith('audio/') ? 'mp3' : 'mpeg';
	if (contentType.includes('x-ms-wmv')) return 'wmv';
	if (contentType.includes('wav')) return 'wav';
	if (contentType.includes('flac')) return 'flac';
	if (contentType.includes('aac')) return 'aac';
	if (contentType.includes('aiff')) return 'aiff';
	if (contentType.includes('ogg')) return 'ogg';
	return 'mp4';
}

function imageContentTypeForSharpFormat(format: string | undefined, fallback: string): string {
	switch (format) {
		case 'png':
			return 'image/png';
		case 'gif':
			return 'image/gif';
		case 'webp':
			return 'image/webp';
		case 'avif':
		case 'heif':
			return 'image/avif';
		case 'tiff':
			return 'image/tiff';
		case 'svg':
			return 'image/svg+xml';
		case 'jxl':
			return 'image/jxl';
		default:
			return fallback;
	}
}

export async function stripNonJpegImageMetadata(data: Uint8Array): Promise<Uint8Array> {
	return (await stripNonJpegImageMetadataForUpload(data, 'application/octet-stream')).body;
}

export async function stripNonJpegImageMetadataForUpload(
	data: Uint8Array,
	contentType: string,
): Promise<StrippedMediaBuffer> {
	const normalizedContentType = normalizeContentType(contentType);
	if (isPng(data)) {
		return {
			body: stripPngMetadataChunks(data),
			contentType: normalizedContentType === 'image/apng' ? 'image/apng' : 'image/png',
		};
	}
	const image = sharp(data, {animated: true});
	const metadata = await image.metadata();
	switch (metadata.format) {
		case 'png':
			return {body: await image.png().toBuffer(), contentType: 'image/png'};
		case 'gif':
			return {body: await image.gif().toBuffer(), contentType: 'image/gif'};
		case 'webp':
			return {body: await image.webp({lossless: true}).toBuffer(), contentType: 'image/webp'};
		case 'heif':
			return {body: await image.avif().toBuffer(), contentType: 'image/avif'};
		case 'tiff':
			return {body: await image.tiff().toBuffer(), contentType: 'image/tiff'};
		default: {
			Logger.warn({format: metadata.format}, 'Unknown image format, trying exiftool metadata stripping');
			const outputContentType = imageContentTypeForSharpFormat(metadata.format, normalizedContentType);
			return {
				body: await stripMetadataWithExiftool(data, imageExtensionForContentType(outputContentType)),
				contentType: outputContentType,
			};
		}
	}
}

const PNG_SIGNATURE = new Uint8Array([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]);
const PNG_CHUNKS_TO_KEEP = new Set([
	'IHDR',
	'PLTE',
	'IDAT',
	'IEND',
	'tRNS',
	'gAMA',
	'cHRM',
	'sRGB',
	'iCCP',
	'cICP',
	'acTL',
	'fcTL',
	'fdAT',
]);

function isPng(data: Uint8Array): boolean {
	if (data.length < PNG_SIGNATURE.length) return false;
	for (let i = 0; i < PNG_SIGNATURE.length; i++) if (data[i] !== PNG_SIGNATURE[i]) return false;
	return true;
}

function isCriticalPngChunk(type: string): boolean {
	const first = type.charCodeAt(0);
	return first >= 0x41 && first <= 0x5a;
}

function stripPngMetadataChunks(data: Uint8Array): Uint8Array {
	if (!isPng(data)) return data;
	const chunks: Array<Uint8Array> = [data.slice(0, PNG_SIGNATURE.length)];
	let offset = PNG_SIGNATURE.length;
	while (offset + 12 <= data.length) {
		const length =
			((data[offset]! << 24) | (data[offset + 1]! << 16) | (data[offset + 2]! << 8) | data[offset + 3]!) >>> 0;
		const chunkEnd = offset + 12 + length;
		if (chunkEnd > data.length) {
			throw new Error('Invalid PNG chunk length while stripping metadata');
		}
		const type = String.fromCharCode(data[offset + 4]!, data[offset + 5]!, data[offset + 6]!, data[offset + 7]!);
		if (PNG_CHUNKS_TO_KEEP.has(type) || isCriticalPngChunk(type)) {
			chunks.push(data.slice(offset, chunkEnd));
		}
		offset = chunkEnd;
		if (type === 'IEND') break;
	}
	const totalLength = chunks.reduce((sum, chunk) => sum + chunk.length, 0);
	const output = new Uint8Array(totalLength);
	let cursor = 0;
	for (const chunk of chunks) {
		output.set(chunk, cursor);
		cursor += chunk.length;
	}
	return output;
}

function imageExtensionForContentType(contentType: string): string {
	if (contentType.includes('svg')) return 'svg';
	if (contentType.includes('tiff')) return 'tiff';
	if (contentType.includes('bmp')) return 'bmp';
	if (contentType.includes('jxl')) return 'jxl';
	if (contentType.includes('heic')) return 'heic';
	if (contentType.includes('heif')) return 'heif';
	if (contentType.includes('avif')) return 'avif';
	if (contentType.includes('webp')) return 'webp';
	if (contentType.includes('gif')) return 'gif';
	if (contentType.includes('png')) return 'png';
	return 'img';
}

async function stripMetadataWithExiftool(data: Uint8Array, extension: string): Promise<Uint8Array> {
	const filePath = temporaryFile({extension});
	try {
		await fs.promises.writeFile(filePath, data);
		await execFilePromise('exiftool', ['-all=', '-overwrite_original', '-F', filePath]);
		const result = await fs.promises.readFile(filePath);
		return new Uint8Array(result);
	} finally {
		await cleanupTempFiles([filePath]);
	}
}

async function stripMediaMetadata(data: Uint8Array, contentType: string): Promise<StrippedMediaBuffer | null> {
	const normalizedContentType = normalizeContentType(contentType);
	if (!isMediaContentType(normalizedContentType)) return null;
	if (isJpegContentType(normalizedContentType)) {
		const processed = await processJpegData(data, normalizedContentType);
		return {
			body: processed.body,
			contentType: processed.contentType,
			...(processed.width && processed.height ? {width: processed.width, height: processed.height} : {}),
		};
	}
	if (normalizedContentType.startsWith('image/')) {
		return stripNonJpegImageMetadataForUpload(data, normalizedContentType);
	}
	if (normalizedContentType.startsWith('video/') || normalizedContentType.startsWith('audio/')) {
		return {
			body: await stripVideoMetadata(data, normalizedContentType),
			contentType: normalizedContentType,
		};
	}
	return null;
}

export async function buildProcessedMediaObject(
	data: Uint8Array,
	contentType: string,
): Promise<ProcessedMediaObject | null> {
	const stripped = await stripMediaMetadata(data, contentType);
	if (!stripped) return null;
	const dimensions = stripped.width && stripped.height ? {width: stripped.width, height: stripped.height} : undefined;
	return processedMediaObject(stripped.body, stripped.contentType, dimensions);
}

async function stripVideoMetadata(data: Uint8Array, contentType: string): Promise<Uint8Array> {
	const ext = contentTypeToMediaExtension(contentType);
	const inputPath = temporaryFile({extension: ext});
	const outputPath = temporaryFile({extension: ext});
	try {
		await fs.promises.writeFile(inputPath, data);
		await stripVideoMetadataInPlace(inputPath, outputPath);
		const result = await fs.promises.readFile(outputPath);
		return new Uint8Array(result);
	} finally {
		await cleanupTempFiles([inputPath, outputPath]);
	}
}

async function stripVideoMetadataInPlace(inputPath: string, outputPath: string): Promise<void> {
	await execFilePromise('ffmpeg', [
		'-hide_banner',
		'-loglevel',
		'warning',
		'-i',
		inputPath,
		'-map',
		'0',
		'-map_metadata',
		'-1',
		'-map_metadata:s',
		'-1',
		'-map_metadata:c',
		'-1',
		'-map_chapters',
		'-1',
		'-c',
		'copy',
		'-y',
		outputPath,
	]);
}

async function cleanupTempFiles(paths: ReadonlyArray<string>): Promise<
	Array<{
		path: string;
		error: unknown;
	}>
> {
	const cleanupErrors: Array<{
		path: string;
		error: unknown;
	}> = [];
	await Promise.all(
		paths.map((filePath) =>
			fs.promises.unlink(filePath).catch((error) => {
				cleanupErrors.push({path: filePath, error});
			}),
		),
	);
	return cleanupErrors;
}

async function getJpegQualityEstimate(filePath: string): Promise<number | null> {
	try {
		const {stdout} = await execFilePromise('exiftool', ['-JPEGQualityEstimate#', '-n', '-j', filePath]);
		const [entry] = parseJsonArray(stdout) ?? [];
		if (!isJsonRecord(entry)) {
			return null;
		}
		const value = entry?.JPEGQualityEstimate;
		if (typeof value === 'number' && value >= 1 && value <= 100) {
			return Math.round(value);
		}
		return null;
	} catch (error) {
		Logger.warn({error, filePath}, 'Failed to read JPEG quality estimate, falling back to default');
		return null;
	}
}

export async function hashFileSha256(filePath: string): Promise<string> {
	const hash = createHash('sha256');
	const stream = fs.createReadStream(filePath, {highWaterMark: 1024 * 1024});
	try {
		for await (const chunk of stream) {
			hash.update(chunk as Buffer);
		}
	} catch (error) {
		stream.destroy();
		throw error;
	}
	return hash.digest('hex');
}

async function fileSize(filePath: string): Promise<number> {
	const stat = await fs.promises.stat(filePath);
	return stat.size;
}

async function processJpegFileToFile(
	inputPath: string,
	outputPath: string,
	contentType: string,
): Promise<{contentType: string; width?: number; height?: number}> {
	const metadata = await sharp(inputPath).metadata();
	const needsRotation = metadata.orientation != null && metadata.orientation > 1;
	let preserveIcc = true;
	const rotatedPath = needsRotation ? temporaryFile({extension: 'jpg'}) : null;
	try {
		if (needsRotation && rotatedPath) {
			const sourceQuality = await getJpegQualityEstimate(inputPath);
			const chromaSubsampling = metadata.chromaSubsampling === '4:4:4' ? '4:4:4' : '4:2:0';
			await sharp(inputPath)
				.rotate()
				.jpeg({quality: sourceQuality ?? 95, chromaSubsampling})
				.toFile(rotatedPath);
			preserveIcc = metadata.space !== 'cmyk';
			await fs.promises.copyFile(rotatedPath, outputPath);
		} else {
			await fs.promises.copyFile(inputPath, outputPath);
		}
		await stripJpegMetadata(outputPath, inputPath, {preserveIcc, rotated: needsRotation});
	} finally {
		if (rotatedPath) {
			await cleanupTempFiles([rotatedPath]);
		}
	}
	const dimensions =
		metadata.width && metadata.height
			? metadata.orientation != null && metadata.orientation >= 5 && metadata.orientation <= 8
				? {width: metadata.height, height: metadata.width}
				: {width: metadata.width, height: metadata.height}
			: undefined;
	return {contentType, ...dimensions};
}

async function stripNonJpegImageFileToFile(
	inputPath: string,
	outputPath: string,
	contentType: string,
): Promise<{contentType: string; width?: number; height?: number}> {
	const normalizedContentType = normalizeContentType(contentType);
	const probe = await sharp(inputPath).metadata();
	const width = probe.width;
	const height = probe.height;
	const dimensions = width && height ? {width, height} : undefined;
	if (await isPngFile(inputPath)) {
		await stripPngMetadataChunksToFile(inputPath, outputPath);
		return {contentType: normalizedContentType === 'image/apng' ? 'image/apng' : 'image/png', ...dimensions};
	}
	const image = sharp(inputPath, {animated: true});
	const meta = await image.metadata();
	switch (meta.format) {
		case 'png':
			await image.png().toFile(outputPath);
			return {contentType: 'image/png', ...dimensions};
		case 'gif':
			await image.gif().toFile(outputPath);
			return {contentType: 'image/gif', ...dimensions};
		case 'webp':
			await image.webp({lossless: true}).toFile(outputPath);
			return {contentType: 'image/webp', ...dimensions};
		case 'heif':
			await image.avif().toFile(outputPath);
			return {contentType: 'image/avif', ...dimensions};
		case 'tiff':
			await image.tiff().toFile(outputPath);
			return {contentType: 'image/tiff', ...dimensions};
		default: {
			Logger.warn({format: meta.format}, 'Unknown image format, trying exiftool metadata stripping');
			const outputContentType = imageContentTypeForSharpFormat(meta.format, normalizedContentType);
			await stripMetadataWithExiftoolFile(inputPath, outputPath);
			return {contentType: outputContentType, ...dimensions};
		}
	}
}

async function stripVideoFileToFile(
	inputPath: string,
	outputPath: string,
	contentType: string,
): Promise<{contentType: string; width?: number; height?: number}> {
	await stripVideoMetadataInPlace(inputPath, outputPath);
	return {contentType: normalizeContentType(contentType)};
}

export async function processMediaFile(
	inputPath: string,
	outputPath: string,
	contentType: string,
): Promise<ProcessedMediaFile | null> {
	const normalized = normalizeContentType(contentType);
	if (!isMediaContentType(normalized)) {
		return null;
	}
	let result: {contentType: string; width?: number; height?: number};
	if (isJpegContentType(normalized)) {
		result = await processJpegFileToFile(inputPath, outputPath, normalized);
	} else if (normalized.startsWith('image/')) {
		result = await stripNonJpegImageFileToFile(inputPath, outputPath, normalized);
	} else if (normalized.startsWith('video/') || normalized.startsWith('audio/')) {
		result = await stripVideoFileToFile(inputPath, outputPath, normalized);
	} else {
		return null;
	}
	const [contentHash, contentLength] = await Promise.all([hashFileSha256(outputPath), fileSize(outputPath)]);
	return {
		filePath: outputPath,
		contentType: result.contentType,
		contentHash,
		contentLength,
		...(result.width && result.height ? {width: result.width, height: result.height} : {}),
	};
}

async function isPngFile(filePath: string): Promise<boolean> {
	const handle = await fs.promises.open(filePath, 'r');
	try {
		const buf = Buffer.alloc(PNG_SIGNATURE.length);
		const {bytesRead} = await handle.read(buf, 0, PNG_SIGNATURE.length, 0);
		if (bytesRead < PNG_SIGNATURE.length) return false;
		for (let i = 0; i < PNG_SIGNATURE.length; i++) {
			if (buf[i] !== PNG_SIGNATURE[i]) return false;
		}
		return true;
	} finally {
		await handle.close();
	}
}

async function stripPngMetadataChunksToFile(inputPath: string, outputPath: string): Promise<void> {
	const reader = await fs.promises.open(inputPath, 'r');
	const writer = fs.createWriteStream(outputPath);
	try {
		await writer.write(PNG_SIGNATURE);
		let position = PNG_SIGNATURE.length;
		const header = Buffer.alloc(8);
		while (true) {
			const headRead = await reader.read(header, 0, 8, position);
			if (headRead.bytesRead < 8) break;
			const length = ((header[0]! << 24) | (header[1]! << 16) | (header[2]! << 8) | header[3]!) >>> 0;
			const type = String.fromCharCode(header[4]!, header[5]!, header[6]!, header[7]!);
			const chunkTotal = 8 + length + 4;
			const keep = PNG_CHUNKS_TO_KEEP.has(type) || isCriticalPngChunk(type);
			if (keep) {
				const chunk = Buffer.alloc(chunkTotal);
				const {bytesRead} = await reader.read(chunk, 0, chunkTotal, position);
				if (bytesRead < chunkTotal) {
					throw new Error('Invalid PNG chunk length while stripping metadata');
				}
				if (!writer.write(chunk)) {
					await new Promise<void>((resolve) => writer.once('drain', () => resolve()));
				}
			}
			position += chunkTotal;
			if (type === 'IEND') break;
		}
		await new Promise<void>((resolve, reject) => {
			writer.end((err: Error | null | undefined) => (err ? reject(err) : resolve()));
		});
	} catch (error) {
		writer.destroy();
		throw error;
	} finally {
		await reader.close();
	}
}

async function stripMetadataWithExiftoolFile(inputPath: string, outputPath: string): Promise<void> {
	await fs.promises.copyFile(inputPath, outputPath);
	await execFilePromise('exiftool', ['-all=', '-overwrite_original', '-F', outputPath]);
}

async function stripJpegMetadata(
	filePath: string,
	originalFilePath: string,
	opts: {
		preserveIcc: boolean;
		rotated: boolean;
	},
): Promise<void> {
	if (opts.rotated) {
		await execFilePromise('exiftool', [
			'-all=',
			'-tagsfromfile',
			originalFilePath,
			...(opts.preserveIcc ? ['-icc_profile'] : []),
			'-jfif:XResolution<XResolution',
			'-jfif:YResolution<YResolution',
			'-jfif:ResolutionUnit<ResolutionUnit',
			'-overwrite_original',
			'-F',
			filePath,
		]);
		return;
	}
	await execFilePromise('exiftool', [
		'-all=',
		'--JFIF:all',
		...(opts.preserveIcc ? ['--ICC_Profile:all'] : []),
		'-overwrite_original',
		'-F',
		filePath,
	]);
}
