// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {Config} from '../Config';
import {
	IMediaService,
	type MediaProxyFrameRequest,
	type MediaProxyFrameResponse,
	type MediaProxyMetadataRequest,
	type MediaProxyMetadataResponse,
} from '../infrastructure/IMediaService';
import type {IStorageService} from '../infrastructure/IStorageService';

export class TestMediaService extends IMediaService {
	constructor(private readonly storageService: IStorageService) {
		super();
	}

	async getMetadata(request: MediaProxyMetadataRequest): Promise<MediaProxyMetadataResponse | null> {
		if (request.type === 'base64') {
			return this.analyzeBase64Image(request.base64);
		}
		if (request.type === 's3') {
			let stored: Buffer | null = null;
			try {
				stored = Buffer.from(await this.storageService.readObject(request.bucket, request.key));
			} catch {
				stored = null;
			}
			const format = (stored ? this.detectImageFormat(stored) : null) ?? this.getFormatFromFilename(request.key);
			return {
				format,
				content_type: this.contentTypeForFormat(format),
				content_hash: crypto.createHash('md5').update(request.key).digest('hex'),
				size: stored?.length ?? 1024,
				width: 128,
				height: 128,
				animated: stored ? this.isAnimatedImage(stored, format) : format === 'gif',
				nsfw: false,
				placeholder: this.fakePlaceholder(request.key),
			};
		}
		if (request.type === 'upload') {
			const filename = request.upload_filename.toLowerCase();
			const format = this.getFormatFromFilename(filename);
			const uploadedFile = await this.storageService.getObjectMetadata(
				Config.s3.buckets.uploads,
				request.upload_filename,
			);
			return {
				format,
				content_type: this.contentTypeForFormat(format),
				content_hash: crypto.createHash('md5').update(request.upload_filename).digest('hex'),
				size: uploadedFile?.contentLength ?? 1024,
				width: 128,
				height: 128,
				animated: format === 'gif',
				nsfw: false,
				placeholder: this.fakePlaceholder(request.upload_filename),
			};
		}
		if (request.type === 'external') {
			return {
				format: 'png',
				content_type: 'image/png',
				content_hash: crypto.createHash('md5').update(request.url).digest('hex'),
				size: 1024,
				width: 128,
				height: 128,
				animated: false,
				nsfw: false,
				placeholder: this.fakePlaceholder(request.url),
			};
		}
		return null;
	}

	private fakePlaceholder(seed: string): string {
		return crypto.createHash('sha256').update(seed).digest('base64').slice(0, 24);
	}

	getExternalMediaProxyURL(): string {
		return 'https://media-proxy.test';
	}

	async getThumbnail(): Promise<Buffer | null> {
		return Buffer.alloc(1024);
	}

	async extractFrames(_request: MediaProxyFrameRequest): Promise<MediaProxyFrameResponse> {
		return {
			frames: [
				{
					timestamp: 0,
					mime_type: 'image/png',
					base64: 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==',
				},
			],
		};
	}

	private analyzeBase64Image(base64: string): MediaProxyMetadataResponse | null {
		try {
			const buffer = Buffer.from(base64, 'base64');
			if (buffer.length === 0) {
				return null;
			}
			const format = this.detectImageFormat(buffer);
			if (!format) {
				return null;
			}
			return {
				format,
				content_type: this.contentTypeForFormat(format),
				content_hash: crypto.createHash('md5').update(buffer).digest('hex'),
				size: buffer.length,
				width: 128,
				height: 128,
				animated: format === 'gif',
				nsfw: false,
			};
		} catch (_error) {
			return null;
		}
	}

	private detectImageFormat(buffer: Buffer): string | null {
		if (this.isSvg(buffer)) {
			return 'svg';
		}
		if (buffer.length < 12) {
			return null;
		}
		const first12Bytes = buffer.subarray(0, 12);
		if (this.isPng(first12Bytes)) {
			return 'png';
		}
		if (this.isGif(first12Bytes)) {
			return 'gif';
		}
		if (this.isWebP(first12Bytes)) {
			return 'webp';
		}
		if (this.isJpeg(first12Bytes)) {
			return 'jpeg';
		}
		return null;
	}

	private isSvg(buffer: Buffer): boolean {
		const prefix = buffer
			.subarray(0, 1024)
			.toString('utf8')
			.replace(/^\uFEFF/, '')
			.trimStart()
			.toLowerCase();
		return prefix.startsWith('<svg') || (prefix.startsWith('<?xml') && prefix.includes('<svg'));
	}

	private isPng(bytes: Buffer): boolean {
		return bytes[0] === 0x89 && bytes[1] === 0x50 && bytes[2] === 0x4e && bytes[3] === 0x47;
	}

	private isGif(bytes: Buffer): boolean {
		return (
			bytes[0] === 0x47 &&
			bytes[1] === 0x49 &&
			bytes[2] === 0x46 &&
			bytes[3] === 0x38 &&
			(bytes[4] === 0x37 || bytes[4] === 0x39) &&
			bytes[5] === 0x61
		);
	}

	private isWebP(bytes: Buffer): boolean {
		return (
			bytes[0] === 0x52 &&
			bytes[1] === 0x49 &&
			bytes[2] === 0x46 &&
			bytes[3] === 0x46 &&
			bytes[8] === 0x57 &&
			bytes[9] === 0x45 &&
			bytes[10] === 0x42 &&
			bytes[11] === 0x50
		);
	}

	private isJpeg(bytes: Buffer): boolean {
		return bytes[0] === 0xff && bytes[1] === 0xd8 && bytes[2] === 0xff;
	}

	private isAnimatedImage(buffer: Buffer, format: string): boolean {
		if (format === 'gif') return true;
		if (format === 'webp') return buffer.includes(Buffer.from('ANIM'));
		if (format === 'png') return buffer.includes(Buffer.from('acTL'));
		return false;
	}

	private getFormatFromFilename(filename: string): string {
		const ext = filename.split('.').pop()?.toLowerCase() ?? 'png';
		if (['png', 'gif', 'webp', 'jpeg', 'jpg', 'svg'].includes(ext)) {
			return ext === 'jpg' ? 'jpeg' : ext;
		}
		return 'png';
	}

	private contentTypeForFormat(format: string): string {
		if (format === 'jpeg') return 'image/jpeg';
		if (format === 'svg') return 'image/svg+xml';
		return `image/${format}`;
	}
}
