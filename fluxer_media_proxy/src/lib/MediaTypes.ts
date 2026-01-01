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

export const MEDIA_TYPES = {
	IMAGE: {
		extensions: ['jpg', 'jpeg', 'png', 'gif', 'webp', 'avif', 'svg'],
		mimes: {
			jpg: 'image/jpeg',
			jpeg: 'image/jpeg',
			png: 'image/png',
			gif: 'image/gif',
			webp: 'image/webp',
			avif: 'image/avif',
			svg: 'image/svg+xml',
		},
	},
	VIDEO: {
		extensions: ['mp4', 'webm', 'mov', 'mkv', 'avi'],
		mimes: {
			mp4: 'video/mp4',
			webm: 'video/webm',
			mov: 'video/quicktime',
			mkv: 'video/x-matroska',
			avi: 'video/x-msvideo',
		},
		codecs: new Set([
			'h264',
			'avc1',
			'hevc',
			'hev1',
			'hvc1',
			'h265',
			'vp8',
			'vp9',
			'av1',
			'av01',
			'theora',
			'mpeg4',
			'mpeg2video',
			'mpeg1video',
			'h263',
			'prores',
			'mjpeg',
			'wmv1',
			'wmv2',
			'wmv3',
			'vc1',
			'msmpeg4v3',
		]),
		bannedCodecs: new Set(['prores_4444', 'prores_4444xq', 'apch', 'apcn', 'apcs', 'apco', 'ap4h', 'ap4x']),
	},
	AUDIO: {
		extensions: ['mp3', 'wav', 'flac', 'opus', 'aac', 'm4a', 'ogg'],
		mimes: {
			mp3: 'audio/mpeg',
			wav: 'audio/wav',
			flac: 'audio/flac',
			opus: 'audio/opus',
			aac: 'audio/aac',
			m4a: 'audio/mp4',
			ogg: 'audio/ogg',
		},
		codecs: new Set(['aac', 'mp4a', 'mp3', 'opus', 'vorbis', 'flac', 'pcm_s16le', 'pcm_s24le', 'pcm_f32le']),
	},
};

export const SUPPORTED_EXTENSIONS = {
	...MEDIA_TYPES.IMAGE.mimes,
	...MEDIA_TYPES.VIDEO.mimes,
	...MEDIA_TYPES.AUDIO.mimes,
};

export const SUPPORTED_MIME_TYPES = new Set(Object.values(SUPPORTED_EXTENSIONS));

export type SupportedExtension = keyof typeof SUPPORTED_EXTENSIONS;

export type ErrorType =
	| 'timeout'
	| 'upstream_5xx'
	| 'not_found'
	| 'bad_request'
	| 'forbidden'
	| 'unauthorized'
	| 'payload_too_large'
	| 'other';

export interface ErrorContext {
	errorType?: ErrorType;
	errorSource?: string;
}

export interface HonoEnv {
	Variables: {
		tempFiles: Array<string>;
		metricsErrorContext?: ErrorContext;
	};
}
