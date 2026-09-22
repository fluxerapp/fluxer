// SPDX-License-Identifier: AGPL-3.0-or-later

export const MEDIA_PROXY_METADATA_REQUEST_VERSION = 2 as const;

export type MediaProxyNsfwMode = 'block' | 'flag' | 'allow';

export function mediaProxyMetadataPolicy(nsfw: MediaProxyNsfwMode): MediaProxyMetadataPolicy {
	return {
		version: MEDIA_PROXY_METADATA_REQUEST_VERSION,
		nsfw,
	};
}

interface MediaProxyMetadataPolicy {
	version?: typeof MEDIA_PROXY_METADATA_REQUEST_VERSION;
	nsfw: MediaProxyNsfwMode;
}

export type MediaProxyMetadataExternalRequest = {
	type: 'external';
	url: string;
	with_base64?: boolean;
} & MediaProxyMetadataPolicy;
export type MediaProxyMetadataUploadRequest = {
	type: 'upload';
	upload_filename: string;
	filename?: string;
} & MediaProxyMetadataPolicy;
export type MediaProxyMetadataBase64Request = {
	type: 'base64';
	base64: string;
} & MediaProxyMetadataPolicy;
export type MediaProxyMetadataS3Request = {
	type: 's3';
	bucket: string;
	key: string;
	with_base64?: boolean;
} & MediaProxyMetadataPolicy;
export type MediaProxyMetadataRequest =
	| MediaProxyMetadataExternalRequest
	| MediaProxyMetadataUploadRequest
	| MediaProxyMetadataBase64Request
	| MediaProxyMetadataS3Request;

export interface MediaProxyMetadataResponse {
	format: string;
	content_type: string;
	content_hash: string;
	size: number;
	width?: number | null;
	height?: number | null;
	duration?: number;
	placeholder?: string;
	base64?: string;
	animated?: boolean;
	nsfw: boolean;
	nsfw_probability?: number;
}

export interface MediaProxySniffResponse {
	content_type: string | null;
}

export type MediaProxyFrameRequest =
	| {
			type: 'upload';
			upload_filename: string;
	  }
	| {
			type: 's3';
			bucket: string;
			key: string;
	  };

export interface MediaProxyFrameData {
	timestamp: number;
	mime_type: string;
	base64: string;
}

export interface MediaProxyFrameResponse {
	frames: Array<MediaProxyFrameData>;
}

export abstract class IMediaService {
	abstract getMetadata(request: MediaProxyMetadataRequest): Promise<MediaProxyMetadataResponse | null>;

	abstract getExternalMediaProxyURL(url: string): string;

	abstract getThumbnail(uploadFilename: string): Promise<Buffer | null>;

	abstract sniffUpload(uploadFilename: string): Promise<MediaProxySniffResponse | null>;

	abstract extractFrames(request: MediaProxyFrameRequest): Promise<MediaProxyFrameResponse>;
}
