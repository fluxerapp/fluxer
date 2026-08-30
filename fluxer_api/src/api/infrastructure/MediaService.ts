// SPDX-License-Identifier: AGPL-3.0-or-later

import {ExplicitContentCannotBeSentError} from '@fluxer/errors/src/domains/moderation/ExplicitContentCannotBeSentError';
import * as MediaProxyUtils from '@pkgs/media_proxy_utils/src/MediaProxyUtils';
import {ms} from 'itty-time';
import {Config} from '../Config';
import {Logger} from '../Logger';
import * as FetchUtils from '../utils/FetchUtils';
import {isJsonRecord, parseJsonWithGuard} from '../utils/JsonBoundaryUtils';
import {
	IMediaService,
	MEDIA_PROXY_METADATA_REQUEST_VERSION,
	type MediaProxyFrameRequest,
	type MediaProxyFrameResponse,
	type MediaProxyMetadataRequest,
	type MediaProxyMetadataResponse,
	type MediaProxyNsfwMode,
} from './IMediaService';

type MediaProxyRequestBody =
	| MediaProxyMetadataRequest
	| MediaProxyFrameRequest
	| {
			type: 'upload';
			upload_filename: string;
	  };

const MEDIA_PROXY_METADATA_MAX_BYTES = 256 * 1024;
const MEDIA_PROXY_METADATA_WITH_BASE64_MAX_BYTES = 64 * 1024 * 1024;
const MEDIA_PROXY_ERROR_MAX_BYTES = 16 * 1024;
const MEDIA_PROXY_THUMBNAIL_MAX_BYTES = 8 * 1024 * 1024;
const MEDIA_PROXY_FRAMES_MAX_BYTES = 512 * 1024;
const MEDIA_PROXY_REQUEST_TIMEOUT_MS = ms('30 seconds');

function isMediaProxyMetadataResponse(value: unknown): value is MediaProxyMetadataResponse {
	if (!isJsonRecord(value)) return false;
	return (
		typeof value.format === 'string' &&
		typeof value.content_type === 'string' &&
		typeof value.content_hash === 'string' &&
		typeof value.size === 'number' &&
		(value.width === undefined || value.width === null || typeof value.width === 'number') &&
		(value.height === undefined || value.height === null || typeof value.height === 'number') &&
		(value.duration === undefined || typeof value.duration === 'number') &&
		(value.placeholder === undefined || typeof value.placeholder === 'string') &&
		(value.base64 === undefined || typeof value.base64 === 'string') &&
		(value.animated === undefined || typeof value.animated === 'boolean') &&
		typeof value.nsfw === 'boolean' &&
		(value.nsfw_probability === undefined || typeof value.nsfw_probability === 'number')
	);
}

function isMediaProxyFrameResponse(value: unknown): value is MediaProxyFrameResponse {
	if (!isJsonRecord(value) || !Array.isArray(value.frames)) return false;
	return value.frames.every(
		(frame) =>
			isJsonRecord(frame) &&
			typeof frame.timestamp === 'number' &&
			typeof frame.mime_type === 'string' &&
			typeof frame.base64 === 'string',
	);
}

function metadataResponseLimit(request: MediaProxyMetadataRequest): number {
	if ((request.type === 'external' || request.type === 's3') && request.with_base64) {
		return MEDIA_PROXY_METADATA_WITH_BASE64_MAX_BYTES;
	}
	return MEDIA_PROXY_METADATA_MAX_BYTES;
}

function normalizeMetadataDimensions(
	metadata: MediaProxyMetadataResponse,
): Pick<MediaProxyMetadataResponse, 'width' | 'height'> {
	if (
		typeof metadata.width === 'number' &&
		metadata.width > 0 &&
		typeof metadata.height === 'number' &&
		metadata.height > 0
	) {
		return {
			width: metadata.width,
			height: metadata.height,
		};
	}
	return {
		width: null,
		height: null,
	};
}

export class MediaService extends IMediaService {
	private readonly proxyURL: URL;

	constructor() {
		super();
		this.proxyURL = new URL(Config.endpoints.media);
	}

	async getMetadata(request: MediaProxyMetadataRequest): Promise<MediaProxyMetadataResponse | null> {
		const nsfwMode = this.getNsfwMode(request);
		const response = await this.makeRequest('/_metadata', this.toMetadataWireRequest(request, nsfwMode));
		if (!response) {
			return null;
		}
		try {
			const responseText = await FetchUtils.streamToStringWithLimit(response.body, {
				maxBytes: metadataResponseLimit(request),
				headers: response.headers,
				description: 'Media proxy metadata response',
			});
			if (!responseText) {
				Logger.error('Media proxy returned empty response');
				return null;
			}
			const metadata = parseJsonWithGuard(responseText, isMediaProxyMetadataResponse);
			if (!metadata) {
				Logger.error('Media proxy returned invalid metadata response');
				return null;
			}
			if (nsfwMode === 'block' && metadata.nsfw) {
				throw new ExplicitContentCannotBeSentError(metadata.nsfw_probability ?? 0);
			}
			return {
				...metadata,
				format: metadata.format.toLowerCase(),
				...normalizeMetadataDimensions(metadata),
			};
		} catch (error) {
			if (error instanceof ExplicitContentCannotBeSentError) {
				throw error;
			}
			Logger.error({error}, 'Failed to parse media proxy metadata response');
			return null;
		}
	}

	getExternalMediaProxyURL(url: string): string {
		let urlObj: URL;
		try {
			urlObj = new URL(url);
		} catch (_e) {
			return this.handleExternalURL(url);
		}
		if (urlObj.host === this.proxyURL.host) {
			return url;
		}
		return this.handleExternalURL(url);
	}

	async getThumbnail(uploadFilename: string): Promise<Buffer | null> {
		const response = await this.makeRequest('/_thumbnail', {
			type: 'upload',
			upload_filename: uploadFilename,
		});
		if (!response) return null;
		try {
			const bytes = await FetchUtils.streamToBufferWithLimit(response.body, {
				maxBytes: MEDIA_PROXY_THUMBNAIL_MAX_BYTES,
				headers: response.headers,
				description: 'Media proxy thumbnail response',
			});
			return Buffer.from(bytes);
		} catch (error) {
			Logger.error({error, uploadFilename}, 'Failed to parse media proxy thumbnail response');
			return null;
		}
	}

	async extractFrames(request: MediaProxyFrameRequest): Promise<MediaProxyFrameResponse> {
		const response = await this.makeRequest('/_frames', request);
		if (!response) {
			throw new Error('Unable to extract frames: no response from media proxy');
		}
		const responseText = await FetchUtils.streamToStringWithLimit(response.body, {
			maxBytes: MEDIA_PROXY_FRAMES_MAX_BYTES,
			headers: response.headers,
			description: 'Media proxy frames response',
		});
		const data = parseJsonWithGuard(responseText, isMediaProxyFrameResponse);
		if (!data) {
			throw new Error('Unable to extract frames: invalid response from media proxy');
		}
		return data;
	}

	private async makeRequest(endpoint: string, body: MediaProxyRequestBody): Promise<Response | null> {
		try {
			const url = `http://${Config.mediaProxy.host}:${Config.mediaProxy.port}${endpoint}`;
			const response = await fetch(url, {
				method: 'POST',
				body: JSON.stringify(body),
				headers: {
					'Content-Type': 'application/json',
					Authorization: `Bearer ${Config.mediaProxy.secretKey}`,
				},
				signal: AbortSignal.timeout(MEDIA_PROXY_REQUEST_TIMEOUT_MS),
			});
			if (!response.ok) {
				const errorText = await FetchUtils.streamToStringWithLimit(response.body, {
					maxBytes: MEDIA_PROXY_ERROR_MAX_BYTES,
					headers: response.headers,
					description: 'Media proxy error response',
				}).catch(() => 'Could not read error body');
				Logger.error(
					{
						status: response.status,
						statusText: response.statusText,
						errorBody: errorText,
						body: this.sanitizeRequestBody(body),
						endpoint: url,
					},
					'Media proxy request failed',
				);
				return null;
			}
			return response;
		} catch (error) {
			Logger.error({error, endpoint}, 'Failed to make media proxy request');
			return null;
		}
	}

	private sanitizeRequestBody(body: MediaProxyRequestBody): MediaProxyRequestBody {
		if (body?.type === 'base64') {
			return {
				...body,
				base64: '[BASE64_DATA_OMITTED]',
			};
		}
		return body;
	}

	private getNsfwMode(request: MediaProxyMetadataRequest): MediaProxyNsfwMode {
		return request.nsfw;
	}

	private toMetadataWireRequest(
		request: MediaProxyMetadataRequest,
		nsfw: MediaProxyNsfwMode,
	): MediaProxyMetadataRequest {
		return {
			...request,
			version: MEDIA_PROXY_METADATA_REQUEST_VERSION,
			nsfw,
		};
	}

	private handleExternalURL(url: string): string {
		return MediaProxyUtils.getExternalMediaProxyURL({
			inputURL: url,
			mediaProxyEndpoint: Config.endpoints.media,
			mediaProxySecretKey: Config.mediaProxy.secretKey,
		});
	}
}
