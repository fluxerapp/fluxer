// SPDX-License-Identifier: AGPL-3.0-or-later

import {signAttachmentUrl} from '@app/api/attachment/AttachmentUrls';
import {Config} from '@app/api/Config';
import {
	IMediaService,
	MEDIA_PROXY_METADATA_REQUEST_VERSION,
	type MediaProxyMetadataOptions,
	type MediaProxyMetadataRequest,
	type MediaProxyMetadataResponse,
	type MediaProxyNsfwMode,
	type MediaProxySniffResponse,
} from '@app/api/infrastructure/IMediaService';
import {Logger} from '@app/api/Logger';
import * as FetchUtils from '@app/api/utils/FetchUtils';
import {isJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import {ExplicitContentCannotBeSentError} from '@fluxer/errors/src/domains/moderation/ExplicitContentCannotBeSentError';
import {attachmentStorageKeyFromUrl} from '@pkgs/media_proxy_utils/src/AttachmentUrlSignature';
import * as MediaProxyUtils from '@pkgs/media_proxy_utils/src/MediaProxyUtils';
import {ms} from 'itty-time';

type MediaProxyRequestBody =
	| MediaProxyMetadataRequest
	| {
			type: 'upload';
			upload_filename: string;
	  };

const MEDIA_PROXY_METADATA_MAX_BYTES = 256 * 1024;
const MEDIA_PROXY_METADATA_WITH_BASE64_MAX_BYTES = 64 * 1024 * 1024;
const MEDIA_PROXY_ERROR_MAX_BYTES = 16 * 1024;
const MEDIA_PROXY_SNIFF_MAX_BYTES = 1024;
const MEDIA_PROXY_REQUEST_TIMEOUT_MS = ms('30 seconds');
const MEDIA_PROXY_SNIFF_TIMEOUT_MS = ms('2 seconds');
const MEDIA_PROXY_BUFFERED_INPUT_MAX_BYTES = 500 * 1024 * 1024;
const MEDIA_PROXY_SPOOLED_INPUT_BYTES_PER_SECOND = 16 * 1024 * 1024;

function metadataRequestTimeoutMs(sizeBytes: number | undefined): number {
	if (sizeBytes === undefined || sizeBytes <= MEDIA_PROXY_BUFFERED_INPUT_MAX_BYTES) {
		return MEDIA_PROXY_REQUEST_TIMEOUT_MS;
	}
	return (
		MEDIA_PROXY_REQUEST_TIMEOUT_MS + Math.ceil(sizeBytes / MEDIA_PROXY_SPOOLED_INPUT_BYTES_PER_SECOND) * ms('1 second')
	);
}

function isMediaProxySniffResponse(value: unknown): value is MediaProxySniffResponse {
	return isJsonRecord(value) && (value.content_type === null || typeof value.content_type === 'string');
}

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

	async getMetadata(
		request: MediaProxyMetadataRequest,
		options: MediaProxyMetadataOptions = {},
	): Promise<MediaProxyMetadataResponse | null> {
		const nsfwMode = this.getNsfwMode(request);
		const response = await this.makeRequest(
			'/_metadata',
			this.toMetadataWireRequest(request, nsfwMode),
			metadataRequestTimeoutMs(options.sizeBytes),
		);
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
		if (attachmentStorageKeyFromUrl(url, Config.endpoints.media) !== null) {
			return signAttachmentUrl(url);
		}
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

	async sniffUpload(uploadFilename: string): Promise<MediaProxySniffResponse | null> {
		const response = await this.makeRequest(
			'/_sniff',
			{
				type: 'upload',
				upload_filename: uploadFilename,
			},
			MEDIA_PROXY_SNIFF_TIMEOUT_MS,
		);
		if (!response) return null;
		try {
			const responseText = await FetchUtils.streamToStringWithLimit(response.body, {
				maxBytes: MEDIA_PROXY_SNIFF_MAX_BYTES,
				headers: response.headers,
				description: 'Media proxy sniff response',
			});
			const sniff = parseJsonWithGuard(responseText, isMediaProxySniffResponse);
			if (!sniff) {
				Logger.error({uploadFilename}, 'Media proxy returned invalid sniff response');
				return null;
			}
			return {content_type: sniff.content_type};
		} catch (error) {
			Logger.error({error, uploadFilename}, 'Failed to read media proxy sniff response');
			return null;
		}
	}

	private async makeRequest(
		endpoint: string,
		body: MediaProxyRequestBody,
		timeoutMs: number = MEDIA_PROXY_REQUEST_TIMEOUT_MS,
	): Promise<Response | null> {
		try {
			const url = `http://${Config.mediaProxy.host}:${Config.mediaProxy.port}${endpoint}`;
			const response = await fetch(url, {
				method: 'POST',
				body: JSON.stringify(body),
				headers: {
					'Content-Type': 'application/json',
					Authorization: `Bearer ${Config.mediaProxy.secretKey}`,
				},
				signal: AbortSignal.timeout(timeoutMs),
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
