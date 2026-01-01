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

import {Config} from '~/Config';
import {ExplicitContentCannotBeSentError} from '~/Errors';
import {Logger} from '~/Logger';
import * as MediaProxyUtils from '~/utils/MediaProxyUtils';
import {IMediaService, type MediaProxyMetadataRequest, type MediaProxyMetadataResponse} from './IMediaService';

type MediaProxyRequestBody = MediaProxyMetadataRequest | {type: 'upload'; upload_filename: string};

export class MediaService extends IMediaService {
	private readonly proxyURL: URL;

	constructor() {
		super();
		this.proxyURL = new URL(Config.endpoints.media);
	}

	async getMetadata(request: MediaProxyMetadataRequest): Promise<MediaProxyMetadataResponse | null> {
		const response = await this.makeRequest('/_metadata', request);
		if (!response) {
			return null;
		}

		try {
			const responseText = await response.text();
			if (!responseText) {
				Logger.error('Media proxy returned empty response');
				return null;
			}

			const metadata = JSON.parse(responseText) as MediaProxyMetadataResponse;

			if (!request.isNSFWAllowed && metadata.nsfw) {
				throw new ExplicitContentCannotBeSentError(metadata.nsfw_probability ?? 0, metadata.nsfw_predictions ?? {});
			}

			return metadata;
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
			const arrayBuffer = await response.arrayBuffer();
			return Buffer.from(arrayBuffer);
		} catch (error) {
			Logger.error({error, uploadFilename}, 'Failed to parse media proxy thumbnail response');
			return null;
		}
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
			});

			if (!response.ok) {
				const errorText = await response.text().catch(() => 'Could not read error body');
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

	private handleExternalURL(url: string): string {
		return MediaProxyUtils.getExternalMediaProxyURL({
			inputURL: url,
			mediaProxyEndpoint: Config.endpoints.media,
			mediaProxySecretKey: Config.mediaProxy.secretKey,
		});
	}
}
