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

export type MediaProxyMetadataRequest =
	| {
			type: 'external';
			url: string;
			with_base64?: boolean;
			isNSFWAllowed: boolean;
	  }
	| {
			type: 'upload';
			upload_filename: string;
			isNSFWAllowed: boolean;
	  }
	| {
			type: 'base64';
			base64: string;
			isNSFWAllowed: boolean;
	  }
	| {
			type: 's3';
			bucket: string;
			key: string;
			with_base64?: boolean;
			isNSFWAllowed: boolean;
	  };

export interface MediaProxyMetadataResponse {
	format: string;
	content_type: string;
	content_hash: string;
	size: number;
	width?: number;
	height?: number;
	duration?: number;
	placeholder?: string;
	base64?: string;
	animated?: boolean;
	nsfw: boolean;
	nsfw_probability?: number;
	nsfw_predictions?: Record<string, number>;
}

export abstract class IMediaService {
	abstract getMetadata(request: MediaProxyMetadataRequest): Promise<MediaProxyMetadataResponse | null>;
	abstract getExternalMediaProxyURL(url: string): string;
	abstract getThumbnail(uploadFilename: string): Promise<Buffer | null>;
}
