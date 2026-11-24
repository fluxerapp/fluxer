// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IMediaService, MediaProxyMetadataRequest} from '@app/api/infrastructure/IMediaService';
import {decodeValidatedImage} from '@app/api/infrastructure/ImageUploadValidation';
import {ASSET_FORMAT_POLICY, type AssetKind} from '@fluxer/constants/src/AssetFormatPolicy';
import {describe, expect, it, vi} from 'vitest';

const PNG_BASE64 = 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==';

const ASSET_KINDS = (Object.keys(ASSET_FORMAT_POLICY) as Array<AssetKind>).filter((kind) => kind !== 'attachment');

function classifierFlaggingMediaService(): {service: IMediaService; requests: Array<MediaProxyMetadataRequest>} {
	const requests: Array<MediaProxyMetadataRequest> = [];
	const service = {
		getMetadata: vi.fn(async (request: MediaProxyMetadataRequest) => {
			requests.push(request);
			return {
				format: 'png',
				content_type: 'image/png',
				content_hash: 'hash',
				size: 68,
				width: 1,
				height: 1,
				animated: false,
				nsfw: true,
				nsfw_probability: 0.99,
			};
		}),
	} as unknown as IMediaService;
	return {service, requests};
}

describe('decodeValidatedImage', () => {
	it.each(ASSET_KINDS)('accepts a %s the classifier would flag and never asks to block', async (kind) => {
		const {service, requests} = classifierFlaggingMediaService();
		const result = await decodeValidatedImage(service, {
			base64Image: `data:image/png;base64,${PNG_BASE64}`,
			errorPath: kind,
			kind,
			maxSize: 1024 * 1024,
		});
		expect(result.metadata.format).toBe('png');
		expect(requests).toHaveLength(1);
		expect(requests[0]).toMatchObject({type: 'base64', nsfw: 'allow'});
	});
});
