// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IMediaService, MediaProxyMetadataResponse} from '@app/api/infrastructure/IMediaService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {User} from '@app/api/models/User';
import {
	type AssetKind,
	formatAssetUploadExtensions,
	getPolicy,
	isExtensionAllowed,
} from '@fluxer/constants/src/AssetFormatPolicy';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {resolveLimit} from '@fluxer/limits/src/LimitResolver';

export function resolveAssetSizeLimit(
	limitConfigService: Pick<LimitConfigService, 'getConfigSnapshot'>,
	key: LimitKey,
	fallback: number,
	guildFeatures: Iterable<string> | null = null,
	user: User | null = null,
): number {
	const ctx = createLimitMatchContext({user, guildFeatures});
	const resolved = resolveLimit(limitConfigService.getConfigSnapshot(), ctx, key, {
		evaluationContext: guildFeatures ? 'guild' : 'user',
	});
	if (!Number.isFinite(resolved) || resolved < 0) {
		return fallback;
	}
	return Math.floor(resolved);
}

export async function decodeValidatedImage(
	mediaService: IMediaService,
	params: {base64Image: string; errorPath: string; kind: AssetKind; maxSize: number},
): Promise<{imageBuffer: Uint8Array; metadata: MediaProxyMetadataResponse}> {
	const {base64Image, errorPath, kind, maxSize} = params;
	const base64Data = base64Image.includes(',') ? base64Image.split(',')[1] : base64Image;
	if (Buffer.byteLength(base64Data, 'base64') > maxSize) {
		throw InputValidationError.fromCode(errorPath, ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT, {maxSize});
	}
	const imageBuffer = new Uint8Array(Buffer.from(base64Data, 'base64'));
	const metadata = requireAllowedMetadata({
		metadata: await mediaService.getMetadata({type: 'base64', base64: base64Data, version: 2, nsfw: 'allow'}),
		kind,
		errorPath,
	});
	return {imageBuffer, metadata};
}

function requireAllowedMetadata(params: {
	metadata: MediaProxyMetadataResponse | null;
	kind: AssetKind;
	errorPath: string;
}): MediaProxyMetadataResponse {
	const {metadata, kind, errorPath} = params;
	if (metadata == null || !isExtensionAllowed(kind, metadata.format)) {
		throwInvalidImageFormat(errorPath, kind);
	}
	const animated = metadata.animated ?? false;
	if (!animated) return metadata;
	if (getPolicy(kind).animated === 'never' || isAnimatedAvif(metadata)) {
		throwInvalidImageFormat(errorPath, kind);
	}
	return metadata;
}

function isAnimatedAvif(metadata: MediaProxyMetadataResponse): boolean {
	const format = metadata.format.toLowerCase();
	const contentType = metadata.content_type.toLowerCase();
	return format === 'avif' || contentType === 'image/avif' || contentType === 'image/avif-sequence';
}

function throwInvalidImageFormat(errorPath: string, kind: AssetKind): never {
	throw InputValidationError.fromCode(errorPath, ValidationErrorCodes.INVALID_IMAGE_FORMAT, {
		supportedExtensions: formatSupportedExtensions(kind),
	});
}

function formatSupportedExtensions(kind: AssetKind): string {
	return formatAssetUploadExtensions(kind, {labelStyle: 'extension'});
}
