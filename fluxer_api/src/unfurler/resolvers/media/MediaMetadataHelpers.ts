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

import {MessageAttachmentFlags} from '~/Constants';
import type {MessageEmbedResponse} from '~/channel/EmbedTypes';
import type {MediaProxyMetadataResponse} from '~/infrastructure/IMediaService';

interface BuildMediaOptions {
	width?: number;
	height?: number;
	description?: string;
}

export function buildEmbedMediaPayload(
	url: string,
	metadata: MediaProxyMetadataResponse | null,
	options: BuildMediaOptions = {},
): MessageEmbedResponse['image'] {
	const flags =
		(metadata?.animated ? MessageAttachmentFlags.IS_ANIMATED : 0) |
		(metadata?.nsfw ? MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA : 0);

	return {
		url,
		width: options.width ?? metadata?.width ?? undefined,
		height: options.height ?? metadata?.height ?? undefined,
		description: options.description ?? undefined,
		placeholder: metadata?.placeholder ?? undefined,
		flags,
		content_hash: metadata?.content_hash ?? undefined,
		content_type: metadata?.content_type ?? undefined,
		duration: metadata?.duration ?? undefined,
	};
}
