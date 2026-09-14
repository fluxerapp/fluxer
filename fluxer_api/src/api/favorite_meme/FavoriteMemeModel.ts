// SPDX-License-Identifier: AGPL-3.0-or-later

import {userIdToChannelId} from '@app/api/BrandedTypes';
import {makeAttachmentCdnUrl} from '@app/api/channel/services/message/MessageHelpers';
import type {FavoriteMeme} from '@app/api/models/FavoriteMeme';
import {assertSafeByteSize} from '@app/api/utils/ByteSizeUtils';
import type {FavoriteMemeResponse} from '@fluxer/schema/src/domains/meme/MemeSchemas';

export function mapFavoriteMemeToResponse(meme: FavoriteMeme): FavoriteMemeResponse {
	const url = makeAttachmentCdnUrl(userIdToChannelId(meme.userId), meme.attachmentId, meme.filename);
	return {
		id: meme.id.toString(),
		user_id: meme.userId.toString(),
		name: meme.name,
		alt_text: meme.altText ?? null,
		tags: meme.tags || [],
		attachment_id: meme.attachmentId.toString(),
		filename: meme.filename,
		content_type: meme.contentType,
		content_hash: meme.contentHash ?? null,
		size: assertSafeByteSize(meme.size, 'favorite meme size'),
		width: meme.width ?? null,
		height: meme.height ?? null,
		duration: meme.duration ?? null,
		url,
		is_gifv: meme.isGifv ?? false,
		gif_slug: meme.gifSlug ?? null,
		gif_provider: meme.gifProvider ?? null,
		media: meme.mediaFormats ?? null,
		placeholder: meme.placeholder ?? null,
	};
}
