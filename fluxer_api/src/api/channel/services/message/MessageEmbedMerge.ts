// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageEmbedTypes} from '@fluxer/constants/src/ChannelConstants';
import {MAX_EMBEDS_PER_MESSAGE} from '@fluxer/constants/src/LimitConstants';
import type {MessageEmbed} from '../../../database/types/MessageTypes';

// The single definition of the rich-first embed-merge invariant. A re-derived
// embed set (URL auto-embeds from unfurling) must never drop a rich embed the
// author already set on the message. Keep the existing rich embeds, place them
// first (rich-first), then append the freshly-derived embeds.
//
// Called by the two persist paths that re-derive embeds for an
// already-persisted message: the synchronous content-only edit
// (MessagePersistenceService.updateMessage) and the deferred unfurl persist
// (ExtractEmbeds.updateMessageEmbeds). The realtime broadcast
// (ExtractEmbeds.dispatchEmbedUpdate) does NOT call this helper -- it
// re-broadcasts the already-merged latestMessage.embeds verbatim (it consumes
// the result of this function, it does not re-run the merge), so the DB write and
// the gateway MESSAGE_UPDATE always ship the identical array.
export function mergeRichFirst(
	existing: ReadonlyArray<MessageEmbed>,
	derived: ReadonlyArray<MessageEmbed>,
): Array<MessageEmbed> {
	const preservedRichEmbeds = existing.filter((embed) => embed.type === MessageEmbedTypes.RICH);
	const merged = [...preservedRichEmbeds, ...derived];
	// MAX_EMBEDS_PER_MESSAGE is a hard TOTAL cap, enforced at send/edit time on
	// the full client-supplied embed array (MessageValidationService:115). The
	// derived unfurl set is independently capped (ExtractEmbeds.buildOrderedEmbeds,
	// EmbedService.getInitialUrlEmbeds), but preservedRich + derived can exceed
	// the total, so re-cap here. Re-cap rich-first: the author rich embeds are
	// the entire point of this merge and must NEVER be dropped, so only the
	// derived tail is truncated. If preservedRich alone already meets/exceeds the
	// cap, keep them all (slice is a no-op or keeps every rich embed) rather than
	// truncate author content -- under-counting derived is always preferable to
	// losing an embed the author explicitly set.
	if (merged.length <= MAX_EMBEDS_PER_MESSAGE) {
		return merged;
	}
	if (preservedRichEmbeds.length >= MAX_EMBEDS_PER_MESSAGE) {
		return preservedRichEmbeds;
	}
	return merged.slice(0, MAX_EMBEDS_PER_MESSAGE);
}
