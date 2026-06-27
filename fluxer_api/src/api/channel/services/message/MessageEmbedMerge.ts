// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageEmbedTypes} from '@fluxer/constants/src/ChannelConstants';
import type {MessageEmbed} from '../../../database/types/MessageTypes';

// The single definition of the rich-first embed-merge invariant. A re-derived
// embed set (URL auto-embeds from unfurling) must never drop a rich embed the
// author already set on the message. Keep the existing rich embeds, place them
// first (rich-first), then append the freshly-derived embeds.
//
// Used by every path that re-derives embeds for an already-persisted message:
// the synchronous content-only edit (MessagePersistenceService.updateMessage),
// the deferred unfurl persist (ExtractEmbeds.updateMessageEmbeds), and the
// realtime broadcast of that same update (ExtractEmbeds.dispatchEmbedUpdate),
// so the DB write and the gateway MESSAGE_UPDATE always ship the same array.
export function mergeRichFirst(
	existing: ReadonlyArray<MessageEmbed>,
	derived: ReadonlyArray<MessageEmbed>,
): Array<MessageEmbed> {
	const preservedRichEmbeds = existing.filter((embed) => embed.type === MessageEmbedTypes.RICH);
	return [...preservedRichEmbeds, ...derived];
}
