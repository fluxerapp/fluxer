// SPDX-License-Identifier: AGPL-3.0-or-later

import Sticker from '@app/features/emoji/state/EmojiSticker';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import type {GuildSticker} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';

export interface GuildStickersUpdatePayload {
	guild_id: string;
	stickers: ReadonlyArray<GuildSticker>;
}

export function handleGuildStickersUpdate(data: GuildStickersUpdatePayload, _context: GatewayHandlerContext): void {
	Sticker.handleGuildStickersUpdate(data.guild_id, data.stickers);
}
