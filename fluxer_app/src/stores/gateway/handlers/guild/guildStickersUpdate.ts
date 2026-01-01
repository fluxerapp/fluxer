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

import type {GuildSticker} from '~/records/GuildStickerRecord';
import StickerStore from '~/stores/StickerStore';
import type {GatewayHandlerContext} from '../index';

interface GuildStickersUpdatePayload {
	guild_id: string;
	stickers: ReadonlyArray<GuildSticker>;
}

export function handleGuildStickersUpdate(data: GuildStickersUpdatePayload, _context: GatewayHandlerContext): void {
	StickerStore.handleGuildStickersUpdate(data.guild_id, data.stickers);
}
