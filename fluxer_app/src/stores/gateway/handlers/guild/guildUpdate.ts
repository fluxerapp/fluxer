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

import type {Guild} from '~/records/GuildRecord';
import EmojiStore from '~/stores/EmojiStore';
import GuildAvailabilityStore from '~/stores/GuildAvailabilityStore';
import GuildListStore from '~/stores/GuildListStore';
import GuildStore from '~/stores/GuildStore';
import NagbarStore from '~/stores/NagbarStore';
import PermissionStore from '~/stores/PermissionStore';
import QuickSwitcherStore from '~/stores/QuickSwitcherStore';
import StickerStore from '~/stores/StickerStore';
import type {GatewayHandlerContext} from '../index';

export function handleGuildUpdate(data: Guild, _context: GatewayHandlerContext): void {
	GuildAvailabilityStore.setGuildAvailable(data.id);
	GuildStore.handleGuildUpdate(data);

	GuildListStore.handleGuild(data);
	StickerStore.handleGuildUpdate(data);
	NagbarStore.handleGuildUpdate({guild: data});
	EmojiStore.handleGuildUpdate({guild: data});
	PermissionStore.handleGuild();

	QuickSwitcherStore.recomputeIfOpen();
}
