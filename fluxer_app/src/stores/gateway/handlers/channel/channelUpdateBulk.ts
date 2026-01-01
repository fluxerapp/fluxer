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

import type {Channel} from '~/records/ChannelRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildReadStateStore from '~/stores/GuildReadStateStore';
import PermissionStore from '~/stores/PermissionStore';
import QuickSwitcherStore from '~/stores/QuickSwitcherStore';
import type {GatewayHandlerContext} from '../index';

interface ChannelUpdateBulkPayload {
	channels: Array<Channel>;
}

export function handleChannelUpdateBulk(data: ChannelUpdateBulkPayload, _context: GatewayHandlerContext): void {
	for (const channel of data.channels) {
		ChannelStore.handleChannelCreate({channel});
		PermissionStore.handleChannelUpdate(channel.id);
		GuildReadStateStore.handleGenericUpdate(channel.id);
	}
	QuickSwitcherStore.recomputeIfOpen();
}
