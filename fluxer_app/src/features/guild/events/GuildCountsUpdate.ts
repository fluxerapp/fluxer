// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildCount from '@app/features/guild/state/GuildCount';

export interface GuildCountsUpdatePayload {
	counts?: ReadonlyArray<{
		guild_id: string;
		member_count: number;
		online_count: number;
	}>;
}

export function handleGuildCountsUpdate(data: GuildCountsUpdatePayload, _context: GatewayHandlerContext): void {
	const counts = Array.isArray(data?.counts) ? data.counts : [];
	GuildCount.handleCountsResponse(counts);
}
