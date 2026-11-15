// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import type {PresenceRecord} from '@app/features/gateway/types/GatewayPresenceTypes';
import Presence from '@app/features/presence/state/Presence';

export interface PresenceUpdateBulkPayload {
	presences: Array<PresenceRecord>;
	guild_id?: string;
}

export function handlePresenceUpdateBulk(data: PresenceUpdateBulkPayload, _context: GatewayHandlerContext): void {
	const guildId = data.guild_id;
	for (const presence of data.presences) {
		(
			presence as {
				guild_id?: string;
			}
		).guild_id = guildId;
		Presence.handlePresenceUpdate(presence);
	}
}
