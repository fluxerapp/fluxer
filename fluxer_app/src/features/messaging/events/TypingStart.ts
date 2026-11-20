// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildMembers from '@app/features/member/state/GuildMembers';
import * as PersonaCommands from '@app/features/personas/commands/Personas';
import RollingTypingStore from '@app/features/typing/rolling/RollingTypingStore';
import Personas from '@app/features/user/state/Personas';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';

interface TypingStartPayload {
	channel_id: string;
	persona_id?: string;
	user_id: string;
	timestamp: number;
	guild_id?: string;
	member?: GuildMemberData;
}

export function handleTypingStart(data: TypingStartPayload, _context: GatewayHandlerContext): void {
	if (data.guild_id && data.member) {
		GuildMembers.hydrateIfMissing(data.guild_id, data.member);
	}
	if (data.persona_id && !Personas.getPersona(data.persona_id)) {
		void PersonaCommands.getPersona(data.user_id, data.persona_id).then((persona) => {
			Personas.cachePersonas([persona]);
		});
	}
	RollingTypingStore.start(data.channel_id, data.user_id, data.persona_id ?? null, 'gateway');
}
