// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildMembers from '@app/features/member/state/GuildMembers';
import TypingIndicator from '@app/features/typing/state/TypingIndicator';
import Personas from '@app/features/user/state/Personas';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type { PersonaSnapshot } from '@fluxer/schema/src/domains/persona/PersonaSchemas.js';
import * as PersonaCommands from '@app/features/personas/commands/Personas';

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
	TypingIndicator.startRemoteTyping(data.channel_id, data.user_id, data.persona_id || null);
}
