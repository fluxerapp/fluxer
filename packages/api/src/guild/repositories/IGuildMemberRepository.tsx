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

import type {GuildID, UserID} from '@fluxer/api/src/BrandedTypes';
import type {GuildMemberRow} from '@fluxer/api/src/database/types/GuildTypes';
import type {GuildMember} from '@fluxer/api/src/models/GuildMember';

export abstract class IGuildMemberRepository {
	abstract getMember(guildId: GuildID, userId: UserID): Promise<GuildMember | null>;
	abstract listMembers(guildId: GuildID): Promise<Array<GuildMember>>;
	abstract countMembers(guildId: GuildID): Promise<number>;
	abstract upsertMember(data: GuildMemberRow): Promise<GuildMember>;
	abstract deleteMember(guildId: GuildID, userId: UserID): Promise<void>;
	abstract listMembersPaginated(guildId: GuildID, limit: number, afterUserId?: UserID): Promise<Array<GuildMember>>;
}
