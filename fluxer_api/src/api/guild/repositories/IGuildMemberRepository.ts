// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {GuildMemberRow, GuildMembershipMetadataRow} from '@app/api/database/types/GuildTypes';
import type {GuildMember} from '@app/api/models/GuildMember';

export abstract class IGuildMemberRepository {
	abstract getMember(guildId: GuildID, userId: UserID): Promise<GuildMember | null>;

	abstract listMembers(guildId: GuildID): Promise<Array<GuildMember>>;

	abstract countMembers(guildId: GuildID): Promise<number>;

	abstract upsertMember(data: GuildMemberRow): Promise<GuildMember>;

	abstract deleteMember(guildId: GuildID, userId: UserID): Promise<void>;

	abstract listMembersPaginated(guildId: GuildID, limit: number, afterUserId?: UserID): Promise<Array<GuildMember>>;

	abstract getMembershipMetadata(guildId: GuildID, userId: UserID): Promise<GuildMembershipMetadataRow | null>;

	abstract upsertMembershipMetadata(data: GuildMembershipMetadataRow, ttlSeconds: number): Promise<void>;
}
