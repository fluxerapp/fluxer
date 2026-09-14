// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import {Logger} from '@app/api/Logger';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {User} from '@app/api/models/User';
import {getGuildMemberSearchService} from '@app/api/SearchFactory';
import type {IGuildMemberSearchService} from '@app/api/search/IGuildMemberSearchService';

interface GuildMemberSearchIndexOptions {
	includeDefault?: boolean;
}

function getGuildMemberIndexService(options: GuildMemberSearchIndexOptions = {}): IGuildMemberSearchService | null {
	const includeDefault = options.includeDefault ?? true;
	const service = getGuildMemberSearchService();
	return includeDefault ? service : null;
}

export class GuildMemberSearchIndexService {
	async updateUserMembers(
		user: User,
		guildIds: Array<GuildID>,
		guildRepository: IGuildRepositoryAggregate,
	): Promise<void> {
		if (guildIds.length === 0) return;
		const guilds = await guildRepository.listGuilds(guildIds);
		const indexedGuilds = guilds.filter((guild) => guild.membersIndexedAt != null);
		if (indexedGuilds.length === 0) return;
		const members = await Promise.all(indexedGuilds.map((guild) => guildRepository.getMember(guild.id, user.id)));
		for (const member of members) {
			if (member) {
				void this.updateMember(member, user);
			}
		}
	}

	async indexMember(member: GuildMember, user: User, options?: GuildMemberSearchIndexOptions): Promise<void> {
		try {
			const searchService = getGuildMemberIndexService(options);
			if (!searchService) {
				return;
			}
			await searchService.indexMember(member, user);
		} catch (error) {
			Logger.error(
				{
					guildId: member.guildId.toString(),
					userId: member.userId.toString(),
					error,
				},
				'Failed to index guild member in search',
			);
		}
	}

	async updateMember(member: GuildMember, user: User, options?: GuildMemberSearchIndexOptions): Promise<void> {
		try {
			const searchService = getGuildMemberIndexService(options);
			if (!searchService) {
				return;
			}
			await searchService.updateMember(member, user);
		} catch (error) {
			Logger.error(
				{
					guildId: member.guildId.toString(),
					userId: member.userId.toString(),
					error,
				},
				'Failed to update guild member in search index',
			);
		}
	}

	async deleteMember(guildId: GuildID, userId: UserID, options?: GuildMemberSearchIndexOptions): Promise<void> {
		try {
			const searchService = getGuildMemberIndexService(options);
			if (!searchService) {
				return;
			}
			await searchService.deleteMember(guildId, userId);
		} catch (error) {
			Logger.error(
				{
					guildId: guildId.toString(),
					userId: userId.toString(),
					error,
				},
				'Failed to delete guild member from search index',
			);
		}
	}
}
