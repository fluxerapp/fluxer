// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {getGuildMemberSearchService} from '@app/api/SearchFactory';
import {chunkArray} from '@app/api/utils/ArrayUtils';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	guildId: z.string(),
	lastUserId: z.string().nullable().optional(),
});
const PAGE_SIZE = 1000;
const INDEX_CONCURRENCY = 4;
const INDEX_CHUNK_SIZE = 500;

const indexGuildMembers: WorkerTaskHandler = async (payload, _helpers) => {
	const validated = PayloadSchema.parse(payload);
	const searchService = getGuildMemberSearchService();
	if (!searchService) {
		return;
	}
	const guildId = createGuildID(BigInt(validated.guildId));
	const {guildRepository, userRepository} = getWorkerDependencies();
	try {
		let cursor: UserID | undefined = validated.lastUserId ? createUserID(BigInt(validated.lastUserId)) : undefined;
		let totalIndexed = 0;
		let hasMore = true;
		while (hasMore) {
			const members = await guildRepository.listMembersPaginated(guildId, PAGE_SIZE, cursor);
			if (members.length === 0) {
				break;
			}
			const uniqueUserIds = Array.from(new Set(members.map((m) => m.userId)));
			const users = await userRepository.listUsers(uniqueUserIds);
			const userMap = new Map<UserID, User>(users.map((u) => [u.id, u]));
			const membersWithUsers = members.flatMap((member) => {
				const user = userMap.get(member.userId);
				return user ? [{member, user}] : [];
			});
			const chunks = chunkArray(membersWithUsers, INDEX_CHUNK_SIZE);
			for (const batch of chunkArray(chunks, INDEX_CONCURRENCY)) {
				await Promise.all(batch.map((chunk) => searchService.indexMembers(chunk)));
			}
			totalIndexed += membersWithUsers.length;
			Logger.debug(
				{
					guildId: guildId.toString(),
					batchSize: membersWithUsers.length,
					totalIndexed,
					hasMore: members.length === PAGE_SIZE,
				},
				'Indexed guild member batch',
			);
			hasMore = members.length === PAGE_SIZE;
			if (hasMore) {
				cursor = members[members.length - 1]!.userId;
			}
		}
		Logger.debug({guildId: guildId.toString(), totalIndexed}, 'Guild member indexing complete');
		const guild = await guildRepository.findUnique(guildId);
		if (guild) {
			await guildRepository.upsertPartial(guildId, {members_indexed_at: new Date()}, guild.toRow());
		}
	} catch (error) {
		Logger.error({error, guildId: guildId.toString()}, 'Failed to index guild members');
		throw error;
	}
};

export default indexGuildMembers;
