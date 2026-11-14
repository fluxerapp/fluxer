// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createApplicationID, createGuildID, createUserID} from '@app/api/BrandedTypes';
import {fetchMany} from '@app/api/database/CassandraQueryExecution';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {
	createUserCacheService,
	getApplicationRepository,
	getGuildRepository,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {banUser} from '@app/api/moderation/tests/ModerationTestUtils';
import {createOAuth2Application, createUniqueApplicationName} from '@app/api/oauth/tests/OAuth2TestUtils';
import {RelationshipsByTarget} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {UserRelationshipRepository} from '@app/api/user/repositories/UserRelationshipRepository';
import applicationProcessDeletion from '@app/api/worker/tasks/ApplicationProcessDeletion';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {RelationshipTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('applicationProcessDeletion task', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	beforeEach(async () => {
		await harness.resetData();
		setWorkerDependenciesForTest({
			userRepository: getUserRepository(),
			guildRepository: getGuildRepository(),
			applicationRepository: getApplicationRepository(),
			userCacheService: createUserCacheService(),
			gatewayService: getGatewayService(),
		});
	});
	afterEach(() => {
		clearWorkerDependencies();
	});

	test('removes guild bans of the deleted bot, also when the bot was already anonymised', async () => {
		const account = await createTestAccount(harness);
		const createResult = await createOAuth2Application(harness, account.token, {
			name: createUniqueApplicationName(),
		});
		const guild = await createGuild(harness, account.token, `Bot Ban Worker Cleanup ${Date.now()}`);
		const guildId = createGuildID(BigInt(guild.id));
		const botUserId = createUserID(BigInt(createResult.botUserId));
		await banUser(harness, account.token, guild.id, createResult.botUserId);
		const userRepository = getUserRepository();
		await userRepository.updateFlags(botUserId, (flags) => flags | UserFlags.DELETED);
		const guildRepository = getGuildRepository();
		expect(await guildRepository.getBan(guildId, botUserId)).not.toBeNull();

		await applicationProcessDeletion(
			{applicationId: createResult.application.id},
			createTestWorkerHelpers({jobId: 7200000000000000001n}),
		);

		expect(await guildRepository.getBan(guildId, botUserId)).toBeNull();
		expect(await guildRepository.listBans(guildId)).toHaveLength(0);
	});
	test('keeps the bot as author of its messages and clears its profile', async () => {
		const account = await createTestAccount(harness);
		const createResult = await createOAuth2Application(harness, account.token, {
			name: createUniqueApplicationName(),
		});
		const botUserId = createUserID(BigInt(createResult.botUserId));
		const userRepository = getUserRepository();
		const botUser = await userRepository.findUniqueAssert(botUserId);
		await userRepository.patchUpsert(
			botUserId,
			{
				avatar_hash: 'a'.repeat(32),
				banner_hash: 'b'.repeat(32),
				bio: 'bot bio',
				pronouns: 'it/its',
				accent_color: 0x4641d9,
			},
			botUser.toRow(),
		);
		const guild = await createBuilder<{
			id: string;
			system_channel_id?: string;
		}>(harness, account.token)
			.post('/guilds')
			.body({name: `Bot Message Retention ${Date.now()}`})
			.expect(HTTP_STATUS.OK)
			.execute();
		const channelId = guild.system_channel_id;
		if (!channelId) {
			throw new Error('Guild response missing system channel');
		}
		const seeded = await createBuilder<{
			messages: Array<{
				message_id: string;
			}>;
		}>(harness, '')
			.post('/test/messages/seed')
			.body({
				channel_id: channelId,
				author_id: createResult.botUserId,
				clear_existing: true,
				messages: [{content: 'bot message before deletion'}],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const botMessageId = seeded.messages[0]?.message_id;
		if (!botMessageId) {
			throw new Error('Seeded message response missing message id');
		}

		await applicationProcessDeletion(
			{applicationId: createResult.application.id},
			createTestWorkerHelpers({jobId: 7200000000000000000n}),
		);

		expect(
			await getApplicationRepository().getApplication(createApplicationID(BigInt(createResult.application.id))),
		).toBeNull();
		const deletedBot = await userRepository.findUniqueAssert(botUserId);
		expect(deletedBot.isBot).toBe(true);
		expect(deletedBot.flags & UserFlags.DELETED).toBe(UserFlags.DELETED);
		expect(deletedBot.username).toBe('DeletedUser');
		expect(deletedBot.avatarHash).toBeNull();
		expect(deletedBot.bannerHash).toBeNull();
		expect(deletedBot.bio).toBeNull();
		expect(deletedBot.pronouns).toBeNull();
		expect(deletedBot.accentColor).toBeNull();
		const messages = await createBuilder<Array<MessageResponse>>(harness, account.token)
			.get(`/channels/${channelId}/messages`)
			.expect(HTTP_STATUS.OK)
			.execute();
		const foundMessage = messages.find((message) => message.id === botMessageId);
		expect(foundMessage?.author.id).toBe(createResult.botUserId);
		expect(foundMessage?.author.bot).toBe(true);
		expect(foundMessage?.author.username).toBe('DeletedUser');
		expect(foundMessage?.author.discriminator).toBe('0000');
		expect(foundMessage?.author.avatar).toBeNull();
		const authorCount = await createBuilderWithoutAuth<{
			count: number;
		}>(harness)
			.get(`/test/users/${createResult.botUserId}/messages/count`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(authorCount.count).toBe(1);
	});
	test('removes every relationship of the bot', async () => {
		const owner = await createTestAccount(harness);
		const friend = await createTestAccount(harness);
		const createResult = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
		});
		const botUserId = createUserID(BigInt(createResult.botUserId));
		const friendId = createUserID(BigInt(friend.userId));
		const relationships = new UserRelationshipRepository();
		for (const [source, target, type] of [
			[friendId, botUserId, RelationshipTypes.FRIEND],
			[botUserId, friendId, RelationshipTypes.FRIEND],
		] as const) {
			await relationships.upsertRelationship({
				source_user_id: source,
				target_user_id: target,
				type,
				nickname: null,
				since: new Date(),
				share_voice_activity: null,
				version: 1,
			});
		}

		await applicationProcessDeletion(
			{applicationId: createResult.application.id},
			createTestWorkerHelpers({jobId: 7200000000000000001n}),
		);

		expect(await relationships.listRelationships(friendId)).toHaveLength(0);
		expect(await relationships.listRelationships(botUserId)).toHaveLength(0);
		const pointingAtBot = await fetchMany(
			RelationshipsByTarget.selectCql({where: RelationshipsByTarget.where.eq('target_user_id')}),
			{target_user_id: botUserId},
		);
		expect(pointingAtBot).toHaveLength(0);
	});
});
