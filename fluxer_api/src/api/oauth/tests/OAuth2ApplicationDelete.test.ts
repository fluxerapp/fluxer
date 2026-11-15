// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {fetchMany} from '@app/api/database/CassandraQueryExecution';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {getGuildRepository} from '@app/api/middleware/ServiceSingletons';
import {banUser} from '@app/api/moderation/tests/ModerationTestUtils';
import {
	createOAuth2Application,
	createUniqueApplicationName,
	deleteOAuth2Application,
	getOAuth2Application,
	listOAuth2Applications,
} from '@app/api/oauth/tests/OAuth2TestUtils';
import {RelationshipsByTarget} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {UserRelationshipRepository} from '@app/api/user/repositories/UserRelationshipRepository';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {beforeEach, describe, expect, test} from 'vitest';

describe('OAuth2 Application Delete', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('deletes application and invalidates bot token', async () => {
		const account = await createTestAccount(harness);
		const appName = createUniqueApplicationName();
		const createResult = await createOAuth2Application(harness, account.token, {
			name: appName,
			redirect_uris: ['https://example.com/callback'],
		});
		await createBuilder(harness, `Bot ${createResult.botToken}`).get('/users/@me').expect(HTTP_STATUS.OK).execute();
		await deleteOAuth2Application(harness, account.token, createResult.application.id, account.password);
		await createBuilder(harness, account.token)
			.get(`/oauth2/applications/${createResult.application.id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
		const applications = await listOAuth2Applications(harness, account.token);
		const found = applications.find((app) => app.id === createResult.application.id);
		expect(found).toBeUndefined();
		await createBuilder(harness, `Bot ${createResult.botToken}`)
			.get('/users/@me')
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
		const botUser = await createBuilder<{
			id: string;
			username: string;
			discriminator: string;
			avatar: string | null;
		}>(harness, account.token)
			.get(`/users/${createResult.botUserId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(botUser.username).toBe('DeletedUser');
		expect(botUser.discriminator).toBe('0000');
		expect(botUser.avatar).toBeNull();
	});
	test('removes guild bans of the deleted bot', async () => {
		const owner = await createTestAccount(harness);
		const createResult = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
		});
		const guild = await createGuild(harness, owner.token, `Bot Ban Cleanup ${Date.now()}`);
		await banUser(harness, owner.token, guild.id, createResult.botUserId);
		const guildRepository = getGuildRepository();
		const guildId = createGuildID(BigInt(guild.id));
		const botUserId = createUserID(BigInt(createResult.botUserId));
		expect(await guildRepository.getBan(guildId, botUserId)).not.toBeNull();
		await deleteOAuth2Application(harness, owner.token, createResult.application.id, owner.password);
		expect(await guildRepository.getBan(guildId, botUserId)).toBeNull();
		expect(await guildRepository.listBans(guildId)).toHaveLength(0);
	});
	test('keeps bot-authored messages readable after deleting the application', async () => {
		const account = await createTestAccount(harness);
		const createResult = await createOAuth2Application(harness, account.token, {
			name: createUniqueApplicationName(),
		});
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
		await deleteOAuth2Application(harness, account.token, createResult.application.id, account.password);
		const messages = await createBuilder<Array<MessageResponse>>(harness, account.token)
			.get(`/channels/${channelId}/messages`)
			.expect(HTTP_STATUS.OK)
			.execute();
		const foundMessage = messages.find((message) => message.id === botMessageId);
		expect(foundMessage).toBeDefined();
		expect(foundMessage?.author.id).toBe(createResult.botUserId);
		expect(foundMessage?.author.bot).toBe(true);
		expect(foundMessage?.author.username).toBe('DeletedUser');
		expect(foundMessage?.author.discriminator).toBe('0000');
		const authorCount = await createBuilderWithoutAuth<{
			count: number;
		}>(harness)
			.get(`/test/users/${createResult.botUserId}/messages/count`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(authorCount.count).toBe(1);
	});
	test('removes the bot from friend lists and block lists', async () => {
		const owner = await createTestAccount(harness);
		const friend = await createTestAccount(harness);
		const blocker = await createTestAccount(harness);
		const createResult = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
		});
		const botUserId = createUserID(BigInt(createResult.botUserId));
		const friendId = createUserID(BigInt(friend.userId));
		const blockerId = createUserID(BigInt(blocker.userId));
		const relationships = new UserRelationshipRepository();
		for (const [source, target, type] of [
			[friendId, botUserId, RelationshipTypes.FRIEND],
			[botUserId, friendId, RelationshipTypes.FRIEND],
			[blockerId, botUserId, RelationshipTypes.BLOCKED],
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
		await deleteOAuth2Application(harness, owner.token, createResult.application.id, owner.password);
		for (const account of [friend, blocker]) {
			const listed = await createBuilder<Array<{id: string}>>(harness, account.token)
				.get('/users/@me/relationships')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(listed.map((relationship) => relationship.id)).not.toContain(createResult.botUserId);
		}
		expect(await relationships.listRelationships(botUserId)).toHaveLength(0);
		const pointingAtBot = await fetchMany(
			RelationshipsByTarget.selectCql({where: RelationshipsByTarget.where.eq('target_user_id')}),
			{target_user_id: botUserId},
		);
		expect(pointingAtBot).toHaveLength(0);
	});
	test('enforces access control', async () => {
		const owner = await createTestAccount(harness);
		const otherUser = await createTestAccount(harness);
		const createResult = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
		});
		await createBuilder(harness, otherUser.token)
			.delete(`/oauth2/applications/${createResult.application.id}`)
			.body({password: otherUser.password})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		const application = await getOAuth2Application(harness, owner.token, createResult.application.id);
		expect(application.id).toBe(createResult.application.id);
	});
	test('requires sudo verification', async () => {
		const account = await createTestAccount(harness);
		const createResult = await createOAuth2Application(harness, account.token, {
			name: createUniqueApplicationName(),
		});
		await createBuilder(harness, account.token)
			.delete(`/oauth2/applications/${createResult.application.id}`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN, 'SUDO_MODE_REQUIRED')
			.execute();
		const application = await getOAuth2Application(harness, account.token, createResult.application.id);
		expect(application.id).toBe(createResult.application.id);
	});
	test('rejects wrong password', async () => {
		const account = await createTestAccount(harness);
		const createResult = await createOAuth2Application(harness, account.token, {
			name: createUniqueApplicationName(),
		});
		await createBuilder(harness, account.token)
			.delete(`/oauth2/applications/${createResult.application.id}`)
			.body({password: 'wrong-password'})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();
	});
});
