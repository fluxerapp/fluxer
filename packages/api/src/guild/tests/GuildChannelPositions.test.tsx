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

import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {createGuildID} from '@fluxer/api/src/BrandedTypes';
import {ChannelRepository} from '@fluxer/api/src/channel/ChannelRepository';
import {
	acceptInvite,
	addMemberRole,
	createChannel,
	createChannelInvite,
	createGuild,
	createRole,
	getChannel,
	getGuildChannels,
	updateChannelPositions,
} from '@fluxer/api/src/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Guild Channel Positions', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('should reorder channels within guild', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const channel1 = await createChannel(harness, account.token, guild.id, 'channel-1');
		const channel2 = await createChannel(harness, account.token, guild.id, 'channel-2');
		const channel3 = await createChannel(harness, account.token, guild.id, 'channel-3');

		await updateChannelPositions(harness, account.token, guild.id, [
			{id: channel3.id, position: 0},
			{id: channel1.id, position: 1},
			{id: channel2.id, position: 2},
		]);

		const channels = await getGuildChannels(harness, account.token, guild.id);
		const textChannels = channels.filter((c) => c.type === ChannelTypes.GUILD_TEXT);

		expect(textChannels.some((c) => c.id === channel1.id)).toBe(true);
		expect(textChannels.some((c) => c.id === channel2.id)).toBe(true);
		expect(textChannels.some((c) => c.id === channel3.id)).toBe(true);
	});

	test('should move channel to category', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const category = await createChannel(harness, account.token, guild.id, 'Category', ChannelTypes.GUILD_CATEGORY);
		const textChannel = await createChannel(harness, account.token, guild.id, 'text-channel');

		await updateChannelPositions(harness, account.token, guild.id, [{id: textChannel.id, parent_id: category.id}]);

		const updatedChannel = await getChannel(harness, account.token, textChannel.id);
		expect(updatedChannel.parent_id).toBe(category.id);
	});

	test('should move channel out of category', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const category = await createChannel(harness, account.token, guild.id, 'Category', ChannelTypes.GUILD_CATEGORY);
		const textChannel = await createChannel(harness, account.token, guild.id, 'text-channel');

		await updateChannelPositions(harness, account.token, guild.id, [{id: textChannel.id, parent_id: category.id}]);

		let updatedChannel = await getChannel(harness, account.token, textChannel.id);
		expect(updatedChannel.parent_id).toBe(category.id);

		await updateChannelPositions(harness, account.token, guild.id, [{id: textChannel.id, parent_id: null}]);

		updatedChannel = await getChannel(harness, account.token, textChannel.id);
		expect(updatedChannel.parent_id).toBeNull();
	});

	test('should require MANAGE_CHANNELS permission to reorder', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);

		const channel1 = await createChannel(harness, owner.token, guild.id, 'channel-1');
		const channel2 = await createChannel(harness, owner.token, guild.id, 'channel-2');

		await createBuilder(harness, member.token)
			.patch(`/guilds/${guild.id}/channels`)
			.body([
				{id: channel1.id, position: 1},
				{id: channel2.id, position: 0},
			])
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	test('should allow MANAGE_CHANNELS role to reorder channels', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);

		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);

		const managerRole = await createRole(harness, owner.token, guild.id, {
			name: 'Channel Manager',
			permissions: Permissions.MANAGE_CHANNELS.toString(),
		});

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);

		await addMemberRole(harness, owner.token, guild.id, member.userId, managerRole.id);

		const channel1 = await createChannel(harness, owner.token, guild.id, 'channel-1');
		const channel2 = await createChannel(harness, owner.token, guild.id, 'channel-2');

		await updateChannelPositions(harness, member.token, guild.id, [
			{id: channel1.id, position: 1},
			{id: channel2.id, position: 0},
		]);
	});

	test('should reject invalid channel id in position update', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		await createBuilder(harness, account.token)
			.patch(`/guilds/${guild.id}/channels`)
			.body([{id: '999999999999999999', position: 0}])
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	test('should lock permissions when moving to category', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const category = await createChannel(harness, account.token, guild.id, 'Category', ChannelTypes.GUILD_CATEGORY);
		const textChannel = await createChannel(harness, account.token, guild.id, 'text-channel');

		await updateChannelPositions(harness, account.token, guild.id, [
			{id: textChannel.id, parent_id: category.id, lock_permissions: true},
		]);

		const updatedChannel = await getChannel(harness, account.token, textChannel.id);
		expect(updatedChannel.parent_id).toBe(category.id);
	});

	test('should handle moving multiple channels at once', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const channel1 = await createChannel(harness, account.token, guild.id, 'channel-1');
		const channel2 = await createChannel(harness, account.token, guild.id, 'channel-2');
		const channel3 = await createChannel(harness, account.token, guild.id, 'channel-3');

		await updateChannelPositions(harness, account.token, guild.id, [
			{id: channel1.id, position: 2},
			{id: channel2.id, position: 0},
			{id: channel3.id, position: 1},
		]);
	});

	test('should reject category as parent of category', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const category1 = await createChannel(harness, account.token, guild.id, 'Category 1', ChannelTypes.GUILD_CATEGORY);
		const category2 = await createChannel(harness, account.token, guild.id, 'Category 2', ChannelTypes.GUILD_CATEGORY);

		await createBuilder(harness, account.token)
			.patch(`/guilds/${guild.id}/channels`)
			.body([{id: category2.id, parent_id: category1.id}])
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	test('should move a voice channel into a text category with provided position', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const textCategory = await createChannel(harness, account.token, guild.id, 'Text', ChannelTypes.GUILD_CATEGORY);
		const voiceCategory = await createChannel(harness, account.token, guild.id, 'Voice', ChannelTypes.GUILD_CATEGORY);
		const textChannel = await createChannel(harness, account.token, guild.id, 'general', ChannelTypes.GUILD_TEXT);
		const voiceChannel = await createChannel(harness, account.token, guild.id, 'lounge', ChannelTypes.GUILD_VOICE);

		await updateChannelPositions(harness, account.token, guild.id, [{id: textChannel.id, parent_id: textCategory.id}]);
		await updateChannelPositions(harness, account.token, guild.id, [
			{id: voiceChannel.id, parent_id: voiceCategory.id},
		]);

		await updateChannelPositions(harness, account.token, guild.id, [
			{id: voiceChannel.id, parent_id: textCategory.id, position: 1},
		]);

		const channels = await getGuildChannels(harness, account.token, guild.id);
		const movedVoice = channels.find((channel) => channel.id === voiceChannel.id);
		expect(movedVoice?.parent_id).toBe(textCategory.id);

		const destinationSiblings = channels
			.filter((channel) => channel.parent_id === textCategory.id)
			.sort((a, b) => (a.position ?? 0) - (b.position ?? 0));
		expect(destinationSiblings.map((channel) => channel.id)).toEqual([textChannel.id, voiceChannel.id]);
	});

	test('should place moved voice channels after all text siblings', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const textCategory = await createChannel(harness, account.token, guild.id, 'Text', ChannelTypes.GUILD_CATEGORY);
		const voiceCategory = await createChannel(harness, account.token, guild.id, 'Voice', ChannelTypes.GUILD_CATEGORY);
		const textOne = await createChannel(harness, account.token, guild.id, 'one', ChannelTypes.GUILD_TEXT);
		const textTwo = await createChannel(harness, account.token, guild.id, 'two', ChannelTypes.GUILD_TEXT);
		const voiceChannel = await createChannel(harness, account.token, guild.id, 'lounge', ChannelTypes.GUILD_VOICE);

		await updateChannelPositions(harness, account.token, guild.id, [
			{id: textOne.id, parent_id: textCategory.id},
			{id: textTwo.id, parent_id: textCategory.id},
			{id: voiceChannel.id, parent_id: voiceCategory.id},
		]);

		await updateChannelPositions(harness, account.token, guild.id, [
			{id: voiceChannel.id, parent_id: textCategory.id, position: 2},
		]);

		const channels = await getGuildChannels(harness, account.token, guild.id);
		const destinationSiblings = channels
			.filter((channel) => channel.parent_id === textCategory.id)
			.sort((a, b) => (a.position ?? 0) - (b.position ?? 0));
		expect(destinationSiblings.map((channel) => channel.id)).toEqual([textOne.id, textTwo.id, voiceChannel.id]);
	});

	test('should move default general text under the default voice category', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');
		const channels = await getGuildChannels(harness, account.token, guild.id);

		const general = channels.find((channel) => channel.type === ChannelTypes.GUILD_TEXT && channel.name === 'general');
		const voiceCategory = channels.find(
			(channel) => channel.type === ChannelTypes.GUILD_CATEGORY && channel.name === 'Voice Channels',
		);
		const generalVoice = channels.find(
			(channel) => channel.type === ChannelTypes.GUILD_VOICE && channel.name === 'General',
		);

		expect(general).toBeDefined();
		expect(voiceCategory).toBeDefined();
		expect(generalVoice).toBeDefined();

		await updateChannelPositions(harness, account.token, guild.id, [
			{
				id: general!.id,
				parent_id: voiceCategory!.id,
				position: 0,
			},
		]);

		const updatedChannels = await getGuildChannels(harness, account.token, guild.id);
		const voiceCategorySiblings = updatedChannels
			.filter((channel) => channel.parent_id === voiceCategory!.id)
			.sort((a, b) => (a.position ?? 0) - (b.position ?? 0));
		expect(voiceCategorySiblings.map((channel) => channel.id)).toEqual([general!.id, generalVoice!.id]);
	});

	test('should prioritise preceding_sibling_id over position when both are provided', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const category = await createChannel(harness, account.token, guild.id, 'Category', ChannelTypes.GUILD_CATEGORY);
		const textOne = await createChannel(harness, account.token, guild.id, 'one', ChannelTypes.GUILD_TEXT);
		const textTwo = await createChannel(harness, account.token, guild.id, 'two', ChannelTypes.GUILD_TEXT);
		const textThree = await createChannel(harness, account.token, guild.id, 'three', ChannelTypes.GUILD_TEXT);

		await updateChannelPositions(harness, account.token, guild.id, [
			{id: textOne.id, parent_id: category.id},
			{id: textTwo.id, parent_id: category.id},
			{id: textThree.id, parent_id: category.id},
		]);

		await updateChannelPositions(harness, account.token, guild.id, [
			{
				id: textThree.id,
				parent_id: category.id,
				position: 0,
				preceding_sibling_id: textOne.id,
			},
		]);

		const channels = await getGuildChannels(harness, account.token, guild.id);
		const siblings = channels
			.filter((channel) => channel.parent_id === category.id)
			.sort((a, b) => (a.position ?? 0) - (b.position ?? 0));
		expect(siblings.map((channel) => channel.id)).toEqual([textOne.id, textThree.id, textTwo.id]);
	});

	test('should reject preceding sibling from a different parent', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const categoryOne = await createChannel(harness, account.token, guild.id, 'Cat 1', ChannelTypes.GUILD_CATEGORY);
		const categoryTwo = await createChannel(harness, account.token, guild.id, 'Cat 2', ChannelTypes.GUILD_CATEGORY);
		const textOne = await createChannel(harness, account.token, guild.id, 'one', ChannelTypes.GUILD_TEXT);
		const textTwo = await createChannel(harness, account.token, guild.id, 'two', ChannelTypes.GUILD_TEXT);

		await updateChannelPositions(harness, account.token, guild.id, [
			{id: textOne.id, parent_id: categoryOne.id},
			{id: textTwo.id, parent_id: categoryTwo.id},
		]);

		const response = await createBuilder<{
			code: string;
			errors: Array<{path: string; code: string; message: string}>;
		}>(harness, account.token)
			.patch(`/guilds/${guild.id}/channels`)
			.body([
				{
					id: textOne.id,
					parent_id: categoryOne.id,
					preceding_sibling_id: textTwo.id,
				},
			])
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();

		expect(response.code).toBe('INVALID_FORM_BODY');
		expect(response.errors[0]?.path).toBe('preceding_sibling_id');
		expect(response.errors[0]?.code).toBe(ValidationErrorCodes.PRECEDING_CHANNEL_MUST_SHARE_PARENT);
	});

	test('should reject positioning a category relative to its own child', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const category = await createChannel(harness, account.token, guild.id, 'Cat', ChannelTypes.GUILD_CATEGORY);
		const child = await createChannel(harness, account.token, guild.id, 'child', ChannelTypes.GUILD_TEXT);

		await updateChannelPositions(harness, account.token, guild.id, [{id: child.id, parent_id: category.id}]);

		const response = await createBuilder<{
			code: string;
			errors: Array<{path: string; code: string; message: string}>;
		}>(harness, account.token)
			.patch(`/guilds/${guild.id}/channels`)
			.body([
				{
					id: category.id,
					parent_id: null,
					preceding_sibling_id: child.id,
				},
			])
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();

		expect(response.code).toBe('INVALID_FORM_BODY');
		expect(response.errors[0]?.path).toBe('preceding_sibling_id');
		expect(response.errors[0]?.code).toBe(ValidationErrorCodes.CANNOT_POSITION_CHANNEL_RELATIVE_TO_ITSELF);
	});

	test('should reorder top-level categories despite unrelated legacy voice and text ordering', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const milsims = await createChannel(harness, account.token, guild.id, 'MILSIMS', ChannelTypes.GUILD_CATEGORY);
		const coopGames = await createChannel(harness, account.token, guild.id, 'COOP GAMES', ChannelTypes.GUILD_CATEGORY);
		const frontDoor = await createChannel(harness, account.token, guild.id, 'FRONT DOOR', ChannelTypes.GUILD_CATEGORY);
		const coopText = await createChannel(harness, account.token, guild.id, 'coop-text', ChannelTypes.GUILD_TEXT);
		const coopVoice = await createChannel(harness, account.token, guild.id, 'coop-voice', ChannelTypes.GUILD_VOICE);

		await updateChannelPositions(harness, account.token, guild.id, [
			{id: coopText.id, parent_id: coopGames.id},
			{id: coopVoice.id, parent_id: coopGames.id},
		]);

		const channelRepository = new ChannelRepository();
		const channelsBeforeMove = await channelRepository.listGuildChannels(createGuildID(BigInt(guild.id)));
		const storedCoopText = channelsBeforeMove.find((channel) => channel.id.toString() === coopText.id);
		const storedCoopVoice = channelsBeforeMove.find((channel) => channel.id.toString() === coopVoice.id);

		expect(storedCoopText).toBeDefined();
		expect(storedCoopVoice).toBeDefined();
		if (!storedCoopText || !storedCoopVoice) {
			return;
		}

		await channelRepository.upsert({...storedCoopVoice.toRow(), position: 1});
		await channelRepository.upsert({...storedCoopText.toRow(), position: 2});

		await updateChannelPositions(harness, account.token, guild.id, [
			{
				id: frontDoor.id,
				parent_id: null,
				preceding_sibling_id: milsims.id,
			},
		]);

		const channels = await getGuildChannels(harness, account.token, guild.id);
		const orderedRootCategories = channels
			.filter((channel) => channel.type === ChannelTypes.GUILD_CATEGORY && channel.parent_id == null)
			.sort((a, b) => (a.position ?? 0) - (b.position ?? 0))
			.map((channel) => channel.id);

		const milsimsIndex = orderedRootCategories.indexOf(milsims.id);
		const frontDoorIndex = orderedRootCategories.indexOf(frontDoor.id);
		const coopGamesIndex = orderedRootCategories.indexOf(coopGames.id);

		expect(milsimsIndex).toBeGreaterThanOrEqual(0);
		expect(frontDoorIndex).toBeGreaterThanOrEqual(0);
		expect(coopGamesIndex).toBeGreaterThanOrEqual(0);
		expect(frontDoorIndex).toBeGreaterThan(milsimsIndex);
		expect(frontDoorIndex).toBeLessThan(coopGamesIndex);
	});

	test('should reject text channels being positioned below voice channels via preceding_sibling_id', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');

		const category = await createChannel(harness, account.token, guild.id, 'Mixed', ChannelTypes.GUILD_CATEGORY);
		const text = await createChannel(harness, account.token, guild.id, 'text', ChannelTypes.GUILD_TEXT);
		const voice = await createChannel(harness, account.token, guild.id, 'voice', ChannelTypes.GUILD_VOICE);

		await updateChannelPositions(harness, account.token, guild.id, [
			{id: text.id, parent_id: category.id},
			{id: voice.id, parent_id: category.id},
		]);

		const response = await createBuilder<{
			code: string;
			errors: Array<{path: string; code: string; message: string}>;
		}>(harness, account.token)
			.patch(`/guilds/${guild.id}/channels`)
			.body([
				{
					id: text.id,
					parent_id: category.id,
					preceding_sibling_id: voice.id,
				},
			])
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();

		expect(response.code).toBe('INVALID_FORM_BODY');
		expect(response.errors[0]?.path).toBe('preceding_sibling_id');
		expect(response.errors[0]?.code).toBe(ValidationErrorCodes.VOICE_CHANNELS_CANNOT_BE_ABOVE_TEXT_CHANNELS);
	});
});
