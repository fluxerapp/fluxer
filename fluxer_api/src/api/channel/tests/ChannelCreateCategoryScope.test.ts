// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	addMemberRole,
	createChannel,
	createPermissionOverwrite,
	createRole,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Channel create in a category', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});

	async function setupCategoryManager(extraAllow = 0n) {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const category = await createChannel(harness, owner.token, guild.id, 'managed', ChannelTypes.GUILD_CATEGORY);
		const otherCategory = await createChannel(harness, owner.token, guild.id, 'other', ChannelTypes.GUILD_CATEGORY);
		const role = await createRole(harness, owner.token, guild.id, {name: 'creators'});
		await addMemberRole(harness, owner.token, guild.id, member.userId, role.id);
		await createPermissionOverwrite(harness, owner.token, category.id, role.id, {
			type: 0,
			allow: (Permissions.VIEW_CHANNEL | Permissions.MANAGE_CHANNELS | extraAllow).toString(),
			deny: '0',
		});
		return {owner, member, guild, role, category, otherCategory};
	}

	it('lets a category overwrite grant MANAGE_CHANNELS for that category only', async () => {
		const {member, guild, category, otherCategory} = await setupCategoryManager();
		const created = await createBuilder<ChannelResponse>(harness, member.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'member-made', type: ChannelTypes.GUILD_TEXT, parent_id: category.id})
			.execute();
		expect(created.parent_id).toBe(category.id);
		await createBuilder(harness, member.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'root-made', type: ChannelTypes.GUILD_TEXT})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		await createBuilder(harness, member.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'elsewhere', type: ChannelTypes.GUILD_TEXT, parent_id: otherCategory.id})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
	});

	it('honors a category deny of MANAGE_CHANNELS for a guild-wide manager', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const category = await createChannel(harness, owner.token, guild.id, 'locked', ChannelTypes.GUILD_CATEGORY);
		const role = await createRole(harness, owner.token, guild.id, {
			name: 'managers',
			permissions: Permissions.MANAGE_CHANNELS.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, member.userId, role.id);
		await createPermissionOverwrite(harness, owner.token, category.id, role.id, {
			type: 0,
			allow: '0',
			deny: Permissions.MANAGE_CHANNELS.toString(),
		});
		await createBuilder(harness, member.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'denied', type: ChannelTypes.GUILD_TEXT, parent_id: category.id})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
	});

	it('lets a category overwrite grant MANAGE_ROLES for requested overwrites within its own permissions', async () => {
		const {member, guild, role, category} = await setupCategoryManager(Permissions.MANAGE_ROLES);
		await createBuilder<ChannelResponse>(harness, member.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({
				name: 'with-overwrites',
				type: ChannelTypes.GUILD_TEXT,
				parent_id: category.id,
				permission_overwrites: [{id: role.id, type: 0, allow: Permissions.VIEW_CHANNEL.toString(), deny: '0'}],
			})
			.execute();
		await createBuilder(harness, member.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({
				name: 'escalate',
				type: ChannelTypes.GUILD_TEXT,
				parent_id: category.id,
				permission_overwrites: [{id: role.id, type: 0, allow: Permissions.ADMINISTRATOR.toString(), deny: '0'}],
			})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
	});

	it('rejects requested overwrites without MANAGE_ROLES on the category', async () => {
		const {member, guild, role, category} = await setupCategoryManager();
		await createBuilder(harness, member.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({
				name: 'no-roles',
				type: ChannelTypes.GUILD_TEXT,
				parent_id: category.id,
				permission_overwrites: [{id: role.id, type: 0, allow: Permissions.VIEW_CHANNEL.toString(), deny: '0'}],
			})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
	});
});
