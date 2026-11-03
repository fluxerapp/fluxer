// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createPermissionOverwrite, createRole, sendChannelMessage} from '@app/api/channel/tests/ChannelTestUtils';
import {markChannelAsIndexed} from '@app/api/message/tests/MessageTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, assert, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface Registered {
	token: string;
	user_id: string;
}

interface Scene {
	owner: TestAccount;
	guildId: string;
	channelId: string;
	code: string;
	message: MessageResponse;
}

function yearsAgo(years: number): string {
	const date = new Date();
	date.setUTCFullYear(date.getUTCFullYear() - years);
	date.setUTCDate(date.getUTCDate() - 1);
	return date.toISOString().slice(0, 10);
}

describe('Guild visitor reads', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	beforeEach(async () => {
		await harness.reset();
		await getInstanceConfigRepository().updateGuildVisitsConfig({enabled: true, user_basis_points: 10000});
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function createScene(): Promise<Scene> {
		const owner = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, owner.token)
			.post('/guilds')
			.body({name: 'Reading Room'})
			.execute();
		assert(typeof guild.system_channel_id === 'string');
		const channelId = guild.system_channel_id;
		const message = await sendChannelMessage(harness, owner.token, channelId, 'welcome to the reading room');
		const invite = await createBuilder<{code: string}>(harness, owner.token)
			.post(`/channels/${channelId}/invites`)
			.body({})
			.execute();
		return {owner, guildId: guild.id, channelId, code: invite.code, message};
	}

	async function createUnclaimed(dateOfBirth = yearsAgo(30)): Promise<Registered> {
		return await createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.body({username: 'reader', consent: true, date_of_birth: dateOfBirth})
			.execute();
	}

	async function visit(token: string, code: string): Promise<void> {
		await createBuilder(harness, token).post(`/invites/${code}/visit`).body(null).execute();
	}

	async function statusOf(token: string, method: string, path: string, body?: unknown): Promise<number> {
		const response = await harness.requestJson({
			path,
			method,
			body,
			headers: {Authorization: token},
		});
		await response.text();
		return response.status;
	}

	it('reads channels, messages and pins the way @everyone can', async () => {
		const {owner, guildId, channelId, code, message} = await createScene();
		const role = await createRole(harness, owner.token, guildId, {name: 'Mods'});
		await createPermissionOverwrite(harness, owner.token, channelId, role.id, {
			type: 0,
			allow: Permissions.MANAGE_MESSAGES.toString(),
			deny: '0',
		});
		await createBuilder(harness, owner.token)
			.put(`/channels/${channelId}/pins/${message.id}`)
			.body(null)
			.expect(HttpStatus.NO_CONTENT)
			.execute();
		const visitor = await createUnclaimed();
		await visit(visitor.token, code);
		const messages = await createBuilder<Array<MessageResponse>>(harness, visitor.token)
			.get(`/channels/${channelId}/messages`)
			.execute();
		expect(messages.map((entry) => entry.id)).toContain(message.id);
		const single = await createBuilder<MessageResponse>(harness, visitor.token)
			.get(`/channels/${channelId}/messages/${message.id}`)
			.execute();
		expect(single.content).toBe('welcome to the reading room');
		const pins = await createBuilder<{items: Array<{message: {id: string}}>}>(harness, visitor.token)
			.get(`/channels/${channelId}/messages/pins`)
			.execute();
		expect(pins.items.map((item) => item.message.id)).toEqual([message.id]);
		const channel = await createBuilder<ChannelResponse>(harness, visitor.token)
			.get(`/channels/${channelId}`)
			.execute();
		expect((channel.permission_overwrites ?? []).map((overwrite) => overwrite.id)).not.toContain(role.id);
		const guild = await createBuilder<GuildResponse>(harness, visitor.token).get(`/guilds/${guildId}`).execute();
		expect(guild.visiting).toBe(true);
		expect(guild.channels?.map((entry) => entry.id)).toContain(channelId);
		for (const entry of guild.channels ?? []) {
			expect((entry.permission_overwrites ?? []).every((overwrite) => overwrite.id === guildId)).toBe(true);
		}
		const channels = await createBuilder<Array<ChannelResponse>>(harness, visitor.token)
			.get(`/guilds/${guildId}/channels`)
			.execute();
		expect(channels.map((entry) => entry.id)).toContain(channelId);
	});

	it('refuses every route that affects other people or reveals members', async () => {
		const {guildId, channelId, code, message} = await createScene();
		const visitor = await createTestAccount(harness);
		await visit(visitor.token, code);
		const refused: Array<[string, string, unknown?]> = [
			['POST', `/channels/${channelId}/messages`, {content: 'hi'}],
			['PUT', `/channels/${channelId}/messages/${message.id}/reactions/%F0%9F%91%8D/@me`],
			['GET', `/channels/${channelId}/messages/${message.id}/reactions/%F0%9F%91%8D`],
			['POST', `/channels/${channelId}/typing`],
			['POST', `/channels/${channelId}/invites`, {}],
			['GET', `/channels/${channelId}/invites`],
			['POST', `/channels/${channelId}/threads`, {name: 'side thread', type: ChannelTypes.PUBLIC_THREAD}],
			['GET', `/guilds/${guildId}/members`],
			['GET', `/guilds/${guildId}/members/${visitor.userId}`],
			['GET', `/guilds/${guildId}/invites`],
			['GET', `/guilds/${guildId}/audit-logs`],
			['PATCH', `/guilds/${guildId}/members/@me`, {nick: 'lurker'}],
			['GET', `/guilds/${guildId}/visitors`],
			['POST', '/search/messages', {content: 'welcome', context_guild_id: guildId}],
			['PUT', `/channels/${channelId}/pins/${message.id}`],
			['DELETE', `/channels/${channelId}/messages/${message.id}`],
		];
		for (const [method, path, body] of refused) {
			const status = await statusOf(visitor.token, method, path, body);
			expect([HttpStatus.FORBIDDEN, HttpStatus.NOT_FOUND, HttpStatus.BAD_REQUEST]).toContain(status);
		}
		await createBuilder(harness, visitor.token).get(`/channels/${channelId}/messages`).execute();
	});

	it('hides channels that @everyone cannot see', async () => {
		const {owner, guildId, channelId, code} = await createScene();
		const visitor = await createUnclaimed();
		await visit(visitor.token, code);
		await createBuilder(harness, visitor.token).get(`/channels/${channelId}/messages`).execute();
		await createPermissionOverwrite(harness, owner.token, channelId, guildId, {
			type: 0,
			allow: '0',
			deny: Permissions.VIEW_CHANNEL.toString(),
		});
		await createBuilder(harness, visitor.token)
			.get(`/channels/${channelId}/messages`)
			.expect(HttpStatus.NOT_FOUND, APIErrorCodes.UNKNOWN_CHANNEL)
			.execute();
		const channels = await createBuilder<Array<ChannelResponse>>(harness, visitor.token)
			.get(`/guilds/${guildId}/channels`)
			.execute();
		expect(channels.map((entry) => entry.id)).not.toContain(channelId);
	});

	it('picks up a changed @everyone role on the next read', async () => {
		const {owner, guildId, channelId, code} = await createScene();
		const visitor = await createUnclaimed();
		await visit(visitor.token, code);
		await createBuilder(harness, visitor.token).get(`/channels/${channelId}/messages`).execute();
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guildId}/roles/${guildId}`)
			.body({permissions: '0'})
			.execute();
		await createBuilder(harness, visitor.token)
			.get(`/channels/${channelId}/messages`)
			.expect(HttpStatus.NOT_FOUND, APIErrorCodes.UNKNOWN_CHANNEL)
			.execute();
	});

	it('hides adult channels from younger visitors entirely', async () => {
		const {owner, channelId, code} = await createScene();
		await createBuilder(harness, owner.token).patch(`/channels/${channelId}`).body({nsfw: true}).execute();
		const teen = await createUnclaimed(yearsAgo(15));
		await visit(teen.token, code);
		await createBuilder(harness, teen.token)
			.get(`/channels/${channelId}/messages`)
			.expect(HttpStatus.NOT_FOUND, APIErrorCodes.UNKNOWN_CHANNEL)
			.execute();
		const adult = await createUnclaimed(yearsAgo(30));
		await visit(adult.token, code);
		await createBuilder(harness, adult.token).get(`/channels/${channelId}/messages`).execute();
	});

	it('limits how far back accounts without a confirmed email can read', async () => {
		const {channelId, code, message} = await createScene();
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['*']);
		const current = await createBuilder<{limit_config: LimitConfigSnapshot}>(harness, admin.token)
			.get('/admin/limit-config')
			.execute();
		await createBuilder(harness, admin.token)
			.put('/admin/limit-config')
			.body({
				limit_config: {
					traitDefinitions: current.limit_config.traitDefinitions,
					rules: current.limit_config.rules.map((rule) =>
						rule.id === 'default' ? {...rule, limits: {...rule.limits, guild_visit_history_days_unverified: 0}} : rule,
					),
				},
			})
			.execute();
		const unclaimed = await createUnclaimed();
		await visit(unclaimed.token, code);
		const windowed = await createBuilder<Array<MessageResponse>>(harness, unclaimed.token)
			.get(`/channels/${channelId}/messages`)
			.execute();
		expect(windowed).toEqual([]);
		await createBuilder(harness, unclaimed.token)
			.get(`/channels/${channelId}/messages/${message.id}`)
			.expect(HttpStatus.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
			.execute();
		const verified = await createTestAccount(harness);
		await visit(verified.token, code);
		const full = await createBuilder<Array<MessageResponse>>(harness, verified.token)
			.get(`/channels/${channelId}/messages`)
			.execute();
		expect(full.map((entry) => entry.id)).toContain(message.id);
	});

	it('searches one channel by content only', async () => {
		const {owner, channelId, code} = await createScene();
		await markChannelAsIndexed(harness, channelId);
		const visitor = await createUnclaimed();
		await visit(visitor.token, code);
		await createBuilder(harness, visitor.token)
			.post('/search/messages')
			.body({content: 'reading', context_channel_id: channelId, author_id: [owner.userId]})
			.expect(HttpStatus.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		await createBuilder(harness, visitor.token)
			.post('/search/messages')
			.body({content: 'reading', context_channel_id: channelId, mentions: [owner.userId]})
			.expect(HttpStatus.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		await createBuilder(harness, visitor.token)
			.post('/search/messages')
			.body({content: 'reading', context_channel_id: channelId})
			.execute();
	});

	it('runs visitor reads through their own buckets', async () => {
		const {owner, channelId, code} = await createScene();
		const visitor = await createUnclaimed();
		await visit(visitor.token, code);
		const statuses: Array<number> = [];
		for (let attempt = 0; attempt < 11; attempt++) {
			const response = await harness.requestJson({
				path: `/channels/${channelId}/messages`,
				method: 'GET',
				headers: {Authorization: visitor.token, 'x-fluxer-test-enable-rate-limits': 'true'},
			});
			await response.text();
			statuses.push(response.status);
		}
		expect(statuses.slice(0, 10).every((status) => status === HttpStatus.OK)).toBe(true);
		expect(statuses[10]).toBe(HttpStatus.TOO_MANY_REQUESTS);
		const member = await harness.requestJson({
			path: `/channels/${channelId}/messages`,
			method: 'GET',
			headers: {Authorization: owner.token, 'x-fluxer-test-enable-rate-limits': 'true'},
		});
		await member.text();
		expect(member.status).toBe(HttpStatus.OK);
	});
});
