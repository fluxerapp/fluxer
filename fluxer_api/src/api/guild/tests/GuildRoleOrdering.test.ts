// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createRoleID} from '@app/api/BrandedTypes';
import {getMemberListRoleOrderIds} from '@app/api/guild/services/role/RoleOrderAuditUtils';
import {createGuild, createRole, getRoles} from '@app/api/guild/tests/GuildTestUtils';
import {GuildRole} from '@app/api/models/GuildRole';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

type RoleName = 'Alpha' | 'Bravo' | 'Charlie';

describe('Guild role ordering', () => {
	let harness: ApiTestHarness;
	let owner: TestAccount;
	let guild: GuildResponse;
	let roles: Record<RoleName, GuildRoleResponse>;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		owner = await createTestAccount(harness);
		guild = await createGuild(harness, owner.token, 'Role ordering');
		roles = {
			Alpha: await createRole(harness, owner.token, guild.id, {name: 'Alpha'}),
			Bravo: await createRole(harness, owner.token, guild.id, {name: 'Bravo'}),
			Charlie: await createRole(harness, owner.token, guild.id, {name: 'Charlie'}),
		};
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	async function listedPositions(): Promise<Array<{name: string; position: number}>> {
		const listed = await getRoles(harness, owner.token, guild.id);
		return listed.filter((role) => role.id !== guild.id).map(({name, position}) => ({name, position}));
	}

	test('lists equal-position roles in snowflake order', async () => {
		expect(await listedPositions()).toEqual([
			{name: 'Alpha', position: 1},
			{name: 'Bravo', position: 1},
			{name: 'Charlie', position: 1},
		]);
	});

	test.each([
		{
			name: 'omitted positions keep the existing tie order',
			updates: [{role: 'Charlie'}, {role: 'Bravo'}, {role: 'Alpha'}],
			expected: [
				{name: 'Alpha', position: 3},
				{name: 'Bravo', position: 2},
				{name: 'Charlie', position: 1},
			],
		},
		{
			name: 'zero positions keep the existing tie order',
			updates: [
				{role: 'Charlie', position: 0},
				{role: 'Bravo', position: 0},
				{role: 'Alpha', position: 0},
			],
			expected: [
				{name: 'Alpha', position: 3},
				{name: 'Bravo', position: 2},
				{name: 'Charlie', position: 1},
			],
		},
		{
			name: 'the last duplicate position wins',
			updates: [
				{role: 'Alpha', position: 10},
				{role: 'Alpha', position: -1},
			],
			expected: [
				{name: 'Bravo', position: 3},
				{name: 'Charlie', position: 2},
				{name: 'Alpha', position: 1},
			],
		},
	] satisfies Array<{
		name: string;
		updates: Array<{role: RoleName; position?: number}>;
		expected: Array<{name: string; position: number}>;
	}>)('$name', async ({updates, expected}) => {
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/roles`)
			.body(updates.map(({role, ...update}) => ({id: roles[role].id, ...update})))
			.expect(HttpStatus.NO_CONTENT)
			.execute();
		expect(await listedPositions()).toEqual(expected);
	});

	test.each([
		{suffix: '', update: {position: 7}},
		{suffix: '/hoist-positions', update: {hoist_position: 7}},
	])('validates all $suffix updates before writing', async ({suffix, update}) => {
		const before = await getRoles(harness, owner.token, guild.id);
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/roles${suffix}`)
			.body([
				{id: roles.Alpha.id, ...update},
				{id: '1420000000000000999', ...update},
			])
			.expect(HttpStatus.BAD_REQUEST)
			.execute();
		expect(await getRoles(harness, owner.token, guild.id)).toEqual(before);
	});

	test('applies duplicate hoist updates in request order', async () => {
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/roles/hoist-positions`)
			.body([
				{id: roles.Alpha.id, hoist_position: 5},
				{id: roles.Alpha.id, hoist_position: 0},
			])
			.expect(HttpStatus.NO_CONTENT)
			.execute();
		const listed = await getRoles(harness, owner.token, guild.id);
		expect(
			listed.filter((role) => role.id !== guild.id).map(({name, hoist_position}) => ({name, hoist_position})),
		).toEqual([
			{name: 'Alpha', hoist_position: 0},
			{name: 'Bravo', hoist_position: null},
			{name: 'Charlie', hoist_position: null},
		]);
	});
});

test('member-list ordering uses effective hoist position then snowflakes, excluding everyone and non-hoisted roles', () => {
	const roles = [
		{id: 30n, position: 1, hoistPosition: 2, hoist: true},
		{id: 10n, position: 1, hoistPosition: 2, hoist: true},
		{id: 20n, position: 1, hoistPosition: 2, hoist: true},
		{id: 40n, position: 3, hoistPosition: null, hoist: true},
		{id: 50n, position: 9, hoistPosition: 0, hoist: true},
		{id: 60n, position: 10, hoistPosition: 10, hoist: false},
		{id: 1n, position: 10, hoistPosition: 10, hoist: true},
	].map(
		({id, position, hoistPosition, hoist}) =>
			new GuildRole({
				guild_id: createGuildID(1n),
				role_id: createRoleID(id),
				name: 'Role',
				permissions: 0n,
				position,
				hoist_position: hoistPosition,
				color: 0,
				icon_hash: null,
				unicode_emoji: null,
				hoist,
				mentionable: false,
				version: 1,
			}),
	);
	expect(getMemberListRoleOrderIds(roles, createRoleID(1n))).toEqual([40n, 10n, 20n, 30n, 50n]);
});
