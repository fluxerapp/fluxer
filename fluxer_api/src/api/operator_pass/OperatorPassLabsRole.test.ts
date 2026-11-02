// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {GuildService} from '@app/api/guild/services/GuildService';
import type {User} from '@app/api/models/User';
import {OperatorPassLabsRoleService} from '@app/api/operator_pass/OperatorPassLabsRole';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, expect, test, vi} from 'vitest';

const LABS_GUILD_ID = 7001n;
const OPERATOR_ROLE_ID = 7002n;
const VISIONARY_ROLE_ID = 7003n;

const original = {...Config.operatorPass};
const originalSelfHosted = Config.instance.selfHosted;

beforeEach(() => {
	Config.instance.selfHosted = false;
	Config.operatorPass.labsGuildId = LABS_GUILD_ID;
	Config.operatorPass.labsOperatorRoleId = OPERATOR_ROLE_ID;
	Config.operatorPass.labsVisionaryRoleId = VISIONARY_ROLE_ID;
});

afterEach(() => {
	Object.assign(Config.operatorPass, original);
	Config.instance.selfHosted = originalSelfHosted;
});

test('the Labs sync takes the Visionary role from someone who lost Visionary and keeps an Operator role they earned', async () => {
	const downgraded = createUserID(8001n);
	const visionary = createUserID(8002n);
	const plain = createUserID(8003n);
	const members = [
		{userId: downgraded, roleIds: new Set([VISIONARY_ROLE_ID, OPERATOR_ROLE_ID])},
		{userId: visionary, roleIds: new Set<bigint>()},
		{userId: plain, roleIds: new Set<bigint>()},
	];
	const guildRepository = {
		getRole: vi.fn(async () => ({id: VISIONARY_ROLE_ID})),
		listMembersPaginated: vi.fn(async (_guildId: bigint, _size: number, after?: bigint) => (after ? [] : members)),
	};
	const systemAddMemberRole = vi.fn(async () => {});
	const systemRemoveMemberRole = vi.fn(async () => {});
	const users = [
		{id: downgraded, premiumType: UserPremiumTypes.SUBSCRIPTION},
		{id: visionary, premiumType: UserPremiumTypes.LIFETIME},
		{id: plain, premiumType: null},
	] as Array<User>;
	const service = new OperatorPassLabsRoleService(
		guildRepository as unknown as GuildRepository,
		{members: {systemAddMemberRole, systemRemoveMemberRole}} as unknown as GuildService,
	);

	const summary = await service.syncVisionaryRoleForLabsMembers({listUsers: vi.fn(async () => users)}, 500);

	expect(summary).toEqual({granted: 1, removed: 1, failed: 0});
	expect(systemRemoveMemberRole).toHaveBeenCalledOnce();
	expect(systemRemoveMemberRole).toHaveBeenCalledWith(
		expect.objectContaining({targetId: downgraded, roleId: VISIONARY_ROLE_ID}),
	);
	expect(systemAddMemberRole).toHaveBeenCalledOnce();
	expect(systemAddMemberRole).toHaveBeenCalledWith(
		expect.objectContaining({targetId: visionary, roleId: VISIONARY_ROLE_ID}),
	);
});
