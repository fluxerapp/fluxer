// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createRoleID, createUserID} from '@app/api/BrandedTypes';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {
	addMemberRole,
	createRole,
	getGuild,
	getMember,
	removeMemberRole,
	setupTestGuildWithMembers,
	updateGuild,
	updateMember,
	updateRolePositions,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	GuildFeatures,
	GuildVerificationLevel,
	SELF_ASSIGNABLE_ROLE_DESCRIPTION_MAX_LENGTH,
	SELF_ASSIGNABLE_ROLE_SECTION_CONTENT_MAX_LENGTH,
} from '@fluxer/constants/src/GuildConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {GuildAuditLogListResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {GuildSelfAssignableRoleSectionsUpdateRequest} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import type {
	GuildResponse,
	GuildSelfAssignableRoleSectionResponse,
} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {z} from 'zod';

const FEATURE = GuildFeatures.SELF_ASSIGNABLE_ROLES_ENABLED;

type SectionInput = z.input<typeof GuildSelfAssignableRoleSectionsUpdateRequest>['sections'][number];
type RoleInput = SectionInput['roles'][number];

interface FieldError {
	path: string;
	code: string;
}

describe('Self-assignable roles', () => {
	let harness: ApiTestHarness;
	let owner: TestAccount;
	let manager: TestAccount;
	let member: TestAccount;
	let guild: GuildResponse;
	let managerRole: GuildRoleResponse;
	let mageRole: GuildRoleResponse;
	let warriorRole: GuildRoleResponse;

	const sectionRoles = (...ids: Array<string>): Array<RoleInput> => ids.map((id) => ({id}));
	const newRole = (name: string, permissions = 0n) =>
		createRole(harness, owner.token, guild.id, {name, permissions: permissions.toString()});
	const giveRole = (account: TestAccount, role: GuildRoleResponse) =>
		updateMember(harness, owner.token, guild.id, account.userId, {roles: [role.id]});
	const memberRoles = async (account: TestAccount) =>
		(await getMember(harness, owner.token, guild.id, account.userId)).roles;
	const savedSections = async () => (await getGuild(harness, owner.token, guild.id)).self_assignable_role_sections;
	const auditEntries = async (actionType: AuditLogActionType) =>
		(
			await createBuilder<GuildAuditLogListResponse>(harness, owner.token)
				.get(`/guilds/${guild.id}/audit-logs?action_type=${actionType}`)
				.execute()
		).audit_log_entries;

	function putSections<T = Array<GuildSelfAssignableRoleSectionResponse>>(
		account: TestAccount,
		sections: Array<SectionInput>,
		status: number = HTTP_STATUS.OK,
		code?: string,
	): Promise<T> {
		return createBuilder<T>(harness, account.token)
			.put(`/guilds/${guild.id}/self-assignable-roles`)
			.body({sections})
			.expect(status, code)
			.execute();
	}

	function selfRole(
		method: 'put' | 'delete',
		account: TestAccount,
		roleId: string,
		status: number = HTTP_STATUS.NO_CONTENT,
		code?: string,
	) {
		const send = method === 'put' ? addMemberRole : removeMemberRole;
		return send(harness, account.token, guild.id, '@me', roleId, status, code);
	}

	async function setEnabled(account: TestAccount, enabled: boolean, status: number = HTTP_STATUS.OK) {
		const {features} = await getGuild(harness, account.token, guild.id);
		const others = features.filter((feature) => feature !== FEATURE);
		await updateGuild(harness, account.token, guild.id, {features: enabled ? [...others, FEATURE] : others}, status);
	}

	async function offerMageRole() {
		await putSections(owner, [{roles: sectionRoles(mageRole.id)}]);
		await setEnabled(owner, true);
	}

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	beforeEach(async () => {
		await harness.reset();
		const setup = await setupTestGuildWithMembers(harness, 2);
		({owner, guild} = setup);
		[manager, member] = setup.members as [TestAccount, TestAccount];
		managerRole = await newRole('Manager', Permissions.MANAGE_GUILD | Permissions.MANAGE_ROLES);
		mageRole = await newRole('Mage');
		warriorRole = await newRole('Warrior');
		await updateRolePositions(harness, owner.token, guild.id, [
			{id: managerRole.id, position: 3},
			{id: mageRole.id, position: 2},
			{id: warriorRole.id, position: 1},
		]);
		await giveRole(manager, managerRole);
	});

	it('saves sections and role descriptions, keeps section IDs and returns them on the guild', async () => {
		const saved = await putSections(manager, [
			{
				content: '  Choose your class!  ',
				roles: [{id: mageRole.id, description: '  Casts spells  '}, {id: warriorRole.id}],
			},
		]);
		expect(saved).toMatchObject([
			{
				content: 'Choose your class!',
				roles: [
					{id: mageRole.id, description: 'Casts spells'},
					{id: warriorRole.id, description: ''},
				],
			},
		]);
		expect(await savedSections()).toEqual(saved);

		const kept = {
			id: saved[0]!.id,
			content: 'Pick a class',
			roles: [{id: mageRole.id, description: 'Casts spells'}],
			max_roles: null,
			required_role_ids: [],
		};
		const resaved = await putSections(manager, [{roles: sectionRoles(warriorRole.id)}, kept]);
		expect(resaved).toEqual([
			expect.objectContaining({content: '', roles: [{id: warriorRole.id, description: ''}]}),
			kept,
		]);
		expect(resaved[0]!.id).not.toBe(kept.id);

		expect(await putSections(manager, [])).toEqual([]);
		expect(await savedSections()).toEqual([]);
		expect(await putSections(manager, [{content: '', roles: [{id: mageRole.id, description: ''}]}])).toEqual([
			expect.objectContaining({content: '', roles: [{id: mageRole.id, description: ''}]}),
		]);
	});

	it('requires both Manage Community and Manage Roles to save sections', async () => {
		for (const permission of [Permissions.MANAGE_ROLES, Permissions.MANAGE_GUILD]) {
			await giveRole(member, await newRole('Partial', permission));
			await putSections(member, [{roles: sectionRoles(mageRole.id)}], HTTP_STATUS.FORBIDDEN);
		}
	});

	it('only lets editors add roles below their highest role', async () => {
		await putSections(manager, [{roles: sectionRoles(managerRole.id)}], HTTP_STATUS.FORBIDDEN);
		await putSections(owner, [{content: 'Staff', roles: sectionRoles(managerRole.id)}]);
		const kept = await putSections(manager, [
			{content: 'Staff, edited', roles: sectionRoles(managerRole.id)},
			{roles: sectionRoles(mageRole.id)},
		]);
		expect(kept).toMatchObject([{roles: sectionRoles(managerRole.id)}, {roles: sectionRoles(mageRole.id)}]);
		expect(await putSections(manager, [{roles: sectionRoles(mageRole.id)}])).toMatchObject([
			{roles: sectionRoles(mageRole.id)},
		]);
	});

	it('only lets editors open a listed role to more members if it is below their highest role', async () => {
		const levelRole = await newRole('Level 5');
		const [section] = await putSections(owner, [{roles: sectionRoles(managerRole.id, mageRole.id)}]);
		const both = {id: section!.id, roles: sectionRoles(managerRole.id, mageRole.id)};
		await putSections(manager, [{...both, required_role_ids: [levelRole.id]}]);
		await putSections(manager, [{...both, required_role_ids: []}], HTTP_STATUS.FORBIDDEN);
		await putSections(manager, [{...both, required_role_ids: [levelRole.id, warriorRole.id]}], HTTP_STATUS.FORBIDDEN);
		await putSections(
			manager,
			[
				{id: section!.id, roles: sectionRoles(mageRole.id), required_role_ids: [levelRole.id]},
				{roles: sectionRoles(managerRole.id)},
			],
			HTTP_STATUS.FORBIDDEN,
		);
		await putSections(manager, [{...both, content: 'Still locked', required_role_ids: [levelRole.id]}]);
		expect(await savedSections()).toMatchObject([{content: 'Still locked', required_role_ids: [levelRole.id]}]);
		await putSections(owner, [{...both, required_role_ids: []}]);
	});

	it('does not exempt administrators from the rank check', async () => {
		const levelRole = await newRole('Level 5');
		await giveRole(member, await newRole('Admin', Permissions.ADMINISTRATOR));
		const [section] = await putSections(owner, [{roles: sectionRoles(managerRole.id)}]);
		const keep = {id: section!.id, roles: sectionRoles(managerRole.id)};
		await putSections(member, [{...keep, required_role_ids: [levelRole.id]}]);
		await putSections(member, [{...keep, required_role_ids: []}], HTTP_STATUS.FORBIDDEN);
	});

	it('rejects @everyone, unknown, duplicate and missing roles, over-long text and bad limits', async () => {
		const tooLong = 'a'.repeat(SELF_ASSIGNABLE_ROLE_SECTION_CONTENT_MAX_LENGTH + 1);
		const tooLongDescription = 'a'.repeat(SELF_ASSIGNABLE_ROLE_DESCRIPTION_MAX_LENGTH + 1);
		const cases: Array<[Array<SectionInput>, FieldError?]> = [
			[[{roles: sectionRoles(guild.id)}], {path: 'sections.0.roles.0.id', code: ValidationErrorCodes.INVALID_ROLE_ID}],
			[
				[{roles: sectionRoles(mageRole.id, '999999999999999999')}],
				{path: 'sections.0.roles.1.id', code: ValidationErrorCodes.INVALID_ROLE_ID},
			],
			[
				[{roles: sectionRoles(mageRole.id, warriorRole.id)}, {roles: sectionRoles(warriorRole.id)}],
				{path: 'sections.1.roles.0.id', code: ValidationErrorCodes.DUPLICATE_SELF_ASSIGNABLE_ROLE},
			],
			[[{content: 'No roles', roles: []}]],
			[[{content: tooLong, roles: sectionRoles(mageRole.id)}]],
			[
				[{roles: [{id: mageRole.id, description: tooLongDescription}]}],
				{path: 'sections.0.roles.0.description', code: ValidationErrorCodes.STRING_LENGTH_INVALID},
			],
			[Array.from({length: 11}, () => ({roles: sectionRoles(mageRole.id)}))],
			...[0, 26, 1.5].map((max_roles): [Array<SectionInput>] => [[{roles: sectionRoles(mageRole.id), max_roles}]]),
			...[guild.id, '999999999999999999'].map((roleId): [Array<SectionInput>, FieldError] => [
				[{roles: sectionRoles(mageRole.id), required_role_ids: [roleId]}],
				{path: 'sections.0.required_role_ids.0', code: ValidationErrorCodes.INVALID_ROLE_ID},
			]),
		];
		for (const [sections, error] of cases) {
			const body = await putSections<{errors?: Array<FieldError>}>(
				manager,
				sections,
				HTTP_STATUS.BAD_REQUEST,
				'INVALID_FORM_BODY',
			);
			if (error) expect(body.errors).toContainEqual(expect.objectContaining(error));
		}
		expect(await savedSections()).toEqual([]);
	});

	it('lets members add and remove self-assignable roles only while the switch is on', async () => {
		await putSections(owner, [{roles: sectionRoles(mageRole.id)}]);
		await selfRole('put', member, mageRole.id, HTTP_STATUS.FORBIDDEN);

		await setEnabled(owner, true);
		await selfRole('put', member, mageRole.id);
		expect(await memberRoles(member)).toContain(mageRole.id);
		await selfRole('delete', member, mageRole.id);
		expect(await memberRoles(member)).not.toContain(mageRole.id);
		await selfRole('put', member, warriorRole.id, HTTP_STATUS.FORBIDDEN);
		await selfRole('put', member, guild.id, HTTP_STATUS.BAD_REQUEST);

		await selfRole('put', member, mageRole.id);
		await setEnabled(owner, false);
		await selfRole('delete', member, mageRole.id, HTTP_STATUS.FORBIDDEN);
		expect(await memberRoles(member)).toContain(mageRole.id);
	});

	it('swaps roles in one update when a section allows one role', async () => {
		const [rogueRole, bardRole] = [await newRole('Rogue'), await newRole('Bard')];
		await putSections(owner, [
			{roles: sectionRoles(mageRole.id, warriorRole.id), max_roles: 1},
			{roles: sectionRoles(rogueRole.id, bardRole.id)},
		]);
		expect(await savedSections()).toMatchObject([{max_roles: 1}, {max_roles: null}]);
		await setEnabled(owner, true);

		await selfRole('put', member, mageRole.id);
		await selfRole('put', member, warriorRole.id);
		expect(await memberRoles(member)).toEqual([warriorRole.id]);
		const memberEntries = (await auditEntries(AuditLogActionType.MEMBER_ROLE_UPDATE)).filter(
			(entry) => entry.target_id === member.userId,
		);
		expect(memberEntries).toHaveLength(2);

		await selfRole('delete', member, warriorRole.id);
		await selfRole('put', member, rogueRole.id);
		await selfRole('put', member, bardRole.id);
		expect((await memberRoles(member)).sort()).toEqual([rogueRole.id, bardRole.id].sort());
	});

	it("stops members at a section's max_roles above 1", async () => {
		const rogueRole = await newRole('Rogue');
		await putSections(owner, [{roles: sectionRoles(mageRole.id, warriorRole.id, rogueRole.id), max_roles: 2}]);
		await setEnabled(owner, true);
		await selfRole('put', member, mageRole.id);
		await selfRole('put', member, warriorRole.id);
		await selfRole('put', member, rogueRole.id, HTTP_STATUS.BAD_REQUEST, APIErrorCodes.MAX_SELF_ASSIGNED_ROLES);
		await selfRole('delete', member, warriorRole.id);
		await selfRole('put', member, rogueRole.id);
		expect((await memberRoles(member)).sort()).toEqual([mageRole.id, rogueRole.id].sort());
	});

	it('only lets members with a required role add or remove its roles', async () => {
		const levelRole = await newRole('Level 5');
		await putSections(owner, [{roles: sectionRoles(mageRole.id), required_role_ids: [levelRole.id, levelRole.id]}]);
		expect(await savedSections()).toMatchObject([{required_role_ids: [levelRole.id]}]);
		await setEnabled(owner, true);
		await selfRole('put', member, mageRole.id, HTTP_STATUS.FORBIDDEN);

		await giveRole(member, levelRole);
		await selfRole('put', member, mageRole.id);
		await updateMember(harness, owner.token, guild.id, member.userId, {roles: [mageRole.id]});
		await selfRole('delete', member, mageRole.id, HTTP_STATUS.FORBIDDEN);
		expect(await memberRoles(member)).toEqual([mageRole.id]);
		await updateMember(harness, owner.token, guild.id, member.userId, {roles: [mageRole.id, levelRole.id]});
		await selfRole('delete', member, mageRole.id);
		expect(await memberRoles(member)).toEqual([levelRole.id]);
	});

	it('keeps a section locked when its only required role is deleted', async () => {
		const levelRole = await newRole('Level 5');
		const [saved] = await putSections(owner, [{roles: sectionRoles(mageRole.id), required_role_ids: [levelRole.id]}]);
		await setEnabled(owner, true);
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/roles/${levelRole.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(await savedSections()).toMatchObject([{required_role_ids: [levelRole.id]}]);
		await selfRole('put', member, mageRole.id, HTTP_STATUS.FORBIDDEN);

		await putSections(owner, [{id: saved!.id, roles: sectionRoles(mageRole.id), required_role_ids: [levelRole.id]}]);
		await putSections(
			owner,
			[{roles: sectionRoles(mageRole.id), required_role_ids: [levelRole.id]}],
			HTTP_STATUS.BAD_REQUEST,
			'INVALID_FORM_BODY',
		);
	});

	it('makes members meet the verification level before picking a role', async () => {
		await offerMageRole();
		await updateGuild(harness, owner.token, guild.id, {verification_level: GuildVerificationLevel.MEDIUM});
		await selfRole('put', member, mageRole.id, HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCOUNT_TOO_NEW_FOR_GUILD);
		await giveRole(member, warriorRole);
		await selfRole('put', member, mageRole.id);
		expect((await memberRoles(member)).sort()).toEqual([mageRole.id, warriorRole.id].sort());
	});

	it('still lets people with Manage Roles use @me for other roles', async () => {
		await selfRole('put', manager, warriorRole.id);
		expect(await memberRoles(manager)).toContain(warriorRole.id);
	});

	it('rejects people who are not members', async () => {
		await offerMageRole();
		await selfRole('put', await createTestAccount(harness), mageRole.id, HTTP_STATUS.NOT_FOUND);
	});

	it('keeps temporary members temporary until a moderator gives them the role', async () => {
		const repository = new GuildRepository();
		const guildId = createGuildID(BigInt(guild.id));
		const memberId = createUserID(BigInt(member.userId));
		const row = (await repository.getMember(guildId, memberId))!.toRow();
		await repository.upsertMember({...row, temporary: true}, row);
		await offerMageRole();
		await selfRole('put', member, mageRole.id);
		const updated = await repository.getMember(guildId, memberId);
		expect(updated?.roleIds.has(createRoleID(BigInt(mageRole.id)))).toBe(true);
		expect(updated?.isTemporary).toBe(true);
		await addMemberRole(harness, owner.token, guild.id, member.userId, mageRole.id);
		expect((await repository.getMember(guildId, memberId))?.isTemporary).toBe(false);
	});

	it('requires Manage Roles to flip the switch', async () => {
		await giveRole(member, await newRole('Community', Permissions.MANAGE_GUILD));
		await setEnabled(member, true, HTTP_STATUS.FORBIDDEN);
		await updateGuild(harness, member.token, guild.id, {name: 'Still allowed'});
		await setEnabled(manager, true);
		expect((await getGuild(harness, owner.token, guild.id)).features).toContain(FEATURE);
	});

	it('removes deleted roles from sections', async () => {
		await putSections(owner, [
			{content: 'Classes', roles: [{id: warriorRole.id, description: 'Hits things'}]},
			{content: 'Only mages', roles: [{id: mageRole.id, description: 'Casts spells'}]},
		]);
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/roles/${mageRole.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(await savedSections()).toMatchObject([
			{content: 'Classes', roles: [{id: warriorRole.id, description: 'Hits things'}]},
			{content: 'Only mages', roles: []},
		]);
	});

	it('records audit log entries for section saves and self-assignment', async () => {
		await offerMageRole();
		await selfRole('put', member, mageRole.id);
		const sectionSave = (await auditEntries(AuditLogActionType.GUILD_UPDATE)).find((entry) =>
			entry.changes?.some((change) => change.key === 'self_assignable_role_ids'),
		);
		expect(sectionSave?.user_id).toBe(owner.userId);
		expect(sectionSave?.changes).toContainEqual(
			expect.objectContaining({key: 'self_assignable_role_ids', new_value: [mageRole.id]}),
		);
		await selfRole('delete', member, mageRole.id);
		await selfRole('put', manager, warriorRole.id);
		const ownRoleOptions = async (account: TestAccount) =>
			(await auditEntries(AuditLogActionType.MEMBER_ROLE_UPDATE))
				.filter((entry) => entry.user_id === account.userId && entry.target_id === account.userId)
				.map((entry) => entry.options);
		expect(await ownRoleOptions(member)).toEqual([
			{role_name: mageRole.name, self_assignable: true},
			{role_name: mageRole.name, self_assignable: true},
		]);
		expect(await ownRoleOptions(manager)).toEqual([{role_name: warriorRole.name}]);

		await putSections(owner, [{roles: [{id: mageRole.id, description: 'Casts spells'}]}]);
		const changes = (await auditEntries(AuditLogActionType.GUILD_UPDATE)).flatMap((entry) => entry.changes ?? []);
		expect(changes).toContainEqual(
			expect.objectContaining({key: 'self_assignable_role_descriptions', new_value: ['Casts spells']}),
		);
	});
});
