// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {createPermissionOverwrite} from '@app/api/channel/tests/ChannelTestUtils';
import {createTestGuildStack} from '@app/api/channel/tests/CrosspostTestUtils';
import {
	acceptInvite,
	createChannel,
	createChannelInvite,
	createGuild,
	createRole,
	getMember,
	leaveGuild,
} from '@app/api/guild/tests/GuildTestUtils';
import {getMessages} from '@app/api/message/tests/MessageTestUtils';
import {getSnowflakeService, getWorkerService, setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {
	getCacheService,
	getChannelRepository,
	getGuildRepository,
	getOperatorPassWallService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {OperatorPassHoldingService} from '@app/api/operator_pass/OperatorPassHoldingService';
import {
	type OperatorPassLabsGreetingDeps,
	retractOperatorPassLabsGreeting,
	runOperatorPassLabsGreeting,
} from '@app/api/operator_pass/OperatorPassLabsGreeting';
import {
	type LabsDeliveryJob,
	OPERATOR_PASS_LABS_DELIVERY_TASK,
	OPERATOR_PASS_LABS_GREETING_TASK,
	OPERATOR_PASS_LABS_VISIONARY_ROLE_SYNC_TASK,
	type OperatorPassLabsRoleDeps,
	OperatorPassLabsRoleService,
} from '@app/api/operator_pass/OperatorPassLabsRole';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {type AccountStateDeps, applyHideProfile} from '@app/api/user/services/AccountStateApplier';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	type ProfilePrivacyLevel,
	ProfilePrivacyLevels,
	UserFlags,
	UserNotificationSettings,
} from '@fluxer/constants/src/UserConstants';
import {TestEmailService} from '@pkgs/email/src/TestEmailService';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import {afterAll, afterEach, beforeAll, expect, test, vi} from 'vitest';

const DELIVERED_AT = new Date('2026-11-01T16:05:00.000Z');

let harness: ApiTestHarness;
const originalOperatorPass = {...Config.operatorPass};
const originalSelfHosted = Config.instance.selfHosted;

beforeAll(async () => {
	harness = await createApiTestHarness();
});

afterEach(() => {
	Object.assign(Config.operatorPass, originalOperatorPass);
	Config.instance.selfHosted = originalSelfHosted;
});

afterAll(async () => {
	await harness.shutdown();
});

const OPERATORS_ACCESS = Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES;

async function setUpLabs() {
	const owner = await createTestAccount(harness);
	const labs = await createGuild(harness, owner.token, 'Labs');
	const operatorRole = await createRole(harness, owner.token, labs.id, {name: 'Operator'});
	const visionaryRole = await createRole(harness, owner.token, labs.id, {name: 'Visionary'});
	const operators = await createChannel(harness, owner.token, labs.id, 'operators');
	await createPermissionOverwrite(harness, owner.token, operators.id, labs.id, {
		type: 0,
		allow: '0',
		deny: Permissions.VIEW_CHANNEL.toString(),
	});
	for (const role of [operatorRole, visionaryRole]) {
		await createPermissionOverwrite(harness, owner.token, operators.id, role.id, {
			type: 0,
			allow: OPERATORS_ACCESS.toString(),
			deny: '0',
		});
	}
	Config.instance.selfHosted = false;
	Config.operatorPass.labsGuildId = BigInt(labs.id);
	Config.operatorPass.labsOperatorRoleId = BigInt(operatorRole.id);
	Config.operatorPass.labsVisionaryRoleId = BigInt(visionaryRole.id);
	Config.operatorPass.labsOperatorsChannelId = BigInt(operators.id);
	const stack = createTestGuildStack();
	const repository = new OperatorPassRepository();
	const roleDeps: OperatorPassLabsRoleDeps = {
		guildRepository: getGuildRepository(),
		guildService: stack.guildService,
		userRepository: getUserRepository(),
		cacheService: getCacheService(),
		channelRepository: getChannelRepository(),
		channelOverwrites: stack.channelService.channelData.operations,
	};
	const roles = new OperatorPassLabsRoleService(roleDeps);
	const greetingDeps: OperatorPassLabsGreetingDeps = {
		repository,
		userRepository: getUserRepository(),
		guildRepository: getGuildRepository(),
		channelRepository: getChannelRepository(),
		messages: stack.channelService.messages.send,
		deletion: stack.channelService.messages.deletion,
		snowflakeService: getSnowflakeService(),
		cacheService: getCacheService(),
	};
	return {owner, labs, operatorRole, visionaryRole, operators, repository, roles, roleDeps, greetingDeps, stack};
}

async function memberOverwrite(channelId: string, userId: UserID) {
	const channel = await getChannelRepository().channelData.findUnique(createChannelID(BigInt(channelId)));
	return channel?.permissionOverwrites.get(userId) ?? null;
}

async function withRecordingWorker<T>(run: (worker: RecordingWorkerService) => Promise<T>): Promise<T> {
	const previousWorker = getWorkerService();
	const worker = new RecordingWorkerService();
	setInjectedWorkerService(worker);
	try {
		return await run(worker);
	} finally {
		setInjectedWorkerService(previousWorker);
	}
}

async function createOperator(repository: OperatorPassRepository): Promise<{account: TestAccount; userId: UserID}> {
	const account = await createTestAccount(harness);
	const userId = createUserID(BigInt(account.userId));
	await repository.createUnit({
		pass_id: `labs-test:${userId}:1`,
		purchase_id: null,
		buyer_id: userId,
		holder_id: userId,
		source: 'purchase',
		status: 'held',
		transferable: false,
		delivered_at: DELIVERED_AT,
		first_delivered_at: DELIVERED_AT,
		paused_at: null,
		ended_at: null,
		end_reason: null,
		withdrawal_country: null,
		withdrawal_days: null,
		version: 1,
	});
	await repository.saveOperatorPass({
		user_id: userId,
		granted_at: DELIVERED_AT,
		source: 'purchase',
		pass_count: 1,
		checkout_session_id: null,
		payment_intent_id: null,
		amount_minor: null,
		currency: null,
		paused_at: null,
		terms_version: null,
		terms_accepted_at: null,
		waiver_consent_at: null,
		waiver_consent_version: null,
		waiver_consent_ip: null,
		waiver_consent_user_agent: null,
		version: 1,
	});
	return {account, userId};
}

async function deliver(roles: OperatorPassLabsRoleService, userId: UserID, greeting: boolean | null) {
	await roles.deliver(await getUserRepository().findUniqueAssert(userId), greeting);
}

test('delivery adds Operators to Labs, never adds back someone who left, and one message welcomes the window once', async () => {
	const {owner, labs, operatorRole, operators, repository, roles, greetingDeps} = await setUpLabs();
	const first = await createOperator(repository);
	const second = await createOperator(repository);
	const leaver = await createOperator(repository);
	const quiet = await createOperator(repository);
	const invite = await createChannelInvite(harness, owner.token, labs.system_channel_id!);
	await acceptInvite(harness, leaver.account.token, invite.code);
	await leaveGuild(harness, leaver.account.token, labs.id);

	await deliver(roles, first.userId, null);
	await deliver(roles, second.userId, true);
	await deliver(roles, leaver.userId, true);
	await deliver(roles, quiet.userId, false);

	for (const operator of [first, second, quiet]) {
		const member = await getMember(harness, owner.token, labs.id, operator.account.userId);
		expect(member.roles).toContain(operatorRole.id);
		const settings = await getUserRepository().findGuildSettings(operator.userId, createGuildID(BigInt(labs.id)));
		expect(settings?.messageNotifications).toBe(UserNotificationSettings.ONLY_MENTIONS);
	}
	expect(await getGuildRepository().getMember(createGuildID(BigInt(labs.id)), leaver.userId)).toBeNull();
	expect((await repository.findLabsMember(leaver.userId))?.left_reason).toBe('left');

	expect(await runOperatorPassLabsGreeting(greetingDeps)).toEqual({greeted: 2, messages: 1, dropped: 0});
	await deliver(roles, first.userId, null);
	expect(await runOperatorPassLabsGreeting(greetingDeps)).toEqual({greeted: 0, messages: 0, dropped: 0});

	const messages = await getMessages(harness, owner.token, operators.id);
	expect(messages).toHaveLength(1);
	expect(messages[0]?.content).toContain(`<@${first.userId}>`);
	expect(messages[0]?.content).toContain(`<@${second.userId}>`);
	expect(messages[0]?.content).not.toContain(`<@${quiet.userId}>`);
	expect(messages[0]?.mentions?.map((user) => user.id).sort()).toEqual(
		[first.account.userId, second.account.userId].map(String).sort(),
	);
});

test('a run that sent the welcome but stopped before recording it does not send it again', async () => {
	const {owner, operators, repository, roles, greetingDeps} = await setUpLabs();
	const operator = await createOperator(repository);
	await deliver(roles, operator.userId, true);
	const failing = Object.create(repository) as OperatorPassRepository;
	failing.recordLabsGreeted = async () => {
		throw new Error('lost the connection');
	};

	await expect(runOperatorPassLabsGreeting({...greetingDeps, repository: failing})).rejects.toThrow(
		'lost the connection',
	);
	expect(await runOperatorPassLabsGreeting(greetingDeps)).toEqual({greeted: 1, messages: 0, dropped: 0});

	expect(await getMessages(harness, owner.token, operators.id)).toHaveLength(1);
	expect((await repository.findLabsMember(operator.userId))?.greeted_at).not.toBeNull();
});

test('turning the welcome off before the window ends means no welcome', async () => {
	const {owner, operators, repository, roles, greetingDeps} = await setUpLabs();
	const operator = await createOperator(repository);
	await deliver(roles, operator.userId, true);
	await repository.setLabsGreeting(operator.userId, false);

	expect(await runOperatorPassLabsGreeting(greetingDeps)).toEqual({greeted: 0, messages: 0, dropped: 1});
	expect(await getMessages(harness, owner.token, operators.id)).toHaveLength(0);
});

test('turning the welcome on in settings after a delivery with it off still welcomes the person once', async () => {
	const {owner, operators, repository, roles, greetingDeps} = await setUpLabs();
	const operator = await createOperator(repository);
	await deliver(roles, operator.userId, false);
	expect(await runOperatorPassLabsGreeting(greetingDeps)).toEqual({greeted: 0, messages: 0, dropped: 0});

	await repository.setLabsGreeting(operator.userId, true);
	expect(await roles.queueGreetingIfDue(await getUserRepository().findUniqueAssert(operator.userId))).toBe(true);

	expect(await runOperatorPassLabsGreeting(greetingDeps)).toEqual({greeted: 1, messages: 1, dropped: 0});
	expect(await roles.queueGreetingIfDue(await getUserRepository().findUniqueAssert(operator.userId))).toBe(false);
	expect(await getMessages(harness, owner.token, operators.id)).toHaveLength(1);
});

test('joining from checkout adds an Operator once, keeps members as they are and never adds back someone who left or was banned', async () => {
	const {owner, labs, operatorRole, repository, roles} = await setUpLabs();
	const joiner = await createOperator(repository);
	const leaver = await createOperator(repository);
	const banned = await createOperator(repository);
	const noPass = await createTestAccount(harness);
	const invite = await createChannelInvite(harness, owner.token, labs.system_channel_id!);
	await acceptInvite(harness, leaver.account.token, invite.code);
	await leaveGuild(harness, leaver.account.token, labs.id);
	await acceptInvite(harness, banned.account.token, invite.code);
	const join = async (userId: UserID) => roles.joinFromCheckout(await getUserRepository().findUniqueAssert(userId));

	expect(await join(joiner.userId)).toBe('joined');
	expect(await join(joiner.userId)).toBe('joined');
	expect((await getMember(harness, owner.token, labs.id, joiner.account.userId)).roles).toContain(operatorRole.id);
	expect(await join(leaver.userId)).toBe('left');
	await createBuilder(harness, owner.token)
		.put(`/guilds/${labs.id}/bans/${banned.account.userId}`)
		.body({})
		.expect(HTTP_STATUS.NO_CONTENT)
		.execute();
	expect(await join(banned.userId)).toBe('banned');
	expect(await join(createUserID(BigInt(noPass.userId)))).toBe('not_held');
	expect(await getGuildRepository().getMember(createGuildID(BigInt(labs.id)), leaver.userId)).toBeNull();
});

function retryJob(userId: UserID, payload: WorkerJobPayload): LabsDeliveryJob {
	return {
		userId,
		greeting: payload.greeting as boolean | null,
		origin: String(payload.origin),
		retry: Number(payload.retry),
	};
}

test('a failed add to Labs is queued again with backoff and the retry adds the person with the checkout welcome choice', async () => {
	const {owner, labs, operatorRole, repository, roles, roleDeps, greetingDeps, stack} = await setUpLabs();
	const operator = await createOperator(repository);
	const members = Object.create(stack.guildService.members) as typeof stack.guildService.members;
	members.addUserToGuild = async () => {
		throw new Error('gateway timed out');
	};
	const failing = new OperatorPassLabsRoleService({
		...roleDeps,
		guildService: {members} as typeof stack.guildService,
	});
	const previousWorker = getWorkerService();
	const worker = new RecordingWorkerService();
	setInjectedWorkerService(worker);
	try {
		const before = Date.now();
		await failing.deliver(await getUserRepository().findUniqueAssert(operator.userId), false);
		expect(await getGuildRepository().getMember(createGuildID(BigInt(labs.id)), operator.userId)).toBeNull();
		const [queued] = worker.byTask(OPERATOR_PASS_LABS_DELIVERY_TASK);
		expect(queued?.payload).toMatchObject({user_id: operator.userId.toString(), greeting: false, retry: 1});
		expect(queued?.options?.runAt?.getTime()).toBeGreaterThanOrEqual(before + 60_000);

		await failing.retryDelivery(retryJob(operator.userId, queued!.payload), getUserRepository());
		const [, second] = worker.byTask(OPERATOR_PASS_LABS_DELIVERY_TASK);
		expect(second?.payload.retry).toBe(2);
		expect(second?.options?.jobKey).not.toBe(queued?.options?.jobKey);

		await roles.retryDelivery(retryJob(operator.userId, second!.payload), getUserRepository());
		const member = await getMember(harness, owner.token, labs.id, operator.account.userId);
		expect(member.roles).toContain(operatorRole.id);
		expect(worker.byTask(OPERATOR_PASS_LABS_DELIVERY_TASK)).toHaveLength(2);
		expect(await runOperatorPassLabsGreeting(greetingDeps)).toEqual({greeted: 0, messages: 0, dropped: 0});
	} finally {
		setInjectedWorkerService(previousWorker);
	}
});

test('a run welcomes everyone queued in that window, fifty people per message', async () => {
	const {owner, operators, repository, roles, greetingDeps} = await setUpLabs();
	const operatorIds: Array<UserID> = [];
	for (let index = 0; index < 101; index++) {
		const operator = await createOperator(repository);
		await deliver(roles, operator.userId, true);
		operatorIds.push(operator.userId);
	}

	expect(await runOperatorPassLabsGreeting(greetingDeps)).toEqual({greeted: 101, messages: 3, dropped: 0});

	const messages = await getMessages(harness, owner.token, operators.id);
	expect(messages).toHaveLength(3);
	expect(messages.map((message) => message.mentions?.length).sort()).toEqual([1, 50, 50]);
	expect(messages.flatMap((message) => message.mentions?.map((user) => user.id) ?? []).sort()).toEqual(
		operatorIds.map(String).sort(),
	);
	expect(await repository.listLabsGreetings()).toHaveLength(0);
});

test('turning #operators off keeps the Operator role away through every sync, and turning it on gives it back', async () => {
	const {owner, labs, operatorRole, repository, roles} = await setUpLabs();
	const operator = await createOperator(repository);
	const holding = new OperatorPassHoldingService({
		repository,
		userRepository: getUserRepository(),
		roleService: roles,
		gatewayService: {dispatchPresence: vi.fn()} as unknown as ConstructorParameters<
			typeof OperatorPassHoldingService
		>[0]['gatewayService'],
		emailService: new TestEmailService(),
		wallService: getOperatorPassWallService(),
		findPurchase: (purchaseId) => repository.findPurchase(purchaseId),
	});
	const roleIds = async () => (await getMember(harness, owner.token, labs.id, operator.account.userId)).roles;
	await deliver(roles, operator.userId, null);
	expect(await roleIds()).toContain(operatorRole.id);

	const hidden = await holding.updateLabsOperatorsChannel(
		await getUserRepository().findUniqueAssert(operator.userId),
		false,
	);
	expect(await roleIds()).not.toContain(operatorRole.id);
	expect((await holding.queries.describePasses(hidden)).labs.show_operators).toBe(false);

	await roles.syncLabsRolesForMembers(100);
	await deliver(roles, operator.userId, null);
	expect(await roles.joinFromCheckout(await getUserRepository().findUniqueAssert(operator.userId))).toBe('joined');
	expect(await roleIds()).not.toContain(operatorRole.id);

	const shown = await holding.updateLabsOperatorsChannel(hidden, true);
	expect(await roleIds()).toContain(operatorRole.id);
	expect((await holding.queries.describePasses(shown)).labs.show_operators).toBe(true);
});

async function setProfilePrivacy(account: TestAccount, level: ProfilePrivacyLevel) {
	await createBuilder(harness, account.token)
		.patch('/users/@me/settings')
		.body({profile_privacy: level})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function setHiddenBadges(account: TestAccount, hidden: Array<string>) {
	await createBuilder(harness, account.token)
		.patch('/users/@me')
		.body({hidden_profile_badges: hidden})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function writeProfilePrivacyWithoutSync(userId: UserID, level: ProfilePrivacyLevel) {
	const settings = await getUserRepository().findSettings(userId);
	await getUserRepository().upsertSettings({...settings!.toRow(), profile_privacy: level}, settings!.toRow());
}

async function canOpen(account: TestAccount, channelId: string): Promise<boolean> {
	const response = await createBuilder(harness, account.token).get(`/channels/${channelId}`).executeRaw();
	return response.response.status === HTTP_STATUS.OK;
}

test('limiting the profile or hiding the badge swaps the Operator role for a member overwrite on #operators, and undoing it swaps back', async () => {
	const {owner, labs, operatorRole, operators, repository, roles} = await setUpLabs();
	const operator = await createOperator(repository);
	const roleIds = async () => (await getMember(harness, owner.token, labs.id, operator.account.userId)).roles;
	await deliver(roles, operator.userId, null);
	expect(await roleIds()).toEqual([operatorRole.id]);
	expect(await memberOverwrite(operators.id, operator.userId)).toBeNull();
	expect(await canOpen(operator.account, operators.id)).toBe(true);

	await setProfilePrivacy(operator.account, ProfilePrivacyLevels.FRIENDS_ONLY);
	expect(await roleIds()).toEqual([]);
	expect(await memberOverwrite(operators.id, operator.userId)).toMatchObject({
		type: 1,
		allow: OPERATORS_ACCESS,
		deny: 0n,
	});
	expect(await canOpen(operator.account, operators.id)).toBe(true);

	await setProfilePrivacy(operator.account, ProfilePrivacyLevels.ALL_GUILDS);
	expect(await roleIds()).toEqual([operatorRole.id]);
	expect(await memberOverwrite(operators.id, operator.userId)).toBeNull();

	await setHiddenBadges(operator.account, ['operator_pass']);
	expect(await roleIds()).toEqual([]);
	expect(await memberOverwrite(operators.id, operator.userId)).not.toBeNull();

	await setHiddenBadges(operator.account, []);
	await setProfilePrivacy(operator.account, ProfilePrivacyLevels.SMALL_GUILDS_ONLY);
	await setProfilePrivacy(operator.account, ProfilePrivacyLevels.ALL_GUILDS);
	await setProfilePrivacy(operator.account, ProfilePrivacyLevels.FRIENDS_ONLY);
	expect(await roleIds()).toEqual([]);
	expect(await memberOverwrite(operators.id, operator.userId)).not.toBeNull();
	expect(await canOpen(operator.account, operators.id)).toBe(true);
});

test('a hidden holder has no role that other members can see and no audit log entry says who hid', async () => {
	const {owner, labs, operators, repository, roles} = await setUpLabs();
	const operator = await createOperator(repository);
	await deliver(roles, operator.userId, null);
	await setHiddenBadges(operator.account, ['operator_pass']);
	await setHiddenBadges(operator.account, []);
	await setProfilePrivacy(operator.account, ProfilePrivacyLevels.FRIENDS_ONLY);

	const outsider = await createTestAccount(harness);
	const invite = await createChannelInvite(harness, owner.token, labs.system_channel_id!);
	await acceptInvite(harness, outsider.token, invite.code);
	const seen = await getMember(harness, outsider.token, labs.id, operator.account.userId);
	expect(seen.roles).toEqual([]);
	expect(await canOpen(outsider, operators.id)).toBe(false);

	const audit = async (actionType: AuditLogActionType) =>
		createBuilder<{audit_log_entries: Array<{target_id: string | null}>}>(harness, owner.token)
			.get(`/guilds/${labs.id}/audit-logs?action_type=${actionType}`)
			.expect(HTTP_STATUS.OK)
			.execute();
	for (const actionType of [
		AuditLogActionType.MEMBER_ROLE_UPDATE,
		AuditLogActionType.CHANNEL_OVERWRITE_CREATE,
		AuditLogActionType.CHANNEL_OVERWRITE_UPDATE,
		AuditLogActionType.CHANNEL_OVERWRITE_DELETE,
	]) {
		const entries = (await audit(actionType)).audit_log_entries;
		expect(entries.filter((entry) => entry.target_id === operator.account.userId)).toEqual([]);
	}
});

test('hiding a profile removes the Operator role and showing it again gives the role back', async () => {
	const {owner, labs, operatorRole, operators, repository, roles} = await setUpLabs();
	const operator = await createOperator(repository);
	const roleIds = async () => (await getMember(harness, owner.token, labs.id, operator.account.userId)).roles;
	await deliver(roles, operator.userId, null);
	const deps = {
		users: getUserRepository(),
		dispatch: {userUpdated: async () => {}, profileChanged: async () => {}},
		labsRoles: roles,
	} as unknown as AccountStateDeps;
	const hide = (on: boolean) =>
		applyHideProfile(deps, {id: `hide-${on}`, type: 'hide_profile', user_id: operator.userId.toString(), on} as never);

	await hide(true);
	expect(await roleIds()).toEqual([]);
	expect(await memberOverwrite(operators.id, operator.userId)).not.toBeNull();

	await hide(false);
	expect(await roleIds()).toEqual([operatorRole.id]);
	expect(await memberOverwrite(operators.id, operator.userId)).toBeNull();
});

test('a failed update is queued again with backoff and the retry finishes it', async () => {
	const {owner, labs, operatorRole, operators, repository, roles, roleDeps, stack} = await setUpLabs();
	const operator = await createOperator(repository);
	await deliver(roles, operator.userId, null);
	const members = Object.create(stack.guildService.members) as typeof stack.guildService.members;
	members.systemSyncManagedRoles = async () => {
		throw new Error('gateway timed out');
	};
	const failing = new OperatorPassLabsRoleService({...roleDeps, guildService: {members} as typeof stack.guildService});
	await writeProfilePrivacyWithoutSync(operator.userId, ProfilePrivacyLevels.FRIENDS_ONLY);

	await withRecordingWorker(async (worker) => {
		const before = Date.now();
		await failing.resyncLabsRoles(operator.userId);
		expect((await getMember(harness, owner.token, labs.id, operator.account.userId)).roles).toEqual([operatorRole.id]);
		expect(await memberOverwrite(operators.id, operator.userId)).not.toBeNull();
		const [queued] = worker.byTask(OPERATOR_PASS_LABS_VISIONARY_ROLE_SYNC_TASK);
		expect(queued?.payload).toMatchObject({user_id: operator.userId.toString(), retry: 0});
		expect(queued?.options?.runAt?.getTime()).toBeGreaterThanOrEqual(before + 60_000);

		await failing.retryLabsRoleSync(operator.userId, 1);
		const [, second] = worker.byTask(OPERATOR_PASS_LABS_VISIONARY_ROLE_SYNC_TASK);
		expect(second?.payload.retry).toBe(1);

		await roles.retryLabsRoleSync(operator.userId, 2);
		expect((await getMember(harness, owner.token, labs.id, operator.account.userId)).roles).toEqual([]);
		expect(worker.byTask(OPERATOR_PASS_LABS_VISIONARY_ROLE_SYNC_TASK)).toHaveLength(2);
	});
});

test('an update that finds another one running is queued, and the queued run applies the newest setting', async () => {
	const {owner, labs, operatorRole, repository, roles} = await setUpLabs();
	const operator = await createOperator(repository);
	await deliver(roles, operator.userId, null);
	const lockKey = `operator_pass:labs_roles_lock:${operator.userId}`;
	const token = await getCacheService().acquireLock(lockKey, 60);
	expect(token).not.toBeNull();

	await withRecordingWorker(async (worker) => {
		await setProfilePrivacy(operator.account, ProfilePrivacyLevels.FRIENDS_ONLY);
		expect((await getMember(harness, owner.token, labs.id, operator.account.userId)).roles).toEqual([operatorRole.id]);
		expect(worker.byTask(OPERATOR_PASS_LABS_VISIONARY_ROLE_SYNC_TASK)).toHaveLength(1);
		await getCacheService().releaseLock(lockKey, token!);

		await roles.retryLabsRoleSync(operator.userId, 1);
		expect((await getMember(harness, owner.token, labs.id, operator.account.userId)).roles).toEqual([]);
	});
});

test('the daily sync moves people who already limited their profile onto a member overwrite, granting before it removes', async () => {
	const {owner, labs, operatorRole, operators, repository, roles, roleDeps} = await setUpLabs();
	const operator = await createOperator(repository);
	const shown = await createOperator(repository);
	for (const {userId} of [operator, shown]) {
		await roles.joinFromCheckout(await getUserRepository().findUniqueAssert(userId));
	}
	await writeProfilePrivacyWithoutSync(operator.userId, ProfilePrivacyLevels.FRIENDS_ONLY);
	const roleIds = async (userId: UserID) => (await getMember(harness, owner.token, labs.id, userId.toString())).roles;
	expect(await roleIds(operator.userId)).toEqual([operatorRole.id]);

	const failing = new OperatorPassLabsRoleService({
		...roleDeps,
		channelOverwrites: {
			systemSetMemberOverwrite: async () => {
				throw new Error('gateway timed out');
			},
		},
	});
	expect(await failing.syncLabsRolesForMembers(100)).toMatchObject({failed: 1});
	expect(await roleIds(operator.userId)).toEqual([operatorRole.id]);

	expect(await roles.syncLabsRolesForMembers(100)).toEqual({granted: 1, removed: 1, failed: 0});
	expect(await roleIds(operator.userId)).toEqual([]);
	expect(await memberOverwrite(operators.id, operator.userId)).not.toBeNull();
	expect(await roleIds(shown.userId)).toEqual([operatorRole.id]);
	expect(await roles.syncLabsRolesForMembers(100)).toEqual({granted: 0, removed: 0, failed: 0});
});

test('leaving Labs or losing the pass takes the member overwrite away', async () => {
	const {labs, operators, repository, roles} = await setUpLabs();
	const leaver = await createOperator(repository);
	await deliver(roles, leaver.userId, null);
	await setProfilePrivacy(leaver.account, ProfilePrivacyLevels.FRIENDS_ONLY);
	expect(await memberOverwrite(operators.id, leaver.userId)).not.toBeNull();

	await withRecordingWorker(async (worker) => {
		await leaveGuild(harness, leaver.account.token, labs.id);
		expect(worker.byTask(OPERATOR_PASS_LABS_VISIONARY_ROLE_SYNC_TASK)).toHaveLength(1);
	});
	expect(await roles.syncLabsRolesForMembers(100)).toMatchObject({removed: 1, failed: 0});
	expect(await memberOverwrite(operators.id, leaver.userId)).toBeNull();
});

test('joining Labs with a limited profile gives no role and opens #operators through a member overwrite', async () => {
	const {owner, labs, operators, repository, roles} = await setUpLabs();
	const operator = await createOperator(repository);
	await setProfilePrivacy(operator.account, ProfilePrivacyLevels.SMALL_GUILDS_ONLY);
	const invite = await createChannelInvite(harness, owner.token, labs.system_channel_id!);
	await withRecordingWorker(async (worker) => {
		await acceptInvite(harness, operator.account.token, invite.code);
		expect(worker.byTask(OPERATOR_PASS_LABS_VISIONARY_ROLE_SYNC_TASK)).toHaveLength(1);
	});
	await roles.retryLabsRoleSync(operator.userId, 0);
	expect((await getMember(harness, owner.token, labs.id, operator.account.userId)).roles).toEqual([]);
	expect(await canOpen(operator.account, operators.id)).toBe(true);
});

test('nobody is welcomed by name while their profile is limited or hidden', async () => {
	const {owner, operators, repository, roles, greetingDeps} = await setUpLabs();
	const limited = await createOperator(repository);
	const hidden = await createOperator(repository);
	await setProfilePrivacy(limited.account, ProfilePrivacyLevels.FRIENDS_ONLY);
	await deliver(roles, limited.userId, true);
	await deliver(roles, hidden.userId, true);
	const user = await getUserRepository().findUniqueAssert(hidden.userId);
	await getUserRepository().patchUpsert(hidden.userId, {flags: user.flags | UserFlags.PROFILE_HIDDEN}, user.toRow());

	expect(await runOperatorPassLabsGreeting(greetingDeps)).toEqual({greeted: 0, messages: 0, dropped: 1});
	expect(await getMessages(harness, owner.token, operators.id)).toHaveLength(0);
});

test('a welcome claimed by a run that stopped is not sent once the person hides their badge', async () => {
	const {owner, operators, repository, roles, greetingDeps} = await setUpLabs();
	const hider = await createOperator(repository);
	const other = await createOperator(repository);
	await deliver(roles, hider.userId, true);
	await deliver(roles, other.userId, true);
	const stopping = {
		...greetingDeps,
		messages: {
			...greetingDeps.messages,
			sendSystemGuildMessage: async () => {
				throw new Error('lost the connection');
			},
		},
	} as OperatorPassLabsGreetingDeps;
	await expect(runOperatorPassLabsGreeting(stopping)).rejects.toThrow('lost the connection');
	await writeProfilePrivacyWithoutSync(hider.userId, ProfilePrivacyLevels.FRIENDS_ONLY);

	expect(await runOperatorPassLabsGreeting(greetingDeps)).toEqual({greeted: 1, messages: 1, dropped: 1});
	const messages = await getMessages(harness, owner.token, operators.id);
	expect(messages).toHaveLength(1);
	expect(messages[0]?.content).not.toContain(`<@${hider.userId}>`);
	expect(messages[0]?.mentions?.map((user) => user.id)).toEqual([other.account.userId]);
});

test('hiding the badge after the welcome takes the name out of it, and deletes a welcome that only had that name', async () => {
	const {owner, operators, repository, roles, greetingDeps} = await setUpLabs();
	const first = await createOperator(repository);
	const second = await createOperator(repository);
	await deliver(roles, first.userId, true);
	await deliver(roles, second.userId, true);
	expect(await runOperatorPassLabsGreeting(greetingDeps)).toMatchObject({greeted: 2, messages: 1});

	await withRecordingWorker(async (worker) => {
		await setHiddenBadges(first.account, ['operator_pass']);
		const queued = worker.byTask(OPERATOR_PASS_LABS_GREETING_TASK);
		expect(queued.map((job) => job.payload)).toEqual([{retract_user_id: first.userId.toString()}]);
	});
	expect(await retractOperatorPassLabsGreeting(greetingDeps, first.userId)).toBe(true);
	let messages = await getMessages(harness, owner.token, operators.id);
	expect(messages).toHaveLength(1);
	expect(messages[0]?.content).not.toContain(`<@${first.userId}>`);
	expect(messages[0]?.content).toContain(`<@${second.userId}>`);
	expect(messages[0]?.mentions?.map((user) => user.id)).toEqual([second.account.userId]);
	expect(await retractOperatorPassLabsGreeting(greetingDeps, first.userId)).toBe(false);

	await setProfilePrivacy(second.account, ProfilePrivacyLevels.FRIENDS_ONLY);
	expect(await retractOperatorPassLabsGreeting(greetingDeps, second.userId)).toBe(true);
	messages = await getMessages(harness, owner.token, operators.id);
	expect(messages).toHaveLength(0);
	expect((await repository.findLabsMember(second.userId))?.greeted_at).not.toBeNull();
	expect(await roles.queueGreetingIfDue(await getUserRepository().findUniqueAssert(second.userId))).toBe(false);
});

test('a welcome is kept when the person shows the badge again before the name is taken out', async () => {
	const {owner, operators, repository, roles, greetingDeps} = await setUpLabs();
	const operator = await createOperator(repository);
	await deliver(roles, operator.userId, true);
	await runOperatorPassLabsGreeting(greetingDeps);
	await writeProfilePrivacyWithoutSync(operator.userId, ProfilePrivacyLevels.FRIENDS_ONLY);
	await writeProfilePrivacyWithoutSync(operator.userId, ProfilePrivacyLevels.ALL_GUILDS);

	expect(await retractOperatorPassLabsGreeting(greetingDeps, operator.userId)).toBe(false);
	expect(await getMessages(harness, owner.token, operators.id)).toHaveLength(1);
});
