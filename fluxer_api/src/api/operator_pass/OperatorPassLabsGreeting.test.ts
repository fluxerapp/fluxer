// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
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
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {
	type OperatorPassLabsGreetingDeps,
	runOperatorPassLabsGreeting,
} from '@app/api/operator_pass/OperatorPassLabsGreeting';
import {
	type LabsDeliveryJob,
	OPERATOR_PASS_LABS_DELIVERY_TASK,
	OperatorPassLabsRoleService,
} from '@app/api/operator_pass/OperatorPassLabsRole';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserNotificationSettings} from '@fluxer/constants/src/UserConstants';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import {afterAll, afterEach, beforeAll, expect, test} from 'vitest';

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

async function setUpLabs() {
	const owner = await createTestAccount(harness);
	const labs = await createGuild(harness, owner.token, 'Labs');
	const operatorRole = await createRole(harness, owner.token, labs.id, {name: 'Operator'});
	const visionaryRole = await createRole(harness, owner.token, labs.id, {name: 'Visionary'});
	const operators = await createChannel(harness, owner.token, labs.id, 'operators');
	Config.instance.selfHosted = false;
	Config.operatorPass.labsGuildId = BigInt(labs.id);
	Config.operatorPass.labsOperatorRoleId = BigInt(operatorRole.id);
	Config.operatorPass.labsVisionaryRoleId = BigInt(visionaryRole.id);
	Config.operatorPass.labsOperatorsChannelId = BigInt(operators.id);
	const stack = createTestGuildStack();
	const repository = new OperatorPassRepository();
	const roles = new OperatorPassLabsRoleService(getGuildRepository(), stack.guildService);
	const greetingDeps: OperatorPassLabsGreetingDeps = {
		repository,
		userRepository: getUserRepository(),
		guildRepository: getGuildRepository(),
		channelRepository: getChannelRepository(),
		messages: stack.channelService.messages.send,
		snowflakeService: getSnowflakeService(),
		cacheService: getCacheService(),
	};
	return {owner, labs, operatorRole, operators, repository, roles, greetingDeps};
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
	const {owner, labs, operatorRole, repository, roles, greetingDeps} = await setUpLabs();
	const operator = await createOperator(repository);
	const stack = createTestGuildStack();
	const members = Object.create(stack.guildService.members) as typeof stack.guildService.members;
	members.addUserToGuild = async () => {
		throw new Error('gateway timed out');
	};
	const failing = new OperatorPassLabsRoleService(getGuildRepository(), {members} as typeof stack.guildService);
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
