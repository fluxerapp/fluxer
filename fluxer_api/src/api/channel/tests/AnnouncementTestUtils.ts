// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createWebhookID} from '@app/api/BrandedTypes';
import {resetApiServicesForTesting} from '@app/api/CreateApiContext';
import {
	acceptInvite,
	addMemberRole,
	createChannelInvite,
	createGuild,
	createPermissionOverwrite,
	createRole,
} from '@app/api/channel/tests/ChannelTestUtils';
import {createTestChannelService} from '@app/api/channel/tests/CrosspostTestUtils';
import {getGatewayService, getSnowflakeService, setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {
	getAvatarService,
	getCacheService,
	getChannelRepository,
	getGuildRepository,
	getLimitConfigService,
	getPurgeQueue,
	getStorageService,
	getUserRepository,
	getWebhookRepository,
} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {SyncTaskWorkerService} from '@app/api/test/SyncTaskWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {WebhookRepository} from '@app/api/webhook/WebhookRepository';
import crosspostMessage from '@app/api/worker/tasks/CrosspostMessage';
import crosspostMessageChunk from '@app/api/worker/tasks/CrosspostMessageChunk';
import removeChannelFollowers from '@app/api/worker/tasks/RemoveChannelFollowers';
import syncCrosspostCopies from '@app/api/worker/tasks/SyncCrosspostCopies';
import syncCrosspostedMessage from '@app/api/worker/tasks/SyncCrosspostedMessage';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import type {WorkerDependencies} from '@app/api/worker/WorkerDependencies';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import type {FollowedChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelFollowSchemas';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

export interface AnnouncementSourceGuild {
	owner: TestAccount;
	member: TestAccount;
	moderator: TestAccount;
	guild: GuildResponse;
	ann: ChannelResponse;
}

export interface AnnouncementTargetGuild {
	owner: TestAccount;
	webhookManager: TestAccount;
	guild: GuildResponse;
	t1: ChannelResponse;
	t2: ChannelResponse;
	voice: ChannelResponse;
	ageRestricted: ChannelResponse;
	contentWarning: ChannelResponse;
}

export interface AnnouncementWorld {
	a: AnnouncementSourceGuild;
	b: AnnouncementTargetGuild;
}

export async function createGuildChannel(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	body: Record<string, unknown>,
): Promise<ChannelResponse> {
	return createBuilder<ChannelResponse>(harness, token).post(`/guilds/${guildId}/channels`).body(body).execute();
}

export async function addGuildMember(
	harness: ApiTestHarness,
	owner: TestAccount,
	guild: GuildResponse,
	member: TestAccount,
): Promise<void> {
	const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
	await acceptInvite(harness, member.token, invite.code);
}

export async function grantGuildRole(
	harness: ApiTestHarness,
	owner: TestAccount,
	guildId: string,
	member: TestAccount,
	permissions: bigint,
	name = 'Announcement role',
): Promise<string> {
	const role = await createRole(harness, owner.token, guildId, {
		name,
		permissions: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES | permissions).toString(),
	});
	await addMemberRole(harness, owner.token, guildId, member.userId, role.id);
	return role.id;
}

export async function createAnnouncementSourceGuild(
	harness: ApiTestHarness,
	name = 'Announcement Source',
): Promise<AnnouncementSourceGuild> {
	const owner = await createTestAccount(harness);
	const member = await createTestAccount(harness);
	const moderator = await createTestAccount(harness);
	const guild = await createGuild(harness, owner.token, name);
	const ann = await createGuildChannel(harness, owner.token, guild.id, {
		name: 'news',
		type: ChannelTypes.GUILD_ANNOUNCEMENT,
	});
	await addGuildMember(harness, owner, guild, member);
	await addGuildMember(harness, owner, guild, moderator);
	await grantGuildRole(harness, owner, guild.id, moderator, Permissions.MANAGE_MESSAGES, 'Announcement moderator');
	return {owner, member, moderator, guild, ann};
}

export async function createAnnouncementTargetGuild(
	harness: ApiTestHarness,
	name = 'Announcement Target',
): Promise<AnnouncementTargetGuild> {
	const owner = await createTestAccount(harness);
	const webhookManager = await createTestAccount(harness);
	const guild = await createGuild(harness, owner.token, name);
	const t1 = await createGuildChannel(harness, owner.token, guild.id, {name: 't1', type: ChannelTypes.GUILD_TEXT});
	const t2 = await createGuildChannel(harness, owner.token, guild.id, {name: 't2', type: ChannelTypes.GUILD_TEXT});
	const voice = await createGuildChannel(harness, owner.token, guild.id, {
		name: 'voice',
		type: ChannelTypes.GUILD_VOICE,
	});
	const ageRestricted = await createGuildChannel(harness, owner.token, guild.id, {
		name: 'restricted',
		type: ChannelTypes.GUILD_TEXT,
		nsfw_override: true,
	});
	const contentWarning = await createGuildChannel(harness, owner.token, guild.id, {
		name: 'warned',
		type: ChannelTypes.GUILD_TEXT,
		content_warning_level: ContentWarningLevel.CONTENT_WARNING,
	});
	await addGuildMember(harness, owner, guild, webhookManager);
	await createPermissionOverwrite(harness, owner.token, t1.id, webhookManager.userId, {
		type: 1,
		allow: Permissions.MANAGE_WEBHOOKS.toString(),
		deny: '0',
	});
	return {owner, webhookManager, guild, t1, t2, voice, ageRestricted, contentWarning};
}

export async function announcementWorld(harness: ApiTestHarness): Promise<AnnouncementWorld> {
	const a = await createAnnouncementSourceGuild(harness);
	const b = await createAnnouncementTargetGuild(harness);
	await addGuildMember(harness, a.owner, a.guild, b.owner);
	await addGuildMember(harness, a.owner, a.guild, b.webhookManager);
	return {a, b};
}

export function followRequest(
	harness: ApiTestHarness,
	token: string,
	sourceChannelId: string,
	targetChannelId: string,
) {
	return createBuilder<FollowedChannelResponse>(harness, token)
		.post(`/channels/${sourceChannelId}/followers`)
		.body({webhook_channel_id: targetChannelId});
}

export async function follow(
	harness: ApiTestHarness,
	token: string,
	sourceChannelId: string,
	targetChannelId: string,
): Promise<FollowedChannelResponse> {
	return followRequest(harness, token, sourceChannelId, targetChannelId).execute();
}

export function publishRequest(harness: ApiTestHarness, token: string, channelId: string, messageId: string) {
	return createBuilder<MessageResponse>(harness, token).post(`/channels/${channelId}/messages/${messageId}/crosspost`);
}

export async function publish(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
): Promise<MessageResponse> {
	return publishRequest(harness, token, channelId, messageId).execute();
}

export async function findWebhookRow(webhookId: string) {
	return new WebhookRepository().findUnique(createWebhookID(BigInt(webhookId)));
}

export async function setGuildFeatures(
	harness: ApiTestHarness,
	guildId: string,
	features: {add?: Array<string>; remove?: Array<string>},
): Promise<void> {
	await createBuilder(harness, '')
		.post(`/test/guilds/${guildId}/features`)
		.body({add_features: features.add ?? [], remove_features: features.remove ?? []})
		.execute();
}

export async function setLimitOverride(
	harness: ApiTestHarness,
	limits: Partial<Record<LimitKey, number>>,
): Promise<() => Promise<void>> {
	const admin = await setUserACLs(harness, await createTestAccount(harness), [
		AdminACLs.AUTHENTICATE,
		AdminACLs.INSTANCE_LIMIT_CONFIG_VIEW,
		AdminACLs.INSTANCE_LIMIT_CONFIG_UPDATE,
	]);
	const current = await createBuilder<{limit_config: LimitConfigSnapshot}>(harness, admin.token)
		.get('/admin/limit-config')
		.execute();
	const writeConfig = async (rules: LimitConfigSnapshot['rules']) => {
		await createBuilder(harness, admin.token)
			.put('/admin/limit-config')
			.body({limit_config: {traitDefinitions: current.limit_config.traitDefinitions, rules}})
			.execute();
	};
	await writeConfig([...current.limit_config.rules, {id: 'announcement_test_override', limits}]);
	return async () => {
		await writeConfig(current.limit_config.rules);
	};
}

export const crosspostTaskHandlers: Record<string, WorkerTaskHandler> = {
	crosspostMessage,
	crosspostMessageChunk,
	syncCrosspostedMessage,
	syncCrosspostCopies,
	removeChannelFollowers,
};

export function enableCrosspostWorker(
	extraHandlers: Record<string, WorkerTaskHandler> = {},
	extraDependencies: Partial<WorkerDependencies> = {},
): SyncTaskWorkerService {
	const worker = new SyncTaskWorkerService({...crosspostTaskHandlers, ...extraHandlers}, {deferred: true});
	setInjectedWorkerService(worker);
	resetApiServicesForTesting();
	setWorkerDependenciesForTest({
		channelRepository: getChannelRepository(),
		webhookRepository: getWebhookRepository(),
		userRepository: getUserRepository(),
		guildRepository: getGuildRepository(),
		gatewayService: getGatewayService(),
		storageService: getStorageService(),
		avatarService: getAvatarService(),
		purgeQueue: getPurgeQueue(),
		snowflakeService: getSnowflakeService(),
		cacheService: getCacheService(),
		limitConfigService: getLimitConfigService(),
		channelService: createTestChannelService(),
		workerService: worker,
		...extraDependencies,
	});
	return worker;
}

export function disableCrosspostWorker(): void {
	setInjectedWorkerService(new NoopWorkerService());
	resetApiServicesForTesting();
	clearWorkerDependencies();
}
