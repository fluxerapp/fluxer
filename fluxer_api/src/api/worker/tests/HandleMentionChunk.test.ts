// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterEach, describe, expect, test} from 'vitest';
import {
	type ChannelID,
	createChannelID,
	createGuildID,
	createMessageID,
	createUserID,
	type GuildID,
	type MessageID,
	type UserID,
} from '../../BrandedTypes';
import type {RecentMentionRow} from '../../database/types/UserTypes';
import {UserGuildSettings} from '../../models/UserGuildSettings';
import type {ReadStateService} from '../../read_state/ReadStateService';
import {NoopLogger} from '../../test/mocks/NoopLogger';
import type {UserRepository} from '../../user/repositories/UserRepository';
import handleMentionChunk from '../tasks/HandleMentionChunk';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '../WorkerContext';

const CHANNEL_ID = '2000';
const MESSAGE_ID = '3000';
const GUILD_ID = '4000';

interface SuppressionFlags {
	suppressEveryone?: boolean;
	suppressRoles?: boolean;
}

function createGuildSettings(userId: UserID, guildId: GuildID, flags: SuppressionFlags): UserGuildSettings {
	return new UserGuildSettings({
		user_id: userId,
		guild_id: guildId,
		message_notifications: null,
		muted: false,
		mute_config: null,
		mobile_push: false,
		suppress_everyone: flags.suppressEveryone ?? false,
		suppress_roles: flags.suppressRoles ?? false,
		hide_muted_channels: false,
		channel_overrides: null,
		unread_badges: null,
		version: 1,
	});
}

function createHarness(settingsByUserId: Record<string, SuppressionFlags>) {
	const settingsReads: Array<string> = [];
	const incrementedUserIds: Array<string> = [];
	const recentMentions: Array<RecentMentionRow> = [];
	const userRepository = {
		async findGuildSettings(userId: UserID, guildId: GuildID | null): Promise<UserGuildSettings | null> {
			settingsReads.push(userId.toString());
			const flags = settingsByUserId[userId.toString()];
			if (!flags || guildId == null) {
				return null;
			}
			return createGuildSettings(userId, guildId, flags);
		},
		async createRecentMentions(mentions: Array<RecentMentionRow>): Promise<void> {
			recentMentions.push(...mentions);
		},
	} as unknown as UserRepository;
	const readStateService = {
		async bulkIncrementMentionCounts(
			updates: Array<{userId: UserID; channelId: ChannelID; messageId: MessageID}>,
		): Promise<void> {
			for (const update of updates) {
				incrementedUserIds.push(update.userId.toString());
			}
		},
	} as unknown as ReadStateService;
	setWorkerDependenciesForTest({userRepository, readStateService});
	return {settingsReads, incrementedUserIds, recentMentions};
}

function createHelpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 1n,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

async function runChunk(mentions: Array<{userId: string; direct?: boolean; role?: boolean; everyone?: boolean}>) {
	await handleMentionChunk(
		{channelId: CHANNEL_ID, messageId: MESSAGE_ID, guildId: GUILD_ID, mentions},
		createHelpers(),
	);
}

describe('handleMentionChunk', () => {
	afterEach(() => {
		clearWorkerDependencies();
	});

	test('reads no guild settings for a direct-only mention chunk', async () => {
		const harness = createHarness({});

		await runChunk([
			{userId: '10', direct: true},
			{userId: '11', direct: true},
			{userId: '12', direct: true},
		]);

		expect(harness.settingsReads).toEqual([]);
		expect(harness.incrementedUserIds).toEqual(['10', '11', '12']);
		expect(
			harness.recentMentions.map((mention) => [mention.user_id.toString(), mention.is_everyone, mention.is_role]),
		).toEqual([
			['10', false, false],
			['11', false, false],
			['12', false, false],
		]);
	});

	test('reads guild settings only for everyone and role mentions', async () => {
		const harness = createHarness({});

		await runChunk([
			{userId: '10', direct: true},
			{userId: '11', everyone: true},
			{userId: '12', role: true},
		]);

		expect(harness.settingsReads).toEqual(['11', '12']);
	});

	test('honours suppressEveryone for everyone mentions', async () => {
		const harness = createHarness({'11': {suppressEveryone: true}});

		await runChunk([
			{userId: '10', everyone: true},
			{userId: '11', everyone: true},
		]);

		expect(harness.settingsReads).toEqual(['10', '11']);
		expect(harness.incrementedUserIds).toEqual(['10']);
		expect(harness.recentMentions.map((mention) => [mention.user_id.toString(), mention.is_everyone])).toEqual([
			['10', true],
		]);
	});

	test('honours suppressRoles for role mentions', async () => {
		const harness = createHarness({'11': {suppressRoles: true}});

		await runChunk([
			{userId: '10', role: true},
			{userId: '11', role: true},
		]);

		expect(harness.settingsReads).toEqual(['10', '11']);
		expect(harness.incrementedUserIds).toEqual(['10']);
		expect(harness.recentMentions.map((mention) => [mention.user_id.toString(), mention.is_role])).toEqual([
			['10', true],
		]);
	});

	test('still applies suppression flags to recent mentions when a user is also directly mentioned', async () => {
		const harness = createHarness({'10': {suppressEveryone: true, suppressRoles: true}});

		await runChunk([{userId: '10', direct: true, everyone: true, role: true}]);

		expect(harness.settingsReads).toEqual(['10']);
		expect(harness.incrementedUserIds).toEqual(['10']);
		expect(harness.recentMentions).toEqual([
			{
				user_id: createUserID(10n),
				channel_id: createChannelID(BigInt(CHANNEL_ID)),
				message_id: createMessageID(BigInt(MESSAGE_ID)),
				guild_id: createGuildID(BigInt(GUILD_ID)),
				is_everyone: false,
				is_role: false,
			},
		]);
	});

	test('reads no guild settings outside a guild', async () => {
		const harness = createHarness({});

		await handleMentionChunk(
			{channelId: CHANNEL_ID, messageId: MESSAGE_ID, mentions: [{userId: '10', direct: true}]},
			createHelpers(),
		);

		expect(harness.settingsReads).toEqual([]);
		expect(harness.incrementedUserIds).toEqual(['10']);
		expect(harness.recentMentions).toEqual([]);
	});
});
