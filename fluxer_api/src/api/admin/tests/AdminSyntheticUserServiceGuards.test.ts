// SPDX-License-Identifier: AGPL-3.0-or-later

import {DELETED_USER_ID} from '@fluxer/constants/src/UserConstants';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {describe, expect, test} from 'vitest';
import {createGuildID, createUserID, type UserID} from '../../BrandedTypes';
import type {IChannelRepository} from '../../channel/IChannelRepository';
import {UserMessageDeletionService} from '../../channel/services/message/UserMessageDeletionService';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import {GuildMemberOperationsService} from '../../guild/services/member/GuildMemberOperationsService';
import type {IPurgeQueue} from '../../infrastructure/BunnyPurgeQueue';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {IStorageService} from '../../infrastructure/IStorageService';

const SYNTHETIC_USER_IDS: Array<[string, UserID]> = [
	['0', createUserID(0n)],
	['1', createUserID(DELETED_USER_ID)],
];

const GUILD_ID = createGuildID(7000n);

function unusableDependency(): object {
	return new Proxy(
		{},
		{
			get() {
				throw new Error('dependency touched before the synthetic user guard ran');
			},
		},
	);
}

function createMessageDeletionService(): UserMessageDeletionService {
	const unusable = unusableDependency();
	return new UserMessageDeletionService({
		channelRepository: unusable as IChannelRepository,
		gatewayService: unusable as IGatewayService,
		storageService: unusable as IStorageService,
		purgeQueue: unusable as IPurgeQueue,
	});
}

function createGuildMemberOperationsService(): GuildMemberOperationsService {
	const guildRepository = {
		async findUnique() {
			return {id: GUILD_ID, features: new Set(), memberCount: 0};
		},
		async getMember() {
			return null;
		},
	} as unknown as IGuildRepositoryAggregate;
	const unusable = unusableDependency();
	return new GuildMemberOperationsService(
		guildRepository,
		unusable as never,
		unusable as never,
		unusable as never,
		unusable as never,
		unusable as never,
		unusable as never,
		unusable as never,
		unusable as never,
		unusable as never,
		unusable as never,
		unusable as never,
	);
}

describe('service level guards for synthetic accounts', () => {
	test.each(SYNTHETIC_USER_IDS)('deleteUserMessagesBulk refuses user %s', async (_label, userId) => {
		await expect(createMessageDeletionService().deleteUserMessagesBulk(userId)).rejects.toBeInstanceOf(
			UnknownUserError,
		);
	});
	test.each(SYNTHETIC_USER_IDS)('addUserToGuild refuses user %s', async (_label, userId) => {
		await expect(
			createGuildMemberOperationsService().addUserToGuild(
				{
					userId,
					guildId: GUILD_ID,
					skipBanCheck: true,
					skipGuildLimitCheck: true,
					skipRiskGate: true,
					requestCache: new Map(),
				} as never,
				unusableDependency() as never,
			),
		).rejects.toBeInstanceOf(UnknownUserError);
	});
});
