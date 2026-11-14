// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {CrosspostWorkerService} from '@app/api/channel/services/message/CrosspostPropagation';
import {UserMessageDeletionService} from '@app/api/channel/services/message/UserMessageDeletionService';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {DELETED_USER_ID} from '@fluxer/constants/src/UserConstants';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {describe, expect, test} from 'vitest';

const SYNTHETIC_USER_IDS: Array<[string, UserID]> = [
	['0', createUserID(0n)],
	['1', createUserID(DELETED_USER_ID)],
];

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
		channelRepository: unusable as ChannelRepository,
		gatewayService: unusable as IGatewayService,
		storageService: unusable as IStorageService,
		purgeQueue: unusable as IPurgeQueue,
		workerService: unusable as CrosspostWorkerService,
	});
}

describe('service level guards for synthetic accounts', () => {
	test.each(SYNTHETIC_USER_IDS)('deleteUserMessagesBulk refuses user %s', async (_label, userId) => {
		await expect(createMessageDeletionService().deleteUserMessagesBulk(userId)).rejects.toBeInstanceOf(
			UnknownUserError,
		);
	});
});
