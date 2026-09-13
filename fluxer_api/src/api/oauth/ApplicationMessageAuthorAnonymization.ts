// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import {MessageAnonymizationService} from '@app/api/channel/services/message/MessageAnonymizationService';
import {EMPTY_USER_ROW} from '@app/api/database/types/UserTypes';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {Logger} from '@app/api/Logger';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {
	DELETED_USER_DISCRIMINATOR,
	DELETED_USER_GLOBAL_NAME,
	DELETED_USER_USERNAME,
	UserFlags,
} from '@fluxer/constants/src/UserConstants';

interface RemapAuthorMessagesToDeletedUserParams {
	originalAuthorId: UserID;
	channelRepository: IChannelRepository;
	userRepository: IUserRepository;
	snowflakeService: ISnowflakeService;
}

async function createDeletedMessageAuthorUser(params: {
	userRepository: IUserRepository;
	snowflakeService: ISnowflakeService;
}): Promise<UserID> {
	const deletedUserId = createUserID(await params.snowflakeService.generate());
	await params.userRepository.create({
		...EMPTY_USER_ROW,
		user_id: deletedUserId,
		username: DELETED_USER_USERNAME,
		discriminator: DELETED_USER_DISCRIMINATOR,
		global_name: DELETED_USER_GLOBAL_NAME,
		bot: false,
		system: false,
		flags: UserFlags.DELETED,
	});
	await params.userRepository.deleteUserSecondaryIndices(deletedUserId);
	return deletedUserId;
}

export async function remapAuthorMessagesToDeletedUser(
	params: RemapAuthorMessagesToDeletedUserParams,
): Promise<UserID | null> {
	const {originalAuthorId, channelRepository, userRepository, snowflakeService} = params;
	const hasMessages = await channelRepository.listMessagesByAuthor(originalAuthorId, 1);
	if (hasMessages.length === 0) {
		return null;
	}
	const replacementAuthorId = await createDeletedMessageAuthorUser({
		userRepository,
		snowflakeService,
	});
	const anonymizationService = new MessageAnonymizationService(channelRepository);
	await anonymizationService.anonymizeMessagesByAuthor(originalAuthorId, replacementAuthorId);
	Logger.info(
		{originalAuthorId: originalAuthorId.toString(), replacementAuthorId: replacementAuthorId.toString()},
		'Remapped authored messages to deleted user id',
	);
	return replacementAuthorId;
}
