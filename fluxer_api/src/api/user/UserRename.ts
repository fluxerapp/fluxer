// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {UserID} from '@app/api/BrandedTypes';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {GuildMemberSearchIndexService} from '@app/api/guild/services/member/GuildMemberSearchIndexService';
import type {DiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import {Logger} from '@app/api/Logger';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {
	type PartialUserChangePropagationDeps,
	propagateUserUpdate,
} from '@app/api/user/services/PartialUserChangePropagation';
import type {ContactChangeReason} from '@app/api/user/services/UserContactChangeLogService';
import {reserveUsername} from '@app/api/user/UniqueUsernames';
import {USERNAME_MODE_DISCRIMINATOR} from '@app/api/user/UserTag';
import {TagAlreadyTakenError} from '@fluxer/errors/src/domains/user/TagAlreadyTakenError';

export interface UserRenameDeps {
	apiContext: ApiContext;
	discriminatorService: DiscriminatorService;
	userUpdateDeps: PartialUserChangePropagationDeps;
	guildRepository: GuildRepository;
}

async function reindexGuildMembers(deps: UserRenameDeps, user: User): Promise<void> {
	try {
		const guildIds = await deps.apiContext.services.users.getUserGuildIds(user.id);
		await new GuildMemberSearchIndexService().updateUserMembers(user, guildIds, deps.guildRepository);
	} catch (error) {
		Logger.error({userId: user.id.toString(), error}, 'Failed to reindex guild members after a rename');
	}
}

async function chooseDiscriminator(
	deps: UserRenameDeps,
	user: User,
	uniqueUsernames: boolean,
	{username, discriminator}: {username: string; discriminator?: number},
): Promise<number> {
	if (uniqueUsernames) return USERNAME_MODE_DISCRIMINATOR;
	const result = await deps.discriminatorService.generateDiscriminator({
		username,
		requestedDiscriminator: discriminator,
		user,
	});
	if (!result.available || result.discriminator === -1) {
		throw new TagAlreadyTakenError();
	}
	return result.discriminator;
}

export async function renameUser(
	deps: UserRenameDeps,
	user: User,
	tag: {username: string; discriminator?: number},
	{reason, actorUserId}: {reason: ContactChangeReason; actorUserId: UserID | null},
): Promise<User> {
	const {users, cache, contactChangeLog} = deps.apiContext.services;
	const uniqueUsernames = !user.isBot && (await getInstanceConfigRepository().usesUniqueUsernames());
	const reservation = uniqueUsernames ? await reserveUsername({users, cache}, tag.username, user.id) : null;
	let updatedUser: User;
	try {
		const discriminator = await chooseDiscriminator(deps, user, uniqueUsernames, tag);
		updatedUser = await users.patchUpsert(user.id, {username: tag.username, discriminator}, user.toRow());
	} finally {
		await reservation?.release();
	}
	await propagateUserUpdate(deps.userUpdateDeps, user, updatedUser);
	await contactChangeLog.recordDiff({oldUser: user, newUser: updatedUser, reason, actorUserId});
	void reindexGuildMembers(deps, updatedUser);
	return updatedUser;
}
