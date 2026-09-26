// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {IMediaService} from '@app/api/infrastructure/IMediaService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {User} from '@app/api/models/User';
import type {UserGuildSettings} from '@app/api/models/UserGuildSettings';
import type {UserSettings} from '@app/api/models/UserSettings';
import type {IUserAccountRepository} from '@app/api/user/repositories/IUserAccountRepository';
import {BaseUserUpdatePropagator} from '@app/api/user/services/BaseUserUpdatePropagator';
import {propagatePartialUserChange} from '@app/api/user/services/PartialUserChangePropagation';
import {mapUserGuildSettingsToResponse, mapUserSettingsToResponse} from '@app/api/user/UserMappers';

interface UserAccountUpdatePropagatorDeps {
	userCacheService: UserCacheService;
	gatewayService: IGatewayService;
	mediaService: IMediaService;
	userRepository: IUserAccountRepository;
	guildRepository: IGuildRepositoryAggregate;
}

export class UserAccountUpdatePropagator extends BaseUserUpdatePropagator {
	constructor(private readonly deps: UserAccountUpdatePropagatorDeps) {
		super({
			userCacheService: deps.userCacheService,
			gatewayService: deps.gatewayService,
		});
	}

	async propagatePartialUserChange(user: User): Promise<void> {
		await propagatePartialUserChange(this.deps, user);
	}

	async dispatchUserSettingsUpdate({userId, settings}: {userId: UserID; settings: UserSettings}): Promise<void> {
		await this.deps.gatewayService.dispatchPresence({
			userId,
			event: 'USER_SETTINGS_UPDATE',
			data: mapUserSettingsToResponse({settings}),
		});
	}

	async dispatchUserGuildSettingsUpdate({
		userId,
		settings,
	}: {
		userId: UserID;
		settings: UserGuildSettings;
	}): Promise<void> {
		const payload = mapUserGuildSettingsToResponse(settings);
		await this.deps.gatewayService.dispatchPresence({
			userId,
			event: 'USER_GUILD_SETTINGS_UPDATE',
			data: payload,
		});
		if (payload.guild_id !== null) {
			await this.deps.gatewayService.syncPushUserGuildSettings({
				userId,
				guildId: settings.guildId,
				settings: payload,
			});
		}
	}

	async dispatchUserNoteUpdate(params: {userId: UserID; targetId: UserID; note: string}): Promise<void> {
		const {userId, targetId, note} = params;
		await this.deps.gatewayService.dispatchPresence({
			userId,
			event: 'USER_NOTE_UPDATE',
			data: {id: targetId.toString(), note},
		});
	}
}
