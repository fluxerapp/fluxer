// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserGuildSettings} from '@app/api/models/UserGuildSettings';
import type {UserSettings} from '@app/api/models/UserSettings';
import {dispatchUserGuildSettingsUpdate} from '@app/api/user/UserGuildSettingsThreadView';
import {mapUserSettingsToResponse} from '@app/api/user/UserMappers';

interface UserAccountUpdatePropagatorDeps {
	gatewayService: IGatewayService;
}

export class UserAccountUpdatePropagator {
	constructor(private readonly deps: UserAccountUpdatePropagatorDeps) {}

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
		await dispatchUserGuildSettingsUpdate(this.deps.gatewayService, userId, settings);
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
