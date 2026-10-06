// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {Application} from '@app/api/models/Application';
import type {User} from '@app/api/models/User';
import type {IApplicationRepository} from '@app/api/oauth/repositories/IApplicationRepository';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';

export class BotMfaMirrorService {
	constructor(
		private readonly applicationRepository: IApplicationRepository,
		private readonly userRepository: IUserRepository,
		private readonly gatewayService: IGatewayService,
	) {}

	private cloneAuthenticatorTypes(source: User): Set<number> {
		return source.authenticatorTypes ? new Set(source.authenticatorTypes) : new Set();
	}

	private hasSameAuthenticatorTypes(target: User, desired: Set<number>): boolean {
		const current = target.authenticatorTypes ?? new Set<number>();
		if (current.size !== desired.size) return false;
		for (const value of current) {
			if (!desired.has(value)) {
				return false;
			}
		}
		return true;
	}

	private async listApplications(ownerUserId: UserID): Promise<Array<Application>> {
		return this.applicationRepository.listApplicationsByOwner(ownerUserId);
	}

	async syncAuthenticatorTypesForOwner(owner: User): Promise<void> {
		if (owner.isBot) return;
		const desiredTypes = this.cloneAuthenticatorTypes(owner);
		const applications = await this.listApplications(owner.id);
		await Promise.all(
			applications.map(async (application) => {
				if (!application.hasBotUser()) return;
				const botUserId = application.getBotUserId();
				if (!botUserId) return;
				const botUser = await this.userRepository.findUnique(botUserId);
				if (!botUser) return;
				if (this.hasSameAuthenticatorTypes(botUser, desiredTypes)) {
					return;
				}
				const updatedBotUser = await this.userRepository.patchUpsert(
					botUserId,
					{
						authenticator_types: desiredTypes,
					},
					botUser.toRow(),
				);
				if (updatedBotUser) {
					await this.gatewayService.dispatchPresence({
						userId: botUserId,
						event: 'USER_UPDATE',
						data: mapUserToPrivateResponse(updatedBotUser),
					});
				}
			}),
		);
	}
}
