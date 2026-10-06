// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {SudoVerificationBody} from '@app/api/auth/services/SudoVerificationService';
import {requireSudoMode} from '@app/api/auth/services/SudoVerificationService';
import {createApplicationID, type UserID} from '@app/api/BrandedTypes';
import {UsernameNotAvailableError} from '@app/api/infrastructure/DiscriminatorService';
import type {Application} from '@app/api/models/Application';
import type {User} from '@app/api/models/User';
import type {ApplicationService} from '@app/api/oauth/ApplicationService';
import {ApplicationNotOwnedError} from '@app/api/oauth/ApplicationService';
import {mapApplicationToResponse, mapBotProfileToResponse} from '@app/api/oauth/OAuth2Mappers';
import type {IApplicationRepository} from '@app/api/oauth/repositories/IApplicationRepository';
import {MAX_APPLICATIONS_PER_USER} from '@fluxer/constants/src/LimitConstants';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {BotUserNotFoundError} from '@fluxer/errors/src/domains/oauth/BotUserNotFoundError';
import {InvalidClientError} from '@fluxer/errors/src/domains/oauth/InvalidClientError';
import {MaxApplicationsError} from '@fluxer/errors/src/domains/oauth/MaxApplicationsError';
import {UnknownApplicationError} from '@fluxer/errors/src/domains/oauth/UnknownApplicationError';
import type {
	ApplicationCreateRequest,
	ApplicationUpdateRequest,
	BotProfileResponse,
	BotProfileUpdateRequest,
} from '@fluxer/schema/src/domains/oauth/OAuthSchemas';
import type {Context} from 'hono';

export class OAuth2ApplicationsRequestService {
	constructor(
		private readonly apiContext: ApiContext,
		private readonly applicationService: ApplicationService,
		private readonly applicationRepository: IApplicationRepository,
	) {}

	async listApplications(userId: UserID) {
		const applications: Array<Application> = await this.applicationService.listApplicationsByOwner(userId);
		const botUserMap = new Map<string, User>();
		const botUserFetches: Array<{
			id: string;
			promise: Promise<User | null>;
		}> = [];
		for (const app of applications) {
			if (app.hasBotUser()) {
				const botUserId = app.getBotUserId();
				if (botUserId) {
					botUserFetches.push({
						id: botUserId.toString(),
						promise: this.apiContext.services.users.findUnique(botUserId),
					});
				}
			}
		}
		const botUsers = await Promise.all(botUserFetches.map((f) => f.promise));
		for (let i = 0; i < botUsers.length; i++) {
			const user = botUsers[i];
			if (user !== null) {
				botUserMap.set(botUserFetches[i].id, user);
			}
		}
		return applications.map((app: Application) => {
			const botUserId = app.hasBotUser() ? app.getBotUserId() : null;
			const botUser = botUserId ? botUserMap.get(botUserId.toString()) : null;
			return mapApplicationToResponse(app, {botUser: botUser ?? undefined});
		});
	}

	async createApplication(userId: UserID, body: ApplicationCreateRequest) {
		const existingApps = await this.applicationService.listApplicationsByOwner(userId);
		if (existingApps.length >= MAX_APPLICATIONS_PER_USER) {
			throw new MaxApplicationsError(MAX_APPLICATIONS_PER_USER);
		}
		const result = await this.applicationService.createApplication({
			ownerUserId: userId,
			name: body.name,
			redirectUris: body.redirect_uris,
			botPublic: body.bot_public,
			botRequireCodeGrant: body.bot_require_code_grant,
		});
		return mapApplicationToResponse(result.application, {
			botUser: result.botUser,
			botToken: result.botToken,
			clientSecret: result.clientSecret,
		});
	}

	async getApplication(userId: UserID, applicationId: bigint) {
		const appId = createApplicationID(applicationId);
		const application = await this.applicationRepository.getApplication(appId);
		if (!application) {
			throw new UnknownApplicationError();
		}
		if (application.ownerUserId !== userId) {
			throw new AccessDeniedError();
		}
		let botUser = null;
		if (application.hasBotUser()) {
			const botUserId = application.getBotUserId();
			if (botUserId) {
				botUser = await this.apiContext.services.users.findUnique(botUserId);
			}
		}
		return mapApplicationToResponse(application, {botUser});
	}

	async updateApplication(userId: UserID, applicationId: bigint, body: ApplicationUpdateRequest) {
		try {
			const updated = await this.applicationService.updateApplication({
				userId,
				applicationId: createApplicationID(applicationId),
				name: body.name,
				redirectUris: body.redirect_uris,
				botPublic: body.bot_public,
				botRequireCodeGrant: body.bot_require_code_grant,
			});
			let botUser = null;
			if (updated.hasBotUser()) {
				const botUserId = updated.getBotUserId();
				if (botUserId) {
					botUser = await this.apiContext.services.users.findUnique(botUserId);
				}
			}
			return mapApplicationToResponse(updated, {botUser: botUser ?? undefined});
		} catch (err) {
			if (err instanceof ApplicationNotOwnedError) {
				throw new AccessDeniedError();
			}
			if (err instanceof UnknownApplicationError) {
				throw err;
			}
			throw err;
		}
	}

	async deleteApplication(params: {
		ctx: Context;
		userId: UserID;
		body: SudoVerificationBody;
		applicationId: bigint;
	}): Promise<void> {
		await requireSudoMode(params.ctx, params.ctx.get('user'), params.body);
		try {
			await this.applicationService.deleteApplication(params.userId, createApplicationID(params.applicationId));
		} catch (err) {
			if (err instanceof ApplicationNotOwnedError) {
				throw new AccessDeniedError();
			}
			if (err instanceof UnknownApplicationError) {
				throw err;
			}
			throw err;
		}
	}

	async updateBotProfile(
		userId: UserID,
		applicationId: bigint,
		body: BotProfileUpdateRequest,
	): Promise<BotProfileResponse> {
		try {
			const result = await this.applicationService.updateBotProfile(userId, createApplicationID(applicationId), body);
			return mapBotProfileToResponse(result.user);
		} catch (err) {
			if (err instanceof ApplicationNotOwnedError) {
				throw new AccessDeniedError();
			}
			if (err instanceof BotUserNotFoundError) {
				throw err;
			}
			if (err instanceof InvalidClientError || err instanceof UnknownApplicationError) {
				throw new UnknownApplicationError();
			}
			if (err instanceof UsernameNotAvailableError) {
				throw err;
			}
			throw err;
		}
	}
}
