// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {SudoVerificationBody} from '@app/api/auth/services/SudoVerificationService';
import {requireSudoMode} from '@app/api/auth/services/SudoVerificationService';
import {createApplicationID, type UserID} from '@app/api/BrandedTypes';
import type {ApplicationService} from '@app/api/oauth/ApplicationService';
import {findBotUser, mapApplicationToResponse, mapBotProfileToResponse} from '@app/api/oauth/OAuth2Mappers';
import type {ApplicationRepository} from '@app/api/oauth/repositories/ApplicationRepository';
import {MAX_APPLICATIONS_PER_USER} from '@fluxer/constants/src/LimitConstants';
import {MaxApplicationsError} from '@fluxer/errors/src/domains/oauth/MaxApplicationsError';
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
		private readonly applicationRepository: ApplicationRepository,
	) {}

	async listApplications(userId: UserID) {
		const applications = await this.applicationRepository.listApplicationsByOwner(userId);
		return Promise.all(
			applications.map(async (application) =>
				mapApplicationToResponse(application, {
					botUser: await findBotUser(this.apiContext.services.users, application),
				}),
			),
		);
	}

	async createApplication(userId: UserID, body: ApplicationCreateRequest) {
		const existingApps = await this.applicationRepository.listApplicationsByOwner(userId);
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
		const application = await this.applicationService.verifyOwnership(userId, createApplicationID(applicationId));
		return mapApplicationToResponse(application, {
			botUser: await findBotUser(this.apiContext.services.users, application),
		});
	}

	async updateApplication(userId: UserID, applicationId: bigint, body: ApplicationUpdateRequest) {
		const updated = await this.applicationService.updateApplication({
			userId,
			applicationId: createApplicationID(applicationId),
			name: body.name,
			redirectUris: body.redirect_uris,
			botPublic: body.bot_public,
			botRequireCodeGrant: body.bot_require_code_grant,
		});
		return mapApplicationToResponse(updated, {botUser: await findBotUser(this.apiContext.services.users, updated)});
	}

	async deleteApplication(params: {
		ctx: Context;
		userId: UserID;
		body: SudoVerificationBody;
		applicationId: bigint;
	}): Promise<void> {
		await requireSudoMode(params.ctx, params.ctx.get('user'), params.body);
		await this.applicationService.deleteApplication(params.userId, createApplicationID(params.applicationId));
	}

	async updateBotProfile(
		userId: UserID,
		applicationId: bigint,
		body: BotProfileUpdateRequest,
	): Promise<BotProfileResponse> {
		const result = await this.applicationService.updateBotProfile(userId, createApplicationID(applicationId), body);
		return mapBotProfileToResponse(result.user);
	}
}
