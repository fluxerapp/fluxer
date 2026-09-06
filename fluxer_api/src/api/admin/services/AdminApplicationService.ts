// SPDX-License-Identifier: AGPL-3.0-or-later

import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {UnknownApplicationError} from '@fluxer/errors/src/domains/oauth/UnknownApplicationError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	ApplicationAdminResponse,
	ListApplicationsResponse,
	LookupApplicationResponse,
	TransferApplicationOwnershipRequest,
} from '@fluxer/schema/src/domains/admin/AdminApplicationSchemas';
import type {ApiContext} from '../../ApiContext';
import {type ApplicationID, createApplicationID, createUserID, type GuildID, type UserID} from '../../BrandedTypes';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {Application} from '../../models/Application';
import type {IApplicationRepository} from '../../oauth/repositories/IApplicationRepository';
import type {AdminAuditService} from './AdminAuditService';

interface AdminApplicationServiceDeps {
	apiContext: ApiContext;
	applicationRepository: IApplicationRepository;
	auditService: AdminAuditService;
	guildRepository: IGuildRepositoryAggregate;
}

interface UserDisplay {
	username: string | null;
	global_name: string | null;
	discriminator: string | null;
}

export class AdminApplicationService {
	constructor(private readonly deps: AdminApplicationServiceDeps) {}

	async lookupApplication(applicationId: ApplicationID): Promise<LookupApplicationResponse> {
		const {applicationRepository} = this.deps;
		const application = await applicationRepository.getApplication(applicationId);
		if (!application) {
			return {application: null};
		}
		const userDisplays = await this.loadUserDisplays([application.ownerUserId, application.botUserId]);
		return {
			application: this.toResponse(application, userDisplays),
		};
	}

	async listUserApplications(ownerUserId: UserID): Promise<ListApplicationsResponse> {
		const {applicationRepository} = this.deps;
		const {users: userRepository} = this.deps.apiContext.services;
		const owner = await userRepository.findUnique(ownerUserId);
		if (!owner) {
			throw new UnknownUserError();
		}
		const applications = await applicationRepository.listApplicationsByOwner(ownerUserId);
		const ownerDisplay: UserDisplay = {
			username: owner.username,
			global_name: owner.globalName ?? null,
			discriminator: String(owner.discriminator).padStart(4, '0'),
		};
		const botUserIds = applications.map((app) => app.botUserId).filter((id): id is UserID => id !== null);
		const botDisplays = await this.loadUserDisplays(botUserIds);
		const displayMap = new Map<string, UserDisplay>();
		displayMap.set(ownerUserId.toString(), ownerDisplay);
		for (const [id, display] of botDisplays.entries()) {
			displayMap.set(id, display);
		}
		return {
			applications: applications.map((app) => this.toResponse(app, displayMap)),
		};
	}

	async listGuildApplications(guildId: GuildID): Promise<ListApplicationsResponse> {
		const {applicationRepository, guildRepository} = this.deps;
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		const members = await guildRepository.listMembers(guildId);
		const applications: Array<Application> = [];
		for (const member of members) {
			const application = await applicationRepository.getApplication(
				createApplicationID(BigInt(member.userId.toString())),
			);
			if (!application?.botUserId || application.botUserId !== member.userId) continue;
			applications.push(application);
		}
		const userDisplays = await this.loadUserDisplays(
			applications.flatMap((application) => [application.ownerUserId, application.botUserId]),
		);
		return {
			applications: applications.map((application) => this.toResponse(application, userDisplays)),
		};
	}

	async transferApplicationOwnership(
		applicationId: ApplicationID,
		data: TransferApplicationOwnershipRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {applicationRepository, auditService} = this.deps;
		const {users: userRepository} = this.deps.apiContext.services;
		const application = await applicationRepository.getApplication(applicationId);
		if (!application) {
			throw new UnknownApplicationError();
		}
		const newOwnerId = createUserID(data.new_owner_id);
		const newOwner = await userRepository.findUnique(newOwnerId);
		if (!newOwner) {
			throw new UnknownUserError();
		}
		const oldOwnerId = application.ownerUserId;
		const oldRow = application.toRow();
		const updatedApplication = await applicationRepository.upsertApplication(
			{...oldRow, owner_user_id: newOwnerId},
			oldRow,
		);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'application',
			targetId: BigInt(applicationId),
			action: 'transfer_ownership',
			auditLogReason,
			metadata: new Map([
				['old_owner_id', oldOwnerId.toString()],
				['new_owner_id', newOwnerId.toString()],
			]),
		});
		const userDisplays = await this.loadUserDisplays([updatedApplication.ownerUserId, updatedApplication.botUserId]);
		return {
			application: this.toResponse(updatedApplication, userDisplays),
		};
	}

	private async loadUserDisplays(userIds: ReadonlyArray<UserID | null>): Promise<Map<string, UserDisplay>> {
		const {users: userRepository} = this.deps.apiContext.services;
		const uniqueIds = new Set<string>();
		const nonNullIds: Array<UserID> = [];
		for (const userId of userIds) {
			if (userId === null) continue;
			const key = userId.toString();
			if (uniqueIds.has(key)) continue;
			uniqueIds.add(key);
			nonNullIds.push(userId);
		}
		const users = await Promise.all(nonNullIds.map((id) => userRepository.findUnique(id)));
		const map = new Map<string, UserDisplay>();
		for (let i = 0; i < nonNullIds.length; i += 1) {
			const id = nonNullIds[i];
			if (!id) continue;
			const user = users[i];
			if (!user) continue;
			map.set(id.toString(), {
				username: user.username,
				global_name: user.globalName ?? null,
				discriminator: String(user.discriminator).padStart(4, '0'),
			});
		}
		return map;
	}

	private toResponse(application: Application, userDisplays: Map<string, UserDisplay>): ApplicationAdminResponse {
		const emptyDisplay: UserDisplay = {username: null, global_name: null, discriminator: null};
		const ownerDisplay = userDisplays.get(application.ownerUserId.toString()) ?? emptyDisplay;
		const botDisplay = application.botUserId
			? (userDisplays.get(application.botUserId.toString()) ?? emptyDisplay)
			: emptyDisplay;
		return {
			id: application.applicationId.toString(),
			name: application.name,
			owner_user_id: application.ownerUserId.toString(),
			owner_username: ownerDisplay.username,
			owner_global_name: ownerDisplay.global_name,
			owner_discriminator: ownerDisplay.discriminator,
			bot_user_id: application.botUserId?.toString() ?? null,
			bot_username: botDisplay.username,
			bot_global_name: botDisplay.global_name,
			bot_discriminator: botDisplay.discriminator,
			bot_is_public: application.botIsPublic,
			bot_require_code_grant: application.botRequireCodeGrant,
			oauth2_redirect_uris: Array.from(application.oauth2RedirectUris),
			has_client_secret: application.clientSecretHash !== null,
			has_bot_token: application.botTokenHash !== null,
			bot_token_preview: application.botTokenPreview,
			bot_token_created_at: application.botTokenCreatedAt?.toISOString() ?? null,
			client_secret_created_at: application.clientSecretCreatedAt?.toISOString() ?? null,
			version: application.version,
		};
	}
}
