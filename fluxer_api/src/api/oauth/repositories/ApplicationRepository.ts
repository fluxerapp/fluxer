// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ADMIN_OAUTH2_APPLICATION_ID} from '@fluxer/constants/src/Core';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';
import {type ApplicationID, createApplicationID, type UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {SYSTEM_USER_ID} from '../../constants/Core';
import {BatchBuilder, fetchMany, fetchOne} from '../../database/CassandraQueryExecution';
import {buildPatchFromData, executeVersionedUpdate} from '../../database/CassandraVersionedUpdate';
import type {ApplicationByOwnerRow, ApplicationRow} from '../../database/types/OAuth2Types';
import {APPLICATION_COLUMNS} from '../../database/types/OAuth2Types';
import {Application} from '../../models/Application';
import {Applications, ApplicationsByOwner} from '../../Tables';
import {hashPassword} from '../../utils/PasswordUtils';
import type {IApplicationRepository} from './IApplicationRepository';

const SELECT_APPLICATION_CQL = Applications.selectCql({
	where: Applications.where.eq('application_id'),
});
const SELECT_APPLICATION_IDS_BY_OWNER_CQL = ApplicationsByOwner.selectCql({
	columns: ['application_id'],
	where: ApplicationsByOwner.where.eq('owner_user_id'),
});
const FETCH_APPLICATIONS_BY_IDS_CQL = Applications.selectCql({
	where: Applications.where.in('application_id', 'application_ids'),
});

let cachedAdminSecretHash: string | null = null;

export function resetAdminSecretHashForTesting(): void {
	cachedAdminSecretHash = null;
}

async function getAdminSecretHash(): Promise<string | null> {
	const secret = Config.admin.oauthClientSecret;
	if (!secret) {
		return null;
	}
	if (cachedAdminSecretHash === null) {
		cachedAdminSecretHash = await hashPassword(secret);
	}
	return cachedAdminSecretHash;
}

function getAdminRedirectUri(): string {
	return `${Config.endpoints.admin}/oauth2_callback`;
}

function buildAdminApplication(secretHash: string | null): Application {
	const row: ApplicationRow = {
		application_id: createApplicationID(ADMIN_OAUTH2_APPLICATION_ID),
		owner_user_id: SYSTEM_USER_ID,
		name: 'Fluxer Admin',
		bot_user_id: null,
		bot_is_public: false,
		bot_require_code_grant: false,
		oauth2_redirect_uris: new Set<string>([getAdminRedirectUri()]),
		client_secret_hash: secretHash,
		bot_token_hash: null,
		bot_token_preview: null,
		bot_token_created_at: null,
		client_secret_created_at: null,
		version: 1,
	};
	return new Application(row);
}

export class ApplicationRepository implements IApplicationRepository {
	async getApplication(applicationId: ApplicationID): Promise<Application | null> {
		if (applicationId === createApplicationID(ADMIN_OAUTH2_APPLICATION_ID)) {
			const secretHash = await getAdminSecretHash();
			if (secretHash === null) {
				return null;
			}
			return buildAdminApplication(secretHash);
		}
		const row = await fetchOne<ApplicationRow>(SELECT_APPLICATION_CQL, {application_id: applicationId});
		return row ? new Application(row) : null;
	}

	async listApplicationsByOwner(ownerUserId: UserID): Promise<Array<Application>> {
		const ids = await fetchMany<ApplicationByOwnerRow>(SELECT_APPLICATION_IDS_BY_OWNER_CQL, {
			owner_user_id: ownerUserId,
		});
		if (ids.length === 0) {
			return [];
		}
		const rows = await fetchMany<ApplicationRow>(FETCH_APPLICATIONS_BY_IDS_CQL, {
			application_ids: ids.map((r) => r.application_id),
		});
		return rows.map((r) => new Application(r));
	}

	async upsertApplication(data: ApplicationRow, oldData?: ApplicationRow | null): Promise<Application> {
		const applicationId = data.application_id;
		if (applicationId === createApplicationID(ADMIN_OAUTH2_APPLICATION_ID)) {
			throw new ForbiddenError({
				code: APIErrorCodes.FORBIDDEN,
				message: 'Cannot modify the built-in admin OAuth2 application',
			});
		}
		const result = await executeVersionedUpdate<ApplicationRow, 'application_id'>(
			async () => fetchOne<ApplicationRow>(SELECT_APPLICATION_CQL, {application_id: applicationId}),
			(current) => ({
				pk: {application_id: applicationId},
				patch: buildPatchFromData(data, current, APPLICATION_COLUMNS, ['application_id']),
			}),
			Applications,
			{initialData: oldData},
		);
		const batch = new BatchBuilder();
		batch.addPrepared(
			ApplicationsByOwner.upsertAll({
				owner_user_id: data.owner_user_id,
				application_id: data.application_id,
			}),
		);
		if (oldData && oldData.owner_user_id !== data.owner_user_id) {
			batch.addPrepared(
				ApplicationsByOwner.deleteByPk({
					owner_user_id: oldData.owner_user_id,
					application_id: data.application_id,
				}),
			);
		}
		await batch.execute();
		return new Application({...data, version: result.finalVersion});
	}

	async deleteApplication(applicationId: ApplicationID): Promise<void> {
		if (applicationId === createApplicationID(ADMIN_OAUTH2_APPLICATION_ID)) {
			throw new ForbiddenError({
				code: APIErrorCodes.FORBIDDEN,
				message: 'Cannot delete the built-in admin OAuth2 application',
			});
		}
		const applicationRow = await fetchOne<ApplicationRow>(SELECT_APPLICATION_CQL, {application_id: applicationId});
		const application = applicationRow ? new Application(applicationRow) : null;
		if (!application) {
			return;
		}
		const batch = new BatchBuilder();
		batch.addPrepared(Applications.deleteByPk({application_id: applicationId}));
		batch.addPrepared(
			ApplicationsByOwner.deleteByPk({
				owner_user_id: application.ownerUserId,
				application_id: applicationId,
			}),
		);
		await batch.execute();
	}
}
