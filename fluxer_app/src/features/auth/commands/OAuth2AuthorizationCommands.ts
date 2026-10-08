// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('OAuth2AuthorizationCommands');

export interface OAuth2Authorization {
	application: {
		id: string;
		name: string;
		icon: string | null;
		description: string | null;
		bot_public: boolean;
	};
	scopes: Array<string>;
	authorized_at: string;
}

async function requestAuthorizations(): Promise<Array<OAuth2Authorization>> {
	const response = await http.get<Array<OAuth2Authorization>>(Endpoints.OAUTH_AUTHORIZATIONS);
	return response.body;
}

async function requestAuthorizationBulkRemoval(applicationIds: Array<string>): Promise<void> {
	await http.post(Endpoints.OAUTH_AUTHORIZATIONS_REVOKE, {
		body: {
			application_ids: applicationIds,
		},
	});
}

function rethrowAuthorizationFailure(message: string, error: unknown): never {
	logger.error(message, error);
	throw error;
}

export async function listAuthorizations(): Promise<Array<OAuth2Authorization>> {
	try {
		return await requestAuthorizations();
	} catch (error) {
		rethrowAuthorizationFailure('Failed to list OAuth2 authorizations:', error);
	}
}

export async function deauthorizeMany(applicationIds: Array<string>): Promise<void> {
	try {
		await requestAuthorizationBulkRemoval(applicationIds);
	} catch (error) {
		rethrowAuthorizationFailure('Failed to deauthorize applications:', error);
	}
}
