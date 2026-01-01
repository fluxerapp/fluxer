/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';

const logger = new Logger('OAuth2AuthorizationActionCreators');

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

export const listAuthorizations = async (): Promise<Array<OAuth2Authorization>> => {
	try {
		const response = await http.get<Array<OAuth2Authorization>>({url: Endpoints.OAUTH_AUTHORIZATIONS});
		return response.body;
	} catch (error) {
		logger.error('Failed to list OAuth2 authorizations:', error);
		throw error;
	}
};

export const deauthorize = async (applicationId: string): Promise<void> => {
	try {
		await http.delete({url: Endpoints.OAUTH_AUTHORIZATION(applicationId)});
	} catch (error) {
		logger.error('Failed to deauthorize application:', error);
		throw error;
	}
};
