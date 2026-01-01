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
import type {AuthSession} from '~/records/AuthSessionRecord';
import AuthSessionStore from '~/stores/AuthSessionStore';

const logger = new Logger('AuthSessionsService');

export const fetch = async (): Promise<void> => {
	logger.debug('Fetching authentication sessions');
	AuthSessionStore.fetchPending();

	try {
		const response = await http.get<Array<AuthSession>>({url: Endpoints.AUTH_SESSIONS, retries: 2});
		const sessions = response.body ?? [];
		logger.info(`Fetched ${sessions.length} authentication sessions`);
		AuthSessionStore.fetchSuccess(sessions);
	} catch (error) {
		logger.error('Failed to fetch authentication sessions:', error);
		AuthSessionStore.fetchError();
		throw error;
	}
};

export const logout = async (sessionIdHashes: Array<string>): Promise<void> => {
	if (!sessionIdHashes.length) {
		logger.warn('Attempted to logout with empty session list');
		return;
	}
	logger.debug(`Logging out ${sessionIdHashes.length} sessions`);
	AuthSessionStore.logoutPending();
	try {
		await http.post({
			url: Endpoints.AUTH_SESSIONS_LOGOUT,
			body: {session_id_hashes: sessionIdHashes},
			timeout: 10000,
			retries: 0,
		});
		logger.info(`Successfully logged out ${sessionIdHashes.length} sessions`);
		AuthSessionStore.logoutSuccess(sessionIdHashes);
	} catch (error) {
		logger.error('Failed to log out sessions:', error);
		AuthSessionStore.logoutError();
		throw error;
	}
};
