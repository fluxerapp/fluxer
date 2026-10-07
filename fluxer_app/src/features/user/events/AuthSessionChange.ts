// SPDX-License-Identifier: AGPL-3.0-or-later

import AuthSession from '@app/features/auth/state/AuthSession';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('AuthSessionChange');

export interface AuthSessionChangePayload {
	new_token?: string;
	new_auth_session_id_hash?: string | null;
}

export function handleAuthSessionChange(data: AuthSessionChangePayload, context: GatewayHandlerContext): void {
	const token = data.new_token;
	if (token) {
		void SessionManager.setToken(token)
			.then(() => context.socket?.setToken(token))
			.catch((error) => logger.error('Failed to persist an authentication session token change', error));
	}
	if (data.new_auth_session_id_hash) {
		AuthSession.handleAuthSessionChange(data.new_auth_session_id_hash);
	}
}
