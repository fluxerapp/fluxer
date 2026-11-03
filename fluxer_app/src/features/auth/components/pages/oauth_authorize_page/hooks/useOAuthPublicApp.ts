// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {authResponseUserToUserData} from '@app/features/auth/commands/AuthenticationCommands';
import {
	logger,
	type PublicAppData,
} from '@app/features/auth/components/pages/oauth_authorize_page/OAuthAuthorizePageShared';
import accountStorage from '@app/features/auth/state/AccountStorage';
import Accounts from '@app/features/auth/state/Accounts';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import {isAccountTransitionAbortError} from '@app/features/platform/state/AccountTransitionAbort';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {useEffect, useState} from 'react';

type PublicAppStatus = 'idle' | 'loading' | 'ready' | 'session_expired' | 'error';

export interface PublicAppState {
	status: PublicAppStatus;
	data: PublicAppData | null;
	error: unknown;
}

export function useOAuthPublicApp(clientId: string | null): PublicAppState {
	const currentUserId = Accounts.currentUserId;
	const currentAccountKey = Accounts.currentAccountKey;
	const accountWorkAdmitted = !AccountScopedWork.isSuspended;
	const [state, setState] = useState<PublicAppState>(() => ({
		status: clientId ? 'loading' : 'idle',
		data: null,
		error: null,
	}));
	useEffect(() => {
		if (!clientId) {
			setState({status: 'idle', data: null, error: null});
			return;
		}
		setState({status: 'loading', data: null, error: null});
		if (!accountWorkAdmitted) return;
		let cancelled = false;
		(async () => {
			try {
				const resp = await http.get<PublicAppData>(Endpoints.OAUTH_PUBLIC_APPLICATION(clientId));
				if (cancelled) return;
				const currentUser = resp.body.current_user;
				if (currentUserId && !currentUser) {
					logger.warn('OAuth public app fetch returned no current user for a signed-in account');
					setState({status: 'session_expired', data: null, error: null});
					return;
				}
				if (currentUser && currentUser.id === currentUserId && currentAccountKey != null) {
					const freshUserData = authResponseUserToUserData(currentUser);
					if (freshUserData) {
						const userData = {...Accounts.getAccount(currentAccountKey)?.userData, ...freshUserData};
						Accounts.updateAccountUserData(currentAccountKey, userData);
						void accountStorage.updateAccountUserData(currentAccountKey, userData).catch((error) => {
							logger.warn(`Failed to persist account user data for ${currentAccountKey}`, error);
						});
					}
				}
				setState({status: 'ready', data: resp.body, error: null});
			} catch (err) {
				if (cancelled || isAccountTransitionAbortError(err)) return;
				if (err instanceof HttpError && err.status === 401) {
					logger.warn('OAuth public app fetch returned 401', err);
					setState({status: 'session_expired', data: null, error: err});
					return;
				}
				logger.error('Failed to load OAuth public application', err);
				setState({status: 'error', data: null, error: err});
			}
		})();
		return () => {
			cancelled = true;
		};
	}, [clientId, currentAccountKey, currentUserId, accountWorkAdmitted]);
	return state;
}
