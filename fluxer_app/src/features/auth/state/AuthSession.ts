// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResettableStates} from '@app/features/app/state/ResettableStates';
import {AuthSession as AuthSessionModel} from '@app/features/auth/models/AuthSession';
import type {AuthSessionResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {makeAutoObservable} from 'mobx';

type FetchStatus = 'idle' | 'pending' | 'success' | 'error';

class AuthSessions {
	authSessionIdHash: string | null = null;
	authSessions: Array<AuthSessionModel> = [];
	fetchStatus: FetchStatus = 'idle';
	isDeleteError = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	handleGatewayReady(authSessionIdHash: string | null): void {
		this.authSessionIdHash = authSessionIdHash;
	}

	handleAuthSessionChange(authSessionIdHash: string): void {
		this.authSessionIdHash = authSessionIdHash;
	}

	fetchPending(): void {
		this.fetchStatus = 'pending';
	}

	fetchSuccess(authSessions: ReadonlyArray<AuthSessionResponse>): void {
		this.authSessions = authSessions.map((session) => new AuthSessionModel(session));
		this.fetchStatus = 'success';
	}

	fetchError(): void {
		this.fetchStatus = 'error';
	}

	logoutPending(): void {
		this.isDeleteError = false;
	}

	logoutSuccess(sessionIdHashes: ReadonlyArray<string>): void {
		this.authSessions = this.authSessions.filter((session) => !sessionIdHashes.includes(session.id));
	}

	logoutError(): void {
		this.isDeleteError = true;
	}

	reset(): void {
		this.authSessions = [];
		this.fetchStatus = 'idle';
		this.isDeleteError = false;
	}
}

const authSessions = new AuthSessions();

ResettableStates.register(authSessions, () => authSessions.reset());

export default authSessions;
