// SPDX-License-Identifier: AGPL-3.0-or-later

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

	handleGatewayReady(authSessionIdHash: string): void {
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
}

export default new AuthSessions();
