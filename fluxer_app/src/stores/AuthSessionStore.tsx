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

import {makeAutoObservable} from 'mobx';
import {type AuthSession, AuthSessionRecord} from '~/records/AuthSessionRecord';

type FetchStatus = 'idle' | 'pending' | 'success' | 'error';

class AuthSessionStore {
	authSessionIdHash: string | null = null;
	authSessions: Array<AuthSessionRecord> = [];
	fetchStatus: FetchStatus = 'idle';
	isDeleteError = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	handleConnectionOpen(authSessionIdHash: string): void {
		this.authSessionIdHash = authSessionIdHash;
	}

	handleAuthSessionChange(authSessionIdHash: string): void {
		this.authSessionIdHash = authSessionIdHash;
	}

	fetchPending(): void {
		this.fetchStatus = 'pending';
	}

	fetchSuccess(authSessions: ReadonlyArray<AuthSession>): void {
		this.authSessions = authSessions.map((session) => new AuthSessionRecord(session));
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

export default new AuthSessionStore();
