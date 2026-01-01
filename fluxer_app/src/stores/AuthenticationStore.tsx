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

import {action, computed, makeAutoObservable} from 'mobx';
import SessionManager from '~/lib/SessionManager';
import type {UserPrivate} from '~/records/UserRecord';
import * as RouterUtils from '~/utils/RouterUtils';

const LoginState = {
	Default: 'default',
	Mfa: 'mfa',
} as const;
export type LoginState = (typeof LoginState)[keyof typeof LoginState];

export type MfaMethods = {sms: boolean; totp: boolean; webauthn: boolean};

class AuthenticationStore {
	loginState: LoginState = LoginState.Default;
	mfaTicket: string | null = null;
	mfaMethods: MfaMethods | null = null;

	constructor() {
		makeAutoObservable(
			this,
			{
				isAuthenticated: computed,
				authToken: computed,
				currentUserId: computed,
			},
			{autoBind: true},
		);
	}

	get isInMfaState(): boolean {
		return this.loginState === LoginState.Mfa;
	}

	get isAuthenticated(): boolean {
		return SessionManager.isAuthenticated;
	}

	get authToken(): string | null {
		return SessionManager.token;
	}

	get token(): string | null {
		return SessionManager.token;
	}

	get currentMfaTicket(): string | null {
		return this.mfaTicket;
	}

	get availableMfaMethods(): MfaMethods | null {
		return this.mfaMethods;
	}

	get currentUserId(): string | null {
		return SessionManager.userId;
	}

	get userId(): string | null {
		return SessionManager.userId;
	}

	@action
	setUserId(userId: string | null): void {
		SessionManager.setUserId(userId);
	}

	@action
	handleConnectionOpen({user}: {user: UserPrivate}): void {
		SessionManager.setUserId(user.id);
		SessionManager.handleConnectionReady();
	}

	@action
	handleAuthSessionChange({token}: {token: string}): void {
		SessionManager.setToken(token || null);
	}

	handleConnectionClosed({code}: {code: number}): void {
		SessionManager.handleConnectionClosed(code);
		if (code === 4004) {
			this.handleLogout();
		}
	}

	@action
	handleSessionStart({token}: {token: string | null | undefined}): void {
		if (token) {
			SessionManager.setToken(token);
		} else {
			SessionManager.setToken(null);
		}
		this.loginState = LoginState.Default;
		this.mfaTicket = null;
		this.mfaMethods = null;
	}

	@action
	handleMfaTicketSet({ticket, sms, totp, webauthn}: {ticket: string} & MfaMethods): void {
		this.loginState = LoginState.Mfa;
		this.mfaTicket = ticket;
		this.mfaMethods = {sms, totp, webauthn};
	}

	@action
	handleMfaTicketClear(): void {
		this.loginState = LoginState.Default;
		this.mfaTicket = null;
		this.mfaMethods = null;
	}

	@action
	handleLogout(options?: {skipRedirect?: boolean}): void {
		this.loginState = LoginState.Default;
		this.mfaTicket = null;
		this.mfaMethods = null;

		if (!options?.skipRedirect) {
			RouterUtils.replaceWith('/login');
		}
	}

	async fetchGatewayToken(): Promise<string | null> {
		return SessionManager.token;
	}
}

export default new AuthenticationStore();
