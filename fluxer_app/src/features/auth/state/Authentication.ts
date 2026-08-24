// SPDX-License-Identifier: AGPL-3.0-or-later

import SessionManager from '@app/features/platform/state/AuthSession';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import type {UserPrivate} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {action, computed, makeAutoObservable} from 'mobx';

const LoginState = {
	Default: 'default',
	Mfa: 'mfa',
} as const;

export type LoginState = ValueOf<typeof LoginState>;

export interface MfaMethods {
	totp: boolean;
	webauthn: boolean;
}

class Authentication {
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
	handleGatewayReady({user}: {user: UserPrivate}): void {
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
	handleMfaTicketSet({
		ticket,
		totp,
		webauthn,
	}: {
		ticket: string;
	} & MfaMethods): void {
		this.loginState = LoginState.Mfa;
		this.mfaTicket = ticket;
		this.mfaMethods = {totp, webauthn};
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
			void import('@app/features/navigation/utils/RouterUtils').then((module) => {
				module.replaceWith('/login');
			});
		}
	}

	async fetchGatewayToken(): Promise<string | null> {
		return SessionManager.token;
	}
}

export default new Authentication();
