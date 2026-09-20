// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import SessionManager from '@app/features/platform/state/AuthSession';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import type {UserPrivate} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {computed, makeAutoObservable} from 'mobx';

const LoginState = {
	Default: 'default',
	Mfa: 'mfa',
} as const;

export type LoginState = ValueOf<typeof LoginState>;

export interface MfaMethods {
	totp: boolean;
	webauthn: boolean;
	backupCodes: boolean;
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

	setUserId(userId: string | null): void {
		SessionManager.setUserId(userId);
	}

	handleGatewayReady({user}: {user: UserPrivate}): void {
		SessionManager.setUserId(user.id);
		SessionManager.handleConnectionReady();
	}

	handleAuthSessionChange({token}: {token: string}): void {
		SessionManager.setToken(token || null);
	}

	handleConnectionClosed({code}: {code: number}): void {
		SessionManager.handleConnectionClosed(code);
		if (code === 4004) {
			this.handleLogout();
		}
	}

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

	handleMfaTicketSet({
		ticket,
		totp,
		webauthn,
		backupCodes,
	}: {
		ticket: string;
	} & MfaMethods): void {
		this.loginState = LoginState.Mfa;
		this.mfaTicket = ticket;
		this.mfaMethods = {totp, webauthn, backupCodes};
	}

	handleMfaTicketClear(): void {
		this.loginState = LoginState.Default;
		this.mfaTicket = null;
		this.mfaMethods = null;
	}

	handleLogout(options?: {skipRedirect?: boolean}): void {
		ExperimentAssignments.reset();
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
