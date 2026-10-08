// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {UserPrivate} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {computed, makeAutoObservable} from 'mobx';

const logger = new Logger('Authentication');

function persistToken(token: string | null): void {
	void SessionManager.setToken(token).catch((error) => logger.error('Failed to persist the session token', error));
}

export const LoginState = Object.freeze({
	DEFAULT: 'default',
	MFA: 'mfa',
} as const);

export type LoginState = (typeof LoginState)[keyof typeof LoginState];

export interface MfaMethods {
	totp: boolean;
	webauthn: boolean;
	backupCodes: boolean;
}

class Authentication {
	loginState: LoginState = LoginState.DEFAULT;
	currentMfaTicket: string | null = null;
	availableMfaMethods: MfaMethods | null = null;
	currentMfaRuntimeSnapshot: RuntimeConfigSnapshot | null = null;

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

	get isAuthenticated(): boolean {
		return SessionManager.isAuthenticated;
	}

	get authToken(): string | null {
		return SessionManager.token;
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
		persistToken(token || null);
	}

	async handleConnectionClosed({code, accountKey}: {code: number; accountKey?: string | null}): Promise<void> {
		const failedAccountKey = accountKey === undefined ? SessionManager.currentAccountKey : accountKey;
		const result = await SessionManager.handleConnectionClosed(code, failedAccountKey);
		if (code === 4004 && result.invalidatedCurrentSession && !AccountScopedWork.isSuspended) {
			this.handleLogout();
		}
	}

	handleSessionStart({token}: {token: string | null | undefined}): void {
		persistToken(token ?? null);
		this.loginState = LoginState.DEFAULT;
		this.currentMfaTicket = null;
		this.availableMfaMethods = null;
		this.currentMfaRuntimeSnapshot = null;
	}

	handleMfaTicketSet({
		ticket,
		totp,
		webauthn,
		backupCodes,
		runtimeSnapshot,
	}: {
		ticket: string;
		runtimeSnapshot: RuntimeConfigSnapshot | null;
	} & MfaMethods): void {
		this.loginState = LoginState.MFA;
		this.currentMfaTicket = ticket;
		this.availableMfaMethods = {totp, webauthn, backupCodes};
		this.currentMfaRuntimeSnapshot = runtimeSnapshot;
	}

	handleMfaTicketClear(): void {
		this.loginState = LoginState.DEFAULT;
		this.currentMfaTicket = null;
		this.availableMfaMethods = null;
		this.currentMfaRuntimeSnapshot = null;
	}

	handleLogout(options?: {skipRedirect?: boolean}): void {
		ExperimentAssignments.reset();
		this.loginState = LoginState.DEFAULT;
		this.currentMfaTicket = null;
		this.availableMfaMethods = null;
		this.currentMfaRuntimeSnapshot = null;
		if (!options?.skipRedirect) {
			void import('@app/features/navigation/utils/RouterUtils').then((module) => {
				module.replaceWith('/login');
			});
		}
	}
}

export default new Authentication();
