// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import type {AccountRestriction} from '@app/features/auth/state/AccountRestriction';
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

interface MfaMethods {
	totp: boolean;
	webauthn: boolean;
	backupCodes: boolean;
}

class Authentication {
	loginState: LoginState = LoginState.DEFAULT;
	currentMfaTicket: string | null = null;
	availableMfaMethods: MfaMethods | null = null;
	currentMfaRuntimeSnapshot: RuntimeConfigSnapshot | null = null;
	accountRestriction: AccountRestriction | null = null;

	constructor() {
		makeAutoObservable(
			this,
			{
				isAuthenticated: computed,
				currentUserId: computed,
			},
			{autoBind: true},
		);
	}

	get isAuthenticated(): boolean {
		return SessionManager.isAuthenticated;
	}

	get currentUserId(): string | null {
		return SessionManager.userId;
	}

	handleGatewayReady({user}: {user: UserPrivate}): void {
		SessionManager.setUserId(user.id);
		SessionManager.handleConnectionReady();
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
		this.handleMfaTicketClear();
		this.handleAccountRestrictionClear();
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

	handleAccountRestriction(restriction: AccountRestriction): void {
		this.handleMfaTicketClear();
		this.accountRestriction = restriction;
	}

	handleAccountRestrictionClear(): void {
		this.accountRestriction = null;
	}

	handleLogout(options?: {skipRedirect?: boolean}): void {
		ExperimentAssignments.reset();
		this.handleMfaTicketClear();
		this.handleAccountRestrictionClear();
		if (!options?.skipRedirect) {
			void import('@app/features/navigation/utils/RouterUtils').then((module) => {
				module.replaceWith('/login');
			});
		}
	}
}

export default new Authentication();
