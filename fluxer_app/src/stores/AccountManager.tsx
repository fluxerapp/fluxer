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

import {computed, makeAutoObservable} from 'mobx';
import type {UserData} from '~/lib/AccountStorage';
import SessionManager, {type Account, SessionExpiredError} from '~/lib/SessionManager';
import {Routes} from '~/Routes';
import * as PushSubscriptionService from '~/services/push/PushSubscriptionService';
import ConnectionStore from '~/stores/ConnectionStore';
import * as NotificationUtils from '~/utils/NotificationUtils';
import {isInstalledPwa} from '~/utils/PwaUtils';
import * as RouterUtils from '~/utils/RouterUtils';

export type AccountSummary = Account;

class AccountManager {
	constructor() {
		makeAutoObservable(
			this,
			{
				currentUserId: computed,
				currentAccount: computed,
				orderedAccounts: computed,
				canSwitchAccounts: computed,
				isSwitching: computed,
				isLoading: computed,
			},
			{autoBind: true},
		);
	}

	private shouldManagePushSubscriptions(): boolean {
		return isInstalledPwa();
	}

	get currentUserId(): string | null {
		return SessionManager.userId;
	}

	get accounts(): Map<string, AccountSummary> {
		return new Map(SessionManager.accounts.map((a) => [a.userId, a]));
	}

	get isSwitching(): boolean {
		return SessionManager.isSwitching;
	}

	get isLoading(): boolean {
		return SessionManager.isLoggingOut || SessionManager.isSwitching;
	}

	get currentAccount(): AccountSummary | null {
		return SessionManager.currentAccount;
	}

	get orderedAccounts(): Array<AccountSummary> {
		return SessionManager.accounts;
	}

	get canSwitchAccounts(): boolean {
		return SessionManager.canSwitchAccount();
	}

	getAllAccounts(): Array<AccountSummary> {
		return this.orderedAccounts;
	}

	async bootstrap(): Promise<void> {
		await SessionManager.initialize();
	}

	async stashCurrentAccount(): Promise<void> {
		await SessionManager.stashCurrentAccount();
	}

	markAccountAsInvalid(userId: string): void {
		SessionManager.markAccountInvalid(userId);
	}

	async generateTokenForAccount(userId: string): Promise<{token: string; userId: string}> {
		await SessionManager.initialize();

		const account = SessionManager.accounts.find((a) => a.userId === userId);
		if (!account) {
			throw new Error(`No stored data found for account ${userId}`);
		}

		const ok = await SessionManager.validateToken(account.token, account.instance);
		if (!ok) {
			SessionManager.markAccountInvalid(userId);
			throw new SessionExpiredError();
		}

		return {token: account.token, userId};
	}

	async switchToAccount(userId: string): Promise<void> {
		if (this.shouldManagePushSubscriptions()) {
			await PushSubscriptionService.unregisterAllPushSubscriptions();
		}

		await SessionManager.switchAccount(userId);
		ConnectionStore.startSession(SessionManager.token ?? undefined);
		RouterUtils.replaceWith(Routes.ME);

		if (this.shouldManagePushSubscriptions()) {
			void (async () => {
				if (await NotificationUtils.isGranted()) {
					await PushSubscriptionService.registerPushSubscription();
				}
			})();
		}
	}

	async switchToNewAccount(userId: string, token: string, userData?: UserData, skipReload = false): Promise<void> {
		await SessionManager.login(token, userId, userData);
		ConnectionStore.startSession(token);
		if (!skipReload) {
			RouterUtils.replaceWith(Routes.ME);
		}

		if (this.shouldManagePushSubscriptions()) {
			void (async () => {
				if (await NotificationUtils.isGranted()) {
					await PushSubscriptionService.registerPushSubscription();
				}
			})();
		}
	}

	async removeStoredAccount(userId: string): Promise<void> {
		await SessionManager.removeAccount(userId);
	}

	updateAccountUserData(userId: string, userData: UserData): void {
		SessionManager.updateAccountUserData(userId, userData);
	}

	async logout(): Promise<void> {
		await SessionManager.logout();
		RouterUtils.replaceWith('/login');
	}
}

export default new AccountManager();
