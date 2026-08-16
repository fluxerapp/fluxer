// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {UserData} from '@app/features/auth/state/AccountStorage';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import * as NotificationUtils from '@app/features/notification/utils/NotificationUtils';
import * as PushSubscriptionService from '@app/features/platform/push/PushSubscriptionService';
import SessionManager, {type Account, SessionExpiredError} from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {isInstalledPwa} from '@app/features/ui/utils/PwaUtils';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {computed, makeAutoObservable} from 'mobx';

const logger = new Logger('AccountManager');

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

	get accounts(): Map<string, Account> {
		return new Map(SessionManager.accounts.map((a) => [a.userId, a]));
	}

	get isSwitching(): boolean {
		return SessionManager.isSwitching;
	}

	get isLoading(): boolean {
		return SessionManager.isLoggingOut || SessionManager.isSwitching;
	}

	get currentAccount(): Account | null {
		return SessionManager.currentAccount;
	}

	get orderedAccounts(): Array<Account> {
		return SessionManager.accounts;
	}

	get canSwitchAccounts(): boolean {
		return SessionManager.canSwitchAccount();
	}

	getAllAccounts(): Array<Account> {
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

	async generateTokenForAccount(userId: string): Promise<{
		token: string;
		userId: string;
	}> {
		await SessionManager.initialize();
		const account = SessionManager.requireAccountOnCurrentInstance(userId);
		const ok = await SessionManager.validateToken(account.token);
		if (!ok) {
			SessionManager.markAccountInvalid(userId);
			throw new SessionExpiredError();
		}
		return {token: account.token, userId};
	}

	private async leaveActiveVoiceChannel(context: 'account switch' | 'logout'): Promise<void> {
		try {
			await MediaEngine.disconnectFromVoiceChannel('user');
		} catch (err) {
			logger.warn(`Failed to leave active voice channel before ${context}`, err);
		}
	}

	async switchToAccount(userId: string, redirectPath: string | null = Routes.ME): Promise<void> {
		if (userId === SessionManager.userId) {
			return;
		}
		SessionManager.requireAccountOnCurrentInstance(userId);
		if (!SessionManager.canSwitchAccount()) {
			throw new Error(`Cannot switch from state: ${SessionManager.state}`);
		}
		if (this.shouldManagePushSubscriptions()) {
			await PushSubscriptionService.unregisterAllPushSubscriptions();
		}
		await this.leaveActiveVoiceChannel('account switch');
		await SessionManager.switchAccount(userId);
		GatewayConnection.startSession(SessionManager.token ?? undefined);
		if (redirectPath !== null) {
			RouterUtils.replaceWith(redirectPath);
		}
		if (this.shouldManagePushSubscriptions()) {
			void (async () => {
				if (await NotificationUtils.isGranted()) {
					await PushSubscriptionService.registerPushSubscription();
				}
			})();
		}
	}

	async switchToNewAccount(
		userId: string,
		token: string,
		userData?: UserData,
		redirectPath: string | null = Routes.ME,
	): Promise<void> {
		if (SessionManager.userId && SessionManager.userId !== userId) {
			SessionManager.prepareForAccountTransition('account-switch');
		}
		await this.leaveActiveVoiceChannel('account switch');
		await SessionManager.login(token, userId, userData);
		GatewayConnection.startSession(token);
		if (redirectPath !== null) {
			RouterUtils.replaceWith(redirectPath);
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
		await this.leaveActiveVoiceChannel('logout');
		await SessionManager.logout();
		RouterUtils.replaceWith('/login');
	}
}

export default new AccountManager();
