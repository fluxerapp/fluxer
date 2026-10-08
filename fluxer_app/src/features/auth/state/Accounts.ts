// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {ResettableStates} from '@app/features/app/state/ResettableStates';
import RuntimeConfig, {
	type RuntimeConfigSnapshot,
	runtimeConfigSnapshotsAreSameInstance,
	runtimeInstanceKey,
} from '@app/features/app/state/RuntimeConfig';
import AccountAccess, {
	AccountAccessDecision,
	AccountAccessPhase,
	AccountInstanceUnavailableError,
} from '@app/features/auth/state/AccountAccess';
import type {UserData} from '@app/features/auth/state/AccountStorage';
import {getAccountKey, parseAccountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import {ForegroundGatewayConnectionRecoverableError} from '@app/features/gateway/transport/ForegroundGatewayConnectionFailure';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import Navigation from '@app/features/navigation/state/Navigation';
import {abandonUnreachableLastLocation} from '@app/features/navigation/utils/ChannelRouteReachability';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import * as NotificationUtils from '@app/features/notification/utils/NotificationUtils';
import * as PushSubscriptionService from '@app/features/platform/push/PushSubscriptionService';
import {
	AccountScopedWork,
	type AccountScopedWorkSuspension,
	AccountScopedWorkTransitionReason,
} from '@app/features/platform/state/AccountScopedWork';
import SessionManager, {
	type Account,
	type AuthSessionLoginRequest,
	SessionExpiredError,
	SessionState,
} from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Location from '@app/features/ui/state/Location';
import {isInstalledPwa} from '@app/features/ui/utils/PwaUtils';
import Users from '@app/features/user/state/Users';
import {
	VoiceAccountExitReason,
	type VoiceAccountLifecycle,
	voiceAccountLifecycle,
} from '@app/features/voice/VoiceAccountLifecyclePort';
import {computed, makeAutoObservable, observableRef, when} from 'mobx';

const logger = new Logger('Accounts');

const ACCOUNT_REPLACEMENT_PENDING_OPERATIONS_MAX = 1;

export class AccountReplacementOperationCapacityExceededError extends Error {
	constructor() {
		super(`Account replacement operation capacity of ${ACCOUNT_REPLACEMENT_PENDING_OPERATIONS_MAX} reached`);
		this.name = 'AccountReplacementOperationCapacityExceededError';
	}
}

export class AccountSwitchTargetNotFoundError extends Error {
	constructor(accountKey: string) {
		super(`No stored account found for ${accountKey}`);
		this.name = 'AccountSwitchTargetNotFoundError';
	}
}

export class AccountSwitchUnavailableError extends Error {
	constructor(state: string) {
		super(`Cannot switch from state: ${state}`);
		this.name = 'AccountSwitchUnavailableError';
	}
}

export class AccountReplacementRecoveryFailedError extends AggregateError {
	constructor(activationError: unknown, recoveryErrors: ReadonlyArray<unknown>) {
		super([activationError, ...recoveryErrors], 'Account activation failed and account recovery did not complete');
		this.name = 'AccountReplacementRecoveryFailedError';
	}
}

export interface SwitchToNewAccountRequest {
	readonly userId: string;
	readonly token: string;
	readonly userData?: UserData;
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
	readonly redirectPath?: string | null;
}

interface AccountReplacementActivationRequest {
	readonly activate: (suspension: AccountScopedWorkSuspension) => Promise<void>;
	readonly redirectPath: string | null;
	readonly targetAccountKey: string;
}

interface CurrentAccountReauthenticationRequest {
	readonly activate: () => Promise<void>;
	readonly redirectPath: string | null;
	readonly targetAccountKey: string;
}

interface AccountReplacementCommitRequest {
	readonly activate: (suspension: AccountScopedWorkSuspension) => Promise<void>;
	readonly redirectPath: string | null;
	readonly targetAccountKey: string;
}

type AccountViewReplacement =
	| {readonly kind: 'activation'; readonly redirectPath: string | null}
	| {readonly kind: 'rollback'; readonly previousRoute: string}
	| {readonly kind: 'navigating'; readonly stalePath: string};

const VIEW_NAVIGATION_COMMIT_TIMEOUT_MS = 2000;

type AccountActivationOutcome =
	| {readonly kind: 'ready'}
	| {readonly kind: 'connection-recovering'; readonly cause: ForegroundGatewayConnectionRecoverableError};

interface ActivePreviousAccountState {
	readonly kind: 'active';
	readonly accountKey: string;
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

type PreviousAccountState = {readonly kind: 'none'} | ActivePreviousAccountState;

class Accounts {
	private readonly voice: VoiceAccountLifecycle = voiceAccountLifecycle;
	private accountReplacementInFlight = false;
	private viewReplacement: AccountViewReplacement | null = null;

	constructor() {
		makeAutoObservable<Accounts, 'voice' | 'viewReplacement'>(
			this,
			{
				voice: false,
				viewReplacement: observableRef,
				currentUserId: computed,
				currentAccountKey: computed,
				currentAccount: computed,
				accounts: computed,
				orderedAccounts: computed,
				canSwitchAccounts: computed,
				isSwitching: computed,
				isLoading: computed,
			},
			{autoBind: true},
		);
	}

	get currentUserId(): string | null {
		return SessionManager.userId;
	}

	get currentAccount(): Account | null {
		return SessionManager.currentAccount;
	}

	get currentAccountKey(): string | null {
		return SessionManager.currentAccountKey;
	}

	get accounts(): Map<string, Account> {
		return new Map(SessionManager.accounts.map((account) => [account.storageKey, account]));
	}

	get orderedAccounts(): Array<Account> {
		return SessionManager.accounts;
	}

	get transitioning(): boolean {
		return this.accountReplacementInFlight;
	}

	get isSwitching(): boolean {
		return this.accountReplacementInFlight || SessionManager.isSwitching;
	}

	get isLoading(): boolean {
		return SessionManager.isLoggingOut || this.isSwitching;
	}

	get isViewLive(): boolean {
		return this.viewReplacement === null && Users.isCurrentUserHydrated;
	}

	get canSwitchAccounts(): boolean {
		return !this.accountReplacementInFlight && this.canActivateStoredAccount;
	}

	private get canActivateStoredAccount(): boolean {
		if (SessionManager.canSwitchAccount()) {
			return true;
		}
		return (
			SessionManager.isInitialized &&
			SessionManager.state === SessionState.Idle &&
			SessionManager.currentAccountKey === null
		);
	}

	getAllAccounts(): Array<Account> {
		return this.orderedAccounts;
	}

	getAccount(accountKey: string): Account | null {
		return SessionManager.getAccount(accountKey);
	}

	async bootstrap(): Promise<void> {
		await SessionManager.initialize();
	}

	markAccountInvalid(accountKey: string, expectedToken?: string): void {
		SessionManager.markAccountInvalid(accountKey, expectedToken);
	}

	async refreshStoredAccount({
		userId,
		token,
		userData,
		runtimeSnapshot,
	}: Omit<SwitchToNewAccountRequest, 'redirectPath'>): Promise<void> {
		const accountKey = getAccountKey({userId, instance: runtimeSnapshot});
		if (await SessionManager.refreshStoredAccount(accountKey, token, userData)) {
			AccountAccess.forgetAccount(accountKey);
		}
	}

	updateAccountUserData(accountKey: string, userData: UserData): void {
		SessionManager.updateAccountUserData(accountKey, userData);
	}

	async prepareAccountCredentials(
		accountKey: string,
	): Promise<{token: string; userId: string; runtimeSnapshot: RuntimeConfigSnapshot}> {
		let account: Account;
		try {
			account = await SessionManager.prepareStoredAccount(accountKey);
		} catch (error) {
			throw new AccountInstanceUnavailableError(error instanceof Error ? error : new Error(String(error)));
		}
		if (account.instance === undefined) {
			throw new AccountInstanceUnavailableError(new Error(`Stored account ${accountKey} has no instance runtime`));
		}
		const resolvedAccountKey = account.storageKey;
		const decision = await AccountAccess.ensureAccountChecked(resolvedAccountKey, {force: true, resetFailures: false});
		if (decision === AccountAccessDecision.INVALID) {
			this.markAccountInvalid(resolvedAccountKey, account.token);
			throw new SessionExpiredError();
		}
		if (decision === AccountAccessDecision.UNAVAILABLE) {
			throw new AccountInstanceUnavailableError(AccountAccess.getUnavailabilityCause(resolvedAccountKey));
		}
		return {token: account.token, userId: account.userId, runtimeSnapshot: account.instance};
	}

	async switchToAccount(accountKey: string, redirectPath: string | null = Routes.ME): Promise<void> {
		await this.runAccountReplacementExclusive(async () => {
			const account = this.requireAccount(accountKey);
			const resolvedAccountKey = account.storageKey;
			if (resolvedAccountKey === this.currentAccountKey) {
				return;
			}
			if (!this.canActivateStoredAccount) {
				throw new AccountSwitchUnavailableError(SessionManager.state);
			}
			const {token} = await this.prepareAccountCredentials(resolvedAccountKey);
			await this.runAccountReplacementActivation({
				activate: (suspension) =>
					SessionManager.switchAccountWithinAccountActivation(resolvedAccountKey, {validatedToken: token}, suspension),
				redirectPath,
				targetAccountKey: resolvedAccountKey,
			});
		});
	}

	async switchToNewAccount({
		userId,
		token,
		userData,
		runtimeSnapshot,
		redirectPath = Routes.ME,
	}: SwitchToNewAccountRequest): Promise<void> {
		await this.runAccountReplacementExclusive(async () => {
			const targetAccountKey = getAccountKey({userId, instance: runtimeSnapshot});
			const loginRequest = {token, userId, userData, runtimeSnapshot} satisfies AuthSessionLoginRequest;
			if (targetAccountKey === this.currentAccountKey) {
				await this.runCurrentAccountReauthentication({
					activate: () => SessionManager.login(loginRequest),
					redirectPath,
					targetAccountKey,
				});
				return;
			}
			await this.runAccountReplacementActivation({
				activate: (suspension) => SessionManager.loginWithinAccountActivation(loginRequest, suspension),
				redirectPath,
				targetAccountKey,
			});
		});
	}

	async removeStoredAccount(accountKey: string): Promise<void> {
		await this.runAccountReplacementExclusive(async () => {
			const resolvedAccountKey = this.requireAccount(accountKey).storageKey;
			if (this.currentAccountKey === resolvedAccountKey) {
				await this.leaveActiveVoiceChannel(VoiceAccountExitReason.ACCOUNT_REMOVED);
			}
			await SessionManager.removeAccount(resolvedAccountKey);
			AccountAccess.forgetAccount(resolvedAccountKey);
		});
	}

	async logout(): Promise<void> {
		await this.runAccountReplacementExclusive(async () => {
			const currentAccountKey = this.currentAccountKey;
			await this.leaveActiveVoiceChannel(VoiceAccountExitReason.LOGOUT);
			await SessionManager.logout();
			ResettableStates.prepareForAccountTransition('logout');
			ExperimentAssignments.reset();
			if (currentAccountKey !== null) {
				AccountAccess.forgetAccount(currentAccountKey);
			}
			RouterUtils.replaceWith('/login');
		});
	}

	async suspendVoiceForAccountRestriction(accountKey: string): Promise<void> {
		const account = this.currentAccount;
		if (account === null || account.storageKey !== accountKey) {
			return;
		}
		await this.voice.suspendVoiceForAccountRestriction({accountKey: account.storageKey, userId: account.userId});
	}

	private requireAccount(accountKey: string): Account {
		const account = this.getAccount(accountKey);
		if (account === null) {
			throw new AccountSwitchTargetNotFoundError(accountKey);
		}
		return account;
	}

	private async runAccountReplacementExclusive<T>(operation: () => Promise<T>): Promise<T> {
		if (this.accountReplacementInFlight) {
			throw new AccountReplacementOperationCapacityExceededError();
		}
		this.accountReplacementInFlight = true;
		try {
			return await operation();
		} finally {
			this.finishAccountReplacement();
		}
	}

	private finishAccountReplacement(): void {
		if (!this.accountReplacementInFlight) {
			throw new Error('Account replacement completion has no active operation');
		}
		this.accountReplacementInFlight = false;
	}

	private async runCurrentAccountReauthentication({
		activate,
		redirectPath,
		targetAccountKey,
	}: CurrentAccountReauthenticationRequest): Promise<void> {
		if (this.currentAccountKey !== targetAccountKey) {
			throw new Error(
				`Cannot reauthenticate ${targetAccountKey} while ${this.currentAccountKey ?? 'no account'} is active`,
			);
		}
		await activate();
		if (this.currentAccountKey !== targetAccountKey) {
			throw new Error(
				`Reauthentication activated ${this.currentAccountKey ?? 'no account'} instead of ${targetAccountKey}`,
			);
		}
		if (GatewayConnection.foregroundAccountKey === targetAccountKey && GatewayConnection.isReady) {
			const currentToken = SessionManager.token;
			if (currentToken === null) {
				throw new Error(`Reauthenticated account ${targetAccountKey} has no session token`);
			}
			GatewayConnection.reuseReadyForegroundSession(targetAccountKey, currentToken);
		}
		this.assertNoStaleForegroundGateway();
		let outcome: AccountActivationOutcome = {kind: 'ready'};
		try {
			await GatewayConnection.waitForForegroundReady(targetAccountKey);
		} catch (cause) {
			if (!(cause instanceof ForegroundGatewayConnectionRecoverableError)) {
				throw cause;
			}
			outcome = {kind: 'connection-recovering', cause};
		}
		this.navigateAfterAccountActivation(redirectPath);
		this.registerPushSubscriptionAfterActivation();
		this.continueForegroundRecovery(targetAccountKey, outcome);
	}

	private async runAccountReplacementActivation({
		activate,
		redirectPath,
		targetAccountKey,
	}: AccountReplacementActivationRequest): Promise<void> {
		const previous = this.capturePreviousAccountState();
		if (previous.kind === 'active' && previous.accountKey === targetAccountKey) {
			throw new Error(`Account replacement target ${targetAccountKey} is already active`);
		}
		let outcome: AccountActivationOutcome;
		try {
			await this.prepareCurrentAccountForReplacement();
			outcome = await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, (suspension) =>
				this.activateReplacementAccount({activate, redirectPath, targetAccountKey}, previous, suspension),
			);
		} catch (error) {
			if (previous.kind === 'active' && this.currentAccountKey === previous.accountKey) {
				this.registerPushSubscriptionAfterActivation();
			}
			this.keepForegroundRecoveringAfterFailedActivation();
			throw error;
		}
		this.registerPushSubscriptionAfterActivation();
		this.continueForegroundRecovery(targetAccountKey, outcome);
	}

	private async activateReplacementAccount(
		request: AccountReplacementCommitRequest,
		previous: PreviousAccountState,
		suspension: AccountScopedWorkSuspension,
	): Promise<AccountActivationOutcome> {
		await GatewayConnection.beginForegroundPromotion(request.targetAccountKey);
		const previousRoute = RouterUtils.getCurrentPath();
		const stopAwaitingLiveView = this.beginViewReplacement(request, previous);
		try {
			await request.activate(suspension);
			if (SessionManager.currentAccountKey !== request.targetAccountKey) {
				throw new Error(
					`Account activation committed ${SessionManager.currentAccountKey ?? 'no account'} instead of ${request.targetAccountKey}`,
				);
			}
			if (previous.kind === 'active') {
				ResettableStates.prepareForAccountTransition('account-switch');
			}
			this.assertNoStaleForegroundGateway();
			await GatewayConnection.completeForegroundPromotion(request.targetAccountKey);
			if (previous.kind === 'none') {
				this.revealReplacementView();
			}
			await GatewayConnection.waitForForegroundReady(request.targetAccountKey);
			if (previous.kind === 'none' && request.redirectPath === Routes.ME) {
				abandonUnreachableLastLocation();
			}
			this.revealReplacementView();
			return {kind: 'ready'};
		} catch (activationError) {
			stopAwaitingLiveView();
			const recoveryErrors: Array<unknown> = [];
			try {
				await GatewayConnection.rollbackForegroundPromotion(request.targetAccountKey);
			} catch (rollbackError) {
				recoveryErrors.push(rollbackError);
			}
			if (
				this.currentAccountKey === request.targetAccountKey &&
				activationError instanceof ForegroundGatewayConnectionRecoverableError &&
				recoveryErrors.length === 0
			) {
				this.revealReplacementView();
				return {kind: 'connection-recovering', cause: activationError};
			}
			if (previous.kind === 'active') {
				this.viewReplacement = {kind: 'rollback', previousRoute};
				try {
					await this.restorePreviousAccount(previous, suspension);
				} catch (recoveryError) {
					recoveryErrors.push(recoveryError);
				}
				this.revealReplacementView();
			}
			if (recoveryErrors.length > 0) {
				throw new AccountReplacementRecoveryFailedError(activationError, recoveryErrors);
			}
			throw activationError;
		} finally {
			stopAwaitingLiveView();
			if (this.viewReplacement?.kind !== 'navigating') {
				this.viewReplacement = null;
			}
		}
	}

	private beginViewReplacement(request: AccountReplacementCommitRequest, previous: PreviousAccountState): () => void {
		this.viewReplacement = {kind: 'activation', redirectPath: request.redirectPath};
		if (previous.kind === 'none') {
			return () => {};
		}
		return when(
			() => SessionManager.currentAccountKey === request.targetAccountKey && Users.isCurrentUserHydrated,
			() => this.revealReplacementView(),
		);
	}

	private capturePreviousAccountState(): PreviousAccountState {
		const account = this.currentAccount;
		if (account === null) {
			return {kind: 'none'};
		}
		const runtimeSnapshot = RuntimeConfig.getSnapshotOrNull();
		if (runtimeSnapshot === null) {
			throw new Error(`Account ${account.storageKey} has no active instance runtime`);
		}
		return {
			kind: 'active',
			accountKey: account.storageKey,
			runtimeSnapshot,
		};
	}

	private assertNoStaleForegroundGateway(): void {
		const foregroundAccountKey = GatewayConnection.foregroundAccountKey;
		if (foregroundAccountKey === null || foregroundAccountKey === SessionManager.currentAccountKey) {
			return;
		}
		throw new Error(
			`Foreground gateway ${foregroundAccountKey} remained attached after activating ${SessionManager.currentAccountKey ?? 'no account'}`,
		);
	}

	private async prepareCurrentAccountForReplacement(): Promise<void> {
		if (this.currentAccountKey === null) {
			return;
		}
		if (this.shouldManagePushSubscriptions()) {
			await PushSubscriptionService.unregisterAllPushSubscriptions();
		}
		await this.leaveActiveVoiceChannel(VoiceAccountExitReason.ACCOUNT_SWITCH);
	}

	private async restorePreviousAccount(
		previous: ActivePreviousAccountState,
		suspension: AccountScopedWorkSuspension,
	): Promise<void> {
		if (this.currentAccountKey === previous.accountKey) {
			this.assertNoStaleForegroundGateway();
			await this.restorePreviousRuntimeSnapshot(previous.runtimeSnapshot);
			await this.waitForRestoredForeground(previous.accountKey);
			return;
		}
		await GatewayConnection.beginForegroundPromotion(previous.accountKey);
		try {
			const previousAccount = SessionManager.getAccount(previous.accountKey);
			if (previousAccount === null || !previousAccount.isValid) {
				throw new Error(`Previous account ${previous.accountKey} has no valid recovery credential`);
			}
			await SessionManager.switchAccountWithinAccountActivation(
				previous.accountKey,
				{validatedToken: previousAccount.token},
				suspension,
			);
			if (this.currentAccountKey !== previous.accountKey) {
				throw new Error(`Previous account ${previous.accountKey} was not reselected during recovery`);
			}
			this.assertNoStaleForegroundGateway();
			await this.restorePreviousRuntimeSnapshot(previous.runtimeSnapshot);
			ResettableStates.prepareForAccountTransition('account-switch');
			await GatewayConnection.completeForegroundPromotion(previous.accountKey);
			await this.waitForRestoredForeground(previous.accountKey);
		} catch (recoveryError) {
			try {
				await GatewayConnection.rollbackForegroundPromotion(previous.accountKey);
			} catch (rollbackError) {
				throw new AggregateError(
					[recoveryError, rollbackError],
					'Previous account recovery and gateway promotion rollback failed',
				);
			}
			logger.error('Failed to restore the previous account after a failed activation', recoveryError);
			throw recoveryError;
		}
	}

	private async restorePreviousRuntimeSnapshot(snapshot: RuntimeConfigSnapshot): Promise<void> {
		if (runtimeConfigSnapshotsAreSameInstance(snapshot, RuntimeConfig.getSnapshotOrNull())) {
			return;
		}
		const previous = this.currentAccountKey === null ? null : parseAccountStorageKey(this.currentAccountKey);
		if (previous === null) {
			throw new Error(`Previous account ${this.currentAccountKey ?? 'unknown'} has no qualified runtime identity`);
		}
		if (runtimeInstanceKey(snapshot) !== previous.instanceKey) {
			throw new Error(`Previous runtime snapshot does not belong to ${this.currentAccountKey}`);
		}
		await RuntimeConfig.applySnapshotAndWaitForDesktop({
			snapshot,
			signal: null,
		});
		if (!runtimeConfigSnapshotsAreSameInstance(snapshot, RuntimeConfig.getSnapshotOrNull())) {
			throw new Error(`Previous runtime snapshot was not restored for ${this.currentAccountKey}`);
		}
	}

	private async waitForRestoredForeground(accountKey: string): Promise<void> {
		try {
			await GatewayConnection.waitForForegroundReady(accountKey);
		} catch (error) {
			if (!(error instanceof ForegroundGatewayConnectionRecoverableError)) {
				throw error;
			}
			logger.warn(`Restored account ${accountKey} keeps reconnecting its foreground gateway`, error);
		}
	}

	private navigateAfterAccountActivation(redirectPath: string | null): void {
		if (redirectPath === null) {
			return;
		}
		RouterUtils.replaceWith(redirectPath);
	}

	private revealReplacementView(): void {
		const replacement = this.viewReplacement;
		if (replacement === null || replacement.kind === 'navigating') {
			return;
		}
		const path = this.resolveReplacementViewPath(replacement);
		const stalePath = Navigation.pathname;
		if (path === null || path === RouterUtils.getCurrentPath()) {
			this.viewReplacement = null;
			return;
		}
		RouterUtils.replaceWith(path);
		this.holdViewUntilNavigationCommits(stalePath);
	}

	private holdViewUntilNavigationCommits(stalePath: string): void {
		if (Navigation.pathname !== stalePath) {
			this.viewReplacement = null;
			return;
		}
		const marker: AccountViewReplacement = {kind: 'navigating', stalePath};
		this.viewReplacement = marker;
		const release = () => {
			if (this.viewReplacement === marker) {
				this.viewReplacement = null;
			}
		};
		const timeout = setTimeout(release, VIEW_NAVIGATION_COMMIT_TIMEOUT_MS);
		when(
			() => this.viewReplacement !== marker || Navigation.pathname !== stalePath,
			() => {
				clearTimeout(timeout);
				release();
			},
		);
	}

	private resolveReplacementViewPath(
		replacement: Exclude<AccountViewReplacement, {readonly kind: 'navigating'}>,
	): string | null {
		if (replacement.kind === 'rollback') {
			return replacement.previousRoute === '' ? null : replacement.previousRoute;
		}
		if (replacement.redirectPath !== Routes.ME) {
			return replacement.redirectPath;
		}
		if (Users.isCurrentUserHydrated) {
			abandonUnreachableLastLocation();
		}
		return Location.getLastLocation() ?? Routes.ME;
	}

	private keepForegroundRecoveringAfterFailedActivation(): void {
		const accountKey = this.currentAccountKey;
		if (accountKey === null || !SessionManager.isAuthenticated) {
			return;
		}
		if (GatewayConnection.foregroundAccountKey === accountKey && GatewayConnection.isReady) {
			return;
		}
		GatewayConnection.recoverForegroundSession(accountKey);
	}

	private continueForegroundRecovery(accountKey: string, outcome: AccountActivationOutcome): void {
		if (outcome.kind === 'ready') {
			return;
		}
		logger.warn(`Account ${accountKey} authenticated while its foreground gateway is still recovering`, outcome.cause);
		GatewayConnection.recoverForegroundSession(accountKey);
	}

	private shouldManagePushSubscriptions(): boolean {
		return isInstalledPwa();
	}

	private registerPushSubscriptionAfterActivation(): void {
		if (!this.shouldManagePushSubscriptions()) {
			return;
		}
		void (async () => {
			try {
				if (AccountAccess.currentPhase === AccountAccessPhase.ACTION_REQUIRED) {
					return;
				}
				if (await NotificationUtils.isGranted()) {
					await PushSubscriptionService.registerPushSubscription();
				}
			} catch (error) {
				logger.warn('Failed to register the push subscription after account activation', error);
			}
		})();
	}

	private async leaveActiveVoiceChannel(reason: VoiceAccountExitReason): Promise<void> {
		const account = this.currentAccount;
		if (account === null) {
			return;
		}
		await this.voice.leaveVoiceChannelForAccountExit({
			reason,
			accountKey: account.storageKey,
			userId: account.userId,
		});
	}
}

export default new Accounts();
