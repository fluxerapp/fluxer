// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import type {InstanceSnapshotResolution} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig, {
	type CommittedRuntimeConfig,
	type PreparedRuntimeConfig,
	type RuntimeConfigSnapshot,
} from '@app/features/app/state/RuntimeConfig';
import accountStorage, {
	type AccountPresenceIntent,
	type QualifiedStoredAccount,
	type StoredAccount,
	type StoredAccountInventory,
	type StoredAccountInventoryReplacement,
	type StoredAccountList,
	type UserData,
} from '@app/features/auth/state/AccountStorage';
import Sudo from '@app/features/auth/state/AuthSudo';
import {
	AccountScopedWork,
	type AccountScopedWorkTransitionReason,
} from '@app/features/platform/state/AccountScopedWork';
import type {AuthSessionTransitionDependencies} from '@app/features/platform/state/auth_session/AuthSessionTransitionGate';
import {
	DurableSessionCredentialMirror,
	type SessionCredentialMirror,
} from '@app/features/platform/state/auth_session/SessionCredentialMirror';
import AppStorage, {
	activateAppStorageScope,
	deleteAppStorageScope,
	getAppStorageScope,
} from '@app/features/platform/state/PersistentStorage';
import {mirrorGatewayPrebootSession} from '@app/features/platform/state/PrebootMirror';
import {type ResetClientStateOptions, resetClientState} from '@app/features/platform/state/ResetClientState';
import type {InstanceHTTPTarget} from '@app/features/platform/transport/InstanceHTTP';
import {instanceRequest, instanceRequestWithinAccountTransition} from '@app/features/platform/transport/InstanceHTTP';
import LocalPresence from '@app/features/presence/state/LocalPresence';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {UserSettingsAccountTransitionCheckpoint} from '@app/features/user/state/UserSettings';
import {Headers} from '@fluxer/constants/src/Headers';

export interface AuthSessionAccountStorage {
	getAllAccounts(): Promise<StoredAccountList>;
	getAccountInventory(): Promise<StoredAccountInventory>;
	replaceInventoryEntry(request: StoredAccountInventoryReplacement): Promise<QualifiedStoredAccount>;
	stashAccountData(
		userId: string,
		token: string | null,
		userData?: UserData,
		instance?: RuntimeConfigSnapshot,
		presenceIntent?: AccountPresenceIntent | null,
	): Promise<void>;
	upsertAccount(record: StoredAccount, currentInstance: RuntimeConfigSnapshot): Promise<void>;
	restoreAccountData(accountKey: string): Promise<StoredAccount | null>;
	deleteAccount(accountKey: string): Promise<void>;
	updateAccountValidity(accountKey: string, isValid: boolean, expectedToken?: string): Promise<void>;
}

export interface AuthSessionHttpOptions {
	readonly headers?: Record<string, string>;
	readonly auth?: 'session' | 'none';
	readonly target: InstanceHTTPTarget;
	readonly timeoutMs?: number;
	readonly retries?: number;
}

export interface AuthSessionHttp {
	get<T>(url: string, options: AuthSessionHttpOptions): Promise<{body: T}>;
}

export interface AuthSessionDependencies {
	readonly accountStorage: AuthSessionAccountStorage;
	readonly credentialMirror: SessionCredentialMirror;
	readonly http: AuthSessionHttp;
	readonly getRuntimeSnapshot: () => RuntimeConfigSnapshot | null;
	readonly resolveRuntimeEndpoint: (input: string) => Promise<InstanceSnapshotResolution>;
	readonly getStorageScope: () => string;
	readonly resolveAndPrepareRuntimeSnapshot: (snapshot: RuntimeConfigSnapshot) => Promise<PreparedRuntimeConfig>;
	readonly prepareRuntimeSnapshot: (snapshot: RuntimeConfigSnapshot) => Promise<PreparedRuntimeConfig>;
	readonly commitRuntimeSnapshot: (prepared: PreparedRuntimeConfig) => Promise<CommittedRuntimeConfig>;
	readonly publishRuntimeSnapshot: (committed: CommittedRuntimeConfig) => void;
	readonly rollbackPublishedRuntime: (committed: CommittedRuntimeConfig) => void;
	readonly finalizeRuntimeSnapshot: (committed: CommittedRuntimeConfig) => Promise<void>;
	readonly rollbackRuntimeSnapshot: (committed: CommittedRuntimeConfig) => Promise<void>;
	readonly abortRuntimeSnapshot: (prepared: PreparedRuntimeConfig) => Promise<void>;
	readonly deactivateRuntime: () => Promise<void>;
	readonly allowsCrossInstanceSwitching: () => boolean;
	readonly activateStorageScope: (accountKey: string | null) => Promise<void>;
	readonly resetClientState: (options: ResetClientStateOptions, accountScope?: string) => Promise<void>;
	readonly deleteAccountStorageScope: (accountKey: string) => Promise<void>;
	readonly closeLayers: () => void | Promise<void>;
	readonly clearSudoToken: () => void;
	readonly sendInvisiblePresence: (reason: AccountScopedWorkTransitionReason) => void | Promise<void>;
	readonly cleanupGatewaySession: (reason: AccountScopedWorkTransitionReason) => void | Promise<void>;
	readonly resetSyncedUserSettings: (reason: AccountScopedWorkTransitionReason) => Promise<void>;
	readonly captureSyncedUserSettingsCheckpoint: () => Promise<UserSettingsAccountTransitionCheckpoint>;
	readonly restoreSyncedUserSettingsCheckpoint: (checkpoint: UserSettingsAccountTransitionCheckpoint) => Promise<void>;
	readonly accountScopedWork: AuthSessionTransitionDependencies;
	readonly captureLocalPresenceIntent: () => AccountPresenceIntent | null;
	readonly restoreLocalPresenceIntent: (intent: AccountPresenceIntent | null | undefined) => void;
	readonly now: () => number;
}

export function createDefaultAuthSessionDependencies(): AuthSessionDependencies {
	return {
		accountStorage,
		credentialMirror: new DurableSessionCredentialMirror(AppStorage, mirrorGatewayPrebootSession),
		http: {
			get: (url, options) => instanceRequest({method: 'GET', path: url, ...options}),
		},
		getRuntimeSnapshot: () => RuntimeConfig.getSnapshotOrNull(),
		resolveRuntimeEndpoint: (input) => RuntimeConfig.resolveEndpoint({input, signal: null}),
		getStorageScope: () => getAppStorageScope(),
		resolveAndPrepareRuntimeSnapshot: (snapshot) =>
			RuntimeConfig.resolveAndPrepareSnapshot({
				snapshot,
				signal: null,
			}),
		prepareRuntimeSnapshot: (snapshot) => RuntimeConfig.prepareSnapshot({snapshot, signal: null}),
		commitRuntimeSnapshot: (prepared) => RuntimeConfig.commitPreparedSnapshot(prepared),
		publishRuntimeSnapshot: (committed) => RuntimeConfig.publishCommittedSnapshot(committed),
		rollbackPublishedRuntime: (committed) => RuntimeConfig.rollbackPublishedSnapshot(committed),
		finalizeRuntimeSnapshot: (committed) => RuntimeConfig.finalizeCommittedSnapshot(committed),
		rollbackRuntimeSnapshot: (committed) => RuntimeConfig.rollbackCommittedSnapshot(committed),
		abortRuntimeSnapshot: (prepared) => RuntimeConfig.abortPreparedSnapshot(prepared),
		deactivateRuntime: () => RuntimeConfig.deactivate(),
		allowsCrossInstanceSwitching: () => isDesktop(),
		activateStorageScope: (accountKey) => activateAppStorageScope(accountKey),
		resetClientState: (options, accountScope) => resetClientState(options, accountScope),
		deleteAccountStorageScope: (accountKey) => deleteAppStorageScope(accountKey),
		closeLayers: async () => {
			const module = await import('@app/features/ui/state/LayerManager');
			module.default.closeAll();
			const popoutModule = await import('@app/features/voice/state/PopoutWindowManager');
			popoutModule.default.closeAll();
		},
		clearSudoToken: () => Sudo.clearToken(),
		sendInvisiblePresence: (reason) =>
			import('@app/features/gateway/transport/GatewayConnection').then((module) => {
				module.default.sendInvisiblePresenceForCurrentSession(reason);
			}),
		cleanupGatewaySession: (reason) =>
			import('@app/features/gateway/transport/GatewayConnection').then((module) => {
				return module.default.retireForAccountTransition(reason);
			}),
		resetSyncedUserSettings: async () => {
			const module = await import('@app/features/user/state/UserSettings');
			module.default.handleAccountTransition();
		},
		captureSyncedUserSettingsCheckpoint: async () => {
			const module = await import('@app/features/user/state/UserSettings');
			return module.default.captureAccountTransitionCheckpoint();
		},
		restoreSyncedUserSettingsCheckpoint: async (checkpoint) => {
			const module = await import('@app/features/user/state/UserSettings');
			await module.default.restoreAccountTransitionCheckpoint(checkpoint);
		},
		accountScopedWork: {
			runSuspended: (reason, operation) =>
				AccountScopedWork.runSuspended(reason, (suspension) =>
					operation({
						notifyLogout: async (target, token) => {
							await instanceRequestWithinAccountTransition(
								{
									method: 'POST',
									path: Endpoints.AUTH_LOGOUT,
									target,
									auth: 'none',
									headers: {[Headers.AUTHORIZATION]: token},
									timeoutMs: 5000,
									retries: 0,
								},
								suspension,
							);
						},
					}),
				),
		},
		captureLocalPresenceIntent: () => LocalPresence.captureIntent(),
		restoreLocalPresenceIntent: (intent) => LocalPresence.restoreIntent(intent),
		now: () => Date.now(),
	};
}
