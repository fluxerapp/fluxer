// SPDX-License-Identifier: AGPL-3.0-or-later

import AccountAccess, {AccountAccessDecision, AccountAccessPhase} from '@app/features/auth/state/AccountAccess';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {readAuthenticatedRuntimeContext} from '@app/features/auth/state/AuthenticatedRuntime';
import {pruneOrphanedDesktopStateStores} from '@app/features/gateway/snapshot/SnapshotSync';
import BackgroundAccountPresence from '@app/features/gateway/transport/BackgroundAccountPresence';
import {
	backgroundSessionNeedsRestart,
	gatewayEndpointForAccount,
	selectBackgroundAccounts,
} from '@app/features/gateway/transport/BackgroundGatewayAccounts';
import BackgroundGatewaySessions from '@app/features/gateway/transport/BackgroundGatewayConnectionRegistry';
import {BackgroundGatewaySession} from '@app/features/gateway/transport/BackgroundGatewaySession';
import {installDeferredReaction} from '@app/features/gateway/transport/DeferredReaction';
import type {GatewaySocketProperties} from '@app/features/gateway/transport/GatewaySocket';
import {isDesktopNativeGatewayTransportAvailable} from '@app/features/gateway/transport/GatewayWireTransport';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import SessionManager, {type Account} from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getGatewayClientProperties} from '@app/features/platform/utils/ClientInfo';
import {compareStructural, reaction} from 'mobx';

const logger = new Logger('BackgroundGatewaySessionManager');

export const MAX_BACKGROUND_GATEWAY_CONNECTIONS = 6;
export const BACKGROUND_RESUME_STAGGER_MS = 250;

export class InvalidBackgroundGatewayConnectionLimitError extends Error {
	constructor(configured: number) {
		super(`Background gateway connection limit must be a non-negative integer, got ${configured}`);
		this.name = 'InvalidBackgroundGatewayConnectionLimitError';
	}
}

export function readMaxBackgroundGatewayConnections(configured: number | undefined): number {
	if (configured === undefined) {
		return MAX_BACKGROUND_GATEWAY_CONNECTIONS;
	}
	if (!Number.isInteger(configured) || configured < 0) {
		throw new InvalidBackgroundGatewayConnectionLimitError(configured);
	}
	return Math.min(configured, MAX_BACKGROUND_GATEWAY_CONNECTIONS);
}

function normalizeBackgroundStartError(error: unknown): Error {
	return error instanceof Error ? error : new Error(String(error));
}

export class BackgroundGatewaySessionManager {
	private syncRevision = 0;
	private bootstrapped = false;
	private propertiesPromise: Promise<GatewaySocketProperties> | null = null;
	private properties: GatewaySocketProperties | null = null;
	private propertiesGeneration = 0;
	private retryTimer: number | null = null;
	private readonly resumeTimers = new Set<number>();
	private readonly protectedAccountKeys = new Set<string>();
	private disposers: Array<() => void> = [];

	get isBootstrapped(): boolean {
		return this.bootstrapped;
	}

	get connectionCount(): number {
		return BackgroundGatewaySessions.size;
	}

	get cachedProperties(): GatewaySocketProperties | null {
		return this.properties;
	}

	isTransportAvailable(): boolean {
		return isDesktopNativeGatewayTransportAvailable();
	}

	start(): void {
		if (this.bootstrapped || !this.isTransportAvailable()) {
			return;
		}
		this.bootstrapped = true;
		this.disposers.push(
			AccountScopedWork.registerTransition({
				suspend: () => {
					this.invalidateSync();
				},
				resume: () => undefined,
				released: () => this.sync(),
			}),
		);
		this.disposers.push(
			installDeferredReaction(() =>
				reaction(
					() => BackgroundAccountPresence.appearOffline,
					() => {
						for (const session of BackgroundGatewaySessions.values()) {
							session.refreshPresence();
						}
					},
				),
			),
		);
		this.disposers.push(
			installDeferredReaction(() =>
				reaction(
					() => {
						const accounts = SessionManager.accounts.map((account) => ({
							accountKey: getAccountKey(account),
							token: account.token,
							isValid: account.isValid,
							gatewayEndpoint: gatewayEndpointForAccount(account),
							lastActive: account.lastActive,
						}));
						const runtimeContext = SessionManager.isSwitching ? null : readAuthenticatedRuntimeContext();
						return {
							sessionState: SessionManager.state,
							currentAccountKey: SessionManager.currentAccountKey,
							maxConnections:
								runtimeContext === null
									? null
									: readMaxBackgroundGatewayConnections(
											runtimeContext.runtime.features.max_background_gateway_connections,
										),
							accessRevision: AccountAccess.revision,
							accounts,
						};
					},
					() => this.sync(),
					{equals: compareStructural, fireImmediately: true},
				),
			),
		);
		const onlineListener = () => this.resumeAfterConnectivityChange();
		window.addEventListener('online', onlineListener);
		this.disposers.push(() => window.removeEventListener('online', onlineListener));
	}

	shutdown(): void {
		this.bootstrapped = false;
		this.syncRevision += 1;
		this.propertiesGeneration += 1;
		this.propertiesPromise = null;
		this.properties = null;
		this.protectedAccountKeys.clear();
		this.clearRetry();
		this.clearResumeTimers();
		const disposers = this.disposers;
		this.disposers = [];
		for (const dispose of disposers) {
			dispose();
		}
		for (const accountKey of BackgroundGatewaySessions.keys()) {
			this.stop(accountKey);
		}
	}

	protect(accountKey: string): void {
		if (!this.protectedAccountKeys.has(accountKey)) {
			this.protectedAccountKeys.add(accountKey);
			this.invalidateSync();
		}
	}

	unprotect(accountKey: string): void {
		this.protectedAccountKeys.delete(accountKey);
	}

	invalidateSync(): number {
		this.syncRevision += 1;
		return this.syncRevision;
	}

	isCurrentSync(revision: number): boolean {
		return this.bootstrapped && revision === this.syncRevision;
	}

	find(accountKey: string): BackgroundGatewaySession | null {
		return BackgroundGatewaySessions.get(accountKey);
	}

	stop(accountKey: string): void {
		const session = BackgroundGatewaySessions.get(accountKey);
		if (session === null) {
			return;
		}
		BackgroundGatewaySessions.delete(accountKey);
		session.stop();
	}

	adopt(accountKey: string, session: BackgroundGatewaySession): void {
		this.stop(accountKey);
		BackgroundGatewaySessions.set(accountKey, session);
	}

	detachRegistration(accountKey: string): boolean {
		return BackgroundGatewaySessions.delete(accountKey);
	}

	async preloadProperties(): Promise<GatewaySocketProperties> {
		if (this.properties !== null) {
			return this.properties;
		}
		if (this.propertiesPromise === null) {
			const generation = this.propertiesGeneration;
			this.propertiesPromise = getGatewayClientProperties()
				.then((properties) => {
					if (generation !== this.propertiesGeneration) {
						throw new Error('Background gateway client metadata request was superseded');
					}
					this.properties = properties;
					return properties;
				})
				.catch((error: unknown) => {
					if (generation === this.propertiesGeneration) {
						this.propertiesPromise = null;
					}
					throw normalizeBackgroundStartError(error);
				});
		}
		return this.propertiesPromise;
	}

	sync(): void {
		if (
			!this.bootstrapped ||
			!this.isTransportAvailable() ||
			SessionManager.isSwitching ||
			AccountScopedWork.isSuspended
		) {
			return;
		}
		const revision = this.invalidateSync();
		const runtimeContext = readAuthenticatedRuntimeContext();
		if (runtimeContext === null) {
			this.clearRetry();
			for (const accountKey of BackgroundGatewaySessions.keys()) {
				this.stop(accountKey);
			}
			this.pruneSnapshots([]);
			return;
		}
		const {desired, unresolved} = selectBackgroundAccounts({
			accounts: SessionManager.accounts,
			currentAccountKey: SessionManager.currentAccountKey,
			maxConnections: readMaxBackgroundGatewayConnections(
				runtimeContext.runtime.features.max_background_gateway_connections,
			),
		});
		for (const accountKey of BackgroundGatewaySessions.keys()) {
			if (this.protectedAccountKeys.has(accountKey) || unresolved.has(accountKey)) {
				continue;
			}
			const account = desired.get(accountKey);
			const session = BackgroundGatewaySessions.get(accountKey);
			if (session?.hasPendingAuthMutation) {
				continue;
			}
			if (account === undefined || session === null || backgroundSessionNeedsRestart(session.account, account)) {
				this.stop(accountKey);
			}
		}
		for (const [accountKey, account] of desired) {
			if (this.protectedAccountKeys.has(accountKey) || BackgroundGatewaySessions.has(accountKey)) {
				continue;
			}
			void this.startSession(accountKey, account, revision);
		}
		this.scheduleRetry(desired);
		this.pruneSnapshots(desired.keys());
	}

	private pruneSnapshots(desiredAccountKeys: Iterable<string>): void {
		const knownAccountKeys = new Set<string>([
			...BackgroundGatewaySessions.keys(),
			...desiredAccountKeys,
			...this.protectedAccountKeys,
		]);
		const currentAccountKey = SessionManager.currentAccountKey;
		if (currentAccountKey !== null) {
			knownAccountKeys.add(currentAccountKey);
		}
		void pruneOrphanedDesktopStateStores([...knownAccountKeys]).catch((error: unknown) => {
			logger.warn('Failed to prune orphaned app-shell snapshots', error);
		});
	}

	private async startSession(accountKey: string, account: Account, revision: number): Promise<void> {
		const gatewayEndpoint = gatewayEndpointForAccount(account);
		if (gatewayEndpoint === null) {
			return;
		}
		const retryingAfterFailure = AccountAccess.getPhase(accountKey) === AccountAccessPhase.UNAVAILABLE;
		if (retryingAfterFailure && Date.now() < AccountAccess.getRetryEligibleAt(accountKey)) {
			return;
		}
		const decision = await AccountAccess.ensureAccountChecked(
			accountKey,
			retryingAfterFailure ? {force: true, resetFailures: false} : {},
		);
		if (
			!this.isCurrentSync(revision) ||
			this.protectedAccountKeys.has(accountKey) ||
			decision !== AccountAccessDecision.ALLOWED
		) {
			return;
		}
		let properties: GatewaySocketProperties;
		try {
			properties = await this.preloadProperties();
		} catch (error) {
			logger.warn('Failed to gather client metadata for a background gateway session', error);
			AccountAccess.markGatewayUnavailable(accountKey, normalizeBackgroundStartError(error));
			return;
		}
		if (
			!this.isCurrentSync(revision) ||
			this.protectedAccountKeys.has(accountKey) ||
			BackgroundGatewaySessions.has(accountKey)
		) {
			return;
		}
		const session = new BackgroundGatewaySession({
			account,
			gatewayEndpoint,
			properties,
			onStopRequested: () => this.stop(accountKey),
		});
		BackgroundGatewaySessions.set(accountKey, session);
		session.connect();
	}

	private scheduleRetry(desired: ReadonlyMap<string, Account>): void {
		this.clearRetry();
		const now = Date.now();
		let retryAt = Number.POSITIVE_INFINITY;
		for (const accountKey of desired.keys()) {
			if (this.protectedAccountKeys.has(accountKey) || BackgroundGatewaySessions.has(accountKey)) {
				continue;
			}
			if (AccountAccess.getPhase(accountKey) !== AccountAccessPhase.UNAVAILABLE) {
				continue;
			}
			const retryEligibleAt = AccountAccess.getRetryEligibleAt(accountKey);
			if (retryEligibleAt > now) {
				retryAt = Math.min(retryAt, retryEligibleAt);
			}
		}
		if (!Number.isFinite(retryAt)) {
			return;
		}
		this.retryTimer = window.setTimeout(() => {
			this.retryTimer = null;
			this.sync();
		}, retryAt - now);
	}

	private clearRetry(): void {
		if (this.retryTimer !== null) {
			clearTimeout(this.retryTimer);
			this.retryTimer = null;
		}
	}

	private resumeAfterConnectivityChange(): void {
		this.clearResumeTimers();
		const sessions = BackgroundGatewaySessions.values();
		for (let index = 0; index < sessions.length; index += 1) {
			const session = sessions[index];
			const timer = window.setTimeout(() => {
				this.resumeTimers.delete(timer);
				session.probeAfterResume();
			}, index * BACKGROUND_RESUME_STAGGER_MS);
			this.resumeTimers.add(timer);
		}
	}

	private clearResumeTimers(): void {
		for (const timer of this.resumeTimers) {
			clearTimeout(timer);
		}
		this.resumeTimers.clear();
	}
}
