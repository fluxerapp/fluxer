// SPDX-License-Identifier: AGPL-3.0-or-later

import accountStorage, {BrowserAccountStorageUnavailableError} from '@app/features/auth/state/AccountStorage';
import {accountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import type {AppStorageSessionAccount} from '@app/features/platform/state/AppStorageBootstrapContract';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import {readStoredActiveAccountKey} from '@app/features/platform/state/auth_session/AuthSessionStorage';
import {
	getDesktopAccountStorageAPI,
	requireDesktopAccountRecord,
} from '@app/features/platform/state/DesktopAccountStorageAccess';
import {
	commitDesktopLegacyImport,
	type DesktopLegacyImportRequest,
	type DesktopLegacyImportResult,
	DesktopLegacyImportStatus,
	resetDesktopLegacyImportCandidate,
	resolveDesktopLegacyImportRequest,
	runDesktopLegacyImport,
} from '@app/features/platform/state/DesktopLegacyImport';
import {recordDesktopLegacyImportPhase} from '@app/features/platform/state/DesktopLegacyImportState';
import {
	createDesktopPersistentStorageBackend,
	getDesktopStorageAPI,
} from '@app/features/platform/state/DesktopPersistentStorageBackend';
import {
	LegacySessionReconciliationOutcome,
	type LegacySessionReconciliationResult,
	LegacySessionReconciliationSkipReason,
	readLegacySessionCredentials,
} from '@app/features/platform/state/LegacySessionReconciliation';
import {GLOBAL_APP_STORAGE_SCOPE} from '@app/features/platform/state/PersistentStorage';
import {
	installPersistentStorageBackend,
	type PersistentStorageBackend,
} from '@app/features/platform/state/PersistentStorageBackend';
import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	DESKTOP_LEGACY_AUTHORITY_MARKER_KEY,
	DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE,
	DESKTOP_LEGACY_IMPORT_MARKER_KEY,
	DesktopLegacyImportPhase,
	type DesktopStoreStatus,
	readDesktopLegacyImportPhase,
} from '@fluxer/desktop_ipc/src/StorageContract';

const logger = new Logger('AppStorageDesktopAuthority');

interface CommittedDesktopStorageAuthority {
	readonly result: DesktopLegacyImportResult;
	readonly backend: PersistentStorageBackend;
}

export interface DesktopStorageImport {
	readonly result: DesktopLegacyImportResult | null;
	readonly candidate: DesktopStorageAuthorityCandidate | null;
}

export class DesktopStorageAuthorityUnavailableError extends Error {
	public constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'DesktopStorageAuthorityUnavailableError';
	}
}

async function readCommittedDesktopStorageAuthority(): Promise<CommittedDesktopStorageAuthority | null> {
	const storage = getDesktopStorageAPI({allowUnavailable: true});
	if (storage === null) {
		return null;
	}
	let status: DesktopStoreStatus;
	try {
		status = await storage.getStatus();
	} catch (error) {
		throw new DesktopStorageAuthorityUnavailableError('The desktop store status could not be read', {
			cause: error,
		});
	}
	if (!status.available) {
		if (status.authorityExpected) {
			throw new DesktopStorageAuthorityUnavailableError(
				`The desktop store is unavailable after desktop authority was established: ${status.unavailableReason ?? 'unknown reason'}`,
			);
		}
		logger.warn(
			'The committed desktop store needs recovery, serving this boot from the web backend',
			status.unavailableReason ?? status.quarantineReason,
		);
		return null;
	}
	let authority: string | null;
	try {
		authority = await storage.getMarker(DESKTOP_LEGACY_AUTHORITY_MARKER_KEY);
	} catch (error) {
		throw new DesktopStorageAuthorityUnavailableError('The desktop authority marker could not be read', {
			cause: error,
		});
	}
	if (status.quarantined) {
		if (status.authorityExpected) {
			throw new DesktopStorageAuthorityUnavailableError(
				'The authoritative desktop store was quarantined and requires recovery',
			);
		}
		if (authority === DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE) {
			throw new DesktopStorageAuthorityUnavailableError('The quarantined desktop store still claims desktop authority');
		}
		if (authority !== null && authority !== '') {
			throw new DesktopStorageAuthorityUnavailableError('The quarantined desktop authority marker is invalid');
		}
		logger.warn('The desktop store was quarantined, serving this boot from the web backend');
		return null;
	}
	if (authority !== DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE) {
		return null;
	}
	const accounts = getDesktopAccountStorageAPI({allowUnavailable: true});
	if (accounts === null) {
		throw new DesktopStorageAuthorityUnavailableError('The committed desktop account store is unavailable');
	}
	let phaseValue: string | null;
	try {
		phaseValue = await storage.getMarker(DESKTOP_LEGACY_IMPORT_MARKER_KEY);
	} catch (error) {
		throw new DesktopStorageAuthorityUnavailableError('The committed desktop import marker could not be read', {
			cause: error,
		});
	}
	if (readDesktopLegacyImportPhase(phaseValue) !== DesktopLegacyImportPhase.DONE) {
		throw new DesktopStorageAuthorityUnavailableError('The committed desktop import marker is incomplete');
	}
	let accountKeys: Array<string>;
	try {
		accountKeys = (await accounts.getAll()).map((account) => requireDesktopAccountRecord(account).storageKey).sort();
	} catch (error) {
		throw new DesktopStorageAuthorityUnavailableError('The committed desktop accounts could not be read', {
			cause: error,
		});
	}
	return {
		backend: createDesktopPersistentStorageBackend(storage),
		result: {
			status: DesktopLegacyImportStatus.ALREADY_DONE,
			phase: DesktopLegacyImportPhase.DONE,
			refusal: null,
			accountsImported: 0,
			entriesImported: 0,
			accountKeys,
			deferredAccounts: 0,
			failures: 0,
		},
	};
}

async function discardCommittedLegacyHarvest(): Promise<void> {
	const harvestAPI = globalThis.window?.electron?.desktopLegacyHarvest;
	if (harvestAPI == null || typeof harvestAPI.discard !== 'function') {
		return;
	}
	try {
		await harvestAPI.discard();
	} catch (error) {
		logger.warn('The committed legacy harvest could not be discarded, retrying on the next launch', error);
	}
}

function reconciliationMatchesImportedAccounts(
	result: LegacySessionReconciliationResult | null,
	accountKeys: ReadonlyArray<string>,
): boolean {
	if (result === null) {
		return false;
	}
	if (result.outcome === LegacySessionReconciliationOutcome.SKIPPED) {
		return result.reason === LegacySessionReconciliationSkipReason.NO_LIVE_SESSION;
	}
	return result.storageKey !== null && accountKeys.includes(result.storageKey);
}

async function verifyDesktopSession(
	request: DesktopLegacyImportRequest,
	result: LegacySessionReconciliationResult | null,
): Promise<boolean> {
	try {
		if (result === null) {
			return false;
		}
		const currentAccount = request.currentAccount;
		const expectedAccountKey = accountStorageKey(currentAccount.userId, currentAccount.instance);
		if (expectedAccountKey === null) {
			return false;
		}
		const raw = request.legacyStorage;
		const rawMirror = getProtectedLocalStorage();
		const rawPointer = rawMirror === null ? null : readStoredActiveAccountKey(rawMirror);
		const nativePointer = await request.storage.get(GLOBAL_APP_STORAGE_SCOPE, AppStorageKey.AUTH_ACCOUNT_KEY);
		if (rawPointer !== expectedAccountKey || nativePointer?.value !== expectedAccountKey) {
			return false;
		}
		const stored = await request.accounts.get(expectedAccountKey);
		if (stored === null) {
			return false;
		}
		const record = requireDesktopAccountRecord(stored);
		if (record.userId !== currentAccount.userId || record.token !== currentAccount.token) {
			return false;
		}
		if (result.outcome === LegacySessionReconciliationOutcome.SKIPPED) {
			if (result.reason !== LegacySessionReconciliationSkipReason.NO_LIVE_SESSION) {
				return false;
			}
			return readLegacySessionCredentials(raw) === null;
		}
		if (result.storageKey !== expectedAccountKey) {
			return false;
		}
		const live = readLegacySessionCredentials(raw);
		return live !== null && live.userId === currentAccount.userId && live.token === currentAccount.token;
	} catch (error) {
		logger.warn('The desktop session verification failed', error);
		return false;
	}
}

export class DesktopStorageAuthorityCandidate {
	private constructor(
		public readonly request: DesktopLegacyImportRequest,
		public readonly result: DesktopLegacyImportResult,
		private readonly webBackend: PersistentStorageBackend,
	) {}

	public static create(
		request: DesktopLegacyImportRequest,
		result: DesktopLegacyImportResult,
		webBackend: PersistentStorageBackend,
	): DesktopStorageAuthorityCandidate {
		return new DesktopStorageAuthorityCandidate(request, result, webBackend);
	}

	public async commitIfVerified(reconciliation: LegacySessionReconciliationResult | null): Promise<boolean> {
		const ready =
			reconciliationMatchesImportedAccounts(reconciliation, this.result.accountKeys) &&
			(await verifyDesktopSession(this.request, reconciliation));
		if (!ready) {
			logger.warn('The desktop session could not be verified, continuing with the web backend');
			try {
				await resetDesktopLegacyImportCandidate(this.request);
			} catch (error) {
				logger.error('The desktop import candidate could not be rewound', error);
			}
			return false;
		}
		try {
			installPersistentStorageBackend(createDesktopPersistentStorageBackend(this.request.storage));
			await commitDesktopLegacyImport(this.request);
			recordDesktopLegacyImportPhase(DesktopLegacyImportPhase.DONE);
			await discardCommittedLegacyHarvest();
			return true;
		} catch (error) {
			logger.error('The desktop authority commit failed, continuing with the web backend', error);
			installPersistentStorageBackend(this.webBackend);
			recordDesktopLegacyImportPhase(null);
			try {
				await resetDesktopLegacyImportCandidate(this.request);
			} catch (resetError) {
				throw new DesktopStorageAuthorityUnavailableError(
					'The failed desktop authority commit could not be rewound safely',
					{cause: resetError},
				);
			}
			return false;
		}
	}
}

export class DesktopStorageAuthority {
	private constructor(private readonly committed: CommittedDesktopStorageAuthority | null) {}

	public static async prepareBoot(): Promise<DesktopStorageAuthority> {
		const committed = await readCommittedDesktopStorageAuthority();
		if (committed === null) {
			recordDesktopLegacyImportPhase(null);
			return new DesktopStorageAuthority(null);
		}
		recordDesktopLegacyImportPhase(DesktopLegacyImportPhase.DONE);
		await discardCommittedLegacyHarvest();
		installPersistentStorageBackend(committed.backend);
		return new DesktopStorageAuthority(committed);
	}

	public get isCommitted(): boolean {
		return this.committed !== null;
	}

	public get committedResult(): DesktopLegacyImportResult | null {
		return this.committed?.result ?? null;
	}

	public async stageImport(
		currentAccount: AppStorageSessionAccount,
		now: number,
		webBackend: PersistentStorageBackend,
	): Promise<DesktopStorageImport> {
		if (this.committed !== null) {
			throw new Error('A committed desktop storage authority cannot stage another import');
		}
		let result: DesktopLegacyImportResult | null = null;
		try {
			const request = resolveDesktopLegacyImportRequest({
				webBackend,
				accountSource: accountStorage,
				currentAccount: {
					userId: currentAccount.userId,
					token: currentAccount.token,
					instance: currentAccount.instance,
				},
				now,
				legacyStorage: getProtectedLocalStorage(),
			});
			if (request === null) {
				return {result: null, candidate: null};
			}
			result = await runDesktopLegacyImport(request);
			if (
				result.status === DesktopLegacyImportStatus.IMPORTED ||
				result.status === DesktopLegacyImportStatus.ALREADY_DONE
			) {
				return {result, candidate: DesktopStorageAuthorityCandidate.create(request, result, webBackend)};
			}
			if (result.status !== DesktopLegacyImportStatus.DEFERRED) {
				logger.warn('Serving this session from the web backend', result.status, result.refusal);
			}
		} catch (error) {
			if (error instanceof BrowserAccountStorageUnavailableError) {
				throw error;
			}
			logger.error('The desktop storage import could not run, serving this session from the web backend', error);
		}
		return {result, candidate: null};
	}
}
