// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type RuntimeConfigSnapshot,
	runtimeInstanceKey,
	runtimeSnapshotFromDiscovery,
} from '@app/features/app/state/InstanceSnapshotStore';
import {desktopLocalApiEndpoint, isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';
import {randomUuid} from '@app/features/platform/utils/RandomUuid';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {
	DesktopRuntimeAbort,
	DesktopRuntimeCommit,
	DesktopRuntimeCommittedTransactionRequest,
	DesktopRuntimeConfigAPI,
	DesktopRuntimeDeactivation,
	DesktopRuntimePreparation,
	DesktopRuntimeTransactionRequest,
} from '@fluxer/desktop_ipc/src/LocalAppRuntimeContract';
import {parseInstanceDiscoveryDocument} from '@fluxer/instance_bootstrap/src/Discovery';

export interface PreparedDesktopRuntime {
	readonly preparation: DesktopRuntimePreparation;
	readonly snapshot: RuntimeConfigSnapshot;
	readonly transportApiEndpoint: string;
}

export interface CommittedDesktopRuntime {
	readonly preparation: DesktopRuntimePreparation;
	readonly committedRevision: number;
	readonly previousPublishedRevision: number;
}

export class DesktopRuntimeTransactionError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'DesktopRuntimeTransactionError';
	}
}

class DesktopRuntimeTransactionRollbackError extends AggregateError {
	constructor(operationError: unknown, rollbackError: unknown) {
		super(
			[operationError, rollbackError],
			'Desktop runtime transaction failed and its native routing could not be restored',
		);
		this.name = 'DesktopRuntimeTransactionRollbackError';
	}
}

export function runtimeTransportApiEndpoint(snapshot: RuntimeConfigSnapshot): string {
	if (!isDesktopLocalAppDocument()) {
		return snapshot.apiEndpoint;
	}
	const instanceKey = runtimeInstanceKey(snapshot);
	if (instanceKey === null) {
		throw new DesktopRuntimeTransactionError('Desktop transport requires a usable runtime instance key');
	}
	return desktopLocalApiEndpoint(instanceKey);
}

export function requiresDesktopRuntimeTransaction(): boolean {
	return isDesktopLocalAppDocument();
}

class DesktopRuntimeTransactionClient {
	private publishedRevision = 0;

	async prepare(snapshot: RuntimeConfigSnapshot, signal: AbortSignal | null): Promise<PreparedDesktopRuntime> {
		const api = requireDesktopRuntimeConfigAPI();
		const instanceKey = requireRuntimeInstanceKey(snapshot);
		const preparationId = randomUuid();
		let abortOperation: Promise<DesktopRuntimeAbort> | null = null;
		const abort = (): Promise<DesktopRuntimeAbort> => {
			abortOperation ??= api.abort({preparationId});
			return abortOperation;
		};
		const handleAbort = (): void => {
			void abort().catch(() => undefined);
		};
		try {
			signal?.throwIfAborted();
			signal?.addEventListener('abort', handleAbort, {once: true});
			const preparation = await api.prepare({preparationId, instanceKey});
			signal?.throwIfAborted();
			if (preparation.preparationId !== preparationId) {
				throw new DesktopRuntimeTransactionError('Desktop runtime returned a different preparation identity');
			}
			return this.validatePreparation(preparation, instanceKey);
		} catch (error) {
			try {
				await abort();
			} catch (rollbackError) {
				throw new DesktopRuntimeTransactionRollbackError(error, rollbackError);
			}
			throw normalizeDesktopRuntimeError(error, 'Desktop runtime preparation failed');
		} finally {
			signal?.removeEventListener('abort', handleAbort);
		}
	}

	async commit(prepared: PreparedDesktopRuntime): Promise<CommittedDesktopRuntime> {
		const preparation = prepared.preparation;
		const api = requireDesktopRuntimeConfigAPI();
		try {
			this.validatePreparedRuntime(prepared);
		} catch (error) {
			return await this.abortAfterCommitFailure(api, preparation, error);
		}
		const commit: DesktopRuntimeCommit = await api
			.commit(transactionRequest(preparation))
			.catch((error: unknown) => this.abortAfterCommitFailure(api, preparation, error));
		try {
			const committedRevision = requireRevision(commit.revision, false);
			if (
				commit.preparationId !== preparation.preparationId ||
				commit.instanceKey !== preparation.instanceKey ||
				commit.baseRevision !== preparation.baseRevision ||
				committedRevision !== preparation.baseRevision + 1 ||
				committedRevision <= this.publishedRevision
			) {
				throw new DesktopRuntimeTransactionError('Desktop runtime commit does not match its preparation');
			}
			return {
				preparation,
				committedRevision,
				previousPublishedRevision: this.publishedRevision,
			};
		} catch (error) {
			return await this.abortAfterCommitFailure(api, preparation, error);
		}
	}

	publish(committed: CommittedDesktopRuntime): void {
		if (
			this.publishedRevision !== committed.previousPublishedRevision ||
			committed.committedRevision <= this.publishedRevision
		) {
			throw new DesktopRuntimeTransactionError('Desktop runtime commit was superseded before renderer publication');
		}
		this.publishedRevision = committed.committedRevision;
	}

	rollbackPublished(committed: CommittedDesktopRuntime): void {
		if (this.publishedRevision !== committed.committedRevision) {
			throw new DesktopRuntimeTransactionError('Published desktop runtime was superseded before renderer rollback');
		}
		this.publishedRevision = committed.previousPublishedRevision;
	}

	async abort(prepared: PreparedDesktopRuntime): Promise<void> {
		this.validatePreparedRuntime(prepared);
		const result = await requireDesktopRuntimeConfigAPI().abort({
			preparationId: prepared.preparation.preparationId,
		});
		if (result.disposition !== 'aborted') {
			throw new DesktopRuntimeTransactionError(`Desktop runtime preparation abort returned ${result.disposition}`);
		}
	}

	async finalize(committed: CommittedDesktopRuntime): Promise<void> {
		const api = requireDesktopRuntimeConfigAPI();
		const request = committedTransactionRequest(committed);
		const finalized = await api.finalize(request);
		if (
			finalized.preparationId !== request.preparationId ||
			finalized.instanceKey !== request.instanceKey ||
			finalized.baseRevision !== request.baseRevision ||
			finalized.committedRevision !== request.committedRevision
		) {
			throw new DesktopRuntimeTransactionError('Desktop runtime finalization acknowledgement is not exact');
		}
	}

	async rollback(committed: CommittedDesktopRuntime): Promise<void> {
		const rollback = await requireDesktopRuntimeConfigAPI().rollback(committedTransactionRequest(committed));
		if (!Number.isSafeInteger(rollback.revision) || rollback.revision <= committed.committedRevision) {
			throw new DesktopRuntimeTransactionError('Desktop runtime rollback returned an invalid revision');
		}
		if (rollback.activeInstanceKey !== committed.preparation.baseActiveInstanceKey) {
			throw new DesktopRuntimeTransactionError('Desktop runtime rollback restored a different base instance');
		}
	}

	async deactivate(rendererActiveInstanceKey: string): Promise<void> {
		const deactivation: DesktopRuntimeDeactivation = await requireDesktopRuntimeConfigAPI().deactivate({
			rendererActiveInstanceKey,
		});
		const revision = requireRevision(deactivation.revision, true);
		if (revision <= this.publishedRevision) {
			throw new DesktopRuntimeTransactionError('Desktop runtime deactivation returned a stale revision');
		}
		if (deactivation.deactivatedInstanceKey.trim().length === 0) {
			throw new DesktopRuntimeTransactionError('Desktop runtime deactivation returned an invalid instance identity');
		}
		if (deactivation.deactivatedInstanceKey !== rendererActiveInstanceKey) {
			throw new DesktopRuntimeTransactionError('Desktop runtime deactivation cleared a different instance');
		}
		this.publishedRevision = revision;
	}

	private validatePreparation(
		preparation: DesktopRuntimePreparation,
		expectedInstanceKey: string,
	): PreparedDesktopRuntime {
		const baseRevision = requireRevision(preparation.baseRevision, true);
		if (preparation.baseActiveInstanceKey !== null && preparation.baseActiveInstanceKey.trim().length === 0) {
			throw new DesktopRuntimeTransactionError(
				'Desktop runtime preparation contains an invalid base instance identity',
			);
		}
		if (baseRevision < this.publishedRevision) {
			throw new DesktopRuntimeTransactionError('Desktop runtime preparation was superseded before validation');
		}
		const snapshot = runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(preparation.document));
		const documentInstanceKey = runtimeInstanceKey(snapshot);
		if (preparation.instanceKey !== expectedInstanceKey || documentInstanceKey !== expectedInstanceKey) {
			throw new DesktopRuntimeTransactionError('Desktop runtime preparation contains a different instance identity');
		}
		if (preparation.remoteApiEndpoint !== snapshot.apiEndpoint) {
			throw new DesktopRuntimeTransactionError(
				'Desktop runtime preparation remote endpoint does not match its discovery document',
			);
		}
		const transportApiEndpoint = runtimeTransportApiEndpoint(snapshot);
		if (preparation.apiEndpoint !== transportApiEndpoint) {
			throw new DesktopRuntimeTransactionError(
				'Desktop runtime preparation transport does not match its discovery document',
			);
		}
		return {preparation, snapshot, transportApiEndpoint};
	}

	private validatePreparedRuntime(prepared: PreparedDesktopRuntime): void {
		const preparation = prepared.preparation;
		requireRevision(preparation.baseRevision, true);
		const instanceKey = requireRuntimeInstanceKey(prepared.snapshot);
		if (
			preparation.preparationId.length === 0 ||
			preparation.instanceKey !== instanceKey ||
			preparation.remoteApiEndpoint !== prepared.snapshot.apiEndpoint ||
			preparation.apiEndpoint !== prepared.transportApiEndpoint ||
			prepared.transportApiEndpoint !== runtimeTransportApiEndpoint(prepared.snapshot)
		) {
			throw new DesktopRuntimeTransactionError('Prepared desktop runtime is internally inconsistent');
		}
	}

	private async abortAfterCommitFailure(
		api: DesktopRuntimeConfigAPI,
		preparation: DesktopRuntimePreparation,
		operationError: unknown,
	): Promise<never> {
		try {
			const aborted = await api.abort({preparationId: preparation.preparationId});
			switch (aborted.disposition) {
				case 'aborted':
					break;
				case 'rolled-back':
					if (aborted.activeInstanceKey !== preparation.baseActiveInstanceKey) {
						throw new DesktopRuntimeTransactionError('Desktop runtime abort restored a different base instance');
					}
					break;
				case 'absent':
					throw new DesktopRuntimeTransactionError('Desktop runtime abort lost its prepared transaction');
			}
		} catch (rollbackError) {
			throw new DesktopRuntimeTransactionRollbackError(operationError, rollbackError);
		}
		throw normalizeDesktopRuntimeError(operationError, 'Desktop runtime commit failed');
	}
}

function requireDesktopRuntimeConfigAPI(): DesktopRuntimeConfigAPI {
	if (!isDesktopLocalAppDocument()) {
		throw new DesktopRuntimeTransactionError('Desktop runtime transaction requires the desktop app document');
	}
	const api = getElectronAPI()?.desktopRuntimeConfig;
	if (api === undefined) {
		throw new DesktopRuntimeTransactionError('Desktop runtime transaction API is unavailable');
	}
	return api;
}

function requireRuntimeInstanceKey(snapshot: RuntimeConfigSnapshot): string {
	const instanceKey = runtimeInstanceKey(snapshot);
	if (instanceKey === null) {
		throw new DesktopRuntimeTransactionError('Desktop runtime transaction requires a usable instance key');
	}
	return instanceKey;
}

function requireRevision(revision: number, allowZero: boolean): number {
	if (!Number.isSafeInteger(revision) || revision < (allowZero ? 0 : 1)) {
		throw new DesktopRuntimeTransactionError(`Desktop runtime transaction has an invalid revision: ${revision}`);
	}
	return revision;
}

function transactionRequest(preparation: DesktopRuntimePreparation): DesktopRuntimeTransactionRequest {
	return {
		preparationId: preparation.preparationId,
		instanceKey: preparation.instanceKey,
		baseRevision: preparation.baseRevision,
	};
}

function committedTransactionRequest(committed: CommittedDesktopRuntime): DesktopRuntimeCommittedTransactionRequest {
	return {
		...transactionRequest(committed.preparation),
		committedRevision: committed.committedRevision,
	};
}

function normalizeDesktopRuntimeError(error: unknown, fallback: string): Error {
	if (error instanceof Error) {
		return error;
	}
	return new DesktopRuntimeTransactionError(fallback);
}

export default new DesktopRuntimeTransactionClient();
