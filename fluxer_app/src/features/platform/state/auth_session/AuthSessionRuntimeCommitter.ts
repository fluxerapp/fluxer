// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CommittedRuntimeConfig, PreparedRuntimeConfig} from '@app/features/app/state/RuntimeConfig';
import {runInAction} from 'mobx';

export interface AuthSessionRuntimeCommitDependencies {
	readonly commitRuntimeSnapshot: (prepared: PreparedRuntimeConfig) => Promise<CommittedRuntimeConfig>;
	readonly publishRuntimeSnapshot: (committed: CommittedRuntimeConfig) => void;
	readonly rollbackPublishedRuntime: (committed: CommittedRuntimeConfig) => void;
	readonly finalizeRuntimeSnapshot: (committed: CommittedRuntimeConfig) => Promise<void>;
	readonly rollbackRuntimeSnapshot: (committed: CommittedRuntimeConfig) => Promise<void>;
	readonly abortRuntimeSnapshot: (prepared: PreparedRuntimeConfig) => Promise<void>;
}

export interface AuthSessionRuntimePublication {
	readonly publishSessionState: () => void;
	readonly rollbackSessionState: () => void;
}

export class AuthSessionRuntimeCommitter {
	constructor(private readonly dependencies: AuthSessionRuntimeCommitDependencies) {}

	async commit(prepared: PreparedRuntimeConfig | null, publication: AuthSessionRuntimePublication): Promise<void> {
		if (prepared === null) {
			try {
				runInAction(publication.publishSessionState);
			} catch (error) {
				try {
					runInAction(publication.rollbackSessionState);
				} catch (rollbackError) {
					throw new AggregateError([error, rollbackError], 'Failed to publish and restore the account session');
				}
				throw error;
			}
			return;
		}
		const committed = await this.dependencies.commitRuntimeSnapshot(prepared);
		try {
			runInAction(() => {
				this.dependencies.publishRuntimeSnapshot(committed);
				publication.publishSessionState();
			});
			await this.dependencies.finalizeRuntimeSnapshot(committed);
		} catch (error) {
			const rollbackErrors: Array<unknown> = [];
			runInAction(() => {
				try {
					this.dependencies.rollbackPublishedRuntime(committed);
				} catch (rollbackPublishedError) {
					rollbackErrors.push(rollbackPublishedError);
				}
				try {
					publication.rollbackSessionState();
				} catch (rollbackSessionError) {
					rollbackErrors.push(rollbackSessionError);
				}
			});
			try {
				await this.dependencies.rollbackRuntimeSnapshot(committed);
			} catch (rollbackRuntimeError) {
				rollbackErrors.push(rollbackRuntimeError);
			}
			if (rollbackErrors.length > 0) {
				throw new AggregateError([error, ...rollbackErrors], 'Failed to commit and restore the account runtime');
			}
			throw error;
		}
	}

	async abort(prepared: PreparedRuntimeConfig): Promise<void> {
		await this.dependencies.abortRuntimeSnapshot(prepared);
	}
}
