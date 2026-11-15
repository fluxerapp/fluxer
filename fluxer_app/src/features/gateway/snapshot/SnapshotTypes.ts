// SPDX-License-Identifier: AGPL-3.0-or-later

export type StateSnapshotEntries = Record<string, Record<string, string>>;

export interface StateSyncCursor {
	readonly sessionId: string | null;
	readonly schemaEpoch: number | null;
	readonly updatedAt: number;
}

export interface StateSnapshotEntry {
	readonly key: string;
	readonly value: string;
}

export interface StateSnapshotCapture {
	readonly entries: StateSnapshotEntries;
	readonly cursor: StateSyncCursor;
}
