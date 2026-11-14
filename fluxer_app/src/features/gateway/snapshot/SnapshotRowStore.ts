// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SnapshotEntity, SnapshotRowOp} from '@app/features/gateway/snapshot/SnapshotEntities';
import type {StateSnapshotEntry, StateSyncCursor} from '@app/features/gateway/snapshot/SnapshotTypes';

export interface SnapshotRowStore {
	has(storageKey: string): boolean;
	get(storageKey: string, entity: SnapshotEntity, key: string): string | null;
	entriesByPrefix(storageKey: string, entity: SnapshotEntity, keyPrefix: string): Iterable<StateSnapshotEntry>;
	apply(storageKey: string, op: SnapshotRowOp): void;
	writeCursor(storageKey: string, cursor: StateSyncCursor): void;
	evict(storageKey: string): void;
}
