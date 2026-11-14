// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SnapshotAccountMetadataRow, SnapshotEntityRowMap} from '@app/features/gateway/snapshot/SnapshotEntities';

export interface SnapshotReducerShellState {
	readonly selfUserId: string | null;
	readonly shellUserIds: ReadonlySet<string>;
	readonly userSettings: SnapshotEntityRowMap['user_settings'] | null;
	readonly accountMetadata: SnapshotAccountMetadataRow | null;
}
