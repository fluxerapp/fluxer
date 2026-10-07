// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import type {DesktopLegacyImportResult} from '@app/features/platform/state/DesktopLegacyImport';
import type {LegacyAppStorageMigrationResult} from '@app/features/platform/state/LegacyAppStorageMigration';
import type {LegacySessionReconciliationResult} from '@app/features/platform/state/LegacySessionReconciliation';

export interface AppStorageSessionAccount {
	readonly accountKey: string;
	readonly userId: string;
	readonly token: string;
	readonly instance: RuntimeConfigSnapshot;
}

export interface AppStorageFinalizationResult {
	readonly scope: string;
	readonly migration: LegacyAppStorageMigrationResult | null;
	readonly desktopImport: DesktopLegacyImportResult | null;
	readonly desktopBackendInstalled: boolean;
	readonly reconciliation: LegacySessionReconciliationResult | null;
}

export interface AppStorageBootstrapHandle {
	finalizeAfterSessionResolution(account: AppStorageSessionAccount | null): Promise<AppStorageFinalizationResult>;
}
