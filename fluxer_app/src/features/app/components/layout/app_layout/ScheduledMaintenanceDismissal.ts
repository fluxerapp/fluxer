// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import type {MaintenanceStatus} from '@app/features/user/state/StatusPage';

const SCHEDULED_MAINTENANCE_DISMISS_KEY_PREFIX = 'fluxer_scheduled_maintenance_dismissed:';

function getLegacyDismissKey(maintenanceId: string): string {
	return `${SCHEDULED_MAINTENANCE_DISMISS_KEY_PREFIX}${maintenanceId}`;
}

function getDismissKey(maintenanceId: string, status: MaintenanceStatus): string {
	return `${SCHEDULED_MAINTENANCE_DISMISS_KEY_PREFIX}${maintenanceId}:${status}`;
}

export function isScheduledMaintenanceNagbarDismissed(
	maintenanceId: string | null | undefined,
	status: MaintenanceStatus,
): boolean {
	if (!maintenanceId) {
		return false;
	}
	return (
		AppStorage.getItem(getDismissKey(maintenanceId, status)) === '1' ||
		(status === 'scheduled' && AppStorage.getItem(getLegacyDismissKey(maintenanceId)) === '1')
	);
}

export function dismissScheduledMaintenanceNagbar(maintenanceId: string, status: MaintenanceStatus): void {
	AppStorage.setItem(getDismissKey(maintenanceId, status), '1');
}
