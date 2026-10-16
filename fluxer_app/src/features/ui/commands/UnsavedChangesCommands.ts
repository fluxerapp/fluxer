// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TabData} from '@app/features/ui/state/UnsavedChanges';
import UnsavedChanges from '@app/features/ui/state/UnsavedChanges';

export function setUnsavedChanges(tabId: string, hasChanges: boolean): void {
	UnsavedChanges.setUnsavedChanges(tabId, hasChanges);
}

export function triggerFlashEffect(tabId: string): void {
	UnsavedChanges.triggerFlash(tabId);
}

export function clearUnsavedChanges(tabId: string): void {
	UnsavedChanges.clearUnsavedChanges(tabId);
}

export function setTabData(tabId: string, data: TabData): void {
	UnsavedChanges.setTabData(tabId, data);
}
