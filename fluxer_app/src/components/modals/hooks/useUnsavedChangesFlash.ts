/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import React from 'react';
import * as UnsavedChangesActionCreators from '~/actions/UnsavedChangesActionCreators';
import UnsavedChangesStore from '~/stores/UnsavedChangesStore';

export function useUnsavedChangesFlash(selectedTab: string | null) {
	const unsavedChangesStore = UnsavedChangesStore;

	const [flashBanner, setFlashBanner] = React.useState(false);
	const [lastFlashTrigger, setLastFlashTrigger] = React.useState(0);

	const currentTabId = selectedTab || '';
	const showUnsavedBanner = unsavedChangesStore.unsavedChanges[currentTabId] || false;
	const flashTrigger = unsavedChangesStore.flashTriggers[currentTabId] || 0;
	const tabData = unsavedChangesStore.tabData[currentTabId] || {};

	React.useEffect(() => {
		if (flashTrigger > lastFlashTrigger) {
			setFlashBanner(true);
			setLastFlashTrigger(flashTrigger);
			setTimeout(() => setFlashBanner(false), 300);
		}
	}, [flashTrigger, lastFlashTrigger]);

	const checkUnsavedChanges = React.useCallback(
		(tabId?: string): boolean => {
			const checkTabId = tabId || selectedTab;
			if (!checkTabId) return false;

			if (unsavedChangesStore.unsavedChanges[checkTabId]) {
				UnsavedChangesActionCreators.triggerFlashEffect(checkTabId);
				return true;
			}
			return false;
		},
		[selectedTab, unsavedChangesStore.unsavedChanges],
	);

	return {
		showUnsavedBanner,
		flashBanner,
		tabData,
		checkUnsavedChanges,
	};
}
