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

import type {SettingsTab} from './settingsConstants';
import {getMatchedTabTypes, type SettingsSearchResult, searchSettings} from './settingsSearchIndex';

const STAFF_ONLY_CATEGORY = 'staff_only';

export interface FilteredSettingsResult {
	groupedTabs: Record<string, Array<SettingsTab>>;
	searchResults: Array<SettingsSearchResult>;
}

export const filterSettingsTabsForDeveloperMode = (
	groupedTabs: Record<string, Array<SettingsTab>>,
	isDeveloper: boolean,
) => {
	if (isDeveloper) {
		return groupedTabs;
	}

	const filtered: Record<string, Array<SettingsTab>> = {};
	Object.entries(groupedTabs).forEach(([category, tabs]) => {
		if (category === STAFF_ONLY_CATEGORY) {
			return;
		}
		filtered[category] = tabs;
	});

	return filtered;
};

export const filterSettingsTabsByQuery = (
	groupedTabs: Record<string, Array<SettingsTab>>,
	query: string,
): FilteredSettingsResult => {
	const trimmedQuery = query.trim();
	if (trimmedQuery.length === 0) {
		return {groupedTabs, searchResults: []};
	}

	const allTabs = Object.values(groupedTabs).flat();
	const searchResults = searchSettings(trimmedQuery, allTabs);
	const matchedTabTypes = getMatchedTabTypes(searchResults);

	const filtered: Record<string, Array<SettingsTab>> = {};

	Object.entries(groupedTabs).forEach(([category, tabs]) => {
		const matchedTabs = tabs.filter((tab) => matchedTabTypes.has(tab.type));

		if (matchedTabs.length > 0) {
			filtered[category] = matchedTabs;
		}
	});

	return {groupedTabs: filtered, searchResults};
};
