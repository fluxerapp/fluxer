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

import {ThemeTypes} from '~/Constants';

type ThemeValue = (typeof ThemeTypes)[keyof typeof ThemeTypes];

type ExplicitTheme = typeof ThemeTypes.DARK | typeof ThemeTypes.LIGHT | typeof ThemeTypes.COAL;

const STORAGE_KEY = 'theme';
const EXPLICIT_THEMES = new Set<ThemeValue>([ThemeTypes.DARK, ThemeTypes.LIGHT, ThemeTypes.COAL]);

export function isExplicitTheme(theme: string | null | undefined): theme is ExplicitTheme {
	return typeof theme === 'string' && EXPLICIT_THEMES.has(theme as ThemeValue);
}

export async function loadTheme(): Promise<ExplicitTheme | null> {
	try {
		if (!('localStorage' in window)) {
			return null;
		}
		const stored = window.localStorage.getItem(STORAGE_KEY);
		return isExplicitTheme(stored) ? stored : null;
	} catch {
		return null;
	}
}

export async function persistTheme(theme: string | null | undefined): Promise<void> {
	try {
		if (!isExplicitTheme(theme)) {
			return;
		}

		if (!('localStorage' in window)) {
			return;
		}
		window.localStorage.setItem(STORAGE_KEY, theme);
	} catch {}
}
