// SPDX-License-Identifier: AGPL-3.0-or-later

import Theme from '@app/features/theme/state/Theme';
import {
	THEME_STUDIO_DARK_DEFAULT_VARIABLE_VALUES,
	THEME_STUDIO_LIGHT_DEFAULT_VARIABLE_VALUES,
} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeConstants';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import {ThemeTypes} from '@fluxer/constants/src/UserConstants';

export interface ThemeHydrationResult {
	systemPrefersDark: boolean;
	defaultVariableValues: Readonly<Record<string, string>>;
}

export function useThemeHydration(themePreference: ThemeType): ThemeHydrationResult {
	const systemPrefersDark = Theme.systemPrefersDark;
	const resolvedTheme =
		themePreference === ThemeTypes.SYSTEM ? (systemPrefersDark ? ThemeTypes.DARK : ThemeTypes.LIGHT) : themePreference;
	const defaultVariableValues =
		resolvedTheme === ThemeTypes.LIGHT
			? THEME_STUDIO_LIGHT_DEFAULT_VARIABLE_VALUES
			: THEME_STUDIO_DARK_DEFAULT_VARIABLE_VALUES;
	return {systemPrefersDark, defaultVariableValues};
}
