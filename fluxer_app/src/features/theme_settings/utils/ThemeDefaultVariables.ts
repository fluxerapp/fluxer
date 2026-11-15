// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	THEME_DARK_DEFAULT_VARIABLE_VALUES,
	THEME_LIGHT_DEFAULT_VARIABLE_VALUES,
} from '@app/features/theme/variables/ThemeVariableManifest';
import {ThemeTypes} from '@fluxer/constants/src/UserConstants';

export type ThemeBaseKind = 'dark' | 'light';

export function getThemeBaseKind(effectiveTheme: string): ThemeBaseKind {
	return effectiveTheme === ThemeTypes.LIGHT ? 'light' : 'dark';
}

export function getThemeDefaultVariables(baseTheme: ThemeBaseKind): Readonly<Record<string, string>> {
	return baseTheme === 'light' ? THEME_LIGHT_DEFAULT_VARIABLE_VALUES : THEME_DARK_DEFAULT_VARIABLE_VALUES;
}
