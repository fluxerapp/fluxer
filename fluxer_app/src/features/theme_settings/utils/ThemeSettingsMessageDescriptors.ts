// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const OPEN_QUICK_CSS_EDITOR_DESCRIPTOR = msg({
	message: 'Open quick CSS editor',
	comment:
		'Button in Settings > Themes that opens the quick CSS editor in its own window. Also the settings search entry and the keyboard shortcut action for it. Keep short, sentence case.',
});
export const TURN_CUSTOM_CSS_ON_OR_OFF_DESCRIPTOR = msg({
	message: 'Turn custom CSS on or off',
	comment:
		'Keyboard shortcut action that stops applying quick CSS and saved themes in this tab, for example when CSS breaks the app. Also used in the quick CSS editor.',
});
export const CUSTOM_CSS_IS_OFF_DESCRIPTOR = msg({
	message: 'Custom CSS is off.',
	comment:
		'Shown in a notice bar and in the quick CSS editor while quick CSS and saved themes are turned off in this tab, for example after CSS broke the app.',
});
export const USE_DESKTOP_APP_THEME_DESCRIPTOR = msg({
	message: 'Use the desktop app theme for this account',
	comment:
		'Switch label in Settings > Themes on the desktop app, also used as its settings search entry. When on, the account uses the theme shared by every account in the desktop app.',
});
export const SYNC_CUSTOM_CSS_ACROSS_DEVICES_DESCRIPTOR = msg({
	message: 'Sync custom CSS across devices',
	comment: 'Switch label in Settings > Themes, also used as its settings search entry.',
});
export const EXPORT_LIBRARY_DESCRIPTOR = msg({
	message: 'Export library',
	comment:
		'Row title and button in Settings > Themes that saves every theme and asset to one file, also used as its settings search entry.',
});
export const IMPORT_LIBRARY_DESCRIPTOR = msg({
	message: 'Import library',
	comment:
		'Row title and button in Settings > Themes that adds themes and assets from a library file, also used as its settings search entry.',
});
export const THEME_SAVED_DESCRIPTOR = msg({
	message: 'Theme saved.',
	comment: 'Toast after saving a theme in Settings > Themes or the quick CSS editor window.',
});
export const TRY_A_DIFFERENT_NAME_OR_CLEAR_THE_SEARCH_DESCRIPTOR = msg({
	message: 'Try a different name or clear the search.',
	comment: 'Empty search result description in Settings > Themes.',
});
