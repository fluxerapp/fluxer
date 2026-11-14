// SPDX-License-Identifier: AGPL-3.0-or-later

import ThemeLibrary, {type ThemeLibraryTheme} from '@app/features/theme/state/ThemeLibrary';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const LINKED_FILE_DESCRIPTOR = msg({
	message: 'Linked to a file',
	comment: 'Shown under a theme in Settings > Themes that is linked to a CSS file on disk and updates live.',
});
const LINKED_FILE_LIVE_DESCRIPTOR = msg({
	message: 'Linked to {path}. Edit the file in any editor and saved changes apply here right away.',
	comment:
		'Shown for a linked theme in Settings > Themes and the quick CSS editor. {path} is the full path of the CSS file on disk. The theme cannot be edited in the app while linked.',
});
const LINKED_FILE_PATH_DESCRIPTOR = msg({
	message: 'Linked to {path}.',
	comment:
		'Shown for a linked theme while the app is still checking its CSS file, or when this version of the desktop app cannot follow file changes. {path} is the full path of the CSS file on disk.',
});
const LINKED_FILE_MISSING_DESCRIPTOR = msg({
	message: "Can't find {path}. Using the last loaded version.",
	comment:
		'Shown for a linked theme in Settings > Themes when its CSS file was moved or deleted. {path} is the full path of the CSS file.',
});
const LINKED_FILE_UNREADABLE_DESCRIPTOR = msg({
	message: "Can't read {path}. Using the last loaded version.",
	comment:
		'Shown for a linked theme in Settings > Themes when its CSS file exists but cannot be read, for example because it is too large or access was removed. {path} is the full path of the CSS file.',
});

type LinkedThemeFileState = 'checking' | 'live' | 'missing' | 'unreadable';

function getLinkedThemeFileState(theme: ThemeLibraryTheme, path: string): LinkedThemeFileState {
	if (ThemeLibrary.isWatchedFolderTheme(theme)) {
		const folderStatus = ThemeLibrary.folders.find((folder) => folder.path === theme.folderPath)?.status;
		if (folderStatus === 'live') return 'live';
		return folderStatus === 'missing' ? 'missing' : 'unreadable';
	}
	const status = ThemeLibrary.linkedFileStatus.get(path);
	if (status === undefined) return 'checking';
	if (status === 'live') return 'live';
	return status === 'missing' ? 'missing' : 'unreadable';
}

export function isLinkedThemeReadOnly(theme: ThemeLibraryTheme): boolean {
	return Boolean(theme.linkedPath) && ThemeLibrary.canWatchLinkedFiles;
}

export function getLinkedThemeFileProblem(i18n: I18n, theme: ThemeLibraryTheme): string | null {
	const path = theme.linkedPath;
	if (!path) return null;
	switch (getLinkedThemeFileState(theme, path)) {
		case 'missing':
			return i18n._(LINKED_FILE_MISSING_DESCRIPTOR, {path});
		case 'unreadable':
			return i18n._(LINKED_FILE_UNREADABLE_DESCRIPTOR, {path});
		default:
			return null;
	}
}

export function getLinkedThemeFileLine(i18n: I18n, theme: ThemeLibraryTheme): string | null {
	const path = theme.linkedPath;
	if (!path) return null;
	const problem = getLinkedThemeFileProblem(i18n, theme);
	if (problem) return problem;
	const live = ThemeLibrary.canWatchLinkedFiles && getLinkedThemeFileState(theme, path) === 'live';
	return i18n._(live ? LINKED_FILE_LIVE_DESCRIPTOR : LINKED_FILE_PATH_DESCRIPTOR, {path});
}
