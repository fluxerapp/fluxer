// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import ThemeLibrary, {getThemeLabel, type ThemeLibraryTheme} from '@app/features/theme/state/ThemeLibrary';
import {revertQuickCssEditor} from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import ThemeSettingsState from '@app/features/theme_settings/state/ThemeSettingsState';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {getActivePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const DISCARD_CHANGES_DESCRIPTOR = msg({
	message: 'Discard changes to {themeName}?',
	comment: 'Confirmation title in the quick CSS editor window before leaving a theme with unsaved changes.',
});
const DISCARD_DESCRIPTOR = msg({
	message: 'Discard',
	comment: 'Confirm button that throws away unsaved changes to a theme in the quick CSS editor window.',
});
const UNSAVED_CHANGES_DESCRIPTION_DESCRIPTOR = msg({
	message: "Your changes to this theme haven't been saved.",
	comment: 'Confirmation text in the quick CSS editor window before leaving a theme with unsaved changes.',
});

function editedTheme(): ThemeLibraryTheme | undefined {
	return ThemeLibrary.themes.find((candidate) => candidate.id === ThemeSettingsState.editorFile);
}

export function confirmDiscardThemeChanges(i18n: I18n, onDiscard: () => void): void {
	const theme = editedTheme();
	const portalHost = getActivePortalHost();
	const render = modal(() => (
		<ConfirmModal
			title={i18n._(DISCARD_CHANGES_DESCRIPTOR, {themeName: theme ? getThemeLabel(theme) : ''})}
			description={i18n._(UNSAVED_CHANGES_DESCRIPTION_DESCRIPTOR)}
			primaryText={i18n._(DISCARD_DESCRIPTOR)}
			primaryVariant="danger"
			secondaryText={i18n._(CANCEL_DESCRIPTOR)}
			onPrimary={() => {
				const current = editedTheme();
				if (current) revertQuickCssEditor(current.css);
				onDiscard();
			}}
			data-flx="theme-settings.quick-css-editor-discard.confirm-discard-theme-changes.confirm-modal"
		/>
	));
	if (portalHost) {
		ModalCommands.pushToPortalHost({modal: render, portalHost});
		return;
	}
	ModalCommands.push(render);
}

export function requestQuickCssEditorClose(i18n: I18n, close: () => void): void {
	if (!ThemeSettingsState.editorDirty) {
		close();
		return;
	}
	confirmDiscardThemeChanges(i18n, close);
}
