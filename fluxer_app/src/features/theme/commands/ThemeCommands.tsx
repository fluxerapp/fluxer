// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ThemeAcceptModal} from '@app/features/theme/components/modals/ThemeAcceptModal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const IMPORTED_THEME_HAS_BEEN_APPLIED_DESCRIPTOR = msg({
	message: 'Imported theme has been applied.',
	comment: 'Description text in the theme settings commands.',
});
const WE_COULDN_T_APPLY_THIS_THEME_DESCRIPTOR = msg({
	message: "We couldn't apply this theme.",
	comment: 'Error message in the theme settings commands.',
});
const THIS_THEME_LINK_IS_MISSING_DATA_DESCRIPTOR = msg({
	message: 'This theme link is missing data.',
	comment: 'Description text in the theme settings commands.',
});
const logger = new Logger('Themes');

export function applyTheme(css: string, i18n: I18n): void {
	try {
		AccessibilityCommands.update({customThemeCss: css});
		ToastCommands.success(i18n._(IMPORTED_THEME_HAS_BEEN_APPLIED_DESCRIPTOR));
	} catch (error) {
		logger.error('Failed to apply theme:', error);
		showGenericErrorModal({
			title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
			message: () => i18n._(WE_COULDN_T_APPLY_THIS_THEME_DESCRIPTOR),
			dataFlx: 'theme.theme-commands.apply-theme-error-modal',
		});
		throw error;
	}
}

export function openAcceptModal(themeId: string | undefined, i18n: I18n, runtimeSnapshot: RuntimeConfigSnapshot): void {
	if (!themeId) {
		showGenericErrorModal({
			title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
			message: () => i18n._(THIS_THEME_LINK_IS_MISSING_DATA_DESCRIPTOR),
			dataFlx: 'theme.theme-commands.missing-theme-link-data-error-modal',
		});
		return;
	}
	ModalCommands.pushWithKey(
		modal(() => (
			<ThemeAcceptModal
				themeId={themeId}
				runtimeSnapshot={runtimeSnapshot}
				data-flx="theme.theme-commands.open-accept-modal.theme-accept-modal"
			/>
		)),
		`theme-accept-${themeId}`,
	);
}
