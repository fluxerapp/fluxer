import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {ThemeAcceptModal} from '@app/features/theme/components/modals/ThemeAcceptModal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const THIS_THEME_LINK_IS_MISSING_DATA_DESCRIPTOR = msg({
	message: 'This theme link is missing data.',
	comment: 'Description text in the theme settings commands.',
});

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
