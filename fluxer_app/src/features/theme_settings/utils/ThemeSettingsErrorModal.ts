// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import type {ReactNode} from 'react';

export const WE_COULDN_T_SAVE_THAT_THEME_DESCRIPTOR = msg({
	message: "We couldn't save that theme.",
	comment: 'Error message after saving a theme failed in Settings > Themes or the quick CSS editor window.',
});

export function showThemeSettingsErrorModal(i18n: I18n, message: ReactNode | (() => ReactNode), dataFlx: string): void {
	showGenericErrorModal({
		title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
		message,
		dataFlx,
	});
}
