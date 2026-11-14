// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsPageContext} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as ThemeSettingsCommands from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import type {QuickCssEditorTab} from '@app/features/theme_settings/state/ThemeSettingsState';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PictureInPictureIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useContext} from 'react';

const EDIT_NEXT_TO_APP_DESCRIPTOR = msg({
	message: 'Edit next to the app',
	comment:
		'Button in Settings > Themes that closes settings and opens the same controls in the quick CSS editor panel, which floats over the app so changes can be seen while editing.',
});

export const EditNextToAppButton: React.FC<{tab: QuickCssEditorTab}> = observer(({tab}) => {
	const {i18n} = useLingui();
	const inSettings = useContext(SettingsPageContext);
	if (!inSettings || !ThemeSettingsCommands.canOpenQuickCssEditor()) return null;
	return (
		<Button
			variant="secondary"
			small
			leftIcon={<PictureInPictureIcon size={remFromPx(16)} data-flx="theme-settings.edit-next-to-app-button.icon" />}
			onClick={() => ThemeSettingsCommands.openQuickCssEditorTab(tab)}
			data-flx="theme-settings.edit-next-to-app-button.button"
		>
			{i18n._(EDIT_NEXT_TO_APP_DESCRIPTOR)}
		</Button>
	);
});
