// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import WindowsFont from '@app/features/theme/state/WindowsFont';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {getAdvancedSettingRowElementId} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedSettingsItemUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const WINDOWS_FONT_CHANGED_DESCRIPTOR = msg({
	message: 'Text on Windows now uses the system font. You can switch back to Fluxer Sans in settings.',
	comment:
		'Nagbar message shown once to existing Windows users after the default app font changed to the Windows system font. Fluxer Sans is a font name and must not be translated.',
});
const SWITCH_BACK_DESCRIPTOR = msg({
	message: 'Switch back to Fluxer Sans',
	comment:
		'Nagbar button that opens the setting for switching the app font back to Fluxer Sans. Fluxer Sans is a font name and must not be translated.',
});

export const WindowsFontNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const openSetting = () => {
		ModalCommands.push(
			modal(
				() => (
					<UserSettingsModal
						initialTab="advanced_settings"
						initialSubtab={getAdvancedSettingRowElementId('advanced-windows-fluxer-sans')}
						data-flx="app.app-layout.nagbars.windows-font-nagbar.open-setting.user-settings-modal"
					/>
				),
				'user-settings',
			),
		);
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.BRAND].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.BRAND].textColor}
			dismissible
			onDismiss={WindowsFont.dismissNagbar}
			data-flx="app.app-layout.nagbars.windows-font-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={WindowsFont.dismissNagbar}
				message={i18n._(WINDOWS_FONT_CHANGED_DESCRIPTOR)}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={openSetting}
						data-flx="app.app-layout.nagbars.windows-font-nagbar.switch-back-button"
					>
						{i18n._(SWITCH_BACK_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.windows-font-nagbar.content"
			/>
		</Nagbar>
	);
});
