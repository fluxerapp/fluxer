// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import Updater from '@app/features/app/state/Updater';
import {
	DESKTOP_VERSION_HAS_BEEN_DOWNLOADED_DESCRIPTOR,
	RESTART_FLUXER_DESCRIPTOR,
	THE_DESKTOP_UPDATE_HAS_BEEN_DOWNLOADED_DESCRIPTOR,
} from '@app/features/updater/commands/UpdaterModalCommands';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

export const DesktopUpdateReadyNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const version = Updater.updateInfo.native.version;
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.BRAND].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.BRAND].textColor}
			dismissible
			onDismiss={Updater.dismissUpdateReadyNagbar}
			data-flx="app.app-layout.nagbars.desktop-update-ready-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={Updater.dismissUpdateReadyNagbar}
				message={
					version
						? i18n._(DESKTOP_VERSION_HAS_BEEN_DOWNLOADED_DESCRIPTOR, {version, productName: PRODUCT_NAME})
						: i18n._(THE_DESKTOP_UPDATE_HAS_BEEN_DOWNLOADED_DESCRIPTOR, {productName: PRODUCT_NAME})
				}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={() => void Updater.applyUpdate()}
						data-flx="app.app-layout.nagbars.desktop-update-ready-nagbar.restart-button"
					>
						{i18n._(RESTART_FLUXER_DESCRIPTOR, {productName: PRODUCT_NAME})}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.desktop-update-ready-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
