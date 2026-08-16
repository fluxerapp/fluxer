// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {DESKTOP_DOWNLOAD_URL, PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const CORRUPTED_INSTALLATION_MESSAGE_DESCRIPTOR = msg({
	message: 'Your installation is corrupted, and you need to reinstall the app for all functionality to work.',
	comment: 'Critical nagbar body shown when the desktop installation appears corrupted. Keep the tone plain.',
});
const DOWNLOAD_PRODUCT_DESCRIPTOR = msg({
	message: 'Download {productName}',
	comment: 'Nagbar button label that downloads the desktop app. productName is the app name.',
});
export const CorruptedInstallationNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const handleDownload = () => {
		void openExternalUrl(DESKTOP_DOWNLOAD_URL);
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.CRITICAL].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.CRITICAL].textColor}
			data-flx="app.app-layout.nagbars.corrupted-installation-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				message={i18n._(CORRUPTED_INSTALLATION_MESSAGE_DESCRIPTOR)}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleDownload}
						data-flx="app.app-layout.nagbars.corrupted-installation-nagbar.nagbar-button.download"
					>
						{i18n._(DOWNLOAD_PRODUCT_DESCRIPTOR, {productName: PRODUCT_NAME})}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.corrupted-installation-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
