// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/app_layout/nagbars/DesktopDownloadNagbar.module.css';
import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {DESKTOP_DOWNLOAD_URL, PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {DOWNLOAD_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {AndroidLogoIcon, AppleLogoIcon, WindowsLogoIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const DESKTOP_APP_DOWNLOAD_MESSAGE_DESCRIPTOR = msg({
	message: 'Get the {productName} desktop app for system-wide push-to-talk and a few other goodies.',
	comment: 'Nagbar body encouraging web users to download the desktop app. productName is the app name.',
});
export const DesktopDownloadNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const handleDownload = () => {
		openExternalUrl(DESKTOP_DOWNLOAD_URL);
	};
	const handleDismiss = () => {
		NagbarCommands.dismissNagbar('desktopDownloadDismissed');
	};
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.BRAND].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.BRAND].textColor}
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.desktop-download-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={i18n._(DESKTOP_APP_DOWNLOAD_MESSAGE_DESCRIPTOR, {productName: PRODUCT_NAME})}
				actions={
					<>
						<span
							className={styles.platformIcons}
							data-flx="app.app-layout.nagbars.desktop-download-nagbar.platform-icons"
						>
							<AppleLogoIcon
								weight="fill"
								className={styles.platformIcon}
								data-flx="app.app-layout.nagbars.desktop-download-nagbar.platform-icon"
							/>
							<AndroidLogoIcon
								weight="fill"
								className={styles.platformIcon}
								data-flx="app.app-layout.nagbars.desktop-download-nagbar.platform-icon--2"
							/>
							<WindowsLogoIcon
								weight="fill"
								className={styles.platformIcon}
								data-flx="app.app-layout.nagbars.desktop-download-nagbar.platform-icon--3"
							/>
						</span>
						<NagbarButton
							isMobile={isMobile}
							onClick={handleDownload}
							data-flx="app.app-layout.nagbars.desktop-download-nagbar.nagbar-button.download"
						>
							{i18n._(DOWNLOAD_DESCRIPTOR)}
						</NagbarButton>
					</>
				}
				data-flx="app.app-layout.nagbars.desktop-download-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
