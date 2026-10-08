// SPDX-License-Identifier: AGPL-3.0-or-later

import {DESKTOP_DOWNLOAD_URL} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Button} from '@app/features/ui/button/Button';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import styles from '@app/features/voice/components/modals/ScreenSharePickerModal.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {AppleLogoIcon, LinuxLogoIcon, WindowsLogoIcon} from '@phosphor-icons/react';
import type React from 'react';

const BEST_WITH_THE_DESKTOP_APP_DESCRIPTOR = msg({
	message: 'Best with the desktop app',
	comment: 'CTA card title in the screen-share picker promoting the desktop app for richer screen sharing.',
});
const INSTALL_FOR_THE_FULL_SOURCE_PICKER_AND_SMOOTHER_DESCRIPTOR = msg({
	message: 'Install {productName} for the full source picker and smoother screen-sharing controls.',
	comment: 'CTA card body in the screen-share picker promoting the desktop app. {productName} is Fluxer.',
});
const GET_THE_DESKTOP_APP_DESCRIPTOR = msg({
	message: 'Get the desktop app',
	comment: 'CTA button label in the screen-share picker. Opens the desktop download page.',
});
export const DesktopDownloadCta: React.FC = () => {
	const {i18n} = useLingui();
	return (
		<div className={styles.downloadCta} data-flx="voice.screen-share-picker-modal.download-cta">
			<div className={styles.downloadCopy} data-flx="voice.screen-share-picker-modal.download-copy">
				<div className={styles.downloadTitle} data-flx="voice.screen-share-picker-modal.download-title">
					{i18n._(BEST_WITH_THE_DESKTOP_APP_DESCRIPTOR)}
				</div>
				<div className={styles.downloadDescription} data-flx="voice.screen-share-picker-modal.download-description">
					{i18n._(INSTALL_FOR_THE_FULL_SOURCE_PICKER_AND_SMOOTHER_DESCRIPTOR, {productName: RuntimeConfig.productName})}
				</div>
			</div>
			<div className={styles.downloadActions} data-flx="voice.screen-share-picker-modal.download-actions">
				<div
					className={styles.platformIcons}
					aria-hidden={true}
					data-flx="voice.screen-share-picker-modal.platform-icons"
				>
					<AppleLogoIcon
						weight="fill"
						className={styles.platformIcon}
						data-flx="voice.screen-share-picker-modal.platform-icon"
					/>
					<WindowsLogoIcon
						weight="fill"
						className={styles.platformIcon}
						data-flx="voice.screen-share-picker-modal.platform-icon--2"
					/>
					<LinuxLogoIcon
						weight="fill"
						className={styles.platformIcon}
						data-flx="voice.screen-share-picker-modal.platform-icon--3"
					/>
				</div>
				<Button
					variant="primary"
					onClick={() => void openExternalUrl(DESKTOP_DOWNLOAD_URL)}
					data-flx="voice.screen-share-picker-modal.button--3"
				>
					{i18n._(GET_THE_DESKTOP_APP_DESCRIPTOR)}
				</Button>
			</div>
		</div>
	);
};
