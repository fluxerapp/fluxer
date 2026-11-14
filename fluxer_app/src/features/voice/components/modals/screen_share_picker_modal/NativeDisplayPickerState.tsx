// SPDX-License-Identifier: AGPL-3.0-or-later

import {Button} from '@app/features/ui/button/Button';
import styles from '@app/features/voice/components/modals/ScreenSharePickerModal.module.css';
import {DesktopDownloadCta} from '@app/features/voice/components/modals/screen_share_picker_modal/DesktopDownloadCta';
import type {NativePickerCopy} from '@app/features/voice/components/modals/screen_share_picker_modal/useNativePickerCopy';
import type React from 'react';

interface NativeDisplayPickerStateProps {
	copy: NativePickerCopy;
	pickerActionLabel: string;
	onPickerAction: () => void;
	pickerActionPending: boolean;
	showDesktopDownloadCta: boolean;
}

export const NativeDisplayPickerState: React.FC<NativeDisplayPickerStateProps> = ({
	copy,
	pickerActionLabel,
	onPickerAction,
	pickerActionPending,
	showDesktopDownloadCta,
}) => {
	return (
		<div className={styles.state} data-flx="voice.screen-share-picker-modal.state--2">
			<copy.Icon
				className={styles.stateIcon}
				weight="fill"
				aria-hidden={true}
				data-flx="voice.screen-share-picker-modal.state-icon"
			/>
			<div className={styles.stateHeading} data-flx="voice.screen-share-picker-modal.state-heading">
				{copy.title}
			</div>
			<div className={styles.stateTitle} data-flx="voice.screen-share-picker-modal.state-title--2">
				{copy.description}
			</div>
			<div className={styles.stateActions} data-flx="voice.screen-share-picker-modal.state-actions">
				<Button
					onClick={onPickerAction}
					submitting={pickerActionPending}
					data-flx="voice.screen-share-picker-modal.button--2"
				>
					{pickerActionLabel}
				</Button>
			</div>
			{showDesktopDownloadCta && (
				<DesktopDownloadCta data-flx="voice.screen-share-picker-modal.native-display-picker-state.desktop-download-cta" />
			)}
		</div>
	);
};
