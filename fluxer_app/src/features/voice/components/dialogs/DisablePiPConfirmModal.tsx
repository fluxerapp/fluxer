// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import PiP from '@app/features/ui/state/PiP';
import {formatUserSettingsPath} from '@app/features/user/components/settings_utils/SettingsConstants';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import styles from '@app/features/voice/components/dialogs/DisablePiPConfirmModal.module.css';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useRef, useState} from 'react';

const HIDE_PICTURE_IN_PICTURE_POPOUT_DESCRIPTOR = msg({
	message: 'Hide picture-in-picture popout?',
	comment: 'Confirm dialog title shown before hiding the picture-in-picture popout. Question mark intentional.',
});
const CLOSE_POPOUT_DESCRIPTOR = msg({
	message: 'Close popout',
	comment: 'Confirm button on the disable-PiP dialog that hides the PiP popout.',
});
export const DisablePiPConfirmModal = observer(() => {
	const {i18n} = useLingui();
	const [rememberPreference, setRememberPreference] = useState(false);
	const initialFocusRef = useRef<HTMLButtonElement | null>(null);
	const audioVideoSettingsPath = formatUserSettingsPath(i18n, 'voice_video');
	const handleConfirm = () => {
		if (rememberPreference) {
			VoiceSettingsCommands.update({disablePictureInPicturePopoutScreenShare: true});
		} else {
			PiP.setSessionDisable(true);
		}
		PiP.close();
		ModalCommands.pop();
	};
	const handleCancel = () => {
		ModalCommands.pop();
	};
	return (
		<Modal.Root
			size="small"
			centered
			initialFocusRef={initialFocusRef}
			data-flx="voice.disable-pi-p-confirm-modal.modal-root"
		>
			<Modal.Header
				title={i18n._(HIDE_PICTURE_IN_PICTURE_POPOUT_DESCRIPTOR)}
				data-flx="voice.disable-pi-p-confirm-modal.modal-header"
			/>
			<Modal.Content data-flx="voice.disable-pi-p-confirm-modal.modal-content">
				<Modal.ContentLayout data-flx="voice.disable-pi-p-confirm-modal.modal-content-layout">
					<Modal.Description data-flx="voice.disable-pi-p-confirm-modal.description">
						<Trans>
							If you don't save this preference, we'll only hide the popout for this session. You can change this any
							time in {audioVideoSettingsPath}.
						</Trans>
					</Modal.Description>
					<div className={styles.checkboxContainer} data-flx="voice.disable-pi-p-confirm-modal.checkbox-container">
						<Checkbox
							checked={rememberPreference}
							onChange={(checked) => setRememberPreference(checked)}
							size="small"
							data-flx="voice.disable-pi-p-confirm-modal.checkbox.set-remember-preference"
						>
							<span className={styles.checkboxLabel} data-flx="voice.disable-pi-p-confirm-modal.checkbox-label">
								<Trans>Remember this preference</Trans>
							</span>
						</Checkbox>
					</div>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="voice.disable-pi-p-confirm-modal.modal-footer">
				<Button variant="secondary" onClick={handleCancel} data-flx="voice.disable-pi-p-confirm-modal.button.cancel">
					{i18n._(CANCEL_DESCRIPTOR)}
				</Button>
				<Button
					variant="primary"
					onClick={handleConfirm}
					ref={initialFocusRef}
					data-flx="voice.disable-pi-p-confirm-modal.button.confirm"
				>
					{i18n._(CLOSE_POPOUT_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
