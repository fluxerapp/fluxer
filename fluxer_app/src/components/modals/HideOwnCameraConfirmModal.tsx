/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as VoiceSettingsActionCreators from '~/actions/VoiceSettingsActionCreators';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {Checkbox} from '~/components/uikit/Checkbox/Checkbox';
import VoicePromptsStore from '~/stores/VoicePromptsStore';
import styles from './HideOwnCameraConfirmModal.module.css';

export const HideOwnCameraConfirmModal: React.FC = observer(() => {
	const {t} = useLingui();
	const [dontAskAgain, setDontAskAgain] = React.useState(false);
	const initialFocusRef = React.useRef<HTMLButtonElement | null>(null);

	const handleConfirm = () => {
		if (dontAskAgain) VoicePromptsStore.setSkipHideOwnCameraConfirm(true);
		VoiceSettingsActionCreators.update({showMyOwnCamera: false});
		ModalActionCreators.pop();
	};

	const handleCancel = () => {
		ModalActionCreators.pop();
	};

	return (
		<Modal.Root size="small" centered initialFocusRef={initialFocusRef}>
			<Modal.Header title={<Trans>Hide your own camera?</Trans>} />
			<Modal.Content>
				<p className={styles.description}>
					<Trans>
						Turning this off only hides your camera from your own view. Others in the call can still see your camera
						feed.
					</Trans>
				</p>
				<div className={styles.checkboxContainer}>
					<Checkbox checked={dontAskAgain} onChange={(checked) => setDontAskAgain(checked)} size="small">
						<span className={styles.checkboxLabel}>
							<Trans>Don't ask me again</Trans>
						</span>
					</Checkbox>
				</div>
			</Modal.Content>
			<Modal.Footer>
				<Button variant="secondary" onClick={handleCancel}>{t`Cancel`}</Button>
				<Button variant="primary" onClick={handleConfirm} ref={initialFocusRef}>{t`Hide`}</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
