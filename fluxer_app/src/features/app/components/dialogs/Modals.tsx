// SPDX-License-Identifier: AGPL-3.0-or-later

import {ModalStack} from '@app/features/app/components/dialogs/ModalStack';
import styles from '@app/features/app/components/dialogs/Modals.module.css';
import {MediaViewerModal} from '@app/features/messaging/components/modals/MediaViewerModal';
import { PersonaPickerMobileSheet } from '@app/features/personas/components/modals/PersonaPickerMobileSheet';
import { PersonaProfileMobileSheet } from '@app/features/personas/components/modals/PersonaProfileMobileSheet';
import {UserProfileMobileSheet} from '@app/features/user/components/modals/UserProfileMobileSheet';
import {observer} from 'mobx-react-lite';

export const Modals = observer(() => {
	return (
		<div className={styles.modals} data-overlay-pass-through="true" data-flx="app.modals.modals">
			<MediaViewerModal data-flx="app.modals.media-viewer-modal" />
			<PersonaProfileMobileSheet data-flx="app.modals.persona-profile-mobile-sheet" />
			<UserProfileMobileSheet data-flx="app.modals.user-profile-mobile-sheet" />
			<PersonaPickerMobileSheet data-flx="app.modals.persona-picker-mobile-sheet" />
			<ModalStack data-flx="app.modals.modal-stack" />
		</div>
	);
});
