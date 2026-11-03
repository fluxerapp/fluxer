// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import styles from '@app/features/user/components/modals/tabs/privacy_safety_tab/DataDeletionTab.module.css';
import {
	DataRequestModal,
	deleteTabDescription,
} from '@app/features/user/components/modals/tabs/privacy_safety_tab/data_request_modal/DataRequestModal';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

export const DataDeletionTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const handleOpen = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<DataRequestModal variant="delete" data-flx="user.privacy-safety-tab.data-deletion-tab.data-request-modal" />
			)),
		);
	}, []);
	return (
		<div
			className={styles.deleteSection}
			data-flx="user.privacy-safety-tab.data-deletion-tab.data-deletion-tab-content.delete-section"
		>
			<Modal.Description
				className={styles.warningText}
				data-flx="user.privacy-safety-tab.data-deletion-tab.data-deletion-tab-content.warning-text"
			>
				{i18n._(deleteTabDescription())}
			</Modal.Description>
			<Button
				variant="danger"
				onClick={handleOpen}
				data-flx="user.privacy-safety-tab.data-deletion-tab.data-deletion-tab-content.button.open-modal"
			>
				<Trans>Delete my messages</Trans>
			</Button>
		</div>
	);
});
