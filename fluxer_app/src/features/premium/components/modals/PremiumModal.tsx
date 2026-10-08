// SPDX-License-Identifier: AGPL-3.0-or-later

import {PlutoniumContent} from '@app/features/app/components/dialogs/components/PlutoniumContent';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import styles from '@app/features/premium/components/modals/PremiumModal.module.css';
import {type PremiumModalProps, usePremiumModalLogic} from '@app/features/premium/utils/PremiumModalUtils';
import {getPremiumProductFullName} from '@app/features/premium/utils/PremiumUtils';
import {observer} from 'mobx-react-lite';

export const PremiumModal = observer(({defaultGiftMode}: PremiumModalProps) => {
	const modalLogic = usePremiumModalLogic({
		defaultGiftMode,
	});
	return (
		<Modal.Root size="large" data-flx="premium.premium-modal.modal-root">
			<Modal.Header title={getPremiumProductFullName()} data-flx="premium.premium-modal.modal-header" />
			<Modal.Content data-flx="premium.premium-modal.modal-content">
				<div className={styles.contentContainer} data-flx="premium.premium-modal.content-container">
					<PlutoniumContent
						defaultGiftMode={modalLogic.defaultGiftMode}
						data-flx="premium.premium-modal.plutonium-content"
					/>
				</div>
			</Modal.Content>
		</Modal.Root>
	);
});
