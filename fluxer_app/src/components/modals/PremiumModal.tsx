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

import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import * as Modal from '~/components/modals/Modal';
import {type PremiumModalProps, usePremiumModalLogic} from '~/utils/modals/PremiumModalUtils';
import {PlutoniumContent} from './components/PlutoniumContent';
import styles from './PremiumModal.module.css';

export const PremiumModal = observer(({defaultGiftMode = false}: PremiumModalProps) => {
	const modalLogic = usePremiumModalLogic({
		defaultGiftMode,
	});

	return (
		<Modal.Root size="large">
			<Modal.Header title={<Trans>Fluxer Plutonium</Trans>} />
			<Modal.Content>
				<div className={styles.contentContainer}>
					<PlutoniumContent defaultGiftMode={modalLogic.defaultGiftMode} />
				</div>
			</Modal.Content>
		</Modal.Root>
	);
});
