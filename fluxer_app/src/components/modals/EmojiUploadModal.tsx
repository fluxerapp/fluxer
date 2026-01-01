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

import {Plural, Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import styles from '~/components/modals/EmojiUploadModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Spinner} from '~/components/uikit/Spinner';

interface EmojiUploadModalProps {
	count: number;
}

export const EmojiUploadModal: React.FC<EmojiUploadModalProps> = observer(({count}) => {
	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={<Trans>Uploading Emojis</Trans>} hideCloseButton />
			<Modal.Content>
				<div className={styles.container}>
					<Spinner />
					<p className={styles.message}>
						<Trans>
							Uploading <Plural value={count} one="# emoji" other="# emojis" />. This may take a little while.
						</Trans>
					</p>
				</div>
			</Modal.Content>
		</Modal.Root>
	);
});
