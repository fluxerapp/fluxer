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
import {XCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import type {ChannelRecord} from '~/records/ChannelRecord';
import styles from './EditBar.module.css';
import wrapperStyles from './textarea/InputWrapper.module.css';

interface EditBarProps {
	channel: ChannelRecord;
	onCancel: () => void;
}

export const EditBar = observer(({channel, onCancel}: EditBarProps) => {
	const handleStopEdit = () => {
		MessageActionCreators.stopEditMobile(channel.id);
		onCancel();
	};

	const handleKeyDown = (handler: () => void) => (event: React.KeyboardEvent) => {
		if (event.key === 'Enter') handler();
	};

	return (
		<div
			className={`${wrapperStyles.box} ${wrapperStyles.wrapperSides} ${wrapperStyles.roundedTop} ${wrapperStyles.noBottomBorder}`}
		>
			<div className={wrapperStyles.barInner} style={{gridTemplateColumns: '1fr auto'}}>
				<div className={styles.text}>
					<Trans>Editing message</Trans>
				</div>

				<div className={styles.controls}>
					<FocusRing offset={-2}>
						<button
							type="button"
							className={styles.button}
							onClick={handleStopEdit}
							onKeyDown={handleKeyDown(handleStopEdit)}
						>
							<XCircleIcon className={styles.icon} />
						</button>
					</FocusRing>
				</div>
			</div>
			<div className={wrapperStyles.separator} />
		</div>
	);
});
