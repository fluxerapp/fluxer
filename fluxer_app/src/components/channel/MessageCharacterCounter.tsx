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

import {observer} from 'mobx-react-lite';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import {MAX_MESSAGE_LENGTH_PREMIUM} from '~/Constants';
import styles from '~/components/channel/MessageCharacterCounter.module.css';
import {CharacterCounter} from '~/components/uikit/CharacterCounter/CharacterCounter';

interface MessageCharacterCounterProps {
	currentLength: number;
	maxLength: number;
	isPremium: boolean;
	threshold?: number;
}

export const MessageCharacterCounter = observer(
	({currentLength, maxLength, isPremium, threshold = 0.8}: MessageCharacterCounterProps) => {
		if (currentLength <= maxLength * threshold) {
			return null;
		}

		return (
			<div className={styles.container}>
				<CharacterCounter
					currentLength={currentLength}
					maxLength={maxLength}
					isPremium={isPremium}
					premiumMaxLength={MAX_MESSAGE_LENGTH_PREMIUM}
					onUpgradeClick={() => PremiumModalActionCreators.open()}
				/>
			</div>
		);
	},
);
