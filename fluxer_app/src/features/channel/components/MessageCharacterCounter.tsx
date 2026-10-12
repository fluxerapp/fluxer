// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/MessageCharacterCounter.module.css';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {CharacterCounter} from '@app/features/ui/character_counter/CharacterCounter';
import {observer} from 'mobx-react-lite';

interface MessageCharacterCounterProps {
	currentLength: number;
	maxLength: number;
	canUpgrade: boolean;
	premiumMaxLength: number;
}

export const MessageCharacterCounter = observer(
	({currentLength, maxLength, canUpgrade, premiumMaxLength}: MessageCharacterCounterProps) => {
		if (currentLength <= maxLength * 0.8) {
			return null;
		}
		return (
			<div className={styles.container} data-flx="channel.message-character-counter.container">
				<CharacterCounter
					currentLength={currentLength}
					maxLength={maxLength}
					canUpgrade={canUpgrade}
					premiumMaxLength={premiumMaxLength}
					onUpgradeClick={() => PremiumModalCommands.open()}
					data-flx="channel.message-character-counter.character-counter"
				/>
			</div>
		);
	},
);
