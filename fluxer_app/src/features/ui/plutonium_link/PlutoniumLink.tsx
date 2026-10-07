// SPDX-License-Identifier: AGPL-3.0-or-later

import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {getPremiumProductName, shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import styles from '@app/features/ui/plutonium_link/PlutoniumLink.module.css';

export const PlutoniumLink: React.FC = () => {
	if (!shouldShowPremiumFeatures()) {
		return null;
	}
	return (
		<button
			type="button"
			onClick={() => {
				PremiumModalCommands.open();
			}}
			className={styles.link}
			data-flx="ui.plutonium-link.plutonium-link.link.open.button"
		>
			{getPremiumProductName()}
		</button>
	);
};
