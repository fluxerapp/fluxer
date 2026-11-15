// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/dialogs/components/PerksButton.module.css';
import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const PREMIUM_PERKS_DESCRIPTOR = msg({
	message: '{premiumProductName} perks',
	comment: 'Link label that opens the premium perks list. premiumProductName is the premium plan name.',
});
export const PerksButton: React.FC<{
	onClick: () => void;
}> = observer(({onClick}) => {
	const {i18n} = useLingui();
	return (
		<button type="button" onClick={onClick} className={styles.link} data-flx="app.perks-button.link.click.button">
			{i18n._(PREMIUM_PERKS_DESCRIPTOR, {premiumProductName: getPremiumProductName()})}
		</button>
	);
});
