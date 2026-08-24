// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/dialogs/components/plutonium/GiftInventoryBanner.module.css';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {Button} from '@app/features/ui/button/Button';
import type {User} from '@app/features/user/models/User';
import {plural} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import {GiftIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface GiftInventoryBannerProps {
	currentUser: User;
}

export const GiftInventoryBanner: React.FC<GiftInventoryBannerProps> = observer(({currentUser}) => {
	if (!currentUser.hasUnreadGiftInventory || !shouldShowPremiumFeatures()) return null;
	const count = currentUser.unreadGiftInventoryCount ?? 1;
	const giftMessage = plural(
		{count},
		{
			one: 'You have a new gift code waiting for you!',
			other: 'You have # new gift codes waiting for you!',
		},
	);
	return (
		<div className={styles.banner} data-flx="app.plutonium.gift-inventory-banner.banner">
			<div className={styles.content} data-flx="app.plutonium.gift-inventory-banner.content">
				<GiftIcon className={styles.icon} data-flx="app.plutonium.gift-inventory-banner.icon" />
				<div className={styles.textContainer} data-flx="app.plutonium.gift-inventory-banner.text-container">
					<p className={styles.title} data-flx="app.plutonium.gift-inventory-banner.title">
						{giftMessage}
					</p>
				</div>
				<Button
					variant="secondary"
					small
					onClick={() => ComponentBus.dispatch('USER_SETTINGS_TAB_SELECT', {tab: 'gift_inventory'})}
					data-flx="app.plutonium.gift-inventory-banner.button.dispatch"
				>
					<Trans>View gifts</Trans>
				</Button>
			</div>
		</div>
	);
});
