// SPDX-License-Identifier: AGPL-3.0-or-later

import {PREMIUM_PRODUCT_FULL_NAME} from '@app/features/app/config/I18nDisplayConstants';
import * as PlutoniumPageCommands from '@app/features/premium/commands/PlutoniumPageCommands';
import styles from '@app/features/premium/components/plutonium_page/PlutoniumPageLinkCard.module.css';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CrownIcon} from '@phosphor-icons/react';

const LINK_CARD_BODY_DESCRIPTOR = msg({
	message: 'Plans, perks, gifts and your subscription all live on one page.',
	comment: 'Text on the card in settings that links to the Plutonium page.',
});
const LINK_CARD_BUTTON_DESCRIPTOR = msg({
	message: 'Open {premiumProductFullName}',
	comment:
		'Button on the settings card that opens the Plutonium page. premiumProductFullName is the full paid tier name.',
});

export function PlutoniumPageLinkCard() {
	const {i18n} = useLingui();
	return (
		<div className={styles.card} data-flx="premium.plutonium-page-link-card.card">
			<CrownIcon weight="fill" className={styles.icon} aria-hidden="true" />
			<div className={styles.text}>
				<p className={styles.title}>{PREMIUM_PRODUCT_FULL_NAME}</p>
				<p className={styles.body}>{i18n._(LINK_CARD_BODY_DESCRIPTOR)}</p>
			</div>
			<Button
				variant="primary"
				small
				fitContent
				onClick={PlutoniumPageCommands.openPlutoniumPage}
				data-flx="premium.plutonium-page-link-card.open"
			>
				{i18n._(LINK_CARD_BUTTON_DESCRIPTOR, {premiumProductFullName: PREMIUM_PRODUCT_FULL_NAME})}
			</Button>
		</div>
	);
}
