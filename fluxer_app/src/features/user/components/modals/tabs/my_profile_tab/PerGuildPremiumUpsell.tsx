// SPDX-License-Identifier: AGPL-3.0-or-later

import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {PlutoniumUpsell} from '@app/features/ui/plutonium_upsell/PlutoniumUpsell';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

export const PerGuildPremiumUpsell = observer(() => {
	const showPremium = shouldShowPremiumFeatures();
	if (!showPremium) {
		return (
			<WarningAlert data-flx="user.my-profile-tab.per-guild-premium-upsell.instance-notice">
				<Trans>
					Customizing your avatar, banner, accent color, and bio for individual communities is not enabled on this
					instance. Community nicknames and pronouns are available for everyone.
				</Trans>
			</WarningAlert>
		);
	}
	return (
		<PlutoniumUpsell data-flx="user.my-profile-tab.per-guild-premium-upsell.plutonium-upsell">
			<Trans>
				Customizing your avatar, banner, accent color, and bio for individual communities requires{' '}
				{PREMIUM_PRODUCT_NAME}. Community nicknames and pronouns are free for everyone.
			</Trans>
		</PlutoniumUpsell>
	);
});
