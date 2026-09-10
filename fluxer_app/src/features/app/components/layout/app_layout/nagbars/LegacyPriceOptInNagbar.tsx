// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {resolvePriceAnnouncementCampaign} from '@app/features/premium/config/PriceAnnouncementCampaign';
import PremiumState from '@app/features/premium/state/PremiumState';
import {MANAGE_SUBSCRIPTION_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {formatMinorUnitPrice} from '@app/features/premium/utils/PricingUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import NagbarState from '@app/features/ui/state/Nagbar';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import Users from '@app/features/user/state/Users';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {getFormattedLongDate} from '@fluxer/date_utils/src/DateFormatting';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

export const LegacyPriceOptInNagbar = observer(function LegacyPriceOptInNagbar({isMobile}: {isMobile: boolean}) {
	const {i18n} = useLingui();
	const currentUser = Users.currentUser;
	const premiumState = PremiumState.loadedForUserId === currentUser?.id ? PremiumState.state : null;
	const resolved = resolvePriceAnnouncementCampaign(premiumState);
	const campaignId = resolved?.campaign.id ?? null;
	const handleOpenPremiumSettings = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal
					initialTab="plutonium"
					data-flx="app.app-layout.nagbars.legacy-price-opt-in-nagbar.handle-open-premium-settings.user-settings-modal"
				/>
			)),
		);
	}, []);
	const handleDismiss = useCallback(() => {
		if (campaignId == null) return;
		NagbarState.dismissLegacyPriceOptIn(campaignId);
	}, [campaignId]);
	if (!premiumState || !resolved) return null;
	const listPriceSwitch = premiumState.billing.list_price_switch ?? null;
	if (!listPriceSwitch?.available || listPriceSwitch.pending) return null;
	if (listPriceSwitch.currency !== resolved.currency || listPriceSwitch.effective_at == null) return null;
	if (listPriceSwitch.billing_cycle == null) return null;
	const locale = LocaleUtils.getCurrentLocale();
	const currentPrice = formatMinorUnitPrice(listPriceSwitch.current_amount_minor, listPriceSwitch.currency, locale);
	const newPrice = formatMinorUnitPrice(listPriceSwitch.list_amount_minor, listPriceSwitch.currency, locale);
	if (currentPrice == null || newPrice == null) return null;
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.BRAND].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.BRAND].textColor}
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.legacy-price-opt-in-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={i18n._(
					listPriceSwitch.billing_cycle === 'monthly'
						? resolved.campaign.optInMonthlyMessage
						: resolved.campaign.optInYearlyMessage,
					{
						premiumProductName: PREMIUM_PRODUCT_NAME,
						currentPrice,
						newPrice,
						effectiveDate: getFormattedLongDate(listPriceSwitch.effective_at, locale),
					},
				)}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleOpenPremiumSettings}
						data-flx="app.app-layout.nagbars.legacy-price-opt-in-nagbar.nagbar-button.open-premium-settings"
					>
						{i18n._(MANAGE_SUBSCRIPTION_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.legacy-price-opt-in-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
