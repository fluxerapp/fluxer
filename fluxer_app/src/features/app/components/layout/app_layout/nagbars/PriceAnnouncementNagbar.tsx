// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {resolvePriceAnnouncementCampaign} from '@app/features/premium/config/PriceAnnouncementCampaign';
import PremiumState from '@app/features/premium/state/PremiumState';
import {VIEW_PLANS_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {formatMinorUnitPrice} from '@app/features/premium/utils/PricingUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import NagbarState from '@app/features/ui/state/Nagbar';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import Users from '@app/features/user/state/Users';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

export const PriceAnnouncementNagbar = observer(function PriceAnnouncementNagbar({isMobile}: {isMobile: boolean}) {
	const {i18n} = useLingui();
	const currentUser = Users.currentUser;
	const premiumState = PremiumState.loadedForUserId === currentUser?.id ? PremiumState.state : null;
	const resolved = resolvePriceAnnouncementCampaign(premiumState);
	const campaignId = resolved?.campaign.id ?? null;
	const handleOpenPlans = useCallback(() => {
		if (campaignId != null) {
			NagbarState.dismissPriceAnnouncement(campaignId);
		}
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal
					initialTab="plutonium"
					data-flx="app.app-layout.nagbars.price-announcement-nagbar.handle-open-plans.user-settings-modal"
				/>
			)),
		);
	}, [campaignId]);
	const handleDismiss = useCallback(() => {
		if (campaignId == null) return;
		NagbarState.dismissPriceAnnouncement(campaignId);
	}, [campaignId]);
	if (!premiumState || !resolved || premiumState.actual.has_active_paid_premium) return null;
	const locale = LocaleUtils.getCurrentLocale();
	const monthlyPrice = formatMinorUnitPrice(resolved.monthlyAmountMinor, resolved.currency, locale);
	const yearlyPrice = formatMinorUnitPrice(resolved.yearlyAmountMinor, resolved.currency, locale);
	if (monthlyPrice == null || yearlyPrice == null) return null;
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.BRAND].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.BRAND].textColor}
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.price-announcement-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={i18n._(resolved.campaign.announcementMessage, {
					premiumProductName: PREMIUM_PRODUCT_NAME,
					monthlyPrice,
					yearlyPrice,
				})}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleOpenPlans}
						data-flx="app.app-layout.nagbars.price-announcement-nagbar.nagbar-button.open-plans"
					>
						{i18n._(VIEW_PLANS_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.price-announcement-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
