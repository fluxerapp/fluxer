// SPDX-License-Identifier: AGPL-3.0-or-later
import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {openCustomerPortalWithErrorModal} from '@app/features/premium/commands/PremiumCommands';
import {getPremiumGraceEndDate} from '@app/features/premium/utils/PremiumGrace';
import {MANAGE_SUBSCRIPTION_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import Users from '@app/features/user/state/Users';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {getFormattedLongDate} from '@fluxer/date_utils/src/DateFormatting';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const PREMIUM_EXPIRED_MESSAGE_DESCRIPTOR = msg({
	message:
		'Your {premiumProductName} subscription has expired. You have lost all {premiumProductName} perks. Reactivate your subscription to regain access.',
	comment:
		'Nagbar body shown when a premium subscription has expired. {premiumProductName} is the premium product name.',
});
const REACTIVATE_DESCRIPTOR = msg({
	message: 'Reactivate',
	comment: 'Button label on the premium-expired nagbar. Opens billing so the user can reactivate their subscription.',
});
const PREMIUM_GRACE_PERIOD_MESSAGE_DESCRIPTOR = msg({
	message:
		"Your subscription failed to renew, but you still have access to {premiumProductName} perks until {formattedGraceDate}. Take action now or you'll lose all perks.",
	comment:
		'Nagbar body shown while a premium subscription is in its grace period. {premiumProductName} is the premium product name and {formattedGraceDate} is already localized.',
});
export const PremiumStatusNagbar = observer(
	({isMobile, variant}: {isMobile: boolean; variant: 'expired' | 'grace-period'}) => {
		const {i18n} = useLingui();
		const user = Users.currentUser;
		const [loadingPortal, setLoadingPortal] = useState(false);
		const expired = variant === 'expired';
		const handleOpenCustomerPortal = async () => {
			setLoadingPortal(true);
			try {
				await openCustomerPortalWithErrorModal(
					expired
						? 'app.app-layout.nagbars.premium-expired-nagbar.open-customer-portal.generic-error-modal'
						: 'app.app-layout.nagbars.premium-grace-period-nagbar.open-customer-portal.generic-error-modal',
				);
			} finally {
				setLoadingPortal(false);
			}
		};
		const handleDismiss = () => {
			NagbarCommands.dismissNagbar(expired ? 'premiumExpiredDismissed' : 'premiumGracePeriodDismissed');
		};
		if (!user?.premiumUntil || (expired && user.premiumWillCancel)) return null;
		const tone = NAGBAR_TONES[expired ? NagbarToneKind.DANGER : NagbarToneKind.PREMIUM];
		const message = expired
			? i18n._(PREMIUM_EXPIRED_MESSAGE_DESCRIPTOR, {premiumProductName: getPremiumProductName()})
			: i18n._(PREMIUM_GRACE_PERIOD_MESSAGE_DESCRIPTOR, {
					premiumProductName: getPremiumProductName(),
					formattedGraceDate: getFormattedLongDate(
						getPremiumGraceEndDate(new Date(user.premiumUntil), user.premiumGraceEndsAt),
						LocaleUtils.getCurrentLocale(),
					),
				});
		return (
			<Nagbar
				isMobile={isMobile}
				backgroundColor={tone.backgroundColor}
				textColor={tone.textColor}
				dismissible
				onDismiss={handleDismiss}
				data-flx="app.app-layout.nagbars.premium-status-nagbar.nagbar"
			>
				<NagbarContent
					isMobile={isMobile}
					onDismiss={handleDismiss}
					message={message}
					actions={
						<NagbarButton
							isMobile={isMobile}
							onClick={handleOpenCustomerPortal}
							submitting={loadingPortal}
							data-flx="app.app-layout.nagbars.premium-status-nagbar.nagbar-button.open-customer-portal"
						>
							{i18n._(expired ? REACTIVATE_DESCRIPTOR : MANAGE_SUBSCRIPTION_DESCRIPTOR)}
						</NagbarButton>
					}
					data-flx="app.app-layout.nagbars.premium-status-nagbar.nagbar-content"
				/>
			</Nagbar>
		);
	},
);
