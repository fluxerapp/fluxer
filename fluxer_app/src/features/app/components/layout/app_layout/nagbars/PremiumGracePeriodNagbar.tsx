// SPDX-License-Identifier: AGPL-3.0-or-later

import {showPremiumActionErrorModal} from '@app/features/app/components/dialogs/components/plutonium/utils/PremiumActionErrorModal';
import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import {MANAGE_SUBSCRIPTION_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import Users from '@app/features/user/state/Users';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import {getFormattedLongDate} from '@fluxer/date_utils/src/DateFormatting';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const PREMIUM_GRACE_PERIOD_MESSAGE_DESCRIPTOR = msg({
	message:
		"Your subscription failed to renew, but you still have access to {premiumProductName} perks until {formattedGraceDate}. Take action now or you'll lose all perks.",
	comment:
		'Nagbar body shown while a premium subscription is in its grace period. {premiumProductName} is the premium product name and {formattedGraceDate} is already localized.',
});
const CUSTOMER_PORTAL_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't open the billing portal",
	comment:
		'Title of the error modal shown when opening the billing customer portal from the grace-period nagbar fails.',
});
const CUSTOMER_PORTAL_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while opening the billing portal. Please try again in a moment.',
	comment: 'Body of the error modal shown when opening the billing customer portal from the grace-period nagbar fails.',
});
const logger = new Logger('PremiumGracePeriodNagbar');
export const PremiumGracePeriodNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	const [loadingPortal, setLoadingPortal] = useState(false);
	const handleOpenCustomerPortal = async () => {
		setLoadingPortal(true);
		try {
			const url = await PremiumCommands.createCustomerPortalSession();
			void openExternalUrl(url);
		} catch (error) {
			logger.error('Failed to open customer portal', error);
			showPremiumActionErrorModal(
				error,
				{
					fallbackTitle: CUSTOMER_PORTAL_FAILED_TITLE_DESCRIPTOR,
					fallbackMessage: CUSTOMER_PORTAL_FAILED_MESSAGE_DESCRIPTOR,
				},
				'app.app-layout.nagbars.premium-grace-period-nagbar.open-customer-portal.generic-error-modal',
			);
		} finally {
			setLoadingPortal(false);
		}
	};
	const handleDismiss = () => {
		NagbarCommands.dismissNagbar('premiumGracePeriodDismissed');
	};
	if (!user?.premiumUntil || user?.premiumWillCancel) return null;
	const expiryDate = new Date(user.premiumUntil);
	const gracePeriodMs = 3 * MS_PER_DAY;
	const graceEndDate = new Date(expiryDate.getTime() + gracePeriodMs);
	const locale = LocaleUtils.getCurrentLocale();
	const formattedGraceDate = getFormattedLongDate(graceEndDate, locale);
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.PREMIUM].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.PREMIUM].textColor}
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.premium-grace-period-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={i18n._(PREMIUM_GRACE_PERIOD_MESSAGE_DESCRIPTOR, {
					premiumProductName: PREMIUM_PRODUCT_NAME,
					formattedGraceDate,
				})}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleOpenCustomerPortal}
						submitting={loadingPortal}
						data-flx="app.app-layout.nagbars.premium-grace-period-nagbar.nagbar-button.open-customer-portal"
					>
						{i18n._(MANAGE_SUBSCRIPTION_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.premium-grace-period-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
