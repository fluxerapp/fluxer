// SPDX-License-Identifier: AGPL-3.0-or-later

import {showPremiumActionErrorModal} from '@app/features/app/components/dialogs/components/plutonium/utils/PremiumActionErrorModal';
import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import Users from '@app/features/user/state/Users';
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
const CUSTOMER_PORTAL_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't open the billing portal",
	comment:
		'Title of the error modal shown when opening the billing customer portal from the premium-expired nagbar fails.',
});
const CUSTOMER_PORTAL_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while opening the billing portal. Please try again in a moment.',
	comment:
		'Body of the error modal shown when opening the billing customer portal from the premium-expired nagbar fails.',
});
const logger = new Logger('PremiumExpiredNagbar');
export const PremiumExpiredNagbar = observer(({isMobile}: {isMobile: boolean}) => {
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
				'app.app-layout.nagbars.premium-expired-nagbar.open-customer-portal.generic-error-modal',
			);
		} finally {
			setLoadingPortal(false);
		}
	};
	const handleDismiss = () => {
		NagbarCommands.dismissNagbar('premiumExpiredDismissed');
	};
	if (!user?.premiumUntil || user?.premiumWillCancel) return null;
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.DANGER].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.DANGER].textColor}
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.premium-expired-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={i18n._(PREMIUM_EXPIRED_MESSAGE_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleOpenCustomerPortal}
						submitting={loadingPortal}
						data-flx="app.app-layout.nagbars.premium-expired-nagbar.nagbar-button.open-customer-portal"
					>
						{i18n._(REACTIVATE_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.premium-expired-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
