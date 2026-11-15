// SPDX-License-Identifier: AGPL-3.0-or-later

import {PurchaseDisclaimer} from '@app/features/app/components/dialogs/components/PurchaseDisclaimer';
import {useSubscriptionActions} from '@app/features/app/components/dialogs/components/plutonium/hooks/useSubscriptionActions';
import {InvoiceList} from '@app/features/app/components/dialogs/components/plutonium/InvoiceList';
import {RefundsSection} from '@app/features/app/components/dialogs/components/plutonium/RefundsSection';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import GeoIP from '@app/features/app/state/GeoIP';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import PremiumState from '@app/features/premium/state/PremiumState';
import {loadedPremiumStateFor} from '@app/features/premium/utils/BillingTabVisibility';
import {
	PAYMENT_METHODS_SECTION_DESCRIPTOR,
	RECEIPTS_SECTION_DESCRIPTOR,
} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {arePremiumPurchasesAvailable, canServiceStripeSubscriptions} from '@app/features/premium/utils/PremiumUtils';
import {Button} from '@app/features/ui/button/Button';
import Users from '@app/features/user/state/Users';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect} from 'react';

const PaymentsTab = observer(function PaymentsTab() {
	const {i18n} = useLingui();
	const currentUser = Users.currentUser;
	const premiumState = loadedPremiumStateFor(PremiumState, currentUser);
	const countryCode = GeoIP.countryCode;
	const {loadingPortal, handleOpenCustomerPortal} = useSubscriptionActions(countryCode);
	const refreshPremiumState = useCallback(
		() => PremiumCommands.refreshPremiumState(countryCode ?? undefined),
		[countryCode],
	);
	useEffect(() => {
		if (!currentUser?.id) return;
		void refreshPremiumState();
	}, [currentUser?.id, refreshPremiumState]);
	if (!premiumState) return null;
	const showPaymentMethods =
		premiumState.billing.stripe_customer_id != null &&
		(arePremiumPurchasesAvailable() || canServiceStripeSubscriptions());
	const showRefunds = !RuntimeConfig.isSelfHosted();
	const documents = premiumState.billing.documents;
	return (
		<SettingsTabContainer data-flx="user.payments-tab.settings-tab-container">
			{showPaymentMethods && (
				<SettingsSection
					id="payment-methods"
					title={i18n._(PAYMENT_METHODS_SECTION_DESCRIPTOR)}
					data-flx="user.payments-tab.payment-methods"
				>
					<div data-flx="user.payments-tab.payment-methods.actions">
						<Button
							small
							variant="secondary"
							onClick={handleOpenCustomerPortal}
							submitting={loadingPortal}
							fitContent
							data-flx="user.payments-tab.button.open-customer-portal"
						>
							<Trans comment="Billing button that opens Stripe customer portal for payment-method management only.">
								Manage payment methods
							</Trans>
						</Button>
					</div>
				</SettingsSection>
			)}
			{showRefunds && (
				<RefundsSection
					eligibility={
						premiumState.subscription_provider === 'app_store' ? null : premiumState.billing.refund_eligibility
					}
					countryCode={premiumState.pricing.country_code}
					refreshPremiumState={refreshPremiumState}
					data-flx="user.payments-tab.refunds-section"
				/>
			)}
			{documents.length > 0 && (
				<SettingsSection
					id="receipts"
					title={i18n._(RECEIPTS_SECTION_DESCRIPTOR)}
					data-flx="user.payments-tab.receipts"
				>
					<InvoiceList documents={documents} data-flx="user.payments-tab.invoice-list" />
				</SettingsSection>
			)}
			<PurchaseDisclaimer align="left" isPremium data-flx="user.payments-tab.purchase-disclaimer" />
		</SettingsTabContainer>
	);
});

export default PaymentsTab;
