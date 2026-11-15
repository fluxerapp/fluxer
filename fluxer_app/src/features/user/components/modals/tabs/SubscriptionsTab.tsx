// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {useCommunityActions} from '@app/features/app/components/dialogs/components/plutonium/hooks/useCommunityActions';
import {usePremiumData} from '@app/features/app/components/dialogs/components/plutonium/hooks/usePremiumData';
import {useSubscriptionActions} from '@app/features/app/components/dialogs/components/plutonium/hooks/useSubscriptionActions';
import {SubscriptionCard} from '@app/features/app/components/dialogs/components/plutonium/SubscriptionCard';
import {SettingsTabContainer} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import GeoIP from '@app/features/app/state/GeoIP';
import Guilds from '@app/features/guild/state/Guilds';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import * as PlutoniumPageCommands from '@app/features/premium/commands/PlutoniumPageCommands';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import {
	resolvePurchaseBlockedNote,
	resolvePurchaseSurface,
} from '@app/features/premium/components/plutonium_page/PlutoniumPagePurchases';
import {usePurchaseAccountGate} from '@app/features/premium/hooks/usePurchaseAccountGate';
import PremiumState from '@app/features/premium/state/PremiumState';
import {loadedPremiumStateFor} from '@app/features/premium/utils/BillingTabVisibility';
import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import {getSubscriptionStatus} from '@app/features/premium/utils/SubscriptionStatus';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import Users from '@app/features/user/state/Users';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo} from 'react';

const SubscriptionsTab = observer(function SubscriptionsTab() {
	const {i18n} = useLingui();
	const currentUser = Users.currentUser;
	const premiumState = loadedPremiumStateFor(PremiumState, currentUser);
	const countryCode = GeoIP.countryCode;
	const locale = LocaleUtils.getCurrentLocale();
	const guilds = Guilds.getGuilds();
	const visionaryGuild = useMemo(() => guilds.find((guild) => guild.features.has(GuildFeatures.VISIONARY)), [guilds]);
	const subscriptionStatus = getSubscriptionStatus(currentUser, premiumState);
	const {
		priceIds,
		monthlyPrice,
		yearlyPrice,
		currentSubscriptionPrice,
		currentSubscriptionPriceLabel,
		currentSubscriptionListPriceLabel,
		isCurrentSubscriptionGrandfathered,
	} = usePremiumData({premiumState});
	const {
		loadingPortal,
		loadingCancel,
		loadingReactivate,
		loadingEndGrace,
		loadingChangeBillingCycle,
		loadingCancelPendingChange,
		handleOpenCustomerPortal,
		handleCancelSubscription,
		handleEndPremiumGracePeriod,
		handleReactivateSubscription,
		handleChangeSubscriptionBillingCycle,
		handleCancelPendingSubscriptionChange,
	} = useSubscriptionActions(countryCode);
	const {loadingRejoinCommunity, handleCommunityButtonClick} = useCommunityActions(visionaryGuild);
	const accountGate = usePurchaseAccountGate();
	useEffect(() => {
		if (!currentUser?.id) return;
		void PremiumCommands.refreshPremiumState(countryCode ?? undefined);
	}, [countryCode, currentUser?.id]);
	const handleCancelSubscriptionConfirmed = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={<Trans>Cancel subscription?</Trans>}
					description={
						<Trans>
							You keep your perks until your next renewal date. Reactivate before then to keep your subscriber history.
						</Trans>
					}
					primaryText={<Trans>Cancel subscription</Trans>}
					primaryVariant="danger"
					secondaryText={<Trans>Keep subscription</Trans>}
					onPrimary={async () => {
						await handleCancelSubscription();
					}}
					data-flx="user.subscriptions-tab.cancel-subscription.confirm-modal"
				/>
			)),
		);
	}, [handleCancelSubscription]);
	const handleEndPremiumGracePeriodConfirmed = useCallback(() => {
		const PREMIUM_PRODUCT_NAME = getPremiumProductName();
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={<Trans>End grace period now?</Trans>}
					description={
						<Trans>
							You lose all {PREMIUM_PRODUCT_NAME} perks immediately and your subscriber history resets. This cannot be
							undone.
						</Trans>
					}
					primaryText={<Trans>End grace period</Trans>}
					primaryVariant="danger"
					secondaryText={<Trans>Keep grace period</Trans>}
					onPrimary={async () => {
						await handleEndPremiumGracePeriod();
					}}
					data-flx="user.subscriptions-tab.end-grace-period.confirm-modal"
				/>
			)),
		);
	}, [handleEndPremiumGracePeriod]);
	if (!currentUser || !premiumState) return null;
	const paymentDisputeOpen = premiumState.effective.payment_dispute_open === true;
	const purchaseBlockedNote = resolvePurchaseBlockedNote(i18n, accountGate.blockedReason, paymentDisputeOpen);
	const {canSubscribe, billingUnavailable} = resolvePurchaseSurface({
		priceIds,
		userPurchaseDisabled: premiumState.effective.premium_purchase_disabled === true,
		isMember: subscriptionStatus.shouldShowPremiumCard,
		canStartSubscription: subscriptionStatus.canStartSubscription,
		hasBillingRelationship: subscriptionStatus.hasEverPurchased || premiumState.billing.subscription != null,
	});
	return (
		<SettingsTabContainer data-flx="user.subscriptions-tab.settings-tab-container">
			<SubscriptionCard
				locale={locale}
				isVisionary={subscriptionStatus.isVisionary}
				perksDisabled={subscriptionStatus.perksDisabled}
				isGiftSubscription={subscriptionStatus.isGiftSubscription}
				isGiftGrace={subscriptionStatus.isGiftGrace}
				canSubscribe={canSubscribe}
				storeSubscription={subscriptionStatus.storeSubscription}
				premiumUntil={subscriptionStatus.actualPremiumUntil}
				renewsAt={subscriptionStatus.renewsAt}
				billingCycle={subscriptionStatus.billingCycle}
				monthlyPrice={monthlyPrice}
				yearlyPrice={yearlyPrice}
				monthlyAmountMinor={priceIds?.monthly_amount_minor ?? null}
				yearlyAmountMinor={priceIds?.yearly_amount_minor ?? null}
				priceCurrency={priceIds?.currency ?? null}
				currentSubscriptionAmountMinor={currentSubscriptionPrice?.amount_minor ?? null}
				currentSubscriptionCurrency={currentSubscriptionPrice?.currency ?? null}
				currentSubscriptionPriceLabel={currentSubscriptionPriceLabel}
				currentSubscriptionListPriceLabel={currentSubscriptionListPriceLabel}
				isCurrentSubscriptionGrandfathered={isCurrentSubscriptionGrandfathered}
				pendingSubscriptionChange={premiumState.billing.pending_subscription_change ?? null}
				gracePeriodInfo={subscriptionStatus.gracePeriodInfo}
				premiumWillCancel={subscriptionStatus.premiumWillCancel}
				hasEverPurchased={subscriptionStatus.hasEverPurchased}
				shouldUseCancelQuickAction={subscriptionStatus.shouldUseCancelQuickAction}
				shouldUseReactivateQuickAction={subscriptionStatus.shouldUseReactivateQuickAction}
				shouldUseChangePlanQuickAction={subscriptionStatus.shouldUseChangePlanQuickAction}
				loadingPortal={loadingPortal}
				loadingCancel={loadingCancel}
				loadingReactivate={loadingReactivate}
				loadingEndGrace={loadingEndGrace}
				loadingChangeBillingCycle={loadingChangeBillingCycle}
				loadingCancelPendingChange={loadingCancelPendingChange}
				loadingRejoinCommunity={loadingRejoinCommunity}
				scrollToPerks={PlutoniumPageCommands.openPlutoniumPage}
				navigateToRedeemGift={() => ComponentBus.dispatch('USER_SETTINGS_TAB_SELECT', {tab: 'gift_inventory'})}
				handleStartSubscription={PlutoniumPageCommands.openPlutoniumPage}
				handleOpenCustomerPortal={handleOpenCustomerPortal}
				handleReactivateSubscription={handleReactivateSubscription}
				handleCancelSubscription={handleCancelSubscriptionConfirmed}
				handleEndPremiumGracePeriod={handleEndPremiumGracePeriodConfirmed}
				handleChangeSubscriptionBillingCycle={handleChangeSubscriptionBillingCycle}
				handleCancelPendingSubscriptionChange={handleCancelPendingSubscriptionChange}
				handleCommunityButtonClick={handleCommunityButtonClick}
				purchaseDisabled={accountGate.blockedReason !== null || paymentDisputeOpen}
				purchaseDisabledTooltip={purchaseBlockedNote ?? undefined}
				billingUnavailable={billingUnavailable}
				data-flx="user.subscriptions-tab.subscription-card"
			/>
		</SettingsTabContainer>
	);
});

export default SubscriptionsTab;
