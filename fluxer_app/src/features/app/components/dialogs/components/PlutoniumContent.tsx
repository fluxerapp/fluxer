// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {FeatureComparisonTable} from '@app/features/app/components/dialogs/components/FeatureComparisonTable';
import styles from '@app/features/app/components/dialogs/components/PlutoniumContent.module.css';
import {PurchaseDisclaimer} from '@app/features/app/components/dialogs/components/PurchaseDisclaimer';
import {BottomCTASection} from '@app/features/app/components/dialogs/components/plutonium/BottomCTASection';
import {GiftInventoryBanner} from '@app/features/app/components/dialogs/components/plutonium/GiftInventoryBanner';
import {GiftSection} from '@app/features/app/components/dialogs/components/plutonium/GiftSection';
import {useCheckoutActions} from '@app/features/app/components/dialogs/components/plutonium/hooks/useCheckoutActions';
import {useCommunityActions} from '@app/features/app/components/dialogs/components/plutonium/hooks/useCommunityActions';
import {usePremiumData} from '@app/features/app/components/dialogs/components/plutonium/hooks/usePremiumData';
import {useSubscriptionActions} from '@app/features/app/components/dialogs/components/plutonium/hooks/useSubscriptionActions';
import {useSubscriptionStatus} from '@app/features/app/components/dialogs/components/plutonium/hooks/useSubscriptionStatus';
import {SectionHeader} from '@app/features/app/components/dialogs/components/plutonium/PlutoniumSectionHeader';
import {PlutoniumUpsellBanner} from '@app/features/app/components/dialogs/components/plutonium/PlutoniumUpsellBanner';
import {PricingSection} from '@app/features/app/components/dialogs/components/plutonium/PricingSection';
import {PurchaseHistorySection} from '@app/features/app/components/dialogs/components/plutonium/PurchaseHistorySection';
import {SelfServeRefundSection} from '@app/features/app/components/dialogs/components/plutonium/SelfServeRefundSection';
import {SubscriptionCard} from '@app/features/app/components/dialogs/components/plutonium/SubscriptionCard';
import {PREMIUM_PRODUCT_FULL_NAME, PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import GeoIP from '@app/features/app/state/GeoIP';
import Guilds from '@app/features/guild/state/Guilds';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import PremiumState from '@app/features/premium/state/PremiumState';
import {
	CLAIM_ACCOUNT_TO_PURCHASE_PREMIUM_DESCRIPTOR,
	FREE_VS_PREMIUM_DESCRIPTOR,
	VERIFY_EMAIL_TO_PURCHASE_PREMIUM_DESCRIPTOR,
} from '@app/features/premium/utils/PremiumMessageDescriptors';
import type {PricingMode} from '@app/features/premium/utils/PricingUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Users from '@app/features/user/state/Users';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

interface PlutoniumContentProps {
	defaultGiftMode?: boolean;
}

export const PlutoniumContent = observer(({defaultGiftMode = false}: PlutoniumContentProps) => {
	const {i18n} = useLingui();
	const currentUser = Users.currentUser;
	const premiumState = PremiumState.loadedForUserId === currentUser?.id ? PremiumState.state : null;
	const locale = LocaleUtils.getCurrentLocale();
	const mobileLayoutState = MobileLayout;
	const [isGiftMode, setIsGiftMode] = useState(defaultGiftMode);
	const [pricingMode, setPricingMode] = useState<PricingMode>('localized');
	const giftSectionRef = useRef<HTMLDivElement | null>(null);
	const perksSectionRef = useRef<HTMLDivElement | null>(null);
	const countryCode = GeoIP.countryCode;
	const guilds = Guilds.getGuilds();
	const visionaryGuild = useMemo(() => {
		return guilds.find((guild) => guild.features.has(GuildFeatures.VISIONARY));
	}, [guilds]);
	const subscriptionStatus = useSubscriptionStatus(currentUser, premiumState);
	const {
		priceIds,
		monthlyPrice,
		yearlyPrice,
		giftMonthlyPrice,
		giftYearlyPrice,
		hasPricingChoice,
		localizedCurrency,
		baseCurrency,
		currentSubscriptionPrice,
		currentSubscriptionPriceLabel,
		currentSubscriptionListPriceLabel,
		isCurrentSubscriptionGrandfathered,
	} = usePremiumData(countryCode, pricingMode, {premiumState});
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
	const {loadingCheckout, handleSelectPlan} = useCheckoutActions(
		priceIds,
		countryCode,
		pricingMode,
		subscriptionStatus.isGiftSubscription,
		mobileLayoutState.enabled,
	);
	useEffect(() => {
		if (!hasPricingChoice && pricingMode === 'base') {
			setPricingMode('localized');
		}
	}, [hasPricingChoice, pricingMode]);
	useEffect(() => {
		if (!currentUser?.id) return;
		void PremiumCommands.refreshPremiumState(countryCode ?? undefined);
	}, [countryCode, currentUser?.id]);
	const isClaimed = currentUser?.isClaimed() ?? false;
	const isEmailVerified = currentUser?.verified === true;
	const purchaseDisabled = !isClaimed || !isEmailVerified;
	const purchaseDisabledTooltip = !isClaimed
		? i18n._(CLAIM_ACCOUNT_TO_PURCHASE_PREMIUM_DESCRIPTOR, {premiumProductFullName: PREMIUM_PRODUCT_FULL_NAME})
		: i18n._(VERIFY_EMAIL_TO_PURCHASE_PREMIUM_DESCRIPTOR, {premiumProductFullName: PREMIUM_PRODUCT_FULL_NAME});
	const handleSelectPlanGuarded = useCallback(
		(plan: 'monthly' | 'yearly' | 'gift_1_month' | 'gift_1_year') => {
			if (purchaseDisabled) return;
			handleSelectPlan(plan);
		},
		[handleSelectPlan, purchaseDisabled],
	);
	const scrollToPerks = useCallback(() => {
		perksSectionRef.current?.scrollIntoView({behavior: 'auto', block: 'start'});
	}, []);
	const navigateToRedeemGift = useCallback(() => {
		ComponentBus.dispatch('USER_SETTINGS_TAB_SELECT', {tab: 'gift_inventory'});
	}, []);
	const handleCancelSubscriptionConfirmed = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={<Trans>Cancel subscription?</Trans>}
					description={
						<Trans>
							You keep your perks until your next renewal date, then have a 3-day grace period to resubscribe and keep
							your subscriber history.
						</Trans>
					}
					primaryText={<Trans>Cancel subscription</Trans>}
					primaryVariant="danger"
					secondaryText={<Trans>Keep subscription</Trans>}
					onPrimary={async () => {
						await handleCancelSubscription();
					}}
					data-flx="app.plutonium-content.handle-cancel-subscription-confirmed.confirm-modal"
				/>
			)),
		);
	}, [handleCancelSubscription]);
	const handleEndPremiumGracePeriodConfirmed = useCallback(() => {
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
					data-flx="app.plutonium-content.handle-end-premium-grace-period-confirmed.confirm-modal"
				/>
			)),
		);
	}, [handleEndPremiumGracePeriod]);
	if (!currentUser) return null;
	if (defaultGiftMode) {
		return (
			<div className={styles.giftModeContainer} data-flx="app.plutonium-content.gift-mode-container">
				<PlutoniumUpsellBanner data-flx="app.plutonium-content.plutonium-upsell-banner" />
				<GiftSection
					giftSectionRef={giftSectionRef}
					countryCode={countryCode}
					pricingMode={pricingMode}
					setPricingMode={setPricingMode}
					hasPricingChoice={hasPricingChoice}
					localizedCurrency={localizedCurrency}
					baseCurrency={baseCurrency}
					giftMonthlyPrice={giftMonthlyPrice}
					giftYearlyPrice={giftYearlyPrice}
					loadingCheckout={loadingCheckout}
					handleSelectPlan={handleSelectPlanGuarded}
					purchaseDisabled={purchaseDisabled}
					purchaseDisabledTooltip={purchaseDisabledTooltip}
					data-flx="app.plutonium-content.gift-section"
				/>
				<div ref={perksSectionRef} data-flx="app.plutonium-content.div">
					<section className={styles.perksSection} data-flx="app.plutonium-content.perks-section">
						<SectionHeader
							title={i18n._(FREE_VS_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
							data-flx="app.plutonium-content.section-header"
						/>
						<div
							className={styles.comparisonTableContainer}
							data-flx="app.plutonium-content.comparison-table-container"
						>
							<FeatureComparisonTable data-flx="app.plutonium-content.feature-comparison-table" />
						</div>
					</section>
				</div>
			</div>
		);
	}
	return (
		<div className={styles.mainContainer} data-flx="app.plutonium-content.main-container">
			<GiftInventoryBanner currentUser={currentUser} data-flx="app.plutonium-content.gift-inventory-banner" />
			<div className={styles.header} data-flx="app.plutonium-content.header">
				<h1 className={styles.title} data-flx="app.plutonium-content.title">
					{PREMIUM_PRODUCT_FULL_NAME}
				</h1>
				<p className={styles.description} data-flx="app.plutonium-content.description">
					<Trans>
						Unlock higher limits and exclusive features while supporting an independent communication platform.
					</Trans>
				</p>
			</div>
			{subscriptionStatus.shouldShowPremiumCard && (
				<section className={styles.subscriptionSection} data-flx="app.plutonium-content.subscription-section">
					<SubscriptionCard
						locale={locale}
						isVisionary={subscriptionStatus.isVisionary}
						perksDisabled={subscriptionStatus.perksDisabled}
						isGiftSubscription={subscriptionStatus.isGiftSubscription}
						premiumUntil={subscriptionStatus.actualPremiumUntil}
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
						pendingSubscriptionChange={premiumState?.billing.pending_subscription_change ?? null}
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
						scrollToPerks={scrollToPerks}
						navigateToRedeemGift={navigateToRedeemGift}
						handleOpenCustomerPortal={handleOpenCustomerPortal}
						handleReactivateSubscription={handleReactivateSubscription}
						handleCancelSubscription={handleCancelSubscriptionConfirmed}
						handleEndPremiumGracePeriod={handleEndPremiumGracePeriodConfirmed}
						handleChangeSubscriptionBillingCycle={handleChangeSubscriptionBillingCycle}
						handleCancelPendingSubscriptionChange={handleCancelPendingSubscriptionChange}
						handleCommunityButtonClick={handleCommunityButtonClick}
						purchaseDisabled={purchaseDisabled}
						purchaseDisabledTooltip={purchaseDisabledTooltip}
						data-flx="app.plutonium-content.subscription-card"
					/>
					<div className={styles.disclaimerContainer} data-flx="app.plutonium-content.disclaimer-container">
						<PurchaseDisclaimer align="center" isPremium data-flx="app.plutonium-content.purchase-disclaimer" />
					</div>
				</section>
			)}
			{subscriptionStatus.hasEverPurchased && (
				<>
					<PurchaseHistorySection
						premiumState={premiumState}
						loadingPortal={loadingPortal}
						handleOpenCustomerPortal={handleOpenCustomerPortal}
						data-flx="app.plutonium-content.purchase-history-section"
					/>
					<SelfServeRefundSection
						eligibility={premiumState?.billing.refund_eligibility ?? null}
						refreshPremiumState={() => PremiumCommands.refreshPremiumState(countryCode ?? undefined)}
						data-flx="app.plutonium-content.self-serve-refund-section"
					/>
				</>
			)}
			{!subscriptionStatus.shouldShowPremiumCard ? (
				<PricingSection
					isGiftMode={isGiftMode}
					setIsGiftMode={setIsGiftMode}
					countryCode={countryCode}
					pricingMode={pricingMode}
					setPricingMode={setPricingMode}
					hasPricingChoice={hasPricingChoice}
					localizedCurrency={localizedCurrency}
					baseCurrency={baseCurrency}
					monthlyPrice={monthlyPrice}
					yearlyPrice={yearlyPrice}
					giftMonthlyPrice={giftMonthlyPrice}
					giftYearlyPrice={giftYearlyPrice}
					loadingCheckout={loadingCheckout}
					handleSelectPlan={handleSelectPlanGuarded}
					purchaseDisabled={purchaseDisabled}
					purchaseDisabledTooltip={purchaseDisabledTooltip}
					data-flx="app.plutonium-content.pricing-section"
				/>
			) : (
				<GiftSection
					giftSectionRef={giftSectionRef}
					countryCode={countryCode}
					pricingMode={pricingMode}
					setPricingMode={setPricingMode}
					hasPricingChoice={hasPricingChoice}
					localizedCurrency={localizedCurrency}
					baseCurrency={baseCurrency}
					giftMonthlyPrice={giftMonthlyPrice}
					giftYearlyPrice={giftYearlyPrice}
					loadingCheckout={loadingCheckout}
					handleSelectPlan={handleSelectPlanGuarded}
					purchaseDisabled={purchaseDisabled}
					purchaseDisabledTooltip={purchaseDisabledTooltip}
					data-flx="app.plutonium-content.gift-section--2"
				/>
			)}
			<div ref={perksSectionRef} data-flx="app.plutonium-content.div--2">
				<section className={styles.perksSection} data-flx="app.plutonium-content.perks-section--2">
					<SectionHeader
						title={i18n._(FREE_VS_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
						data-flx="app.plutonium-content.section-header--2"
					/>
					<div
						className={styles.comparisonTableContainer}
						data-flx="app.plutonium-content.comparison-table-container--2"
					>
						<FeatureComparisonTable data-flx="app.plutonium-content.feature-comparison-table--2" />
					</div>
				</section>
			</div>
			{!subscriptionStatus.isPremium && (
				<BottomCTASection
					isGiftMode={isGiftMode}
					countryCode={countryCode}
					pricingMode={pricingMode}
					setPricingMode={setPricingMode}
					hasPricingChoice={hasPricingChoice}
					localizedCurrency={localizedCurrency}
					baseCurrency={baseCurrency}
					monthlyPrice={monthlyPrice}
					yearlyPrice={yearlyPrice}
					giftMonthlyPrice={giftMonthlyPrice}
					giftYearlyPrice={giftYearlyPrice}
					loadingCheckout={loadingCheckout}
					handleSelectPlan={handleSelectPlanGuarded}
					purchaseDisabled={purchaseDisabled}
					purchaseDisabledTooltip={purchaseDisabledTooltip}
					data-flx="app.plutonium-content.bottom-cta-section"
				/>
			)}
		</div>
	);
});
