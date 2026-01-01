/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import {CrownIcon, NetworkSlashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {GuildFeatures} from '~/Constants';
import {Spinner} from '~/components/uikit/Spinner';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import GeoIPStore from '~/stores/GeoIPStore';
import GuildStore from '~/stores/GuildStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import UserStore from '~/stores/UserStore';
import * as LocaleUtils from '~/utils/LocaleUtils';
import {getFormattedPrice, PricingTier} from '~/utils/PricingUtils';
import {FeatureComparisonTable} from './FeatureComparisonTable';
import styles from './PlutoniumContent.module.css';
import {PurchaseDisclaimer} from './PurchaseDisclaimer';
import {BottomCTASection} from './plutonium/BottomCTASection';
import {GiftInventoryBanner} from './plutonium/GiftInventoryBanner';
import {GiftSection} from './plutonium/GiftSection';
import {useCheckoutActions} from './plutonium/hooks/useCheckoutActions';
import {useCommunityActions} from './plutonium/hooks/useCommunityActions';
import {usePremiumData} from './plutonium/hooks/usePremiumData';
import {useSubscriptionActions} from './plutonium/hooks/useSubscriptionActions';
import {useSubscriptionStatus} from './plutonium/hooks/useSubscriptionStatus';
import {PlutoniumUpsellBanner} from './plutonium/PlutoniumUpsellBanner';
import {PricingSection} from './plutonium/PricingSection';
import {PurchaseHistorySection} from './plutonium/PurchaseHistorySection';
import {SectionHeader} from './plutonium/SectionHeader';
import {SubscriptionCard} from './plutonium/SubscriptionCard';
import {VisionarySection} from './plutonium/VisionarySection';
import {Slate} from './Slate';

export const PlutoniumContent: React.FC<{defaultGiftMode?: boolean}> = observer(({defaultGiftMode = false}) => {
	const {t} = useLingui();
	const currentUser = UserStore.currentUser;
	const locale = LocaleUtils.getCurrentLocale();
	const formatter = new Intl.NumberFormat(locale);
	const mobileLayoutState = MobileLayoutStore;

	const [isGiftMode, setIsGiftMode] = React.useState(defaultGiftMode);
	const giftSectionRef = React.useRef<HTMLDivElement | null>(null);
	const perksSectionRef = React.useRef<HTMLDivElement | null>(null);

	const countryCode = GeoIPStore.countryCode;
	const guilds = GuildStore.getGuilds();

	const visionaryGuild = React.useMemo(() => {
		return guilds.find((guild) => guild.features.has(GuildFeatures.VISIONARY));
	}, [guilds]);

	const operatorGuild = React.useMemo(() => {
		return guilds.find((guild) => guild.features.has(GuildFeatures.OPERATOR));
	}, [guilds]);

	const subscriptionStatus = useSubscriptionStatus(currentUser);
	const {visionarySlots, priceIds, loadingSlots, slotsError, isVisionarySoldOut} = usePremiumData(countryCode);
	const {
		loadingPortal,
		loadingCancel,
		loadingReactivate,
		handleOpenCustomerPortal,
		handleCancelSubscription,
		handleReactivateSubscription,
	} = useSubscriptionActions();
	const {
		loadingRejoinCommunity,
		isCommunityMenuOpen,
		communityButtonRef,
		handleCommunityButtonPointerDown,
		handleCommunityButtonClick,
	} = useCommunityActions(visionaryGuild, operatorGuild);
	const {loadingCheckout, handleSelectPlan} = useCheckoutActions(
		priceIds,
		subscriptionStatus.isGiftSubscription,
		mobileLayoutState.enabled,
	);

	const monthlyPrice = React.useMemo(() => getFormattedPrice(PricingTier.Monthly, countryCode), [countryCode]);
	const yearlyPrice = React.useMemo(() => getFormattedPrice(PricingTier.Yearly, countryCode), [countryCode]);
	const visionaryPrice = React.useMemo(() => getFormattedPrice(PricingTier.Visionary, countryCode), [countryCode]);

	const scrollToPerks = React.useCallback(() => {
		perksSectionRef.current?.scrollIntoView({behavior: 'smooth', block: 'start'});
	}, []);

	const handlePerksKeyDown = React.useCallback(
		(event: React.KeyboardEvent<HTMLSpanElement>) => {
			if (event.key === 'Enter' || event.key === ' ') {
				event.preventDefault();
				scrollToPerks();
			}
		},
		[scrollToPerks],
	);

	const navigateToRedeemGift = React.useCallback(() => {
		ComponentDispatch.dispatch('USER_SETTINGS_TAB_SELECT', {tab: 'gift_inventory'});
	}, []);

	if (!currentUser) return null;

	if (defaultGiftMode) {
		return (
			<div className={styles.giftModeContainer}>
				<PlutoniumUpsellBanner />

				<GiftSection
					giftSectionRef={giftSectionRef}
					monthlyPrice={monthlyPrice}
					yearlyPrice={yearlyPrice}
					visionaryPrice={visionaryPrice}
					visionarySlots={visionarySlots}
					loadingCheckout={loadingCheckout}
					loadingSlots={loadingSlots}
					isVisionarySoldOut={isVisionarySoldOut}
					handleSelectPlan={handleSelectPlan}
				/>

				{loadingSlots ? (
					<div className={styles.spinnerContainer} aria-live="polite">
						<Spinner />
					</div>
				) : slotsError ? (
					<Slate
						icon={NetworkSlashIcon}
						title={t`Network error`}
						description={t`Failed to load Visionary information. Please try again later.`}
					/>
				) : !isVisionarySoldOut ? (
					<VisionarySection
						visionarySlots={visionarySlots}
						formatter={formatter}
						isVisionary={subscriptionStatus.isVisionary}
						isPremium={subscriptionStatus.isPremium}
						isGiftSubscription={subscriptionStatus.isGiftSubscription}
						loadingCheckout={loadingCheckout}
						loadingSlots={loadingSlots}
						handleSelectPlan={handleSelectPlan}
					/>
				) : null}

				<div ref={perksSectionRef}>
					<section className={styles.perksSection}>
						<SectionHeader title={<Trans>Free vs Plutonium</Trans>} />
						<div className={styles.comparisonTableContainer}>
							<FeatureComparisonTable formatter={formatter} />
						</div>
					</section>
				</div>
			</div>
		);
	}

	return (
		<div className={styles.mainContainer}>
			<GiftInventoryBanner currentUser={currentUser} />

			<div className={styles.header}>
				<div className={styles.iconContainer}>
					<CrownIcon className={styles.icon} weight="fill" />
				</div>
				<h1 className={styles.title}>
					<Trans>Fluxer Plutonium</Trans>
				</h1>
				<p className={styles.description}>
					<Trans>
						Unlock higher limits and exclusive features while supporting an independent communication platform.
					</Trans>
				</p>
			</div>

			{subscriptionStatus.hasEverPurchased && (
				<PurchaseHistorySection loadingPortal={loadingPortal} handleOpenCustomerPortal={handleOpenCustomerPortal} />
			)}

			{subscriptionStatus.shouldShowPremiumCard && (
				<section className={styles.subscriptionSection}>
					<SubscriptionCard
						currentUser={currentUser}
						locale={locale}
						isVisionary={subscriptionStatus.isVisionary}
						isGiftSubscription={subscriptionStatus.isGiftSubscription}
						billingCycle={subscriptionStatus.billingCycle}
						monthlyPrice={monthlyPrice}
						yearlyPrice={yearlyPrice}
						gracePeriodInfo={subscriptionStatus.gracePeriodInfo}
						premiumWillCancel={subscriptionStatus.premiumWillCancel}
						subscriptionCardColorClass={subscriptionStatus.subscriptionCardColorClass}
						subscriptionStatusColor={subscriptionStatus.subscriptionStatusColor}
						hasEverPurchased={subscriptionStatus.hasEverPurchased}
						isVisionarySoldOut={isVisionarySoldOut}
						shouldUseCancelQuickAction={subscriptionStatus.shouldUseCancelQuickAction}
						shouldUseReactivateQuickAction={subscriptionStatus.shouldUseReactivateQuickAction}
						loadingPortal={loadingPortal}
						loadingCancel={loadingCancel}
						loadingReactivate={loadingReactivate}
						loadingRejoinCommunity={loadingRejoinCommunity}
						loadingCheckout={loadingCheckout}
						loadingSlots={loadingSlots}
						isCommunityMenuOpen={isCommunityMenuOpen}
						communityButtonRef={communityButtonRef}
						scrollToPerks={scrollToPerks}
						handlePerksKeyDown={handlePerksKeyDown}
						navigateToRedeemGift={navigateToRedeemGift}
						handleSelectPlan={handleSelectPlan}
						handleOpenCustomerPortal={handleOpenCustomerPortal}
						handleReactivateSubscription={handleReactivateSubscription}
						handleCancelSubscription={handleCancelSubscription}
						handleCommunityButtonPointerDown={handleCommunityButtonPointerDown}
						handleCommunityButtonClick={handleCommunityButtonClick}
					/>
					<div className={styles.disclaimerContainer}>
						<PurchaseDisclaimer align="center" isPremium />
					</div>
				</section>
			)}

			{!subscriptionStatus.shouldShowPremiumCard ? (
				<PricingSection
					isGiftMode={isGiftMode}
					setIsGiftMode={setIsGiftMode}
					monthlyPrice={monthlyPrice}
					yearlyPrice={yearlyPrice}
					visionaryPrice={visionaryPrice}
					visionarySlots={visionarySlots}
					loadingCheckout={loadingCheckout}
					loadingSlots={loadingSlots}
					isVisionarySoldOut={isVisionarySoldOut}
					handleSelectPlan={handleSelectPlan}
				/>
			) : (
				<GiftSection
					giftSectionRef={giftSectionRef}
					monthlyPrice={monthlyPrice}
					yearlyPrice={yearlyPrice}
					visionaryPrice={visionaryPrice}
					visionarySlots={visionarySlots}
					loadingCheckout={loadingCheckout}
					loadingSlots={loadingSlots}
					isVisionarySoldOut={isVisionarySoldOut}
					handleSelectPlan={handleSelectPlan}
				/>
			)}

			{loadingSlots ? (
				<div className={styles.spinnerContainer} aria-live="polite">
					<Spinner />
				</div>
			) : slotsError ? (
				<Slate
					icon={NetworkSlashIcon}
					title={t`Network error`}
					description={t`Failed to load Visionary information. Please try again later.`}
				/>
			) : !isVisionarySoldOut ? (
				<VisionarySection
					visionarySlots={visionarySlots}
					formatter={formatter}
					isVisionary={subscriptionStatus.isVisionary}
					isPremium={subscriptionStatus.isPremium}
					isGiftSubscription={subscriptionStatus.isGiftSubscription}
					loadingCheckout={loadingCheckout}
					loadingSlots={loadingSlots}
					handleSelectPlan={handleSelectPlan}
				/>
			) : null}

			<div ref={perksSectionRef}>
				<section className={styles.perksSection}>
					<SectionHeader title={<Trans>Free vs Plutonium</Trans>} />
					<div className={styles.comparisonTableContainer}>
						<FeatureComparisonTable formatter={formatter} />
					</div>
				</section>
			</div>

			{!subscriptionStatus.isPremium && (
				<BottomCTASection
					isGiftMode={isGiftMode}
					monthlyPrice={monthlyPrice}
					yearlyPrice={yearlyPrice}
					visionaryPrice={visionaryPrice}
					loadingCheckout={loadingCheckout}
					loadingSlots={loadingSlots}
					isVisionarySoldOut={isVisionarySoldOut}
					handleSelectPlan={handleSelectPlan}
				/>
			)}
		</div>
	);
});
