// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {useSubscriptionActions} from '@app/features/app/components/dialogs/components/plutonium/hooks/useSubscriptionActions';
import {ScheduledSubscriptionChangeDescription} from '@app/features/app/components/dialogs/components/plutonium/ScheduledSubscriptionChangeDescription';
import {SubscriptionActions} from '@app/features/app/components/dialogs/components/plutonium/SubscriptionActions';
import {SubscriptionBillingChangeDescription} from '@app/features/app/components/dialogs/components/plutonium/SubscriptionBillingChangeDescription';
import styles from '@app/features/app/components/dialogs/components/plutonium/SubscriptionCard.module.css';
import {SubscriptionDescription} from '@app/features/app/components/dialogs/components/plutonium/SubscriptionDescription';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import PremiumState from '@app/features/premium/state/PremiumState';
import {
	CLAIM_ACCOUNT_TO_PURCHASE_OR_REDEEM_PREMIUM_DESCRIPTOR,
	PREMIUM_SUBSCRIPTION_DESCRIPTOR,
} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {getPremiumProductFullName, getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import type {GracePeriodInfo} from '@app/features/premium/utils/SubscriptionStatus';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import Users from '@app/features/user/state/Users';
import {getFormattedLongDate} from '@fluxer/date_utils/src/DateFormatting';
import {formatMinorUnitPrice} from '@fluxer/number_utils/src/PriceFormatting';
import type {PendingSubscriptionChangeResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {PremiumStoreSubscriptionState} from '@fluxer/schema/src/domains/premium/StoreBillingSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const LEGACY_RATE_WITH_PRICE_DESCRIPTOR = msg({
	message: "You're on a legacy rate. The current price for this plan is {currentSubscriptionListPriceLabel}.",
	comment:
		'Plutonium subscription card tooltip shown when the user is on a grandfathered price; current list price is interpolated.',
});
const LEGACY_RATE_NO_PRICE_DESCRIPTOR = msg({
	message: "You're on a legacy rate no longer available to new subscribers.",
	comment:
		'Plutonium subscription card tooltip shown when the user is on a grandfathered price that is no longer offered.',
});
const SWITCH_TO_YEARLY_BILLING_DESCRIPTOR = msg({
	message: 'Switch to yearly billing?',
	comment: 'Billing confirmation title for changing an active subscription to yearly billing.',
});
const SWITCH_TO_MONTHLY_BILLING_DESCRIPTOR = msg({
	message: 'Switch to monthly billing?',
	comment: 'Billing confirmation title for changing an active subscription to monthly billing.',
});
const SWITCH_TO_YEARLY_DESCRIPTOR = msg({
	message: 'Switch to yearly',
	comment: 'Button confirming a change to yearly subscription billing.',
});
const SWITCH_TO_MONTHLY_DESCRIPTOR = msg({
	message: 'Switch to monthly',
	comment: 'Button confirming a change to monthly subscription billing.',
});
const SWITCH_TO_THE_NEW_PRICE_TITLE_DESCRIPTOR = msg({
	message: 'Switch to the new price?',
	comment: 'Billing confirmation title for moving an active subscription down to the current price.',
});
const SWITCH_PRICE_DESCRIPTOR = msg({
	message: 'Switch price',
	comment: 'Button confirming a move of an active subscription down to the current price.',
});
const VISIONARY_DESCRIPTOR = msg({
	message: 'Visionary',
	comment:
		'Name of the Visionary plan, shown as the heading of the Subscriptions settings tab for a Visionary. Keep it as is.',
});
const NOT_NOW_DESCRIPTOR = msg({
	message: 'Not now',
	comment: 'Button that cancels a subscription billing change.',
});

interface SubscriptionCardProps {
	locale: string;
	isVisionary: boolean;
	perksDisabled: boolean;
	isGiftSubscription: boolean;
	isGiftGrace: boolean;
	canSubscribe: boolean;
	storeSubscription: PremiumStoreSubscriptionState | null;
	premiumUntil: Date | null;
	renewsAt: Date | null;
	billingCycle: string | null;
	monthlyPrice: string;
	yearlyPrice: string;
	monthlyAmountMinor: number | null;
	yearlyAmountMinor: number | null;
	priceCurrency: string | null;
	currentSubscriptionAmountMinor: number | null;
	currentSubscriptionCurrency: string | null;
	currentSubscriptionPriceLabel: string | null;
	currentSubscriptionListPriceLabel: string | null;
	isCurrentSubscriptionGrandfathered: boolean;
	pendingSubscriptionChange: PendingSubscriptionChangeResponse;
	gracePeriodInfo: GracePeriodInfo;
	premiumWillCancel: boolean;
	hasEverPurchased: boolean;
	shouldUseCancelQuickAction: boolean;
	shouldUseReactivateQuickAction: boolean;
	shouldUseChangePlanQuickAction: boolean;
	loadingPortal: boolean;
	loadingCancel: boolean;
	loadingReactivate: boolean;
	loadingEndGrace: boolean;
	loadingChangeBillingCycle: 'monthly' | 'yearly' | null;
	loadingCancelPendingChange: boolean;
	loadingRejoinCommunity: boolean;
	scrollToPerks: () => void;
	navigateToRedeemGift: () => void;
	handleStartSubscription: () => void;
	handleOpenCustomerPortal: () => void;
	handleReactivateSubscription: () => void;
	handleCancelSubscription: () => void;
	handleEndPremiumGracePeriod: () => void;
	handleChangeSubscriptionBillingCycle: (billingCycle: 'monthly' | 'yearly') => void;
	handleCancelPendingSubscriptionChange: () => void;
	handleCommunityButtonClick: () => void;
	purchaseDisabled?: boolean;
	purchaseDisabledTooltip?: React.ReactNode;
	billingUnavailable?: boolean;
}

export const SubscriptionCard: React.FC<SubscriptionCardProps> = observer(
	({
		locale,
		isVisionary,
		perksDisabled,
		isGiftSubscription,
		isGiftGrace,
		canSubscribe,
		storeSubscription,
		premiumUntil,
		renewsAt,
		billingCycle,
		monthlyPrice,
		yearlyPrice,
		monthlyAmountMinor,
		yearlyAmountMinor,
		priceCurrency,
		currentSubscriptionAmountMinor,
		currentSubscriptionCurrency,
		currentSubscriptionPriceLabel,
		currentSubscriptionListPriceLabel,
		isCurrentSubscriptionGrandfathered,
		pendingSubscriptionChange,
		gracePeriodInfo,
		premiumWillCancel,
		hasEverPurchased,
		shouldUseCancelQuickAction,
		shouldUseReactivateQuickAction,
		shouldUseChangePlanQuickAction,
		loadingPortal,
		loadingCancel,
		loadingReactivate,
		loadingEndGrace,
		loadingChangeBillingCycle,
		loadingCancelPendingChange,
		loadingRejoinCommunity,
		scrollToPerks,
		navigateToRedeemGift,
		handleStartSubscription,
		handleOpenCustomerPortal,
		handleReactivateSubscription,
		handleCancelSubscription,
		handleEndPremiumGracePeriod,
		handleChangeSubscriptionBillingCycle,
		handleCancelPendingSubscriptionChange,
		handleCommunityButtonClick,
		purchaseDisabled = false,
		purchaseDisabledTooltip,
		billingUnavailable = false,
	}) => {
		const {i18n} = useLingui();
		const {loadingSwitchToListPrice, handleSwitchToListPrice} = useSubscriptionActions();
		const {isInGracePeriod, isExpired: isFullyExpired, graceEndDate} = gracePeriodInfo;
		const tooltipText: string | (() => React.ReactNode) =
			purchaseDisabledTooltip != null
				? () => purchaseDisabledTooltip
				: i18n._(CLAIM_ACCOUNT_TO_PURCHASE_OR_REDEEM_PREMIUM_DESCRIPTOR, {
						premiumProductFullName: getPremiumProductFullName(),
					});
		let targetBillingCycle: 'monthly' | 'yearly' | null = null;
		if (billingCycle === 'monthly') {
			targetBillingCycle = 'yearly';
		} else if (billingCycle === 'yearly') {
			targetBillingCycle = 'monthly';
		}
		const effectiveMonthlyPrice =
			billingCycle === 'monthly' && currentSubscriptionPriceLabel ? currentSubscriptionPriceLabel : monthlyPrice;
		const effectiveYearlyPrice =
			billingCycle === 'yearly' && currentSubscriptionPriceLabel ? currentSubscriptionPriceLabel : yearlyPrice;
		const hasPendingSubscriptionChange = pendingSubscriptionChange != null && !premiumWillCancel && !storeSubscription;
		const currentUserId = Users.currentUser?.id;
		const listPriceSwitch =
			!storeSubscription && currentUserId != null && PremiumState.loadedForUserId === currentUserId
				? (PremiumState.state?.billing.list_price_switch ?? null)
				: null;
		const countryCode = PremiumState.state?.pricing.country_code;
		const listPriceNewLabel = listPriceSwitch
			? formatMinorUnitPrice(listPriceSwitch.list_amount_minor, listPriceSwitch.currency, locale, countryCode)
			: null;
		const listPriceCurrentLabel = listPriceSwitch
			? formatMinorUnitPrice(listPriceSwitch.current_amount_minor, listPriceSwitch.currency, locale, countryCode)
			: null;
		const listPriceEffectiveDate = listPriceSwitch?.effective_at
			? getFormattedLongDate(listPriceSwitch.effective_at, locale)
			: null;
		const hasPendingListPriceSwitch = listPriceSwitch?.pending === true && !premiumWillCancel;
		let pendingChangeDate: string | null = null;
		if (pendingSubscriptionChange) {
			pendingChangeDate = getFormattedLongDate(new Date(pendingSubscriptionChange.effective_at), locale);
		} else if (hasPendingListPriceSwitch) {
			pendingChangeDate = listPriceEffectiveDate;
		}
		const pendingInitialPriceLabel = pendingSubscriptionChange
			? formatMinorUnitPrice(
					pendingSubscriptionChange.initial_amount_minor,
					pendingSubscriptionChange.currency,
					locale,
					countryCode,
				)
			: null;
		const pendingRecurringPriceLabel = pendingSubscriptionChange
			? formatMinorUnitPrice(
					pendingSubscriptionChange.recurring_amount_minor,
					pendingSubscriptionChange.currency,
					locale,
					countryCode,
				)
			: null;
		const pendingCreditPriceLabel = pendingSubscriptionChange
			? formatMinorUnitPrice(
					pendingSubscriptionChange.credit_amount_minor,
					pendingSubscriptionChange.currency,
					locale,
					countryCode,
				)
			: null;
		const canSwitchToListPrice =
			listPriceSwitch?.available === true &&
			!premiumWillCancel &&
			!hasPendingSubscriptionChange &&
			listPriceNewLabel != null &&
			listPriceCurrentLabel != null &&
			listPriceEffectiveDate != null;
		const shouldSuggestCancelingPendingChange =
			listPriceSwitch?.available === false &&
			listPriceSwitch.reason === 'conflicting_pending_change' &&
			listPriceSwitch.list_amount_minor != null &&
			listPriceSwitch.current_amount_minor != null &&
			listPriceSwitch.list_amount_minor < listPriceSwitch.current_amount_minor;
		const shouldMentionListPriceWhileCancelling =
			listPriceSwitch?.available === false &&
			listPriceSwitch.reason === 'subscription_cancelling' &&
			listPriceSwitch.list_amount_minor != null &&
			listPriceSwitch.current_amount_minor != null &&
			listPriceSwitch.list_amount_minor < listPriceSwitch.current_amount_minor;
		const pendingTargetPriceLabel = hasPendingListPriceSwitch ? listPriceNewLabel : null;
		const grandfatheredTooltip =
			isCurrentSubscriptionGrandfathered && currentSubscriptionListPriceLabel
				? i18n._(LEGACY_RATE_WITH_PRICE_DESCRIPTOR, {currentSubscriptionListPriceLabel})
				: i18n._(LEGACY_RATE_NO_PRICE_DESCRIPTOR);
		const handleConfirmBillingCycleChange = useCallback(
			(targetCycle: 'monthly' | 'yearly') => {
				const isSwitchingToYearly = targetCycle === 'yearly';
				const renewalPrice = isSwitchingToYearly ? effectiveYearlyPrice : effectiveMonthlyPrice;
				const targetAmountMinor = isSwitchingToYearly ? yearlyAmountMinor : monthlyAmountMinor;
				const firstInvoiceCreditAmountMinor =
					billingCycle === 'monthly' &&
					isSwitchingToYearly &&
					currentSubscriptionCurrency === priceCurrency &&
					currentSubscriptionAmountMinor != null &&
					targetAmountMinor != null
						? Math.min(currentSubscriptionAmountMinor, targetAmountMinor)
						: null;
				const firstRenewalPriceLabel =
					firstInvoiceCreditAmountMinor != null && targetAmountMinor != null
						? formatMinorUnitPrice(
								targetAmountMinor - firstInvoiceCreditAmountMinor,
								priceCurrency,
								locale,
								countryCode,
							)
						: null;
				const creditLabel =
					firstInvoiceCreditAmountMinor != null
						? formatMinorUnitPrice(firstInvoiceCreditAmountMinor, priceCurrency, locale, countryCode)
						: null;
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={
								isSwitchingToYearly
									? i18n._(SWITCH_TO_YEARLY_BILLING_DESCRIPTOR)
									: i18n._(SWITCH_TO_MONTHLY_BILLING_DESCRIPTOR)
							}
							description={
								<SubscriptionBillingChangeDescription
									isSwitchingToYearly={isSwitchingToYearly}
									firstRenewalPriceLabel={firstRenewalPriceLabel}
									creditLabel={creditLabel}
									renewalPrice={renewalPrice}
									data-flx="app.plutonium.subscription-card.handle-confirm-billing-cycle-change.subscription-billing-change-description"
								/>
							}
							primaryText={
								isSwitchingToYearly ? i18n._(SWITCH_TO_YEARLY_DESCRIPTOR) : i18n._(SWITCH_TO_MONTHLY_DESCRIPTOR)
							}
							primaryVariant="primary"
							secondaryText={i18n._(NOT_NOW_DESCRIPTOR)}
							onPrimary={async () => {
								await handleChangeSubscriptionBillingCycle(targetCycle);
							}}
							data-flx="app.plutonium.subscription-card.handle-confirm-billing-cycle-change.confirm-modal"
						/>
					)),
				);
			},
			[
				billingCycle,
				countryCode,
				currentSubscriptionAmountMinor,
				currentSubscriptionCurrency,
				effectiveMonthlyPrice,
				effectiveYearlyPrice,
				handleChangeSubscriptionBillingCycle,
				i18n,
				locale,
				monthlyAmountMinor,
				priceCurrency,
				yearlyAmountMinor,
			],
		);
		const handleConfirmSwitchToListPrice = useCallback(() => {
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(SWITCH_TO_THE_NEW_PRICE_TITLE_DESCRIPTOR)}
						description={
							<Trans comment="Billing confirmation body for moving an active subscription down to the current price. {listPriceEffectiveDate} is a date, and {listPriceCurrentLabel} and {listPriceNewLabel} are currency amounts, all already formatted and localized by code; never write a date or an amount into the translation.">
								On{' '}
								<strong data-flx="app.plutonium.subscription-card.handle-confirm-switch-to-list-price.strong">
									{listPriceEffectiveDate}
								</strong>{' '}
								your subscription moves from {listPriceCurrentLabel} to {listPriceNewLabel}. Nothing is charged today,
								and the rest of your subscription stays exactly as it is.
							</Trans>
						}
						primaryText={i18n._(SWITCH_PRICE_DESCRIPTOR)}
						primaryVariant="primary"
						secondaryText={i18n._(NOT_NOW_DESCRIPTOR)}
						onPrimary={async () => {
							await handleSwitchToListPrice();
						}}
						data-flx="app.plutonium.subscription-card.handle-confirm-switch-to-list-price.confirm-modal"
					/>
				)),
			);
		}, [handleSwitchToListPrice, i18n, listPriceCurrentLabel, listPriceEffectiveDate, listPriceNewLabel]);

		return (
			<SettingsSection
				id="subscription"
				title={
					isVisionary
						? i18n._(VISIONARY_DESCRIPTOR)
						: i18n._(PREMIUM_SUBSCRIPTION_DESCRIPTOR, {premiumProductName: getPremiumProductName()})
				}
				description={
					<SubscriptionDescription
						locale={locale}
						isFullyExpired={isFullyExpired}
						isInGracePeriod={isInGracePeriod}
						graceEndDate={graceEndDate}
						gracePeriodInfo={gracePeriodInfo}
						isGiftGrace={isGiftGrace}
						storeSubscription={storeSubscription}
						premiumWillCancel={premiumWillCancel}
						premiumUntil={premiumUntil}
						isGiftSubscription={isGiftSubscription}
						perksDisabled={perksDisabled}
						isVisionary={isVisionary}
						billingCycle={billingCycle}
						effectiveMonthlyPrice={effectiveMonthlyPrice}
						effectiveYearlyPrice={effectiveYearlyPrice}
						isCurrentSubscriptionGrandfathered={isCurrentSubscriptionGrandfathered}
						grandfatheredTooltip={grandfatheredTooltip}
						scrollToPerks={scrollToPerks}
						data-flx="app.plutonium.subscription-card.subscription-description"
					/>
				}
				data-flx="app.plutonium.subscription-card.section"
			>
				{!isVisionary &&
					(hasPendingSubscriptionChange || hasPendingListPriceSwitch) &&
					pendingChangeDate &&
					!isInGracePeriod &&
					!isFullyExpired &&
					!isGiftSubscription && (
						<div className={styles.note} data-flx="app.plutonium.subscription-card.pending-change-info">
							<ScheduledSubscriptionChangeDescription
								hasPendingListPriceSwitch={hasPendingListPriceSwitch}
								pendingTargetPriceLabel={pendingTargetPriceLabel}
								pendingChangeDate={pendingChangeDate}
								pendingSubscriptionChange={pendingSubscriptionChange}
								pendingInitialPriceLabel={pendingInitialPriceLabel}
								pendingRecurringPriceLabel={pendingRecurringPriceLabel}
								pendingCreditPriceLabel={pendingCreditPriceLabel}
								data-flx="app.plutonium.subscription-card.scheduled-subscription-change-description"
							/>
						</div>
					)}
				{!isVisionary &&
					shouldSuggestCancelingPendingChange &&
					!isInGracePeriod &&
					!isFullyExpired &&
					!isGiftSubscription && (
						<div className={styles.note} data-flx="app.plutonium.subscription-card.list-price-switch-blocked-info">
							<Trans comment="Plutonium subscription card hint shown to a subscriber who could move to a lower price but has another billing change already scheduled.">
								Cancel the scheduled change to move to the current price instead.
							</Trans>
						</div>
					)}
				{!isVisionary &&
					shouldMentionListPriceWhileCancelling &&
					listPriceNewLabel &&
					premiumUntil &&
					!isInGracePeriod &&
					!isFullyExpired &&
					!isGiftSubscription &&
					(() => {
						const cancelDate = getFormattedLongDate(premiumUntil, locale);
						return (
							<div className={styles.note} data-flx="app.plutonium.subscription-card.list-price-switch-cancelling-info">
								<Trans comment="Plutonium subscription card line shown to a subscriber on a legacy price whose subscription is already set to cancel. {cancelDate} is a date and {listPriceNewLabel} is a currency amount, both already formatted and localized by code; never write a date or an amount into the translation.">
									Your subscription still ends on{' '}
									<strong data-flx="app.plutonium.subscription-card.strong--15">{cancelDate}</strong>. The current price
									is now {listPriceNewLabel}. If you reactivate, you can switch to it from here.
								</Trans>
							</div>
						);
					})()}
				{!isVisionary &&
					canSwitchToListPrice &&
					listPriceEffectiveDate &&
					!isInGracePeriod &&
					!isFullyExpired &&
					!isGiftSubscription && (
						<div className={styles.note} data-flx="app.plutonium.subscription-card.list-price-switch-info">
							<Trans comment="Plutonium subscription card line offering a move to a lower price. {listPriceEffectiveDate} is a date already formatted and localized by code; never write a date into the translation.">
								A lower price is available. Switching takes effect on{' '}
								<strong data-flx="app.plutonium.subscription-card.strong--14">{listPriceEffectiveDate}</strong> and
								changes nothing else.
							</Trans>
						</div>
					)}
				{!isVisionary &&
					renewsAt &&
					!premiumWillCancel &&
					!isInGracePeriod &&
					!isFullyExpired &&
					!isGiftSubscription &&
					(() => {
						const renewalDate = getFormattedLongDate(renewsAt, locale);
						return (
							<div className={styles.note} data-flx="app.plutonium.subscription-card.renewal-info">
								<Trans>
									Renews on <strong data-flx="app.plutonium.subscription-card.strong--11">{renewalDate}.</strong>
								</Trans>
							</div>
						);
					})()}
				<div className={styles.actions} data-flx="app.plutonium.subscription-card.actions">
					<SubscriptionActions
						storeSubscription={storeSubscription}
						isInGracePeriod={isInGracePeriod}
						premiumWillCancel={premiumWillCancel}
						isGiftSubscription={isGiftSubscription}
						isGiftGrace={isGiftGrace}
						billingUnavailable={billingUnavailable}
						canSubscribe={canSubscribe}
						purchaseDisabled={purchaseDisabled}
						tooltipText={tooltipText}
						isFullyExpired={isFullyExpired}
						hasEverPurchased={hasEverPurchased}
						shouldUseReactivateQuickAction={shouldUseReactivateQuickAction}
						loadingReactivate={loadingReactivate}
						loadingPortal={loadingPortal}
						isVisionary={isVisionary}
						loadingRejoinCommunity={loadingRejoinCommunity}
						shouldUseChangePlanQuickAction={shouldUseChangePlanQuickAction}
						targetBillingCycle={targetBillingCycle}
						hasPendingSubscriptionChange={hasPendingSubscriptionChange}
						hasPendingListPriceSwitch={hasPendingListPriceSwitch}
						loadingChangeBillingCycle={loadingChangeBillingCycle}
						canSwitchToListPrice={canSwitchToListPrice}
						listPriceNewLabel={listPriceNewLabel}
						loadingSwitchToListPrice={loadingSwitchToListPrice}
						loadingCancelPendingChange={loadingCancelPendingChange}
						pendingSubscriptionChange={pendingSubscriptionChange}
						shouldUseCancelQuickAction={shouldUseCancelQuickAction}
						loadingCancel={loadingCancel}
						gracePeriodInfo={gracePeriodInfo}
						loadingEndGrace={loadingEndGrace}
						handleStartSubscription={handleStartSubscription}
						navigateToRedeemGift={navigateToRedeemGift}
						handleReactivateSubscription={handleReactivateSubscription}
						handleOpenCustomerPortal={handleOpenCustomerPortal}
						handleCommunityButtonClick={handleCommunityButtonClick}
						handleConfirmBillingCycleChange={handleConfirmBillingCycleChange}
						handleConfirmSwitchToListPrice={handleConfirmSwitchToListPrice}
						handleCancelPendingSubscriptionChange={handleCancelPendingSubscriptionChange}
						handleCancelSubscription={handleCancelSubscription}
						handleEndPremiumGracePeriod={handleEndPremiumGracePeriod}
						data-flx="app.plutonium.subscription-card.subscription-actions"
					/>
				</div>
			</SettingsSection>
		);
	},
);
