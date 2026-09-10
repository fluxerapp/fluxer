// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {PerksButton} from '@app/features/app/components/dialogs/components/PerksButton';
import {useSubscriptionActions} from '@app/features/app/components/dialogs/components/plutonium/hooks/useSubscriptionActions';
import type {GracePeriodInfo} from '@app/features/app/components/dialogs/components/plutonium/hooks/useSubscriptionStatus';
import statusStyles from '@app/features/app/components/dialogs/components/plutonium/PurchaseHistoryStatus.module.css';
import styles from '@app/features/app/components/dialogs/components/plutonium/SubscriptionCard.module.css';
import {
	PAYMENT_PROVIDER_NAME,
	PREMIUM_PRODUCT_FULL_NAME,
	PREMIUM_PRODUCT_NAME,
} from '@app/features/app/config/I18nDisplayConstants';
import {JOIN_COMMUNITY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import PremiumState from '@app/features/premium/state/PremiumState';
import {
	CLAIM_ACCOUNT_TO_PURCHASE_OR_REDEEM_PREMIUM_DESCRIPTOR,
	MANAGE_SUBSCRIPTION_DESCRIPTOR,
	PREMIUM_SUBSCRIPTION_DESCRIPTOR,
} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {formatMinorUnitPrice} from '@app/features/premium/utils/PricingUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import Users from '@app/features/user/state/Users';
import {getFormattedLongDate} from '@fluxer/date_utils/src/DateFormatting';
import type {PendingSubscriptionChangeResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
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
const NOT_NOW_DESCRIPTOR = msg({
	message: 'Not now',
	comment: 'Button that cancels a subscription billing change.',
});

interface SubscriptionCardProps {
	locale: string;
	isVisionary: boolean;
	perksDisabled: boolean;
	isGiftSubscription: boolean;
	premiumUntil: Date | null;
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
	handleOpenCustomerPortal: () => void;
	handleReactivateSubscription: () => void;
	handleCancelSubscription: () => void;
	handleEndPremiumGracePeriod: () => void;
	handleChangeSubscriptionBillingCycle: (
		billingCycle: 'monthly' | 'yearly',
		effectiveAt?: 'now' | 'period_end',
	) => void;
	handleCancelPendingSubscriptionChange: () => void;
	handleCommunityButtonClick: () => void;
	purchaseDisabled?: boolean;
	purchaseDisabledTooltip?: React.ReactNode;
}

function getStatusBadgeClass(args: {
	isFullyExpired: boolean;
	isInGracePeriod: boolean;
	perksDisabled: boolean;
	premiumWillCancel: boolean;
	hasPendingSubscriptionChange: boolean;
	isVisionary: boolean;
	isGiftSubscription: boolean;
}): string {
	if (args.isFullyExpired) return statusStyles.danger;
	if (args.isInGracePeriod) return statusStyles.pending;
	if (args.perksDisabled) return statusStyles.muted;
	if (args.premiumWillCancel) return statusStyles.pending;
	if (args.hasPendingSubscriptionChange) return statusStyles.pending;
	if (args.isVisionary) return statusStyles.neutral;
	if (args.isGiftSubscription) return statusStyles.neutral;
	return statusStyles.success;
}

export const SubscriptionCard: React.FC<SubscriptionCardProps> = observer(
	({
		locale,
		isVisionary,
		perksDisabled,
		isGiftSubscription,
		premiumUntil,
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
		handleOpenCustomerPortal,
		handleReactivateSubscription,
		handleCancelSubscription,
		handleEndPremiumGracePeriod,
		handleChangeSubscriptionBillingCycle,
		handleCancelPendingSubscriptionChange,
		handleCommunityButtonClick,
		purchaseDisabled = false,
		purchaseDisabledTooltip,
	}) => {
		const {i18n} = useLingui();
		const {loadingSwitchToListPrice, handleSwitchToListPrice} = useSubscriptionActions();
		const {isInGracePeriod, isExpired: isFullyExpired, graceEndDate} = gracePeriodInfo;
		const tooltipText: string | (() => React.ReactNode) =
			purchaseDisabledTooltip != null
				? () => purchaseDisabledTooltip
				: i18n._(CLAIM_ACCOUNT_TO_PURCHASE_OR_REDEEM_PREMIUM_DESCRIPTOR, {
						premiumProductFullName: PREMIUM_PRODUCT_FULL_NAME,
					});
		const targetBillingCycle = billingCycle === 'monthly' ? 'yearly' : billingCycle === 'yearly' ? 'monthly' : null;
		const effectiveMonthlyPrice =
			billingCycle === 'monthly' && currentSubscriptionPriceLabel ? currentSubscriptionPriceLabel : monthlyPrice;
		const effectiveYearlyPrice =
			billingCycle === 'yearly' && currentSubscriptionPriceLabel ? currentSubscriptionPriceLabel : yearlyPrice;
		const hasPendingSubscriptionChange = pendingSubscriptionChange != null && !premiumWillCancel;
		const currentUserId = Users.currentUser?.id;
		const listPriceSwitch =
			currentUserId != null && PremiumState.loadedForUserId === currentUserId
				? (PremiumState.state?.billing.list_price_switch ?? null)
				: null;
		const listPriceNewLabel = listPriceSwitch
			? formatMinorUnitPrice(listPriceSwitch.list_amount_minor, listPriceSwitch.currency, locale)
			: null;
		const listPriceCurrentLabel = listPriceSwitch
			? formatMinorUnitPrice(listPriceSwitch.current_amount_minor, listPriceSwitch.currency, locale)
			: null;
		const listPriceEffectiveDate = listPriceSwitch?.effective_at
			? getFormattedLongDate(listPriceSwitch.effective_at, locale)
			: null;
		const hasPendingListPriceSwitch = listPriceSwitch?.pending === true && !premiumWillCancel;
		const pendingChangeDate = pendingSubscriptionChange
			? getFormattedLongDate(new Date(pendingSubscriptionChange.effective_at), locale)
			: hasPendingListPriceSwitch
				? listPriceEffectiveDate
				: null;
		const pendingInitialPriceLabel = pendingSubscriptionChange
			? formatMinorUnitPrice(pendingSubscriptionChange.initial_amount_minor, pendingSubscriptionChange.currency, locale)
			: null;
		const pendingRecurringPriceLabel = pendingSubscriptionChange
			? formatMinorUnitPrice(
					pendingSubscriptionChange.recurring_amount_minor,
					pendingSubscriptionChange.currency,
					locale,
				)
			: null;
		const pendingCreditPriceLabel = pendingSubscriptionChange
			? formatMinorUnitPrice(pendingSubscriptionChange.credit_amount_minor, pendingSubscriptionChange.currency, locale)
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
				const effectiveAt =
					premiumWillCancel || (billingCycle === 'monthly' && isSwitchingToYearly) ? 'period_end' : 'now';
				const targetAmountMinor = isSwitchingToYearly ? yearlyAmountMinor : monthlyAmountMinor;
				const firstInvoiceCreditAmountMinor =
					effectiveAt === 'period_end' &&
					billingCycle === 'monthly' &&
					isSwitchingToYearly &&
					currentSubscriptionCurrency === priceCurrency &&
					currentSubscriptionAmountMinor != null &&
					targetAmountMinor != null
						? Math.min(currentSubscriptionAmountMinor, targetAmountMinor)
						: null;
				const firstRenewalPriceLabel =
					firstInvoiceCreditAmountMinor != null && targetAmountMinor != null
						? formatMinorUnitPrice(targetAmountMinor - firstInvoiceCreditAmountMinor, priceCurrency, locale)
						: null;
				const creditLabel =
					firstInvoiceCreditAmountMinor != null
						? formatMinorUnitPrice(firstInvoiceCreditAmountMinor, priceCurrency, locale)
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
								effectiveAt === 'period_end' ? (
									isSwitchingToYearly && firstRenewalPriceLabel && creditLabel ? (
										<Trans>
											Takes effect at your next renewal. We'll apply a {creditLabel} credit from your current monthly
											payment, so your first yearly charge will be{' '}
											<strong data-flx="app.plutonium.subscription-card.handle-confirm-billing-cycle-change.strong">
												{firstRenewalPriceLabel}
											</strong>
											. Future yearly renewals will be {renewalPrice}/year.
										</Trans>
									) : isSwitchingToYearly ? (
										<Trans>
											Takes effect at your next renewal. Your subscription stays monthly until then, and your next
											renewal will be{' '}
											<strong data-flx="app.plutonium.subscription-card.handle-confirm-billing-cycle-change.strong">
												{renewalPrice}/year
											</strong>
											.
										</Trans>
									) : (
										<Trans>
											Takes effect at your next renewal. Your subscription stays yearly until then, and your next
											renewal will be{' '}
											<strong data-flx="app.plutonium.subscription-card.handle-confirm-billing-cycle-change.strong--2">
												{renewalPrice}/month
											</strong>
											.
										</Trans>
									)
								) : isSwitchingToYearly ? (
									<Trans>
										Takes effect immediately. A prorated charge may apply via {PAYMENT_PROVIDER_NAME}. Your next renewal
										will be{' '}
										<strong data-flx="app.plutonium.subscription-card.handle-confirm-billing-cycle-change.strong">
											{renewalPrice}/year
										</strong>
										.
									</Trans>
								) : (
									<Trans>
										Takes effect immediately. A prorated adjustment may apply via {PAYMENT_PROVIDER_NAME}. Your next
										renewal will be{' '}
										<strong data-flx="app.plutonium.subscription-card.handle-confirm-billing-cycle-change.strong--2">
											{renewalPrice}/month
										</strong>
										.
									</Trans>
								)
							}
							primaryText={
								isSwitchingToYearly ? i18n._(SWITCH_TO_YEARLY_DESCRIPTOR) : i18n._(SWITCH_TO_MONTHLY_DESCRIPTOR)
							}
							primaryVariant="primary"
							secondaryText={i18n._(NOT_NOW_DESCRIPTOR)}
							onPrimary={async () => {
								await handleChangeSubscriptionBillingCycle(targetCycle, effectiveAt);
							}}
							data-flx="app.plutonium.subscription-card.handle-confirm-billing-cycle-change.confirm-modal"
						/>
					)),
				);
			},
			[
				billingCycle,
				currentSubscriptionAmountMinor,
				currentSubscriptionCurrency,
				effectiveMonthlyPrice,
				effectiveYearlyPrice,
				handleChangeSubscriptionBillingCycle,
				i18n,
				locale,
				monthlyAmountMinor,
				premiumWillCancel,
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
		const wrapIfDisabled = (element: React.ReactElement, key: string, disabled: boolean) =>
			disabled ? (
				<Tooltip key={key} text={tooltipText} data-flx="app.plutonium.subscription-card.wrap-if-disabled.tooltip">
					<div data-flx="app.plutonium.subscription-card.wrap-if-disabled.div">{element}</div>
				</Tooltip>
			) : (
				element
			);
		const badgeClass = getStatusBadgeClass({
			isFullyExpired,
			isInGracePeriod,
			perksDisabled,
			premiumWillCancel,
			hasPendingSubscriptionChange,
			isVisionary,
			isGiftSubscription,
		});
		return (
			<div className={styles.card} data-flx="app.plutonium.subscription-card.card">
				<div className={styles.grid} data-flx="app.plutonium.subscription-card.grid">
					<div className={styles.content} data-flx="app.plutonium.subscription-card.content">
						<div className={styles.header} data-flx="app.plutonium.subscription-card.header">
							<h3 className={styles.title} data-flx="app.plutonium.subscription-card.title">
								{isVisionary ? (
									<Trans>Visionary</Trans>
								) : (
									i18n._(PREMIUM_SUBSCRIPTION_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})
								)}
							</h3>
							<span className={badgeClass} data-flx="app.plutonium.subscription-card.badge">
								{isFullyExpired ? (
									<Trans comment="Subscription status badge meaning premium access has fully expired.">Expired</Trans>
								) : isInGracePeriod ? (
									<Trans comment="Subscription status badge meaning access remains briefly after subscription end.">
										Grace period
									</Trans>
								) : perksDisabled ? (
									<Trans comment="Subscription status badge meaning premium is paid but perks are temporarily disabled.">
										Perks paused
									</Trans>
								) : premiumWillCancel ? (
									<Trans comment="Subscription status badge meaning cancellation is scheduled for the renewal date.">
										Canceling
									</Trans>
								) : hasPendingSubscriptionChange ? (
									<Trans comment="Subscription status badge meaning a future billing plan change is scheduled.">
										Change scheduled
									</Trans>
								) : isVisionary ? (
									<Trans comment="Subscription status badge meaning permanent premium access.">Lifetime</Trans>
								) : isGiftSubscription ? (
									<Trans comment="Subscription status badge meaning premium access came from a gift.">Gift</Trans>
								) : (
									<Trans comment="Subscription status badge meaning the paid subscription is active.">Active</Trans>
								)}
							</span>
						</div>
						<div className={styles.description} data-flx="app.plutonium.subscription-card.description">
							{isFullyExpired ? (
								(() => {
									const expiredDate = graceEndDate ? getFormattedLongDate(graceEndDate, locale) : undefined;
									return (
										<Trans>
											Expired on <strong data-flx="app.plutonium.subscription-card.strong">{expiredDate}</strong>.{' '}
											<PerksButton
												onClick={scrollToPerks}
												data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks"
											/>{' '}
											are no longer active. You can resubscribe at any time.
										</Trans>
									);
								})()
							) : isInGracePeriod ? (
								(() => {
									const graceDate = graceEndDate ? getFormattedLongDate(graceEndDate, locale) : undefined;
									return (
										<Trans>
											Your subscription ended but{' '}
											<PerksButton
												onClick={scrollToPerks}
												data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--2"
											/>{' '}
											stay active until{' '}
											<strong data-flx="app.plutonium.subscription-card.strong--2">{graceDate}</strong>. Resubscribe
											before then to keep your subscriber history.
										</Trans>
									);
								})()
							) : isGiftSubscription ? (
								(() => {
									const giftEndDate = premiumUntil ? getFormattedLongDate(premiumUntil, locale) : undefined;
									return (
										<Trans>
											Gifted{' '}
											<PerksButton
												onClick={scrollToPerks}
												data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--3"
											/>{' '}
											until <strong data-flx="app.plutonium.subscription-card.strong--5">{giftEndDate}</strong>. Does
											not renew automatically. Redeem more gift codes to extend.
										</Trans>
									);
								})()
							) : premiumWillCancel && premiumUntil ? (
								(() => {
									const cancelDate = getFormattedLongDate(premiumUntil, locale);
									return (
										<Trans>
											Cancels on <strong data-flx="app.plutonium.subscription-card.strong--8">{cancelDate}</strong>.{' '}
											<PerksButton
												onClick={scrollToPerks}
												data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--4"
											/>{' '}
											remain active until then.
										</Trans>
									);
								})()
							) : perksDisabled ? (
								<Trans>Your subscription is active but perks are temporarily paused.</Trans>
							) : isVisionary ? (
								<Trans>
									All{' '}
									<PerksButton
										onClick={scrollToPerks}
										data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--5"
									/>
									, forever. No billing, no renewals.
								</Trans>
							) : billingCycle === 'monthly' ? (
								<>
									<Trans>
										All{' '}
										<PerksButton
											onClick={scrollToPerks}
											data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--6"
										/>{' '}
										for{' '}
										<strong data-flx="app.plutonium.subscription-card.strong--9">{effectiveMonthlyPrice}/month</strong>.
									</Trans>
									{isCurrentSubscriptionGrandfathered && (
										<Tooltip text={grandfatheredTooltip} data-flx="app.plutonium.subscription-card.tooltip">
											<span
												className={styles.legacyRateBadge}
												data-flx="app.plutonium.subscription-card.legacy-rate-badge"
											>
												<Trans>Legacy rate</Trans>
											</span>
										</Tooltip>
									)}
								</>
							) : billingCycle === 'yearly' ? (
								<>
									<Trans>
										All{' '}
										<PerksButton
											onClick={scrollToPerks}
											data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--7"
										/>{' '}
										for{' '}
										<strong data-flx="app.plutonium.subscription-card.strong--10">{effectiveYearlyPrice}/year</strong>.
									</Trans>
									{isCurrentSubscriptionGrandfathered && (
										<Tooltip text={grandfatheredTooltip} data-flx="app.plutonium.subscription-card.tooltip--2">
											<span
												className={styles.legacyRateBadge}
												data-flx="app.plutonium.subscription-card.legacy-rate-badge--2"
											>
												<Trans>Legacy rate</Trans>
											</span>
										</Tooltip>
									)}
								</>
							) : (
								<Trans>
									All{' '}
									<PerksButton
										onClick={scrollToPerks}
										data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--8"
									/>{' '}
									included with your subscription.
								</Trans>
							)}
						</div>
						{!isVisionary &&
							(hasPendingSubscriptionChange || hasPendingListPriceSwitch) &&
							pendingChangeDate &&
							!isInGracePeriod &&
							!isFullyExpired &&
							!isGiftSubscription && (
								<div
									className={styles.pendingChangeInfo}
									data-flx="app.plutonium.subscription-card.pending-change-info"
								>
									{hasPendingListPriceSwitch ? (
										pendingTargetPriceLabel ? (
											<Trans comment="Plutonium subscription card line shown when a move to a lower price is scheduled. {pendingChangeDate} is a date and {pendingTargetPriceLabel} is a currency amount, both already formatted and localized by code; never write a date or an amount into the translation.">
												New price scheduled for{' '}
												<strong data-flx="app.plutonium.subscription-card.strong--13">{pendingChangeDate}</strong>.
												Renewals will be {pendingTargetPriceLabel} from then on.
											</Trans>
										) : (
											<Trans comment="Plutonium subscription card line shown when a move to a lower price is scheduled but the new amount is unknown. {pendingChangeDate} is a date already formatted and localized by code; never write a date into the translation.">
												New price scheduled for{' '}
												<strong data-flx="app.plutonium.subscription-card.strong--13">{pendingChangeDate}</strong>.
											</Trans>
										)
									) : pendingSubscriptionChange?.target_billing_cycle === 'yearly' ? (
										pendingInitialPriceLabel && pendingRecurringPriceLabel ? (
											pendingCreditPriceLabel ? (
												<Trans>
													Yearly upgrade scheduled for{' '}
													<strong data-flx="app.plutonium.subscription-card.strong--12">{pendingChangeDate}</strong>.
													First yearly charge: {pendingInitialPriceLabel} after a {pendingCreditPriceLabel} credit.
													Future yearly renewals: {pendingRecurringPriceLabel}/year.
												</Trans>
											) : (
												<Trans>
													Yearly upgrade scheduled for{' '}
													<strong data-flx="app.plutonium.subscription-card.strong--12">{pendingChangeDate}</strong>.
													First yearly charge: {pendingInitialPriceLabel}. Future yearly renewals:{' '}
													{pendingRecurringPriceLabel}/year.
												</Trans>
											)
										) : (
											<Trans>
												Yearly upgrade scheduled for{' '}
												<strong data-flx="app.plutonium.subscription-card.strong--12">{pendingChangeDate}</strong>.
											</Trans>
										)
									) : pendingInitialPriceLabel && pendingRecurringPriceLabel ? (
										<Trans>
											Monthly billing starts on{' '}
											<strong data-flx="app.plutonium.subscription-card.strong--12">{pendingChangeDate}</strong>. First
											monthly charge: {pendingInitialPriceLabel}. Future monthly renewals: {pendingRecurringPriceLabel}
											/month.
										</Trans>
									) : (
										<Trans>
											Monthly billing starts on{' '}
											<strong data-flx="app.plutonium.subscription-card.strong--12">{pendingChangeDate}</strong>.
										</Trans>
									)}
								</div>
							)}
						{!isVisionary &&
							shouldSuggestCancelingPendingChange &&
							!isInGracePeriod &&
							!isFullyExpired &&
							!isGiftSubscription && (
								<div
									className={styles.pendingChangeInfo}
									data-flx="app.plutonium.subscription-card.list-price-switch-blocked-info"
								>
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
									<div
										className={styles.pendingChangeInfo}
										data-flx="app.plutonium.subscription-card.list-price-switch-cancelling-info"
									>
										<Trans comment="Plutonium subscription card line shown to a subscriber on a legacy price whose subscription is already set to cancel. {cancelDate} is a date and {listPriceNewLabel} is a currency amount, both already formatted and localized by code; never write a date or an amount into the translation.">
											Your subscription still ends on{' '}
											<strong data-flx="app.plutonium.subscription-card.strong--15">{cancelDate}</strong>. The current
											price is now {listPriceNewLabel}. If you reactivate, you can switch to it from here.
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
								<div
									className={styles.pendingChangeInfo}
									data-flx="app.plutonium.subscription-card.list-price-switch-info"
								>
									<Trans comment="Plutonium subscription card line offering a move to a lower price. {listPriceEffectiveDate} is a date already formatted and localized by code; never write a date into the translation.">
										A lower price is available. Switching takes effect on{' '}
										<strong data-flx="app.plutonium.subscription-card.strong--14">{listPriceEffectiveDate}</strong> and
										changes nothing else.
									</Trans>
								</div>
							)}
						{!isVisionary &&
							premiumUntil &&
							!premiumWillCancel &&
							!isInGracePeriod &&
							!isFullyExpired &&
							!isGiftSubscription &&
							(() => {
								const renewalDate = getFormattedLongDate(premiumUntil, locale);
								return (
									<div className={styles.renewalInfo} data-flx="app.plutonium.subscription-card.renewal-info">
										<Trans>
											Renews on <strong data-flx="app.plutonium.subscription-card.strong--11">{renewalDate}.</strong>
										</Trans>
									</div>
								);
							})()}
					</div>
					<div className={styles.actions} data-flx="app.plutonium.subscription-card.actions">
						{isGiftSubscription ? (
							wrapIfDisabled(
								<Button
									variant="primary"
									onClick={navigateToRedeemGift}
									small
									className={styles.actionButton}
									disabled={purchaseDisabled}
									data-flx="app.plutonium.subscription-card.action-button.navigate-to-redeem-gift"
								>
									<Trans comment="Billing button for entering a premium gift code.">Redeem gift code</Trans>
								</Button>,
								'redeem-gift',
								purchaseDisabled,
							)
						) : (
							<>
								{hasEverPurchased &&
									wrapIfDisabled(
										<Button
											variant={isFullyExpired || isInGracePeriod || premiumWillCancel ? 'primary' : 'secondary'}
											onClick={shouldUseReactivateQuickAction ? handleReactivateSubscription : handleOpenCustomerPortal}
											submitting={shouldUseReactivateQuickAction ? loadingReactivate : loadingPortal}
											small
											className={styles.actionButton}
											disabled={purchaseDisabled && shouldUseReactivateQuickAction}
											data-flx="app.plutonium.subscription-card.action-button.reactivate-subscription"
										>
											{isFullyExpired ? (
												<Trans comment="Billing button for starting a new subscription after premium expired.">
													Resubscribe
												</Trans>
											) : isInGracePeriod ? (
												<Trans comment="Billing button for restarting a subscription during the grace period.">
													Resubscribe
												</Trans>
											) : premiumWillCancel ? (
												<Trans comment="Billing button that cancels a scheduled subscription cancellation.">
													Reactivate
												</Trans>
											) : isVisionary ? (
												<Trans comment="Billing button that opens the external customer portal.">Customer portal</Trans>
											) : (
												i18n._(MANAGE_SUBSCRIPTION_DESCRIPTOR)
											)}
										</Button>,
										'manage-reactivate',
										purchaseDisabled && shouldUseReactivateQuickAction,
									)}
								{isVisionary && (
									<Button
										variant="secondary"
										onClick={handleCommunityButtonClick}
										submitting={loadingRejoinCommunity}
										small
										className={styles.actionButton}
										data-flx="app.plutonium.subscription-card.action-button.community-button-click"
									>
										{i18n._(JOIN_COMMUNITY_DESCRIPTOR)}
									</Button>
								)}
								{shouldUseChangePlanQuickAction &&
									targetBillingCycle &&
									!hasPendingSubscriptionChange &&
									!hasPendingListPriceSwitch &&
									wrapIfDisabled(
										<Button
											variant="secondary"
											onClick={() => handleConfirmBillingCycleChange(targetBillingCycle)}
											submitting={loadingChangeBillingCycle === targetBillingCycle}
											small
											className={styles.actionButton}
											disabled={purchaseDisabled}
											data-flx="app.plutonium.subscription-card.action-button.confirm-billing-cycle-change"
										>
											{targetBillingCycle === 'yearly' ? (
												<Trans comment="Billing button that opens confirmation to switch to yearly billing.">
													Switch to yearly
												</Trans>
											) : (
												<Trans comment="Billing button that opens confirmation to switch to monthly billing.">
													Switch to monthly
												</Trans>
											)}
										</Button>,
										'change-plan',
										purchaseDisabled,
									)}
								{canSwitchToListPrice && listPriceNewLabel && (
									<Button
										variant="secondary"
										onClick={handleConfirmSwitchToListPrice}
										submitting={loadingSwitchToListPrice}
										small
										className={styles.actionButton}
										data-flx="app.plutonium.subscription-card.action-button.confirm-switch-to-list-price"
									>
										<Trans comment="Billing button that opens confirmation to move the subscription down to the current price. {listPriceNewLabel} is the localized new price.">
											Switch to {listPriceNewLabel}
										</Trans>
									</Button>
								)}
								{(hasPendingSubscriptionChange || hasPendingListPriceSwitch) && (
									<Button
										variant="secondary"
										onClick={handleCancelPendingSubscriptionChange}
										submitting={loadingCancelPendingChange}
										small
										className={styles.actionButton}
										data-flx="app.plutonium.subscription-card.action-button.cancel-pending-subscription-change"
									>
										{hasPendingListPriceSwitch ? (
											<Trans comment="Billing button that cancels a scheduled move to a lower subscription price.">
												Cancel price change
											</Trans>
										) : pendingSubscriptionChange?.target_billing_cycle === 'yearly' ? (
											<Trans comment="Billing button that cancels a scheduled yearly upgrade.">
												Cancel yearly upgrade
											</Trans>
										) : (
											<Trans comment="Billing button that cancels a scheduled billing-cycle change.">
												Cancel scheduled switch
											</Trans>
										)}
									</Button>
								)}
								{shouldUseCancelQuickAction && (
									<Button
										variant="danger"
										onClick={handleCancelSubscription}
										submitting={loadingCancel}
										small
										className={styles.actionButton}
										data-flx="app.plutonium.subscription-card.action-button.cancel-subscription"
									>
										<Trans comment="Billing button that starts subscription cancellation.">Cancel subscription</Trans>
									</Button>
								)}
								{isInGracePeriod && (
									<Button
										variant="danger"
										onClick={handleEndPremiumGracePeriod}
										submitting={loadingEndGrace}
										small
										className={styles.actionButton}
										data-flx="app.plutonium.subscription-card.action-button.end-premium-grace-period"
									>
										<Trans comment="Billing button that ends premium grace period immediately.">End grace now</Trans>
									</Button>
								)}
							</>
						)}
					</div>
				</div>
			</div>
		);
	},
);
