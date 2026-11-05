// SPDX-License-Identifier: AGPL-3.0-or-later

import {JOIN_COMMUNITY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {MANAGE_SUBSCRIPTION_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import type {GracePeriodInfo} from '@app/features/premium/utils/SubscriptionStatus';
import {Button} from '@app/features/ui/button/Button';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import type {PendingSubscriptionChangeResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {PremiumStoreSubscriptionState} from '@fluxer/schema/src/domains/premium/StoreBillingSchemas';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {ReactElement, ReactNode} from 'react';

interface SubscriptionActionsProps {
	storeSubscription: PremiumStoreSubscriptionState | null;
	isInGracePeriod: boolean;
	premiumWillCancel: boolean;
	isGiftSubscription: boolean;
	isGiftGrace: boolean;
	billingUnavailable: boolean;
	canSubscribe: boolean;
	purchaseDisabled: boolean;
	tooltipText: string | (() => ReactNode);
	isFullyExpired: boolean;
	hasEverPurchased: boolean;
	shouldUseReactivateQuickAction: boolean;
	loadingReactivate: boolean;
	loadingPortal: boolean;
	isVisionary: boolean;
	loadingRejoinCommunity: boolean;
	shouldUseChangePlanQuickAction: boolean;
	targetBillingCycle: 'monthly' | 'yearly' | null;
	hasPendingSubscriptionChange: boolean;
	hasPendingListPriceSwitch: boolean;
	loadingChangeBillingCycle: 'monthly' | 'yearly' | null;
	canSwitchToListPrice: boolean;
	listPriceNewLabel: string | null;
	loadingSwitchToListPrice: boolean;
	loadingCancelPendingChange: boolean;
	pendingSubscriptionChange: PendingSubscriptionChangeResponse;
	shouldUseCancelQuickAction: boolean;
	loadingCancel: boolean;
	gracePeriodInfo: GracePeriodInfo;
	loadingEndGrace: boolean;
	handleStartSubscription: () => void;
	navigateToRedeemGift: () => void;
	handleReactivateSubscription: () => void;
	handleOpenCustomerPortal: () => void;
	handleCommunityButtonClick: () => void;
	handleConfirmBillingCycleChange: (cycle: 'monthly' | 'yearly') => void;
	handleConfirmSwitchToListPrice: () => void;
	handleCancelPendingSubscriptionChange: () => void;
	handleCancelSubscription: () => void;
	handleEndPremiumGracePeriod: () => void;
}

export const SubscriptionActions = observer(
	({
		storeSubscription,
		isInGracePeriod,
		premiumWillCancel,
		isGiftSubscription,
		isGiftGrace,
		billingUnavailable,
		canSubscribe,
		purchaseDisabled,
		tooltipText,
		isFullyExpired,
		hasEverPurchased,
		shouldUseReactivateQuickAction,
		loadingReactivate,
		loadingPortal,
		isVisionary,
		loadingRejoinCommunity,
		shouldUseChangePlanQuickAction,
		targetBillingCycle,
		hasPendingSubscriptionChange,
		hasPendingListPriceSwitch,
		loadingChangeBillingCycle,
		canSwitchToListPrice,
		listPriceNewLabel,
		loadingSwitchToListPrice,
		loadingCancelPendingChange,
		pendingSubscriptionChange,
		shouldUseCancelQuickAction,
		loadingCancel,
		gracePeriodInfo,
		loadingEndGrace,
		handleStartSubscription,
		navigateToRedeemGift,
		handleReactivateSubscription,
		handleOpenCustomerPortal,
		handleCommunityButtonClick,
		handleConfirmBillingCycleChange,
		handleConfirmSwitchToListPrice,
		handleCancelPendingSubscriptionChange,
		handleCancelSubscription,
		handleEndPremiumGracePeriod,
	}: SubscriptionActionsProps) => {
		const {i18n} = useLingui();
		const wrapIfDisabled = (element: ReactElement, {key, disabled}: {key: string; disabled: boolean}) =>
			disabled ? (
				<Tooltip key={key} text={tooltipText} data-flx="app.plutonium.subscription-card.wrap-if-disabled.tooltip">
					<div data-flx="app.plutonium.subscription-card.wrap-if-disabled.div">{element}</div>
				</Tooltip>
			) : (
				element
			);
		if (storeSubscription) {
			return (
				<Button
					variant={isInGracePeriod || premiumWillCancel ? 'primary' : 'secondary'}
					onClick={() => void openExternalUrl(storeSubscription.manage_url)}
					small
					data-flx="app.plutonium.subscription-card.action-button.open-store-subscription"
				>
					{i18n._(MANAGE_SUBSCRIPTION_DESCRIPTOR)}
				</Button>
			);
		}
		if (isGiftSubscription || isGiftGrace || billingUnavailable) {
			return (
				<>
					{canSubscribe &&
						!billingUnavailable &&
						wrapIfDisabled(
							<Button
								variant="primary"
								onClick={handleStartSubscription}
								small
								disabled={purchaseDisabled}
								data-flx="app.plutonium.subscription-card.action-button.start-subscription"
							>
								<Trans comment="Billing button that starts a recurring subscription for someone on gifted premium time.">
									Subscribe
								</Trans>
							</Button>,
							{key: 'start-subscription', disabled: purchaseDisabled},
						)}
					<Button
						variant={canSubscribe && !billingUnavailable ? 'secondary' : 'primary'}
						onClick={navigateToRedeemGift}
						small
						data-flx="app.plutonium.subscription-card.action-button.navigate-to-redeem-gift"
					>
						<Trans comment="Billing button for entering a premium gift code.">Redeem gift code</Trans>
					</Button>
				</>
			);
		}
		let primaryAction: ReactNode = false;
		if ((isFullyExpired || isInGracePeriod) && canSubscribe) {
			primaryAction = wrapIfDisabled(
				<Button
					variant="primary"
					onClick={handleStartSubscription}
					small
					disabled={purchaseDisabled}
					data-flx="app.plutonium.subscription-card.action-button.resubscribe"
				>
					{hasEverPurchased ? (
						<Trans comment="Billing button for starting a new subscription after premium ended.">Resubscribe</Trans>
					) : (
						<Trans comment="Billing button that starts a recurring subscription for someone on gifted premium time.">
							Subscribe
						</Trans>
					)}
				</Button>,
				{key: 'resubscribe', disabled: purchaseDisabled},
			);
		} else if (hasEverPurchased) {
			let manageLabel: ReactNode;
			if (isFullyExpired) {
				manageLabel = (
					<Trans comment="Billing button for starting a new subscription after premium expired.">Resubscribe</Trans>
				);
			} else if (isInGracePeriod) {
				manageLabel = (
					<Trans comment="Billing button for restarting a subscription during the grace period.">Resubscribe</Trans>
				);
			} else if (premiumWillCancel) {
				manageLabel = (
					<Trans comment="Billing button that cancels a scheduled subscription cancellation.">Reactivate</Trans>
				);
			} else if (isVisionary) {
				manageLabel = <Trans comment="Billing button that opens the external customer portal.">Customer portal</Trans>;
			} else {
				manageLabel = i18n._(MANAGE_SUBSCRIPTION_DESCRIPTOR);
			}
			primaryAction = wrapIfDisabled(
				<Button
					variant={isFullyExpired || isInGracePeriod || premiumWillCancel ? 'primary' : 'secondary'}
					onClick={shouldUseReactivateQuickAction ? handleReactivateSubscription : handleOpenCustomerPortal}
					submitting={shouldUseReactivateQuickAction ? loadingReactivate : loadingPortal}
					small
					disabled={purchaseDisabled && shouldUseReactivateQuickAction}
					data-flx="app.plutonium.subscription-card.action-button.reactivate-subscription"
				>
					{manageLabel}
				</Button>,
				{key: 'manage-reactivate', disabled: purchaseDisabled && shouldUseReactivateQuickAction},
			);
		}
		const communityAction = isVisionary && (
			<Button
				variant="secondary"
				onClick={handleCommunityButtonClick}
				submitting={loadingRejoinCommunity}
				small
				data-flx="app.plutonium.subscription-card.action-button.community-button-click"
			>
				{i18n._(JOIN_COMMUNITY_DESCRIPTOR)}
			</Button>
		);
		const changePlanAction =
			shouldUseChangePlanQuickAction &&
			targetBillingCycle &&
			!hasPendingSubscriptionChange &&
			!hasPendingListPriceSwitch &&
			wrapIfDisabled(
				<Button
					variant="secondary"
					onClick={() => handleConfirmBillingCycleChange(targetBillingCycle)}
					submitting={loadingChangeBillingCycle === targetBillingCycle}
					small
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
				{key: 'change-plan', disabled: purchaseDisabled},
			);
		const listPriceAction = canSwitchToListPrice && listPriceNewLabel && (
			<Button
				variant="secondary"
				onClick={handleConfirmSwitchToListPrice}
				submitting={loadingSwitchToListPrice}
				small
				data-flx="app.plutonium.subscription-card.action-button.confirm-switch-to-list-price"
			>
				<Trans comment="Billing button that opens confirmation to move the subscription down to the current price. {listPriceNewLabel} is the localized new price.">
					Switch to {listPriceNewLabel}
				</Trans>
			</Button>
		);
		let cancelPendingAction: ReactNode = false;
		if (hasPendingSubscriptionChange || hasPendingListPriceSwitch) {
			let cancelLabel: ReactNode;
			if (hasPendingListPriceSwitch) {
				cancelLabel = (
					<Trans comment="Billing button that cancels a scheduled move to a lower subscription price.">
						Cancel price change
					</Trans>
				);
			} else if (pendingSubscriptionChange?.target_billing_cycle === 'yearly') {
				cancelLabel = (
					<Trans comment="Billing button that cancels a scheduled yearly upgrade.">Cancel yearly upgrade</Trans>
				);
			} else {
				cancelLabel = (
					<Trans comment="Billing button that cancels a scheduled billing-cycle change.">Cancel scheduled switch</Trans>
				);
			}
			cancelPendingAction = (
				<Button
					variant="secondary"
					onClick={handleCancelPendingSubscriptionChange}
					submitting={loadingCancelPendingChange}
					small
					data-flx="app.plutonium.subscription-card.action-button.cancel-pending-subscription-change"
				>
					{cancelLabel}
				</Button>
			);
		}
		return (
			<>
				{primaryAction}
				{communityAction}
				{changePlanAction}
				{listPriceAction}
				{cancelPendingAction}
				{shouldUseCancelQuickAction && (
					<Button
						variant="danger"
						onClick={handleCancelSubscription}
						submitting={loadingCancel}
						small
						data-flx="app.plutonium.subscription-card.action-button.cancel-subscription"
					>
						<Trans comment="Billing button that starts subscription cancellation.">Cancel subscription</Trans>
					</Button>
				)}
				{isInGracePeriod && !gracePeriodInfo.isPaymentRecovery && (
					<Button
						variant="danger"
						onClick={handleEndPremiumGracePeriod}
						submitting={loadingEndGrace}
						small
						data-flx="app.plutonium.subscription-card.action-button.end-premium-grace-period"
					>
						<Trans comment="Billing button that ends premium grace period immediately.">End grace now</Trans>
					</Button>
				)}
			</>
		);
	},
);
