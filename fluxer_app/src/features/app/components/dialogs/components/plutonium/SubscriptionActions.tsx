// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GracePeriodInfo} from '@app/features/app/components/dialogs/components/plutonium/hooks/useSubscriptionStatus';
import styles from '@app/features/app/components/dialogs/components/plutonium/SubscriptionCard.module.css';
import {JOIN_COMMUNITY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {PlutoniumPageButton} from '@app/features/premium/components/plutonium_page/PlutoniumPageButton';
import {MANAGE_SUBSCRIPTION_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
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
				<PlutoniumPageButton
					variant={isInGracePeriod || premiumWillCancel ? 'primary' : 'secondary'}
					onClick={() => void openExternalUrl(storeSubscription.manage_url)}
					small
					className={styles.actionButton}
					data-flx="app.plutonium.subscription-card.action-button.open-store-subscription"
				>
					{i18n._(MANAGE_SUBSCRIPTION_DESCRIPTOR)}
				</PlutoniumPageButton>
			);
		}
		if (isGiftSubscription || isGiftGrace || billingUnavailable) {
			return (
				<>
					{canSubscribe &&
						!billingUnavailable &&
						wrapIfDisabled(
							<PlutoniumPageButton
								variant="primary"
								onClick={handleStartSubscription}
								small
								className={styles.actionButton}
								disabled={purchaseDisabled}
								data-flx="app.plutonium.subscription-card.action-button.start-subscription"
							>
								<Trans comment="Billing button that starts a recurring subscription for someone on gifted premium time.">
									Subscribe
								</Trans>
							</PlutoniumPageButton>,
							{key: 'start-subscription', disabled: purchaseDisabled},
						)}
					<PlutoniumPageButton
						variant={canSubscribe && !billingUnavailable ? 'secondary' : 'primary'}
						onClick={navigateToRedeemGift}
						small
						className={styles.actionButton}
						data-flx="app.plutonium.subscription-card.action-button.navigate-to-redeem-gift"
					>
						<Trans comment="Billing button for entering a premium gift code.">Redeem gift code</Trans>
					</PlutoniumPageButton>
				</>
			);
		}
		let primaryAction: ReactNode = false;
		if ((isFullyExpired || isInGracePeriod) && canSubscribe) {
			primaryAction = wrapIfDisabled(
				<PlutoniumPageButton
					variant="primary"
					onClick={handleStartSubscription}
					small
					className={styles.actionButton}
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
				</PlutoniumPageButton>,
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
				<PlutoniumPageButton
					variant={isFullyExpired || isInGracePeriod || premiumWillCancel ? 'primary' : 'secondary'}
					onClick={shouldUseReactivateQuickAction ? handleReactivateSubscription : handleOpenCustomerPortal}
					loading={shouldUseReactivateQuickAction ? loadingReactivate : loadingPortal}
					small
					className={styles.actionButton}
					disabled={purchaseDisabled && shouldUseReactivateQuickAction}
					data-flx="app.plutonium.subscription-card.action-button.reactivate-subscription"
				>
					{manageLabel}
				</PlutoniumPageButton>,
				{key: 'manage-reactivate', disabled: purchaseDisabled && shouldUseReactivateQuickAction},
			);
		}
		const communityAction = isVisionary && (
			<PlutoniumPageButton
				variant="secondary"
				onClick={handleCommunityButtonClick}
				loading={loadingRejoinCommunity}
				small
				className={styles.actionButton}
				data-flx="app.plutonium.subscription-card.action-button.community-button-click"
			>
				{i18n._(JOIN_COMMUNITY_DESCRIPTOR)}
			</PlutoniumPageButton>
		);
		const changePlanAction =
			shouldUseChangePlanQuickAction &&
			targetBillingCycle &&
			!hasPendingSubscriptionChange &&
			!hasPendingListPriceSwitch &&
			wrapIfDisabled(
				<PlutoniumPageButton
					variant="secondary"
					onClick={() => handleConfirmBillingCycleChange(targetBillingCycle)}
					loading={loadingChangeBillingCycle === targetBillingCycle}
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
				</PlutoniumPageButton>,
				{key: 'change-plan', disabled: purchaseDisabled},
			);
		const listPriceAction = canSwitchToListPrice && listPriceNewLabel && (
			<PlutoniumPageButton
				variant="secondary"
				onClick={handleConfirmSwitchToListPrice}
				loading={loadingSwitchToListPrice}
				small
				className={styles.actionButton}
				data-flx="app.plutonium.subscription-card.action-button.confirm-switch-to-list-price"
			>
				<Trans comment="Billing button that opens confirmation to move the subscription down to the current price. {listPriceNewLabel} is the localized new price.">
					Switch to {listPriceNewLabel}
				</Trans>
			</PlutoniumPageButton>
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
				<PlutoniumPageButton
					variant="secondary"
					onClick={handleCancelPendingSubscriptionChange}
					loading={loadingCancelPendingChange}
					small
					className={styles.actionButton}
					data-flx="app.plutonium.subscription-card.action-button.cancel-pending-subscription-change"
				>
					{cancelLabel}
				</PlutoniumPageButton>
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
					<PlutoniumPageButton
						variant="danger"
						onClick={handleCancelSubscription}
						loading={loadingCancel}
						small
						className={styles.actionButton}
						data-flx="app.plutonium.subscription-card.action-button.cancel-subscription"
					>
						<Trans comment="Billing button that starts subscription cancellation.">Cancel subscription</Trans>
					</PlutoniumPageButton>
				)}
				{isInGracePeriod && !gracePeriodInfo.isPaymentRecovery && (
					<PlutoniumPageButton
						variant="danger"
						onClick={handleEndPremiumGracePeriod}
						loading={loadingEndGrace}
						small
						className={styles.actionButton}
						data-flx="app.plutonium.subscription-card.action-button.end-premium-grace-period"
					>
						<Trans comment="Billing button that ends premium grace period immediately.">End grace now</Trans>
					</PlutoniumPageButton>
				)}
			</>
		);
	},
);
