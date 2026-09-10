// SPDX-License-Identifier: AGPL-3.0-or-later

import {showPremiumActionErrorModal} from '@app/features/app/components/dialogs/components/plutonium/utils/PremiumActionErrorModal';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {getFormattedLongDate} from '@fluxer/date_utils/src/DateFormatting';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useState} from 'react';

const CUSTOMER_PORTAL_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't open the billing portal",
	comment: 'Title of the generic fallback error modal shown when opening the billing customer portal fails.',
});
const CUSTOMER_PORTAL_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while opening the billing portal. Please try again in a moment.',
	comment: 'Body of the generic fallback error modal shown when opening the billing customer portal fails.',
});
const YOUR_SUBSCRIPTION_HAS_BEEN_SET_TO_CANCEL_AT_DESCRIPTOR = msg({
	message: 'Your subscription has been set to cancel at the end of your billing period.',
	comment: 'Toast success shown after a Plutonium subscription is set to cancel at the end of the billing period.',
});
const CANCEL_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't cancel your subscription",
	comment: 'Title of the generic fallback error modal shown when cancelling a Plutonium subscription fails.',
});
const CANCEL_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while cancelling your subscription. Please try again in a moment.',
	comment: 'Body of the generic fallback error modal shown when cancelling a Plutonium subscription fails.',
});
const YOUR_GRACE_PERIOD_HAS_ENDED_DESCRIPTOR = msg({
	message: 'Your grace period has ended.',
	comment: 'Body text in the Plutonium subscription actions. Keep the tone plain and specific.',
});
const END_GRACE_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't end your grace period",
	comment: 'Title of the generic fallback error modal shown when ending the Plutonium grace period fails.',
});
const END_GRACE_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while ending your grace period. Please try again in a moment.',
	comment: 'Body of the generic fallback error modal shown when ending the Plutonium grace period fails.',
});
const YOUR_SUBSCRIPTION_HAS_BEEN_REACTIVATED_DESCRIPTOR = msg({
	message: 'Your subscription has been reactivated.',
	comment: 'Body text in the Plutonium subscription actions. Keep the tone plain and specific.',
});
const REACTIVATE_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't reactivate your subscription",
	comment: 'Title of the generic fallback error modal shown when reactivating a Plutonium subscription fails.',
});
const REACTIVATE_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while reactivating your subscription. Please try again in a moment.',
	comment: 'Body of the generic fallback error modal shown when reactivating a Plutonium subscription fails.',
});
const YOUR_SUBSCRIPTION_HAS_BEEN_SWITCHED_TO_YEARLY_BILLING_DESCRIPTOR = msg({
	message: 'Your subscription has been switched to yearly billing.',
	comment: 'Toast success shown after a Plutonium subscription is switched from monthly to yearly billing.',
});
const YOUR_SUBSCRIPTION_HAS_BEEN_SWITCHED_TO_MONTHLY_BILLING_DESCRIPTOR = msg({
	message: 'Your subscription has been switched to monthly billing.',
	comment: 'Toast success shown after a Plutonium subscription is switched from yearly to monthly billing.',
});
const YOUR_SUBSCRIPTION_WILL_SWITCH_TO_YEARLY_BILLING_DESCRIPTOR = msg({
	message: 'Your subscription will switch to yearly billing at the next renewal.',
	comment: 'Toast success shown after a Plutonium subscription is scheduled to switch to yearly billing.',
});
const YOUR_SUBSCRIPTION_WILL_SWITCH_TO_MONTHLY_BILLING_DESCRIPTOR = msg({
	message: 'Your subscription will switch to monthly billing at the next renewal.',
	comment: 'Toast success shown after a Plutonium subscription is scheduled to switch to monthly billing.',
});
const YOUR_PENDING_BILLING_CHANGE_HAS_BEEN_CANCELED_DESCRIPTOR = msg({
	message: 'Your pending billing change has been canceled.',
	comment: 'Toast success shown after a scheduled Plutonium billing cycle change is canceled.',
});
const CHANGE_CYCLE_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't change your billing cycle",
	comment: 'Title of the generic fallback error modal shown when changing the Plutonium billing cycle fails.',
});
const CHANGE_CYCLE_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while changing your billing cycle. Please try again in a moment.',
	comment: 'Body of the generic fallback error modal shown when changing the Plutonium billing cycle fails.',
});
const CANCEL_PENDING_CHANGE_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't cancel your pending billing change",
	comment:
		'Title of the generic fallback error modal shown when canceling a scheduled Plutonium billing cycle change fails.',
});
const CANCEL_PENDING_CHANGE_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while canceling your pending billing change. Please try again in a moment.',
	comment:
		'Body of the generic fallback error modal shown when canceling a scheduled Plutonium billing cycle change fails.',
});
const YOUR_SUBSCRIPTION_WILL_MOVE_TO_THE_NEW_PRICE_DESCRIPTOR = msg({
	message: 'Your subscription moves to the new price on {effectiveDate}.',
	comment:
		'Toast success shown after a Plutonium subscription is scheduled to move down to the current price. {effectiveDate} is the localized date the new price starts.',
});
const THE_NEW_PRICE_IS_ALREADY_SCHEDULED_DESCRIPTOR = msg({
	message: 'The new price is already scheduled for {effectiveDate}.',
	comment:
		'Toast success shown when a move down to the current price was already scheduled by an earlier request. {effectiveDate} is the localized date the new price starts.',
});
const SWITCH_TO_LIST_PRICE_UNAVAILABLE_DESCRIPTOR = msg({
	message: "Your subscription can't move to the new price right now.",
	comment:
		'Toast error shown when the server refuses to move a Plutonium subscription down to the current price, whatever the reason.',
});
const SWITCH_TO_LIST_PRICE_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't change your price",
	comment:
		'Title of the generic fallback error modal shown when moving a Plutonium subscription down to the current price fails.',
});
const SWITCH_TO_LIST_PRICE_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while changing your price. Please try again in a moment.',
	comment:
		'Body of the generic fallback error modal shown when moving a Plutonium subscription down to the current price fails.',
});
const logger = new Logger('useSubscriptionActions');
export const useSubscriptionActions = (countryCode?: string | null) => {
	const {i18n} = useLingui();
	const [loadingPortal, setLoadingPortal] = useState(false);
	const [loadingCancel, setLoadingCancel] = useState(false);
	const [loadingReactivate, setLoadingReactivate] = useState(false);
	const [loadingEndGrace, setLoadingEndGrace] = useState(false);
	const [loadingChangeBillingCycle, setLoadingChangeBillingCycle] = useState<'monthly' | 'yearly' | null>(null);
	const [loadingCancelPendingChange, setLoadingCancelPendingChange] = useState(false);
	const [loadingSwitchToListPrice, setLoadingSwitchToListPrice] = useState(false);
	const handleOpenCustomerPortal = useCallback(async () => {
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
				'app.plutonium.use-subscription-actions.open-customer-portal.generic-error-modal',
			);
		} finally {
			setLoadingPortal(false);
		}
	}, [i18n]);
	const handleCancelSubscription = useCallback(async () => {
		setLoadingCancel(true);
		try {
			await PremiumCommands.cancelSubscriptionAtPeriodEnd();
			await PremiumCommands.refreshPremiumState(countryCode ?? undefined);
			ToastCommands.success(i18n._(YOUR_SUBSCRIPTION_HAS_BEEN_SET_TO_CANCEL_AT_DESCRIPTOR));
		} catch (error) {
			logger.error('Failed to cancel subscription', error);
			showPremiumActionErrorModal(
				error,
				{
					fallbackTitle: CANCEL_FAILED_TITLE_DESCRIPTOR,
					fallbackMessage: CANCEL_FAILED_MESSAGE_DESCRIPTOR,
				},
				'app.plutonium.use-subscription-actions.cancel-subscription.generic-error-modal',
			);
		} finally {
			setLoadingCancel(false);
		}
	}, [countryCode, i18n]);
	const handleEndPremiumGracePeriod = useCallback(async () => {
		setLoadingEndGrace(true);
		try {
			await PremiumCommands.endPremiumGracePeriod();
			await PremiumCommands.refreshPremiumState(countryCode ?? undefined);
			ToastCommands.success(i18n._(YOUR_GRACE_PERIOD_HAS_ENDED_DESCRIPTOR));
		} catch (error) {
			logger.error('Failed to end premium grace period', error);
			showPremiumActionErrorModal(
				error,
				{
					fallbackTitle: END_GRACE_FAILED_TITLE_DESCRIPTOR,
					fallbackMessage: END_GRACE_FAILED_MESSAGE_DESCRIPTOR,
				},
				'app.plutonium.use-subscription-actions.end-grace-period.generic-error-modal',
			);
		} finally {
			setLoadingEndGrace(false);
		}
	}, [countryCode, i18n]);
	const handleReactivateSubscription = useCallback(async () => {
		setLoadingReactivate(true);
		try {
			await PremiumCommands.reactivateSubscription();
			await PremiumCommands.refreshPremiumState(countryCode ?? undefined);
			ToastCommands.success(i18n._(YOUR_SUBSCRIPTION_HAS_BEEN_REACTIVATED_DESCRIPTOR));
		} catch (error) {
			logger.error('Failed to reactivate subscription', error);
			showPremiumActionErrorModal(
				error,
				{
					fallbackTitle: REACTIVATE_FAILED_TITLE_DESCRIPTOR,
					fallbackMessage: REACTIVATE_FAILED_MESSAGE_DESCRIPTOR,
				},
				'app.plutonium.use-subscription-actions.reactivate-subscription.generic-error-modal',
			);
		} finally {
			setLoadingReactivate(false);
		}
	}, [countryCode, i18n]);
	const handleChangeSubscriptionBillingCycle = useCallback(
		async (
			billingCycle: 'monthly' | 'yearly',
			effectiveAt: PremiumCommands.SubscriptionBillingCycleChangeEffectiveAt = 'now',
		) => {
			setLoadingChangeBillingCycle(billingCycle);
			try {
				await PremiumCommands.changeSubscriptionBillingCycle(billingCycle, effectiveAt);
				await PremiumCommands.refreshPremiumState(countryCode ?? undefined);
				ToastCommands.success(
					effectiveAt === 'period_end'
						? billingCycle === 'yearly'
							? i18n._(YOUR_SUBSCRIPTION_WILL_SWITCH_TO_YEARLY_BILLING_DESCRIPTOR)
							: i18n._(YOUR_SUBSCRIPTION_WILL_SWITCH_TO_MONTHLY_BILLING_DESCRIPTOR)
						: billingCycle === 'yearly'
							? i18n._(YOUR_SUBSCRIPTION_HAS_BEEN_SWITCHED_TO_YEARLY_BILLING_DESCRIPTOR)
							: i18n._(YOUR_SUBSCRIPTION_HAS_BEEN_SWITCHED_TO_MONTHLY_BILLING_DESCRIPTOR),
				);
			} catch (error) {
				logger.error('Failed to change subscription billing cycle', error);
				showPremiumActionErrorModal(
					error,
					{
						fallbackTitle: CHANGE_CYCLE_FAILED_TITLE_DESCRIPTOR,
						fallbackMessage: CHANGE_CYCLE_FAILED_MESSAGE_DESCRIPTOR,
					},
					'app.plutonium.use-subscription-actions.change-billing-cycle.generic-error-modal',
				);
			} finally {
				setLoadingChangeBillingCycle(null);
			}
		},
		[countryCode, i18n],
	);
	const handleSwitchToListPrice = useCallback(async () => {
		setLoadingSwitchToListPrice(true);
		try {
			const result = await PremiumCommands.switchSubscriptionToListPrice();
			await PremiumCommands.refreshPremiumState(countryCode ?? undefined);
			if (result.status === 'ineligible') {
				logger.warn('List price switch refused', {reason: result.reason});
				ToastCommands.error(i18n._(SWITCH_TO_LIST_PRICE_UNAVAILABLE_DESCRIPTOR));
				return;
			}
			const effectiveDate = getFormattedLongDate(result.effective_at, LocaleUtils.getCurrentLocale());
			ToastCommands.success(
				result.status === 'already_scheduled'
					? i18n._(THE_NEW_PRICE_IS_ALREADY_SCHEDULED_DESCRIPTOR, {effectiveDate})
					: i18n._(YOUR_SUBSCRIPTION_WILL_MOVE_TO_THE_NEW_PRICE_DESCRIPTOR, {effectiveDate}),
			);
		} catch (error) {
			logger.error('Failed to switch subscription to the current list price', error);
			showPremiumActionErrorModal(
				error,
				{
					fallbackTitle: SWITCH_TO_LIST_PRICE_FAILED_TITLE_DESCRIPTOR,
					fallbackMessage: SWITCH_TO_LIST_PRICE_FAILED_MESSAGE_DESCRIPTOR,
				},
				'app.plutonium.use-subscription-actions.switch-to-list-price.generic-error-modal',
			);
		} finally {
			setLoadingSwitchToListPrice(false);
		}
	}, [countryCode, i18n]);
	const handleCancelPendingSubscriptionChange = useCallback(async () => {
		setLoadingCancelPendingChange(true);
		try {
			await PremiumCommands.cancelPendingSubscriptionChange();
			await PremiumCommands.refreshPremiumState(countryCode ?? undefined);
			ToastCommands.success(i18n._(YOUR_PENDING_BILLING_CHANGE_HAS_BEEN_CANCELED_DESCRIPTOR));
		} catch (error) {
			logger.error('Failed to cancel pending billing cycle change', error);
			showPremiumActionErrorModal(
				error,
				{
					fallbackTitle: CANCEL_PENDING_CHANGE_FAILED_TITLE_DESCRIPTOR,
					fallbackMessage: CANCEL_PENDING_CHANGE_FAILED_MESSAGE_DESCRIPTOR,
				},
				'app.plutonium.use-subscription-actions.cancel-pending-change.generic-error-modal',
			);
		} finally {
			setLoadingCancelPendingChange(false);
		}
	}, [countryCode, i18n]);
	return {
		loadingPortal,
		loadingCancel,
		loadingReactivate,
		loadingEndGrace,
		loadingChangeBillingCycle,
		loadingCancelPendingChange,
		loadingSwitchToListPrice,
		handleOpenCustomerPortal,
		handleCancelSubscription,
		handleEndPremiumGracePeriod,
		handleReactivateSubscription,
		handleChangeSubscriptionBillingCycle,
		handleCancelPendingSubscriptionChange,
		handleSwitchToListPrice,
	};
};
