// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {
	PAYMENT_PROVIDER_NAME,
	PIX_PAYMENT_METHOD,
	PRODUCT_NAME,
	SUPPORT_EMAIL,
	UPI_PAYMENT_METHOD,
} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {CANCEL_DESCRIPTOR, CLOSE_DESCRIPTOR, OKAY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {CheckoutPaymentMethod, PriceIds} from '@app/features/premium/commands/PremiumCommands';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import PremiumState from '@app/features/premium/state/PremiumState';
import {recordPremiumCheckoutReturnIntent} from '@app/features/premium/utils/PremiumCheckoutReturnIntent';
import {MANAGE_SUBSCRIPTION_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {getPremiumProductFullName, getStoreName} from '@app/features/premium/utils/PremiumUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useState} from 'react';

const CUSTOMER_PORTAL_OPEN_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't open the billing portal",
	comment: 'Title of the error modal shown when opening the billing customer portal fails.',
});
const CUSTOMER_PORTAL_OPEN_FAILED_BODY_DESCRIPTOR = msg({
	message: 'Something went wrong while opening the billing portal. Please try again in a moment.',
	comment: 'Body of the error modal shown when opening the billing customer portal fails.',
});
const PIX_PAYMENT_PROMPT_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Pay with {pixPaymentMethod} Automático to authorize recurring charges directly from your Brazilian bank. Or choose "Use card" to enter a credit card on {paymentProviderName}\'s next screen.',
	comment:
		'Plutonium subscription payment method picker description for Brazil. Explains Pix recurring vs falling back to a card.',
});
const USE_PIX_BUTTON_DESCRIPTOR = msg({
	message: 'Use {pixPaymentMethod}',
	comment: 'Plutonium subscription payment method picker button. Selects Pix as the payment method.',
});
const UPI_PAYMENT_PROMPT_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Pay with {upiPaymentMethod} to set up an RBI-compliant e-mandate from your Indian bank. Or choose "Use card" to enter a credit card on {paymentProviderName}\'s next screen.',
	comment:
		'Plutonium subscription payment method picker description for India. Explains UPI e-mandate vs falling back to a card.',
});
const USE_UPI_BUTTON_DESCRIPTOR = msg({
	message: 'Use {upiPaymentMethod}',
	comment: 'Plutonium subscription payment method picker button. Selects UPI as the payment method.',
});
const GENERIC_PAYMENT_PROMPT_DESCRIPTION_DESCRIPTOR = msg({
	message: "Choose how you'd like to pay for this subscription.",
	comment:
		'Plutonium subscription payment method picker description, generic fallback when no country-specific copy applies.',
});
const USE_ALTERNATIVE_METHOD_BUTTON_DESCRIPTOR = msg({
	message: 'Use alternative method',
	comment: 'Plutonium subscription payment method picker button. Selects the alternative (non-card) method.',
});
const EMAIL_VERIFICATION_REQUIRED_TITLE_DESCRIPTOR = msg({
	message: 'Verify your email first',
	comment: 'Title of the error modal shown when an unverified account tries to purchase Plutonium.',
});
const EMAIL_VERIFICATION_REQUIRED_BODY_DESCRIPTOR = msg({
	message: 'You need to verify your email before you can purchase {premiumProductFullName}.',
	comment:
		'Body of the error modal shown when the account email is unverified and the user tries to purchase Plutonium. Product name is interpolated.',
});
const ALREADY_VISIONARY_TITLE_DESCRIPTOR = msg({
	message: "You're already Visionary",
	comment: 'Modal title shown when a lifetime Visionary tier user tries to start a recurring Plutonium subscription.',
});
const ALREADY_VISIONARY_BODY_DESCRIPTOR = msg({
	message:
		"Visionary already includes permanent access, so a recurring subscription isn't needed. You can still buy gifts for others.",
	comment:
		'Modal body shown when a Visionary user tries to subscribe. Reassures them and points out gift purchases stay available.',
});
const EXISTING_SUBSCRIPTION_TITLE_DESCRIPTOR = msg({
	message: 'Subscription already exists',
	comment:
		'Modal title shown when checkout is blocked because the account already has an active Plutonium subscription.',
});
const EXISTING_SUBSCRIPTION_BODY_DESCRIPTOR = msg({
	message:
		'We found an existing {premiumProductFullName} subscription for this account. Manage it in the secure billing portal to update payment details or check renewal status. If you just paid, wait a minute and reopen this page.',
	comment:
		'Modal body for existing-subscription block. Directs the user to the billing portal and addresses the just-paid race case. Keep plain and reassuring.',
});
const EXISTING_STORE_SUBSCRIPTION_BODY_DESCRIPTOR = msg({
	message:
		'Your {premiumProductFullName} subscription is billed through {storeName}. Manage it in your {storeName} account to change your plan or check renewal status.',
	comment:
		'Modal body for existing-subscription block when the subscription was bought in a mobile app store. {storeName} is the store brand name, App Store or Google Play, and must not be translated. {premiumProductFullName} is the full premium product name.',
});
const PURCHASES_DISABLED_TITLE_DESCRIPTOR = msg({
	message: 'Purchases unavailable',
	comment: 'Modal title shown when purchases are disabled on this account (server-side enforcement).',
});
const PURCHASES_DISABLED_BODY_DESCRIPTOR = msg({
	message: 'Purchases are disabled for this account. Contact {supportEmail} if this looks wrong.',
	comment: 'Modal body shown when purchases are disabled. Provides the support email for appeals.',
});
const PURCHASES_DISABLED_SELF_HOSTED_BODY_DESCRIPTOR = msg({
	message: 'Purchases are disabled for this account.',
	comment: 'Modal body shown on a self-hosted instance when purchases are disabled for the account.',
});
const CHECKOUT_BLOCKED_TITLE_DESCRIPTOR = msg({
	message: 'Checkout unavailable',
	comment: 'Modal title for the generic "checkout blocked" state when no more specific reason is known.',
});
const CHECKOUT_BLOCKED_BODY_DESCRIPTOR = msg({
	message: 'Checkout is blocked for this account. Contact {supportEmail} if you need help.',
	comment: 'Modal body for the generic "checkout blocked" state. Provides the support email.',
});
const CHECKOUT_BLOCKED_SELF_HOSTED_BODY_DESCRIPTOR = msg({
	message: 'Checkout is blocked for this account.',
	comment: 'Modal body for the generic "checkout blocked" state on a self-hosted instance.',
});
const CHECKOUT_START_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't start checkout",
	comment: 'Title of the generic fallback error modal shown when creating a checkout session fails unexpectedly.',
});
const CHECKOUT_START_FAILED_BODY_DESCRIPTOR = msg({
	message: 'Something went wrong while starting checkout. Please try again in a moment.',
	comment: 'Body of the generic fallback error modal shown when creating a checkout session fails unexpectedly.',
});
const GIFT_TIME_KEPT_TITLE_DESCRIPTOR = msg({
	message: 'Your gift time is kept',
	comment:
		'Title of the confirmation shown before checkout when a user with gifted premium time starts a recurring subscription.',
});
const GIFT_TIME_KEPT_BODY_DESCRIPTOR = msg({
	message: "You'll be charged now. Your remaining gift time is added after your paid period, so none of it is lost.",
	comment:
		'Body of the confirmation shown before checkout when a user with gifted premium time starts a recurring subscription. The subscription is charged immediately and the unused gift time is appended after the paid period.',
});
const CONTINUE_TO_CHECKOUT_DESCRIPTOR = msg({
	message: 'Continue to checkout',
	comment: 'Button in the gift time confirmation that proceeds to subscription checkout.',
});
const PRICING_NOT_LOADED_TOAST_DESCRIPTOR = msg({
	message: 'Pricing is still loading.',
	comment: 'Error modal body shown when the user clicks a plan before price IDs have loaded.',
});
const PLAN_UNAVAILABLE_TOAST_DESCRIPTOR = msg({
	message: "This plan isn't available. Contact support.",
	comment: 'Error modal body shown when the selected Plutonium plan has no price ID configured.',
});
const COMPLETE_PAYMENT_MODAL_TITLE_DESCRIPTOR = msg({
	message: 'Complete payment',
	comment: 'Modal title for the mobile checkout confirmation when opening the payment provider in a browser.',
});
const COMPLETE_PAYMENT_MODAL_BODY_DESCRIPTOR = msg({
	message:
		"You are now navigating to {paymentProviderName} to complete the payment. Return to {productName} once you've completed it.",
	comment: 'Modal body for the mobile checkout confirmation. Explains that the user is leaving the app to pay.',
});
const CHOOSE_PAYMENT_METHOD_MODAL_TITLE_DESCRIPTOR = msg({
	message: 'Choose payment method',
	comment: 'Modal title for the payment method picker (local card vs alternative method).',
});
const USE_CARD_BUTTON_DESCRIPTOR = msg({
	message: 'Use card',
	comment: 'Modal secondary button to fall back to standard card checkout instead of an alternative payment method.',
});
const logger = new Logger('useCheckoutActions');

type Plan = 'monthly' | 'yearly' | 'gift_1_month' | 'gift_1_year';
type PremiumPurchaseBlockedReason = 'lifetime' | 'existing_subscription' | 'purchase_disabled';

function getPremiumPurchaseBlockedStoreProvider(body: unknown): 'app_store' | 'google_play' | null {
	if (!body || typeof body !== 'object' || !('provider' in body)) {
		return null;
	}
	const provider = body.provider;
	if (provider === 'app_store' || provider === 'google_play') {
		return provider;
	}
	return null;
}

function getPremiumPurchaseBlockedReason(body: unknown): PremiumPurchaseBlockedReason | null {
	if (!body || typeof body !== 'object' || !('reason' in body)) {
		return null;
	}
	const reason = body.reason;
	if (reason === 'lifetime' || reason === 'existing_subscription' || reason === 'purchase_disabled') {
		return reason;
	}
	return null;
}

const MANDATORY_LOCAL_PAYMENT_CURRENCIES: ReadonlySet<string> = new Set(['BRL']);

function alternativePaymentMethodForCurrency(
	currency: string | null | undefined,
	isGift: boolean,
	plan: Plan,
): CheckoutPaymentMethod | null {
	if (isGift || (plan !== 'monthly' && plan !== 'yearly') || RuntimeConfig.isSelfHosted()) {
		return null;
	}
	if (currency === 'BRL') return 'pix';
	if (currency === 'INR') return 'upi';
	return null;
}

export const useCheckoutActions = (
	priceIds: PriceIds | null,
	countryCode: string | null,
	mobileEnabled: boolean,
	{hasGiftTime = false}: {hasGiftTime?: boolean} = {},
) => {
	const {i18n} = useLingui();
	const [loadingCheckout, setLoadingCheckout] = useState(false);
	const openCustomerPortalFromCheckoutBlock = useCallback(async () => {
		try {
			const url = await PremiumCommands.createCustomerPortalSession();
			await openExternalUrl(url);
		} catch (error) {
			logger.error('Failed to open customer portal from checkout block', error);
			ModalCommands.push(
				modal(() => (
					<GenericErrorModal
						title={i18n._(CUSTOMER_PORTAL_OPEN_FAILED_TITLE_DESCRIPTOR)}
						message={i18n._(CUSTOMER_PORTAL_OPEN_FAILED_BODY_DESCRIPTOR)}
						data-flx="app.plutonium.use-checkout-actions.open-customer-portal.generic-error-modal"
					/>
				)),
			);
		}
	}, [i18n]);
	const getAlternativePaymentMethodPrompt = useCallback(
		(
			currency: string | null | undefined,
			method: CheckoutPaymentMethod,
		): {description: string; primaryText: string} => {
			if (method === 'pix' && currency === 'BRL') {
				return {
					description: i18n._(PIX_PAYMENT_PROMPT_DESCRIPTION_DESCRIPTOR, {
						pixPaymentMethod: PIX_PAYMENT_METHOD,
						paymentProviderName: PAYMENT_PROVIDER_NAME,
					}),
					primaryText: i18n._(USE_PIX_BUTTON_DESCRIPTOR, {pixPaymentMethod: PIX_PAYMENT_METHOD}),
				};
			}
			if (method === 'upi' && currency === 'INR') {
				return {
					description: i18n._(UPI_PAYMENT_PROMPT_DESCRIPTION_DESCRIPTOR, {
						upiPaymentMethod: UPI_PAYMENT_METHOD,
						paymentProviderName: PAYMENT_PROVIDER_NAME,
					}),
					primaryText: i18n._(USE_UPI_BUTTON_DESCRIPTOR, {upiPaymentMethod: UPI_PAYMENT_METHOD}),
				};
			}
			return {
				description: i18n._(GENERIC_PAYMENT_PROMPT_DESCRIPTION_DESCRIPTOR),
				primaryText: i18n._(USE_ALTERNATIVE_METHOD_BUTTON_DESCRIPTOR),
			};
		},
		[i18n],
	);
	const handleCheckoutError = useCallback(
		(error: unknown) => {
			logger.error('Failed to create checkout session', error);
			if (error instanceof HttpError) {
				const body = error.body;
				if (body && typeof body === 'object' && 'code' in body && typeof body.code === 'string') {
					if (
						body.code === APIErrorCodes.EMAIL_VERIFICATION_REQUIRED ||
						body.code === APIErrorCodes.PURCHASE_EMAIL_VERIFICATION_REQUIRED
					) {
						ModalCommands.push(
							modal(() => (
								<GenericErrorModal
									title={i18n._(EMAIL_VERIFICATION_REQUIRED_TITLE_DESCRIPTOR)}
									message={i18n._(EMAIL_VERIFICATION_REQUIRED_BODY_DESCRIPTOR, {
										premiumProductFullName: getPremiumProductFullName(),
									})}
									data-flx="app.plutonium.use-checkout-actions.email-verification-required.generic-error-modal"
								/>
							)),
						);
						return;
					}
					if (body.code === APIErrorCodes.PREMIUM_PURCHASE_BLOCKED) {
						const reason = getPremiumPurchaseBlockedReason(body);
						if (reason === 'lifetime') {
							ModalCommands.push(
								modal(() => (
									<ConfirmModal
										title={i18n._(ALREADY_VISIONARY_TITLE_DESCRIPTOR)}
										description={i18n._(ALREADY_VISIONARY_BODY_DESCRIPTOR)}
										secondaryText={i18n._(CLOSE_DESCRIPTOR)}
										data-flx="app.plutonium.use-checkout-actions.handle-checkout-error.confirm-modal"
									/>
								)),
							);
							return;
						}
						const storeProvider = getPremiumPurchaseBlockedStoreProvider(body);
						if (reason === 'existing_subscription' && storeProvider) {
							const store = PremiumState.state?.store;
							const manageUrl = store?.provider === storeProvider ? store.manage_url : null;
							const description = i18n._(EXISTING_STORE_SUBSCRIPTION_BODY_DESCRIPTOR, {
								premiumProductFullName: getPremiumProductFullName(),
								storeName: getStoreName(storeProvider),
							});
							ModalCommands.push(
								modal(() =>
									manageUrl ? (
										<ConfirmModal
											title={i18n._(EXISTING_SUBSCRIPTION_TITLE_DESCRIPTOR)}
											description={description}
											primaryText={i18n._(MANAGE_SUBSCRIPTION_DESCRIPTOR)}
											primaryVariant="primary"
											secondaryText={i18n._(CLOSE_DESCRIPTOR)}
											onPrimary={() => void openExternalUrl(manageUrl)}
											data-flx="app.plutonium.use-checkout-actions.handle-checkout-error.confirm-modal--5"
										/>
									) : (
										<ConfirmModal
											title={i18n._(EXISTING_SUBSCRIPTION_TITLE_DESCRIPTOR)}
											description={description}
											secondaryText={i18n._(CLOSE_DESCRIPTOR)}
											data-flx="app.plutonium.use-checkout-actions.handle-checkout-error.confirm-modal--5"
										/>
									),
								),
							);
							return;
						}
						if (reason === 'existing_subscription') {
							ModalCommands.push(
								modal(() => (
									<ConfirmModal
										title={i18n._(EXISTING_SUBSCRIPTION_TITLE_DESCRIPTOR)}
										description={i18n._(EXISTING_SUBSCRIPTION_BODY_DESCRIPTOR, {
											premiumProductFullName: getPremiumProductFullName(),
										})}
										primaryText={i18n._(MANAGE_SUBSCRIPTION_DESCRIPTOR)}
										primaryVariant="primary"
										secondaryText={i18n._(CLOSE_DESCRIPTOR)}
										onPrimary={openCustomerPortalFromCheckoutBlock}
										data-flx="app.plutonium.use-checkout-actions.handle-checkout-error.confirm-modal--2"
									/>
								)),
							);
							return;
						}
						if (reason === 'purchase_disabled') {
							ModalCommands.push(
								modal(() => (
									<ConfirmModal
										title={i18n._(PURCHASES_DISABLED_TITLE_DESCRIPTOR)}
										description={
											RuntimeConfig.isSelfHosted()
												? i18n._(PURCHASES_DISABLED_SELF_HOSTED_BODY_DESCRIPTOR)
												: i18n._(PURCHASES_DISABLED_BODY_DESCRIPTOR, {supportEmail: SUPPORT_EMAIL})
										}
										secondaryText={i18n._(CLOSE_DESCRIPTOR)}
										data-flx="app.plutonium.use-checkout-actions.handle-checkout-error.confirm-modal--3"
									/>
								)),
							);
							return;
						}
						ModalCommands.push(
							modal(() => (
								<ConfirmModal
									title={i18n._(CHECKOUT_BLOCKED_TITLE_DESCRIPTOR)}
									description={
										RuntimeConfig.isSelfHosted()
											? i18n._(CHECKOUT_BLOCKED_SELF_HOSTED_BODY_DESCRIPTOR)
											: i18n._(CHECKOUT_BLOCKED_BODY_DESCRIPTOR, {supportEmail: SUPPORT_EMAIL})
									}
									secondaryText={i18n._(CLOSE_DESCRIPTOR)}
									data-flx="app.plutonium.use-checkout-actions.handle-checkout-error.confirm-modal--4"
								/>
							)),
						);
						return;
					}
				}
			}
			ModalCommands.push(
				modal(() => (
					<GenericErrorModal
						title={i18n._(CHECKOUT_START_FAILED_TITLE_DESCRIPTOR)}
						message={i18n._(CHECKOUT_START_FAILED_BODY_DESCRIPTOR)}
						data-flx="app.plutonium.use-checkout-actions.checkout-start-failed.generic-error-modal"
					/>
				)),
			);
		},
		[openCustomerPortalFromCheckoutBlock, i18n],
	);
	const handleSelectPlan = useCallback(
		async (plan: Plan) => {
			if (loadingCheckout) return;
			logger.info('Plan selected', {plan, hasGiftTime});
			const showCheckoutPlanErrorModal = (message: string, flxKey: string) => {
				ModalCommands.push(
					modal(() => (
						<GenericErrorModal
							title={i18n._(CHECKOUT_START_FAILED_TITLE_DESCRIPTOR)}
							message={message}
							data-flx={flxKey}
						/>
					)),
				);
			};
			if (!priceIds) {
				logger.error('Price IDs not loaded yet');
				showCheckoutPlanErrorModal(
					i18n._(PRICING_NOT_LOADED_TOAST_DESCRIPTOR),
					'app.plutonium.use-checkout-actions.pricing-not-loaded.generic-error-modal',
				);
				return;
			}
			const planConfig: Record<Plan, {id: string | null; gift?: boolean}> = {
				monthly: {id: priceIds.monthly ?? null},
				yearly: {id: priceIds.yearly ?? null},
				gift_1_month: {id: priceIds.gift_1_month ?? null, gift: true},
				gift_1_year: {id: priceIds.gift_1_year ?? null, gift: true},
			};
			const selected = planConfig[plan];
			const priceId = selected.id;
			const isGift = selected.gift ?? false;
			if (!priceId) {
				logger.error('Price ID not available for plan', {plan});
				showCheckoutPlanErrorModal(
					i18n._(PLAN_UNAVAILABLE_TOAST_DESCRIPTOR),
					'app.plutonium.use-checkout-actions.plan-unavailable.generic-error-modal',
				);
				return;
			}
			const markCheckoutLaunched = () => {
				if (!isGift) {
					recordPremiumCheckoutReturnIntent('plutonium');
				}
			};
			const openCheckoutUrl = async (
				checkoutUrl: string,
				{skipMobilePrompt = false}: {skipMobilePrompt?: boolean} = {},
			) => {
				if (mobileEnabled && !skipMobilePrompt) {
					ModalCommands.push(
						modal(() => (
							<ConfirmModal
								title={i18n._(COMPLETE_PAYMENT_MODAL_TITLE_DESCRIPTOR)}
								description={i18n._(COMPLETE_PAYMENT_MODAL_BODY_DESCRIPTOR, {
									paymentProviderName: PAYMENT_PROVIDER_NAME,
									productName: PRODUCT_NAME,
								})}
								primaryText={i18n._(OKAY_DESCRIPTOR)}
								primaryVariant="primary"
								secondaryText={i18n._(CANCEL_DESCRIPTOR)}
								onPrimary={() => {
									markCheckoutLaunched();
									void openExternalUrl(checkoutUrl);
								}}
								data-flx="app.plutonium.use-checkout-actions.open-checkout-url.confirm-modal"
							/>
						)),
					);
					return;
				}
				markCheckoutLaunched();
				await openExternalUrl(checkoutUrl);
			};
			const startCheckout = async ({
				skipMobilePrompt = false,
				paymentMethod,
			}: {
				skipMobilePrompt?: boolean;
				paymentMethod?: CheckoutPaymentMethod;
			} = {}) => {
				setLoadingCheckout(true);
				try {
					const checkoutUrl = await PremiumCommands.createCheckoutSession(
						priceId,
						countryCode ?? undefined,
						isGift,
						paymentMethod,
					);
					await openCheckoutUrl(checkoutUrl, {skipMobilePrompt});
				} catch (error) {
					handleCheckoutError(error);
				} finally {
					setLoadingCheckout(false);
				}
			};
			const proceedToCheckout = async ({afterConfirm = false}: {afterConfirm?: boolean} = {}) => {
				const altPaymentMethod = alternativePaymentMethodForCurrency(priceIds.currency, isGift, plan);
				if (altPaymentMethod && MANDATORY_LOCAL_PAYMENT_CURRENCIES.has(priceIds.currency ?? '')) {
					await startCheckout({skipMobilePrompt: afterConfirm, paymentMethod: altPaymentMethod});
					return;
				}
				if (altPaymentMethod) {
					const altPrompt = getAlternativePaymentMethodPrompt(priceIds.currency, altPaymentMethod);
					ModalCommands.push(
						modal(() => (
							<ConfirmModal
								title={i18n._(CHOOSE_PAYMENT_METHOD_MODAL_TITLE_DESCRIPTOR)}
								description={altPrompt.description}
								primaryText={altPrompt.primaryText}
								primaryVariant="primary"
								secondaryText={i18n._(USE_CARD_BUTTON_DESCRIPTOR)}
								onPrimary={() => {
									void startCheckout({skipMobilePrompt: true, paymentMethod: altPaymentMethod});
								}}
								onSecondary={() => {
									void startCheckout({skipMobilePrompt: true});
								}}
								data-flx="app.plutonium.use-checkout-actions.handle-select-plan.confirm-modal--2"
							/>
						)),
					);
					return;
				}
				await startCheckout({skipMobilePrompt: afterConfirm});
			};
			if (hasGiftTime && !isGift) {
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(GIFT_TIME_KEPT_TITLE_DESCRIPTOR)}
							description={i18n._(GIFT_TIME_KEPT_BODY_DESCRIPTOR)}
							primaryText={i18n._(CONTINUE_TO_CHECKOUT_DESCRIPTOR)}
							primaryVariant="primary"
							secondaryText={i18n._(CANCEL_DESCRIPTOR)}
							onPrimary={() => {
								void proceedToCheckout({afterConfirm: true});
							}}
							data-flx="app.plutonium.use-checkout-actions.handle-select-plan.gift-time-confirm-modal"
						/>
					)),
				);
				return;
			}
			await proceedToCheckout();
		},
		[
			handleCheckoutError,
			loadingCheckout,
			priceIds,
			countryCode,
			getAlternativePaymentMethodPrompt,
			hasGiftTime,
			mobileEnabled,
			i18n,
		],
	);
	return {
		loadingCheckout,
		handleSelectPlan,
	};
};
