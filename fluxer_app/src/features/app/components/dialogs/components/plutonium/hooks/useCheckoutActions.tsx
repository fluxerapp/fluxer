// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {
	BLIK_PAYMENT_METHOD,
	PAYMENT_PROVIDER_NAME,
	PIX_PAYMENT_METHOD,
	PREMIUM_PRODUCT_FULL_NAME,
	PRODUCT_NAME,
	SUPPORT_EMAIL,
	UPI_PAYMENT_METHOD,
} from '@app/features/app/config/I18nDisplayConstants';
import {CANCEL_DESCRIPTOR, CLOSE_DESCRIPTOR, OKAY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {CheckoutPaymentMethod, PriceIds} from '@app/features/premium/commands/PremiumCommands';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import {recordPremiumCheckoutReturnIntent} from '@app/features/premium/utils/PremiumCheckoutReturnIntent';
import {MANAGE_SUBSCRIPTION_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
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
		"Pay with {pixPaymentMethod} automático to authorize recurring charges directly from your Brazilian bank. Or choose use card to enter a credit card on {paymentProviderName}'s next screen.",
	comment:
		'Plutonium subscription payment method picker description for Brazil. Explains Pix recurring vs falling back to a card.',
});
const USE_PIX_BUTTON_DESCRIPTOR = msg({
	message: 'Use {pixPaymentMethod}',
	comment: 'Plutonium subscription payment method picker button. Selects Pix as the payment method.',
});
const UPI_PAYMENT_PROMPT_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"Pay with {upiPaymentMethod} to set up an RBI-compliant e-mandate from your Indian bank. Or choose use card to enter a credit card on {paymentProviderName}'s next screen.",
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
const LOCAL_CARD_PROMPT_BRL_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"Use local card to verify a card issued in Brazil before the paid BRL checkout. Choose other methods if you'd rather try {pixPaymentMethod} or another payment method {paymentProviderName} offers on the next screen.",
	comment:
		'Plutonium checkout pre-approval prompt for Brazil. Explains the card verification flow and the Pix alternative.',
});
const OTHER_METHODS_BUTTON_DESCRIPTOR = msg({
	message: 'Other methods',
	comment: 'Plutonium checkout pre-approval prompt secondary button. Opens the alternative payment methods flow.',
});
const LOCAL_CARD_PROMPT_INR_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"Use local card to verify a card issued in India before the paid INR checkout. Choose other methods if you'd rather try {upiPaymentMethod} or another payment method {paymentProviderName} offers on the next screen.",
	comment:
		'Plutonium checkout pre-approval prompt for India. Explains the card verification flow and the UPI alternative.',
});
const LOCAL_CARD_PROMPT_PLN_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"Use local card to verify a card issued in Poland before the paid PLN checkout. {blikPaymentMethod} does not support subscriptions in {paymentProviderName} checkout, so continue only if you want {paymentProviderName2}'s standard payment screen instead.",
	comment:
		'Plutonium checkout pre-approval prompt for Poland. Explains that BLIK is not supported for subscriptions; user must continue to the standard checkout.',
});
const CONTINUE_TO_CHECKOUT_BUTTON_DESCRIPTOR = msg({
	message: 'Continue to checkout',
	comment:
		'Plutonium checkout pre-approval prompt secondary button. Continues straight to the standard checkout screen.',
});
const LOCAL_CARD_PROMPT_TRY_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"Use local card to verify a card issued in Türkiye before the paid TRY checkout. There isn't an app-based local subscription method here, so continuing will take you to {paymentProviderName}'s standard payment screen.",
	comment:
		'Plutonium checkout pre-approval prompt for Türkiye. Explains there is no local wallet method; continuing goes to the standard checkout.',
});
const LOCAL_CARD_PROMPT_DEFAULT_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Use local card to verify a card issued in your billing country before the paid checkout. Choose other methods for any other payment method {paymentProviderName} offers on the next screen.',
	comment:
		'Plutonium checkout pre-approval prompt default. Generic description for countries without specific localized payment guidance.',
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
const PURCHASES_DISABLED_TITLE_DESCRIPTOR = msg({
	message: 'Purchases unavailable',
	comment: 'Modal title shown when purchases are disabled on this account (server-side enforcement).',
});
const PURCHASES_DISABLED_BODY_DESCRIPTOR = msg({
	message: 'Purchases are disabled for this account. Contact {supportEmail} if this looks wrong.',
	comment: 'Modal body shown when purchases are disabled. Provides the support email for appeals.',
});
const CHECKOUT_BLOCKED_TITLE_DESCRIPTOR = msg({
	message: 'Checkout unavailable',
	comment: 'Modal title for the generic "checkout blocked" state when no more specific reason is known.',
});
const CHECKOUT_BLOCKED_BODY_DESCRIPTOR = msg({
	message: 'Checkout is blocked for this account. Contact {supportEmail} if you need help.',
	comment: 'Modal body for the generic "checkout blocked" state. Provides the support email.',
});
const CHECKOUT_START_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't start checkout",
	comment: 'Title of the generic fallback error modal shown when creating a checkout session fails unexpectedly.',
});
const CHECKOUT_START_FAILED_BODY_DESCRIPTOR = msg({
	message: 'Something went wrong while starting checkout. Please try again in a moment.',
	comment: 'Body of the generic fallback error modal shown when creating a checkout session fails unexpectedly.',
});
const GIFT_SUBSCRIPTION_BLOCKS_RECURRING_TOAST_DESCRIPTOR = msg({
	message:
		"You're currently on a gift subscription. It won't renew. You can redeem more gift codes to extend it. Recurring subscriptions can be started after your gift time ends.",
	comment:
		'Error modal body shown when a gift-subscription user tries to start a recurring subscription. Explains the extension/redemption path.',
});
const PRICING_NOT_LOADED_TOAST_DESCRIPTOR = msg({
	message: 'Pricing is still loading.',
	comment: 'Error modal body shown when the user clicks a plan before price IDs have loaded.',
});
const PLAN_UNAVAILABLE_TOAST_DESCRIPTOR = msg({
	message: "This plan isn't available. Contact support.",
	comment: 'Error modal body shown when the selected Plutonium plan has no price ID configured.',
});
const VERIFY_CARD_MODAL_TITLE_DESCRIPTOR = msg({
	message: 'Verify card',
	comment: 'Modal title for the mobile checkout confirmation when opening the localized card verification flow.',
});
const COMPLETE_PAYMENT_MODAL_TITLE_DESCRIPTOR = msg({
	message: 'Complete payment',
	comment: 'Modal title for the mobile checkout confirmation when opening the payment provider in a browser.',
});
const VERIFY_CARD_MODAL_BODY_DESCRIPTOR = msg({
	message:
		"{paymentProviderName} will first verify that your card is eligible for localized pricing, then continue you to payment. Return to {productName} once you've completed it.",
	comment: 'Modal body for the mobile card verification confirmation. Explains the two-step flow and the return path.',
});
const COMPLETE_PAYMENT_MODAL_BODY_DESCRIPTOR = msg({
	message:
		"You are now navigating to {paymentProviderName} to complete the payment. Return to {productName} once you've completed it.",
	comment: 'Modal body for the mobile checkout confirmation. Explains that the user is leaving the app to pay.',
});
const LOCALIZED_VERIFICATION_UNAVAILABLE_TOAST_DESCRIPTOR = msg({
	message: 'Localized card verification is not available right now. Try again later.',
	comment: 'Error modal body shown when the localized card pre-approval session cannot be started.',
});
const CHOOSE_PAYMENT_METHOD_MODAL_TITLE_DESCRIPTOR = msg({
	message: 'Choose payment method',
	comment: 'Modal title for the payment method picker (local card vs alternative method).',
});
const USE_LOCAL_CARD_BUTTON_DESCRIPTOR = msg({
	message: 'Use local card',
	comment: 'Modal primary button to start the localized card pre-approval verification flow.',
});
const USE_CARD_BUTTON_DESCRIPTOR = msg({
	message: 'Use card',
	comment: 'Modal secondary button to fall back to standard card checkout instead of an alternative payment method.',
});
const logger = new Logger('useCheckoutActions');

type Plan = 'monthly' | 'yearly' | 'gift_1_month' | 'gift_1_year';
type CheckoutPromptKind = 'payment' | 'localized_card_preapproval';
type PremiumPurchaseBlockedReason = 'lifetime' | 'existing_subscription' | 'purchase_disabled';

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

function requiresLocalizedCardPreapproval(
	_plan: Plan,
	_currency: string | null | undefined,
	_isGift: boolean,
): boolean {
	return false;
}

function alternativePaymentMethodForCurrency(
	currency: string | null | undefined,
	isGift: boolean,
	plan: Plan,
): CheckoutPaymentMethod | null {
	if (isGift || (plan !== 'monthly' && plan !== 'yearly')) {
		return null;
	}
	if (currency === 'BRL') return 'pix';
	if (currency === 'INR') return 'upi';
	return null;
}

export const useCheckoutActions = (
	priceIds: PriceIds | null,
	countryCode: string | null,
	isGiftSubscription: boolean,
	mobileEnabled: boolean,
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
	const getLocalizedCardPrompt = useCallback(
		(currency: string | null | undefined): {description: string; secondaryText: string} => {
			switch (currency) {
				case 'BRL':
					return {
						description: i18n._(LOCAL_CARD_PROMPT_BRL_DESCRIPTION_DESCRIPTOR, {
							pixPaymentMethod: PIX_PAYMENT_METHOD,
							paymentProviderName: PAYMENT_PROVIDER_NAME,
						}),
						secondaryText: i18n._(OTHER_METHODS_BUTTON_DESCRIPTOR),
					};
				case 'INR':
					return {
						description: i18n._(LOCAL_CARD_PROMPT_INR_DESCRIPTION_DESCRIPTOR, {
							upiPaymentMethod: UPI_PAYMENT_METHOD,
							paymentProviderName: PAYMENT_PROVIDER_NAME,
						}),
						secondaryText: i18n._(OTHER_METHODS_BUTTON_DESCRIPTOR),
					};
				case 'PLN':
					return {
						description: i18n._(LOCAL_CARD_PROMPT_PLN_DESCRIPTION_DESCRIPTOR, {
							blikPaymentMethod: BLIK_PAYMENT_METHOD,
							paymentProviderName: PAYMENT_PROVIDER_NAME,
							paymentProviderName2: PAYMENT_PROVIDER_NAME,
						}),
						secondaryText: i18n._(CONTINUE_TO_CHECKOUT_BUTTON_DESCRIPTOR),
					};
				case 'TRY':
					return {
						description: i18n._(LOCAL_CARD_PROMPT_TRY_DESCRIPTION_DESCRIPTOR, {
							paymentProviderName: PAYMENT_PROVIDER_NAME,
						}),
						secondaryText: i18n._(CONTINUE_TO_CHECKOUT_BUTTON_DESCRIPTOR),
					};
				default:
					return {
						description: i18n._(LOCAL_CARD_PROMPT_DEFAULT_DESCRIPTION_DESCRIPTOR, {
							paymentProviderName: PAYMENT_PROVIDER_NAME,
						}),
						secondaryText: i18n._(OTHER_METHODS_BUTTON_DESCRIPTOR),
					};
			}
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
										premiumProductFullName: PREMIUM_PRODUCT_FULL_NAME,
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
						if (reason === 'existing_subscription') {
							ModalCommands.push(
								modal(() => (
									<ConfirmModal
										title={i18n._(EXISTING_SUBSCRIPTION_TITLE_DESCRIPTOR)}
										description={i18n._(EXISTING_SUBSCRIPTION_BODY_DESCRIPTOR, {
											premiumProductFullName: PREMIUM_PRODUCT_FULL_NAME,
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
										description={i18n._(PURCHASES_DISABLED_BODY_DESCRIPTOR, {
											supportEmail: SUPPORT_EMAIL,
										})}
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
									description={i18n._(CHECKOUT_BLOCKED_BODY_DESCRIPTOR, {
										supportEmail: SUPPORT_EMAIL,
									})}
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
			logger.info('Plan selected', {plan, isGiftSubscription});
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
			if (isGiftSubscription && (plan === 'monthly' || plan === 'yearly')) {
				showCheckoutPlanErrorModal(
					i18n._(GIFT_SUBSCRIPTION_BLOCKS_RECURRING_TOAST_DESCRIPTOR),
					'app.plutonium.use-checkout-actions.gift-subscription-blocked.generic-error-modal',
				);
				return;
			}
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
				{
					promptKind = 'payment',
					skipMobilePrompt = false,
				}: {promptKind?: CheckoutPromptKind; skipMobilePrompt?: boolean} = {},
			) => {
				if (mobileEnabled && !skipMobilePrompt) {
					ModalCommands.push(
						modal(() => (
							<ConfirmModal
								title={
									promptKind === 'localized_card_preapproval'
										? i18n._(VERIFY_CARD_MODAL_TITLE_DESCRIPTOR)
										: i18n._(COMPLETE_PAYMENT_MODAL_TITLE_DESCRIPTOR)
								}
								description={
									promptKind === 'localized_card_preapproval'
										? i18n._(VERIFY_CARD_MODAL_BODY_DESCRIPTOR, {
												paymentProviderName: PAYMENT_PROVIDER_NAME,
												productName: PRODUCT_NAME,
											})
										: i18n._(COMPLETE_PAYMENT_MODAL_BODY_DESCRIPTOR, {
												paymentProviderName: PAYMENT_PROVIDER_NAME,
												productName: PRODUCT_NAME,
											})
								}
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
					await openCheckoutUrl(checkoutUrl, {promptKind: 'payment', skipMobilePrompt});
				} catch (error) {
					handleCheckoutError(error);
				} finally {
					setLoadingCheckout(false);
				}
			};
			const startLocalizedCardPreapproval = async ({skipMobilePrompt = false}: {skipMobilePrompt?: boolean} = {}) => {
				if (!countryCode) {
					showCheckoutPlanErrorModal(
						i18n._(LOCALIZED_VERIFICATION_UNAVAILABLE_TOAST_DESCRIPTOR),
						'app.plutonium.use-checkout-actions.localized-verification-unavailable.generic-error-modal',
					);
					return;
				}
				setLoadingCheckout(true);
				try {
					const checkoutUrl = await PremiumCommands.createLocalizedCardPreapprovalSession(priceId, countryCode);
					await openCheckoutUrl(checkoutUrl, {
						promptKind: 'localized_card_preapproval',
						skipMobilePrompt,
					});
				} catch (error) {
					handleCheckoutError(error);
				} finally {
					setLoadingCheckout(false);
				}
			};
			if (requiresLocalizedCardPreapproval(plan, priceIds.currency, isGift)) {
				const localizedCardPrompt = getLocalizedCardPrompt(priceIds.currency);
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(CHOOSE_PAYMENT_METHOD_MODAL_TITLE_DESCRIPTOR)}
							description={localizedCardPrompt.description}
							primaryText={i18n._(USE_LOCAL_CARD_BUTTON_DESCRIPTOR)}
							primaryVariant="primary"
							secondaryText={localizedCardPrompt.secondaryText}
							onPrimary={() => startLocalizedCardPreapproval({skipMobilePrompt: true})}
							onSecondary={() => {
								void startCheckout({skipMobilePrompt: true});
							}}
							data-flx="app.plutonium.use-checkout-actions.handle-select-plan.confirm-modal"
						/>
					)),
				);
				return;
			}
			const altPaymentMethod = alternativePaymentMethodForCurrency(priceIds.currency, isGift, plan);
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
			await startCheckout();
		},
		[
			handleCheckoutError,
			loadingCheckout,
			priceIds,
			countryCode,
			getLocalizedCardPrompt,
			getAlternativePaymentMethodPrompt,
			isGiftSubscription,
			mobileEnabled,
			i18n,
		],
	);
	return {
		loadingCheckout,
		handleSelectPlan,
	};
};
