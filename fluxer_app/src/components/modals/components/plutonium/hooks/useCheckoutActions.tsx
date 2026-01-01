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

import {useLingui} from '@lingui/react/macro';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import type {PriceIds} from '~/actions/PremiumActionCreators';
import * as PremiumActionCreators from '~/actions/PremiumActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {APIErrorCodes} from '~/Constants';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {HttpError} from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import {openExternalUrl} from '~/utils/NativeUtils';

const logger = new Logger('useCheckoutActions');

type Plan = 'monthly' | 'yearly' | 'visionary' | 'gift1Month' | 'gift1Year' | 'giftVisionary';

export const useCheckoutActions = (priceIds: PriceIds | null, isGiftSubscription: boolean, mobileEnabled: boolean) => {
	const {t} = useLingui();
	const [loadingCheckout, setLoadingCheckout] = React.useState(false);

	const handleCheckoutError = React.useCallback(
		(error: unknown) => {
			logger.error('Failed to create checkout session', error);

			if (error instanceof HttpError) {
				const errorCode = (error.body as any)?.code as string | undefined;
				if (errorCode === APIErrorCodes.PREMIUM_PURCHASE_BLOCKED) {
					ToastActionCreators.error(
						t`You're already Lifetime. Starting a new subscription isn't allowed. You can still buy gifts for others.`,
					);
					return;
				}
			}

			ToastActionCreators.error(t`Failed to start checkout. Please try again.`);
		},
		[t],
	);

	const handleSelectPlan = React.useCallback(
		async (plan: Plan) => {
			if (loadingCheckout) return;

			logger.info('Plan selected', {plan, isGiftSubscription});

			if (isGiftSubscription && (plan === 'monthly' || plan === 'yearly')) {
				ToastActionCreators.error(
					t`You're currently on a gift subscription. It won't renew. You can redeem more gift codes to extend it, or upgrade to Visionary now. Recurring subscriptions can be started after your gift time ends.`,
				);
				return;
			}

			if (!priceIds) {
				logger.error('Price IDs not loaded yet');
				ToastActionCreators.error(t`Please wait for pricing information to load.`);
				return;
			}

			const planConfig: Record<Plan, {id: string | null; gift?: boolean}> = {
				monthly: {id: priceIds.monthly},
				yearly: {id: priceIds.yearly},
				visionary: {id: priceIds.visionary},
				gift1Month: {id: priceIds.gift1Month, gift: true},
				gift1Year: {id: priceIds.gift1Year, gift: true},
				giftVisionary: {id: priceIds.giftVisionary, gift: true},
			};

			const selected = planConfig[plan];
			const priceId = selected.id;
			const isGift = selected.gift ?? false;

			if (!priceId) {
				logger.error('Price ID not available for plan', {plan});
				ToastActionCreators.error(t`This plan is not available at the moment. Please contact support.`);
				return;
			}

			setLoadingCheckout(true);
			try {
				const checkoutUrl = await PremiumActionCreators.createCheckoutSession(priceId, isGift);

				if (mobileEnabled) {
					ModalActionCreators.push(
						modal(() => (
							<ConfirmModal
								title={t`Complete Payment`}
								description={t`You are now navigating to Stripe to complete the payment. Return to Fluxer once you've completed it!`}
								primaryText={t`OK`}
								primaryVariant="primary"
								secondaryText={t`Cancel`}
								onPrimary={() => {
									void openExternalUrl(checkoutUrl);
								}}
							/>
						)),
					);
				} else {
					await openExternalUrl(checkoutUrl);
				}
			} catch (error) {
				handleCheckoutError(error);
			} finally {
				setLoadingCheckout(false);
			}
		},
		[handleCheckoutError, loadingCheckout, priceIds, isGiftSubscription, mobileEnabled, t],
	);

	return {
		loadingCheckout,
		handleSelectPlan,
	};
};
