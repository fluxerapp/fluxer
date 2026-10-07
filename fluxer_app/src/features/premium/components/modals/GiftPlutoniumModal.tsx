// SPDX-License-Identifier: AGPL-3.0-or-later

import {PurchaseDisclaimer} from '@app/features/app/components/dialogs/components/PurchaseDisclaimer';
import {useCheckoutActions} from '@app/features/app/components/dialogs/components/plutonium/hooks/useCheckoutActions';
import {usePremiumData} from '@app/features/app/components/dialogs/components/plutonium/hooks/usePremiumData';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import GeoIP from '@app/features/app/state/GeoIP';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import styles from '@app/features/premium/components/modals/GiftPlutoniumModal.module.css';
import PremiumState from '@app/features/premium/state/PremiumState';
import {BUY_GIFT_DESCRIPTOR, GIFT_PREMIUM_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {areGiftPurchasesAvailable, getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Users from '@app/features/user/state/Users';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo, useState} from 'react';

type GiftPlan = 'gift_1_month' | 'gift_1_year';

const GIFT_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Pay once and get a gift link to share. Gifts never renew.',
	comment: 'Description at the top of the gift purchase dialog.',
});
const ONE_MONTH_DESCRIPTOR = msg({
	message: '1 month',
	comment: 'Gift purchase dialog option for a one-month gift.',
});
const ONE_YEAR_DESCRIPTOR = msg({
	message: '1 year',
	comment: 'Gift purchase dialog option for a one-year gift.',
});
const SAVE_PERCENT_DESCRIPTOR = msg({
	message: 'Save {percent}%',
	comment: 'Badge on the one-year option in the gift purchase dialog. percent is a whole number such as 17.',
});
const GIFT_PLAN_LABEL_DESCRIPTOR = msg({
	message: 'Gift length',
	comment: 'Accessible label of the gift length options in the gift purchase dialog.',
});
const GIFTS_UNAVAILABLE_DESCRIPTOR = msg({
	message: "Gifts aren't available right now.",
	comment: 'Message in the gift purchase dialog when gifts cannot be bought.',
});
const CLAIM_TO_GIFT_DESCRIPTOR = msg({
	message: 'Claim your account to buy gifts.',
	comment: 'Note in the gift purchase dialog for unclaimed accounts.',
});
const VERIFY_TO_GIFT_DESCRIPTOR = msg({
	message: 'Verify your email to buy gifts.',
	comment: 'Note in the gift purchase dialog for accounts without a verified email.',
});

const PRICE_PLACEHOLDER = '...';

function savingsPercent(monthlyMinor: number | null | undefined, yearlyMinor: number | null | undefined) {
	if (monthlyMinor == null || yearlyMinor == null || monthlyMinor <= 0) return null;
	const percent = Math.round((1 - yearlyMinor / (monthlyMinor * 12)) * 100);
	return percent > 0 ? percent : null;
}

export const GiftPlutoniumModal = observer(() => {
	const {i18n} = useLingui();
	const currentUser = Users.currentUser;
	const premiumState = PremiumState.loadedForUserId === currentUser?.id ? PremiumState.state : null;
	const countryCode = GeoIP.countryCode;
	const {priceIds, giftMonthlyPrice, giftYearlyPrice} = usePremiumData({premiumState});
	const {loadingCheckout, handleSelectPlan} = useCheckoutActions(priceIds, countryCode, false, MobileLayout.enabled);
	const [plan, setPlan] = useState<GiftPlan>('gift_1_year');
	useEffect(() => {
		if (!currentUser?.id) return;
		void PremiumCommands.refreshPremiumState(countryCode ?? undefined);
	}, [countryCode, currentUser?.id]);
	const isClaimed = currentUser?.isClaimed() ?? false;
	const isEmailVerified = currentUser?.verified === true;
	const userPurchaseDisabled = premiumState?.effective.premium_purchase_disabled === true;
	const giftsAvailable = areGiftPurchasesAvailable(priceIds, userPurchaseDisabled);
	const blockedNote = !isClaimed
		? i18n._(CLAIM_TO_GIFT_DESCRIPTOR)
		: !isEmailVerified
			? i18n._(VERIFY_TO_GIFT_DESCRIPTOR)
			: null;
	const pricesLoaded = giftMonthlyPrice !== PRICE_PLACEHOLDER && giftYearlyPrice !== PRICE_PLACEHOLDER;
	const percent = savingsPercent(priceIds?.gift_1_month_amount_minor, priceIds?.gift_1_year_amount_minor);
	const options = useMemo<ReadonlyArray<RadioOption<GiftPlan>>>(
		() => [
			{value: 'gift_1_year', name: i18n._(ONE_YEAR_DESCRIPTOR), desc: giftYearlyPrice},
			{value: 'gift_1_month', name: i18n._(ONE_MONTH_DESCRIPTOR), desc: giftMonthlyPrice},
		],
		[giftMonthlyPrice, giftYearlyPrice, i18n],
	);
	const title = i18n._(GIFT_PREMIUM_DESCRIPTOR, {premiumProductName: getPremiumProductName()});
	return (
		<Modal.Root size="small" centered data-flx="premium.gift-plutonium-modal.modal-root">
			<Modal.Header title={title} data-flx="premium.gift-plutonium-modal.modal-header" />
			<Modal.Content data-flx="premium.gift-plutonium-modal.modal-content">
				<Modal.ContentLayout data-flx="premium.gift-plutonium-modal.content-layout">
					{giftsAvailable ? (
						<>
							<Modal.Description data-flx="premium.gift-plutonium-modal.description">
								{i18n._(GIFT_DESCRIPTION_DESCRIPTOR)}
							</Modal.Description>
							<RadioGroup
								options={options}
								value={plan}
								onChange={setPlan}
								disabled={loadingCheckout}
								aria-label={i18n._(GIFT_PLAN_LABEL_DESCRIPTOR)}
								renderContent={(option) => (
									<div className={styles.option} data-flx="premium.gift-plutonium-modal.option">
										<span className={styles.optionName}>
											<span>{option.name}</span>
											{option.value === 'gift_1_year' && percent != null && (
												<span className={styles.badge}>{i18n._(SAVE_PERCENT_DESCRIPTOR, {percent})}</span>
											)}
										</span>
										<span className={styles.optionPrice}>{option.desc}</span>
									</div>
								)}
							/>
							{blockedNote && (
								<p className={styles.note} role="note" data-flx="premium.gift-plutonium-modal.blocked-note">
									{blockedNote}
								</p>
							)}
							<PurchaseDisclaimer align="left" data-flx="premium.gift-plutonium-modal.disclaimer" />
						</>
					) : (
						<Modal.Description data-flx="premium.gift-plutonium-modal.unavailable">
							{i18n._(GIFTS_UNAVAILABLE_DESCRIPTOR)}
						</Modal.Description>
					)}
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="premium.gift-plutonium-modal.modal-footer">
				<Button onClick={ModalCommands.pop} variant="secondary" data-flx="premium.gift-plutonium-modal.button.cancel">
					{i18n._(CANCEL_DESCRIPTOR)}
				</Button>
				{giftsAvailable && (
					<Button
						onClick={() => void handleSelectPlan(plan)}
						submitting={loadingCheckout}
						disabled={blockedNote != null || !pricesLoaded}
						variant="primary"
						data-flx="premium.gift-plutonium-modal.button.buy"
					>
						{i18n._(BUY_GIFT_DESCRIPTOR)}
					</Button>
				)}
			</Modal.Footer>
		</Modal.Root>
	);
});
