// SPDX-License-Identifier: AGPL-3.0-or-later

import {PricingCard} from '@app/features/app/components/dialogs/components/PricingCard';
import gridStyles from '@app/features/app/components/dialogs/components/PricingGrid.module.css';
import {PurchaseDisclaimer} from '@app/features/app/components/dialogs/components/PurchaseDisclaimer';
import styles from '@app/features/app/components/dialogs/components/plutonium/PricingSection.module.css';
import {PurchaseDisabledWrapper} from '@app/features/app/components/dialogs/components/plutonium/PurchaseDisabledWrapper';
import {ToggleButton} from '@app/features/app/components/dialogs/components/ToggleButton';
import {PREMIUM_PRODUCT_FULL_NAME, PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {
	BUY_GIFT_DESCRIPTOR,
	CLAIM_ACCOUNT_TO_PURCHASE_PREMIUM_DESCRIPTOR,
	ONE_TIME_PURCHASE_DESCRIPTOR,
	VIEW_PREMIUM_PERKS_DESCRIPTOR,
} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowDownIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const PURCHASE_MODE_DESCRIPTOR = msg({
	message: 'Purchase mode',
	comment: 'Accessible label for switching between self purchase and gift purchase.',
});
const FOR_ME_DESCRIPTOR = msg({
	message: 'For me',
	comment: 'Purchase mode option for buying a subscription for the current user.',
});
const AS_A_GIFT_DESCRIPTOR = msg({
	message: 'As a gift',
	comment: 'Purchase mode option for buying a gift subscription for another user.',
});
const MONTHLY_DESCRIPTOR = msg({
	message: 'Monthly',
	comment: 'Billing plan title for a monthly recurring subscription.',
});
const PER_MONTH_DESCRIPTOR = msg({
	message: 'per month',
	comment: 'Billing cadence label for a monthly subscription price.',
});
const YEARLY_DESCRIPTOR = msg({
	message: 'Yearly',
	comment: 'Billing plan title for a yearly recurring subscription.',
});
const PER_YEAR_DESCRIPTOR = msg({
	message: 'per year',
	comment: 'Billing cadence label for a yearly subscription price.',
});
const SAVE_17_DESCRIPTOR = msg({
	message: 'Save 17%',
	comment: 'Price badge comparing yearly billing to monthly billing.',
});
const UPGRADE_NOW_DESCRIPTOR = msg({
	message: 'Upgrade now',
	comment: 'Checkout button for starting a paid subscription.',
});
const MESSAGE_1_YEAR_GIFT_DESCRIPTOR = msg({
	message: '1 year gift',
	comment: 'Billing plan title for a one-year gift subscription.',
});
const SAVE_17_2_DESCRIPTOR = msg({
	message: 'Save 17%',
	comment: 'Price badge comparing a one-year gift to monthly gifts.',
});
const MESSAGE_1_MONTH_GIFT_DESCRIPTOR = msg({
	message: '1 month gift',
	comment: 'Billing plan title for a one-month gift subscription.',
});

interface PricingSectionProps {
	isGiftMode: boolean;
	setIsGiftMode: (value: boolean) => void;
	monthlyPrice: string;
	yearlyPrice: string;
	giftMonthlyPrice: string;
	giftYearlyPrice: string;
	loadingCheckout: boolean;
	handleSelectPlan: (plan: 'monthly' | 'yearly' | 'gift_1_month' | 'gift_1_year') => void;
	purchaseDisabled?: boolean;
	purchaseDisabledTooltip?: React.ReactNode;
}

export const PricingSection: React.FC<PricingSectionProps> = observer(
	({
		isGiftMode,
		setIsGiftMode,
		monthlyPrice,
		yearlyPrice,
		giftMonthlyPrice,
		giftYearlyPrice,
		loadingCheckout,
		handleSelectPlan,
		purchaseDisabled = false,
		purchaseDisabledTooltip,
	}) => {
		const {i18n} = useLingui();
		const tooltipText: React.ReactNode =
			purchaseDisabledTooltip ??
			i18n._(CLAIM_ACCOUNT_TO_PURCHASE_PREMIUM_DESCRIPTOR, {premiumProductFullName: PREMIUM_PRODUCT_FULL_NAME});
		return (
			<section className={styles.section} data-flx="app.plutonium.pricing-section.section">
				<div
					className={styles.toggleContainer}
					role="group"
					aria-label={i18n._(PURCHASE_MODE_DESCRIPTOR)}
					data-flx="app.plutonium.pricing-section.toggle-container"
				>
					<ToggleButton
						active={!isGiftMode}
						onClick={() => setIsGiftMode(false)}
						label={i18n._(FOR_ME_DESCRIPTOR)}
						data-flx="app.plutonium.pricing-section.toggle-button.set-is-gift-mode"
					/>
					<ToggleButton
						active={isGiftMode}
						onClick={() => setIsGiftMode(true)}
						label={i18n._(AS_A_GIFT_DESCRIPTOR)}
						data-flx="app.plutonium.pricing-section.toggle-button.set-is-gift-mode--2"
					/>
				</div>
				<div className={gridStyles.gridWrapper} data-flx="app.plutonium.pricing-section.div">
					<div className={gridStyles.gridTwoColumns} data-flx="app.plutonium.pricing-section.div--2">
						{!isGiftMode ? (
							<>
								<PurchaseDisabledWrapper
									disabled={purchaseDisabled}
									tooltipText={tooltipText}
									data-flx="app.plutonium.pricing-section.purchase-disabled-wrapper"
								>
									<PricingCard
										title={i18n._(MONTHLY_DESCRIPTOR)}
										price={monthlyPrice}
										period={i18n._(PER_MONTH_DESCRIPTOR)}
										onSelect={() => handleSelectPlan('monthly')}
										isLoading={loadingCheckout}
										disabled={purchaseDisabled}
										data-flx="app.plutonium.pricing-section.pricing-card.select-plan"
									/>
								</PurchaseDisabledWrapper>
								<PurchaseDisabledWrapper
									disabled={purchaseDisabled}
									tooltipText={tooltipText}
									data-flx="app.plutonium.pricing-section.purchase-disabled-wrapper--2"
								>
									<PricingCard
										title={i18n._(YEARLY_DESCRIPTOR)}
										price={yearlyPrice}
										period={i18n._(PER_YEAR_DESCRIPTOR)}
										badge={i18n._(SAVE_17_DESCRIPTOR)}
										isPopular
										onSelect={() => handleSelectPlan('yearly')}
										buttonText={i18n._(UPGRADE_NOW_DESCRIPTOR)}
										isLoading={loadingCheckout}
										disabled={purchaseDisabled}
										data-flx="app.plutonium.pricing-section.pricing-card.select-plan--2"
									/>
								</PurchaseDisabledWrapper>
							</>
						) : (
							<>
								<PurchaseDisabledWrapper
									disabled={purchaseDisabled}
									tooltipText={tooltipText}
									data-flx="app.plutonium.pricing-section.purchase-disabled-wrapper--3"
								>
									<PricingCard
										title={i18n._(MESSAGE_1_YEAR_GIFT_DESCRIPTOR)}
										price={giftYearlyPrice}
										period={i18n._(ONE_TIME_PURCHASE_DESCRIPTOR)}
										badge={i18n._(SAVE_17_2_DESCRIPTOR)}
										onSelect={() => handleSelectPlan('gift_1_year')}
										buttonText={i18n._(BUY_GIFT_DESCRIPTOR)}
										isLoading={loadingCheckout}
										disabled={purchaseDisabled}
										data-flx="app.plutonium.pricing-section.pricing-card.select-plan--3"
									/>
								</PurchaseDisabledWrapper>
								<PurchaseDisabledWrapper
									disabled={purchaseDisabled}
									tooltipText={tooltipText}
									data-flx="app.plutonium.pricing-section.purchase-disabled-wrapper--4"
								>
									<PricingCard
										title={i18n._(MESSAGE_1_MONTH_GIFT_DESCRIPTOR)}
										price={giftMonthlyPrice}
										period={i18n._(ONE_TIME_PURCHASE_DESCRIPTOR)}
										isPopular
										onSelect={() => handleSelectPlan('gift_1_month')}
										buttonText={i18n._(BUY_GIFT_DESCRIPTOR)}
										isLoading={loadingCheckout}
										disabled={purchaseDisabled}
										data-flx="app.plutonium.pricing-section.pricing-card.select-plan--4"
									/>
								</PurchaseDisabledWrapper>
							</>
						)}
					</div>
				</div>
				<div className={styles.footerContainer} data-flx="app.plutonium.pricing-section.footer-container">
					<PurchaseDisclaimer data-flx="app.plutonium.pricing-section.purchase-disclaimer" />
					<div
						className={styles.scrollPromptContainer}
						data-flx="app.plutonium.pricing-section.scroll-prompt-container"
					>
						<p className={styles.scrollPromptText} data-flx="app.plutonium.pricing-section.scroll-prompt-text">
							{i18n._(VIEW_PREMIUM_PERKS_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
						</p>
						<ArrowDownIcon
							className={styles.scrollPromptIcon}
							weight="bold"
							data-flx="app.plutonium.pricing-section.scroll-prompt-icon"
						/>
					</div>
				</div>
			</section>
		);
	},
);
