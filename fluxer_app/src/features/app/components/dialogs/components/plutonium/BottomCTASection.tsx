// SPDX-License-Identifier: AGPL-3.0-or-later

import {PurchaseDisclaimer} from '@app/features/app/components/dialogs/components/PurchaseDisclaimer';
import styles from '@app/features/app/components/dialogs/components/plutonium/BottomCTASection.module.css';
import {PurchaseDisabledWrapper} from '@app/features/app/components/dialogs/components/plutonium/PurchaseDisabledWrapper';
import {PREMIUM_PRODUCT_FULL_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const CLAIM_YOUR_ACCOUNT_TO_PURCHASE_DESCRIPTOR = msg({
	message: 'Claim your account to purchase {premiumProductFullName}.',
	comment:
		'Plutonium upsell CTA tooltip shown to guests/unclaimed accounts: claiming the account is required before purchase. Product name is interpolated.',
});

interface BottomCTASectionProps {
	isGiftMode: boolean;
	monthlyPrice: string;
	yearlyPrice: string;
	giftMonthlyPrice: string;
	giftYearlyPrice: string;
	loadingCheckout: boolean;
	handleSelectPlan: (plan: 'monthly' | 'yearly' | 'gift_1_month' | 'gift_1_year') => void;
	purchaseDisabled?: boolean;
	purchaseDisabledTooltip?: React.ReactNode;
}

export const BottomCTASection: React.FC<BottomCTASectionProps> = observer(
	({
		isGiftMode,
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
			i18n._(CLAIM_YOUR_ACCOUNT_TO_PURCHASE_DESCRIPTOR, {premiumProductFullName: PREMIUM_PRODUCT_FULL_NAME});
		return (
			<div className={styles.container} data-flx="app.plutonium.bottom-cta-section.container">
				<h2 className={styles.title} data-flx="app.plutonium.bottom-cta-section.title">
					{isGiftMode ? <Trans>Ready to buy a gift?</Trans> : <Trans>Ready to upgrade?</Trans>}
				</h2>
				<div className={styles.buttonContainer} data-flx="app.plutonium.bottom-cta-section.button-container">
					{!isGiftMode ? (
						<>
							<PurchaseDisabledWrapper
								disabled={purchaseDisabled}
								tooltipText={tooltipText}
								data-flx="app.plutonium.bottom-cta-section.purchase-disabled-wrapper"
							>
								<Button
									variant="secondary"
									onClick={() => handleSelectPlan('monthly')}
									submitting={loadingCheckout}
									className={styles.button}
									disabled={purchaseDisabled}
									data-flx="app.plutonium.bottom-cta-section.button.select-plan"
								>
									<Trans>Monthly {monthlyPrice}</Trans>
								</Button>
							</PurchaseDisabledWrapper>
							<PurchaseDisabledWrapper
								disabled={purchaseDisabled}
								tooltipText={tooltipText}
								data-flx="app.plutonium.bottom-cta-section.purchase-disabled-wrapper--2"
							>
								<Button
									variant="primary"
									onClick={() => handleSelectPlan('yearly')}
									submitting={loadingCheckout}
									className={styles.button}
									disabled={purchaseDisabled}
									data-flx="app.plutonium.bottom-cta-section.button.select-plan--2"
								>
									<Trans>Yearly {yearlyPrice}</Trans>
								</Button>
							</PurchaseDisabledWrapper>
						</>
					) : (
						<>
							<PurchaseDisabledWrapper
								disabled={purchaseDisabled}
								tooltipText={tooltipText}
								data-flx="app.plutonium.bottom-cta-section.purchase-disabled-wrapper--3"
							>
								<Button
									variant="secondary"
									onClick={() => handleSelectPlan('gift_1_year')}
									submitting={loadingCheckout}
									className={styles.button}
									disabled={purchaseDisabled}
									data-flx="app.plutonium.bottom-cta-section.button.select-plan--3"
								>
									<Trans>1 year {giftYearlyPrice}</Trans>
								</Button>
							</PurchaseDisabledWrapper>
							<PurchaseDisabledWrapper
								disabled={purchaseDisabled}
								tooltipText={tooltipText}
								data-flx="app.plutonium.bottom-cta-section.purchase-disabled-wrapper--4"
							>
								<Button
									variant="primary"
									onClick={() => handleSelectPlan('gift_1_month')}
									submitting={loadingCheckout}
									className={styles.button}
									disabled={purchaseDisabled}
									data-flx="app.plutonium.bottom-cta-section.button.select-plan--4"
								>
									<Trans>1 month {giftMonthlyPrice}</Trans>
								</Button>
							</PurchaseDisabledWrapper>
						</>
					)}
				</div>
				<PurchaseDisclaimer data-flx="app.plutonium.bottom-cta-section.purchase-disclaimer" />
			</div>
		);
	},
);
