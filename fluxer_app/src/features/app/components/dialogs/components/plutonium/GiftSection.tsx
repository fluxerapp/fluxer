// SPDX-License-Identifier: AGPL-3.0-or-later

import {PricingCard} from '@app/features/app/components/dialogs/components/PricingCard';
import gridStyles from '@app/features/app/components/dialogs/components/PricingGrid.module.css';
import {PurchaseDisclaimer} from '@app/features/app/components/dialogs/components/PurchaseDisclaimer';
import styles from '@app/features/app/components/dialogs/components/plutonium/GiftSection.module.css';
import {SectionHeader} from '@app/features/app/components/dialogs/components/plutonium/PlutoniumSectionHeader';
import {PurchaseDisabledWrapper} from '@app/features/app/components/dialogs/components/plutonium/PurchaseDisabledWrapper';
import {PREMIUM_PRODUCT_FULL_NAME, PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {
	BUY_GIFT_DESCRIPTOR,
	CLAIM_ACCOUNT_TO_PURCHASE_PREMIUM_DESCRIPTOR,
	GIFT_PREMIUM_DESCRIPTOR,
	ONE_TIME_PURCHASE_DESCRIPTOR,
	SHARE_PREMIUM_EXPERIENCE_DESCRIPTOR,
	VIEW_PREMIUM_PERKS_DESCRIPTOR,
} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowDownIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const MESSAGE_1_YEAR_GIFT_DESCRIPTOR = msg({
	message: '1 year gift',
	comment: 'Short label in the Plutonium gift section. Keep the tone plain and specific.',
});
const SAVE_17_DESCRIPTOR = msg({
	message: 'Save 17%',
	comment: 'Short label in the Plutonium gift section. Keep the tone plain and specific.',
});
const MESSAGE_1_MONTH_GIFT_DESCRIPTOR = msg({
	message: '1 month gift',
	comment: 'Short label in the Plutonium gift section. Keep the tone plain and specific.',
});

interface GiftSectionProps {
	giftSectionRef: React.RefObject<HTMLDivElement | null>;
	giftMonthlyPrice: string;
	giftYearlyPrice: string;
	loadingCheckout: boolean;
	handleSelectPlan: (plan: 'gift_1_month' | 'gift_1_year') => void;
	purchaseDisabled?: boolean;
	purchaseDisabledTooltip?: React.ReactNode;
}

export const GiftSection: React.FC<GiftSectionProps> = observer(
	({
		giftSectionRef,
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
			<div ref={giftSectionRef} data-flx="app.plutonium.gift-section.div">
				<section className={styles.section} data-flx="app.plutonium.gift-section.section">
					<SectionHeader
						title={i18n._(GIFT_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
						description={i18n._(SHARE_PREMIUM_EXPERIENCE_DESCRIPTOR, {
							premiumProductName: PREMIUM_PRODUCT_NAME,
						})}
						data-flx="app.plutonium.gift-section.section-header"
					/>
					<div className={gridStyles.gridWrapper} data-flx="app.plutonium.gift-section.div--2">
						<div className={gridStyles.gridTwoColumns} data-flx="app.plutonium.gift-section.div--3">
							<PurchaseDisabledWrapper
								disabled={purchaseDisabled}
								tooltipText={tooltipText}
								data-flx="app.plutonium.gift-section.purchase-disabled-wrapper"
							>
								<PricingCard
									title={i18n._(MESSAGE_1_YEAR_GIFT_DESCRIPTOR)}
									price={giftYearlyPrice}
									period={i18n._(ONE_TIME_PURCHASE_DESCRIPTOR)}
									badge={i18n._(SAVE_17_DESCRIPTOR)}
									onSelect={() => handleSelectPlan('gift_1_year')}
									buttonText={i18n._(BUY_GIFT_DESCRIPTOR)}
									isLoading={loadingCheckout}
									disabled={purchaseDisabled}
									data-flx="app.plutonium.gift-section.pricing-card.select-plan"
								/>
							</PurchaseDisabledWrapper>
							<PurchaseDisabledWrapper
								disabled={purchaseDisabled}
								tooltipText={tooltipText}
								data-flx="app.plutonium.gift-section.purchase-disabled-wrapper--2"
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
									data-flx="app.plutonium.gift-section.pricing-card.select-plan--2"
								/>
							</PurchaseDisabledWrapper>
						</div>
					</div>
					<div className={styles.footerContainer} data-flx="app.plutonium.gift-section.footer-container">
						<PurchaseDisclaimer data-flx="app.plutonium.gift-section.purchase-disclaimer" />
						<div className={styles.scrollPromptContainer} data-flx="app.plutonium.gift-section.scroll-prompt-container">
							<p className={styles.scrollPromptText} data-flx="app.plutonium.gift-section.scroll-prompt-text">
								{i18n._(VIEW_PREMIUM_PERKS_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
							</p>
							<ArrowDownIcon
								className={styles.scrollPromptIcon}
								weight="bold"
								data-flx="app.plutonium.gift-section.scroll-prompt-icon"
							/>
						</div>
					</div>
				</section>
			</div>
		);
	},
);
