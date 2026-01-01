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

import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowDownIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {VisionarySlots} from '~/actions/PremiumActionCreators';
import {PricingCard} from '../PricingCard';
import gridStyles from '../PricingGrid.module.css';
import {PurchaseDisclaimer} from '../PurchaseDisclaimer';
import {ToggleButton} from '../ToggleButton';
import styles from './PricingSection.module.css';

interface PricingSectionProps {
	isGiftMode: boolean;
	setIsGiftMode: (value: boolean) => void;
	monthlyPrice: string;
	yearlyPrice: string;
	visionaryPrice: string;
	visionarySlots: VisionarySlots | null;
	loadingCheckout: boolean;
	loadingSlots: boolean;
	isVisionarySoldOut: boolean;
	handleSelectPlan: (plan: 'monthly' | 'yearly' | 'visionary' | 'gift1Month' | 'gift1Year' | 'giftVisionary') => void;
}

export const PricingSection: React.FC<PricingSectionProps> = observer(
	({
		isGiftMode,
		setIsGiftMode,
		monthlyPrice,
		yearlyPrice,
		visionaryPrice,
		visionarySlots,
		loadingCheckout,
		loadingSlots,
		isVisionarySoldOut,
		handleSelectPlan,
	}) => {
		const {t} = useLingui();

		return (
			<section className={styles.section}>
				<div className={styles.toggleContainer} role="tablist" aria-label={t`Purchase mode`}>
					<ToggleButton active={!isGiftMode} onClick={() => setIsGiftMode(false)} label={t`For Me`} />
					<ToggleButton active={isGiftMode} onClick={() => setIsGiftMode(true)} label={t`As a Gift`} />
				</div>

				<div className={gridStyles.gridWrapper}>
					<div className={gridStyles.gridThreeColumns}>
						{!isGiftMode ? (
							<>
								<PricingCard
									title={t`Monthly`}
									price={monthlyPrice}
									period={t`per month`}
									onSelect={() => handleSelectPlan('monthly')}
									isLoading={loadingCheckout || loadingSlots}
								/>
								<PricingCard
									title={t`Yearly`}
									price={yearlyPrice}
									period={t`per year`}
									badge={t`Save 17%`}
									isPopular
									onSelect={() => handleSelectPlan('yearly')}
									buttonText={t`Upgrade Now`}
									isLoading={loadingCheckout || loadingSlots}
								/>
								<PricingCard
									title={t`Visionary`}
									price={visionaryPrice}
									period={t`one-time, lifetime`}
									remainingSlots={loadingSlots ? undefined : visionarySlots?.remaining}
									onSelect={() => handleSelectPlan('visionary')}
									isLoading={loadingCheckout || loadingSlots}
									disabled={isVisionarySoldOut}
									soldOut={isVisionarySoldOut}
								/>
							</>
						) : (
							<>
								<PricingCard
									title={t`1 Year Gift`}
									price={yearlyPrice}
									period={t`one-time purchase`}
									badge={t`Save 17%`}
									onSelect={() => handleSelectPlan('gift1Year')}
									buttonText={t`Buy Gift`}
									isLoading={loadingCheckout || loadingSlots}
								/>
								<PricingCard
									title={t`1 Month Gift`}
									price={monthlyPrice}
									period={t`one-time purchase`}
									isPopular
									onSelect={() => handleSelectPlan('gift1Month')}
									buttonText={t`Buy Gift`}
									isLoading={loadingCheckout || loadingSlots}
								/>
								<PricingCard
									title={t`Visionary Gift`}
									price={visionaryPrice}
									period={t`one-time, lifetime`}
									remainingSlots={loadingSlots ? undefined : visionarySlots?.remaining}
									onSelect={() => handleSelectPlan('giftVisionary')}
									buttonText={t`Buy Gift`}
									isLoading={loadingCheckout || loadingSlots}
									disabled={isVisionarySoldOut}
									soldOut={isVisionarySoldOut}
								/>
							</>
						)}
					</div>
				</div>

				<div className={styles.footerContainer}>
					<PurchaseDisclaimer />
					<div className={styles.scrollPromptContainer}>
						<p className={styles.scrollPromptText}>
							<Trans>Scroll down to view all the sweet perks you get with Plutonium</Trans>
						</p>
						<ArrowDownIcon className={styles.scrollPromptIcon} weight="bold" />
					</div>
				</div>
			</section>
		);
	},
);
