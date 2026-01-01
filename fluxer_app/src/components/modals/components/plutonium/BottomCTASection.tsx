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

import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {Button} from '~/components/uikit/Button/Button';
import {PurchaseDisclaimer} from '../PurchaseDisclaimer';
import styles from './BottomCTASection.module.css';

interface BottomCTASectionProps {
	isGiftMode: boolean;
	monthlyPrice: string;
	yearlyPrice: string;
	visionaryPrice: string;
	loadingCheckout: boolean;
	loadingSlots: boolean;
	isVisionarySoldOut: boolean;
	handleSelectPlan: (plan: 'monthly' | 'yearly' | 'visionary' | 'gift1Month' | 'gift1Year' | 'giftVisionary') => void;
}

export const BottomCTASection: React.FC<BottomCTASectionProps> = observer(
	({
		isGiftMode,
		monthlyPrice,
		yearlyPrice,
		visionaryPrice,
		loadingCheckout,
		loadingSlots,
		isVisionarySoldOut,
		handleSelectPlan,
	}) => {
		return (
			<div className={styles.container}>
				<h2 className={styles.title}>
					{isGiftMode ? <Trans>Ready to Buy a Gift?</Trans> : <Trans>Ready to Upgrade?</Trans>}
				</h2>
				<div className={styles.buttonContainer}>
					{!isGiftMode ? (
						<>
							<Button
								variant="secondary"
								onClick={() => handleSelectPlan('monthly')}
								submitting={loadingCheckout || loadingSlots}
								className={styles.button}
							>
								<Trans>Monthly {monthlyPrice}</Trans>
							</Button>
							<Button
								variant="primary"
								onClick={() => handleSelectPlan('yearly')}
								submitting={loadingCheckout || loadingSlots}
								className={styles.button}
							>
								<Trans>Yearly {yearlyPrice}</Trans>
							</Button>
							<Button
								variant="primary"
								onClick={() => handleSelectPlan('visionary')}
								submitting={loadingCheckout || loadingSlots}
								disabled={isVisionarySoldOut}
								className={styles.button}
							>
								{isVisionarySoldOut ? <Trans>Visionary Sold Out</Trans> : <Trans>Visionary {visionaryPrice}</Trans>}
							</Button>
						</>
					) : (
						<>
							<Button
								variant="secondary"
								onClick={() => handleSelectPlan('gift1Year')}
								submitting={loadingCheckout || loadingSlots}
								className={styles.button}
							>
								<Trans>1 Year {yearlyPrice}</Trans>
							</Button>
							<Button
								variant="primary"
								onClick={() => handleSelectPlan('gift1Month')}
								submitting={loadingCheckout || loadingSlots}
								className={styles.button}
							>
								<Trans>1 Month {monthlyPrice}</Trans>
							</Button>
							<Button
								variant="primary"
								onClick={() => handleSelectPlan('giftVisionary')}
								submitting={loadingCheckout || loadingSlots}
								disabled={isVisionarySoldOut}
								className={styles.button}
							>
								{isVisionarySoldOut ? (
									<Trans>Visionary Gift Sold Out</Trans>
								) : (
									<Trans>Visionary {visionaryPrice}</Trans>
								)}
							</Button>
						</>
					)}
				</div>
				<PurchaseDisclaimer />
			</div>
		);
	},
);
