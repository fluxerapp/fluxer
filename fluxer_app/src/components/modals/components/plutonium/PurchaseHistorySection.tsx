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
import styles from './PurchaseHistorySection.module.css';

interface PurchaseHistorySectionProps {
	loadingPortal: boolean;
	handleOpenCustomerPortal: () => void;
}

export const PurchaseHistorySection: React.FC<PurchaseHistorySectionProps> = observer(
	({loadingPortal, handleOpenCustomerPortal}) => {
		return (
			<section className={styles.section}>
				<div className={styles.card}>
					<div className={styles.grid}>
						<div className={styles.content}>
							<h3 className={styles.title}>
								<Trans>Purchase History</Trans>
							</h3>
							<p className={styles.description}>
								<Trans>View all your past purchases and invoices securely in the customer portal.</Trans>
							</p>
						</div>
						<Button
							variant="primary"
							onClick={handleOpenCustomerPortal}
							submitting={loadingPortal}
							className={styles.button}
						>
							<Trans>View Purchase History</Trans>
						</Button>
					</div>
				</div>
			</section>
		);
	},
);
