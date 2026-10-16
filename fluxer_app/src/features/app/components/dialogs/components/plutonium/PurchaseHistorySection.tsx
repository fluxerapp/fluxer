// SPDX-License-Identifier: AGPL-3.0-or-later

import {InvoiceList} from '@app/features/app/components/dialogs/components/plutonium/InvoiceList';
import styles from '@app/features/app/components/dialogs/components/plutonium/PurchaseHistorySection.module.css';
import {Accordion} from '@app/features/ui/accordion/Accordion';
import {Button} from '@app/features/ui/button/Button';
import type {PremiumStateResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useState} from 'react';

interface PurchaseHistorySectionProps {
	premiumState: PremiumStateResponse | null;
	loadingPortal: boolean;
	handleOpenCustomerPortal: () => void;
}

export const PurchaseHistorySection: React.FC<PurchaseHistorySectionProps> = observer(
	({premiumState, loadingPortal, handleOpenCustomerPortal}) => {
		const invoices = premiumState?.billing.invoices ?? [];
		const [invoicesExpanded, setInvoicesExpanded] = useState(false);
		return (
			<section className={styles.section} data-flx="app.plutonium.purchase-history-section.section">
				<div className={styles.card} data-flx="app.plutonium.purchase-history-section.card">
					<div className={styles.grid} data-flx="app.plutonium.purchase-history-section.grid">
						<div className={styles.content} data-flx="app.plutonium.purchase-history-section.content">
							<h3 className={styles.title} data-flx="app.plutonium.purchase-history-section.title">
								<Trans>Purchase history</Trans>
							</h3>
						</div>
						<Button
							variant="secondary"
							onClick={handleOpenCustomerPortal}
							submitting={loadingPortal}
							small
							className={styles.button}
							data-flx="app.plutonium.purchase-history-section.button.open-customer-portal"
						>
							<Trans comment="Billing button that opens Stripe customer portal for payment-method management only.">
								Manage payment methods
							</Trans>
						</Button>
					</div>
					<div className={styles.accordionStack} data-flx="app.plutonium.purchase-history-section.accordion-stack">
						<Accordion
							id="plutonium-invoices"
							className={styles.accordion}
							compact
							title={<Trans comment="Accordion title in the premium billing panel.">Billing history</Trans>}
							expanded={invoicesExpanded}
							onExpandedChange={setInvoicesExpanded}
							data-flx="app.plutonium.purchase-history-section.plutonium-invoices"
						>
							<InvoiceList invoices={invoices} data-flx="app.plutonium.purchase-history-section.invoice-list" />
						</Accordion>
					</div>
				</div>
			</section>
		);
	},
);
