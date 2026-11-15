// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PendingSubscriptionChangeResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

interface ScheduledSubscriptionChangeDescriptionProps {
	hasPendingListPriceSwitch: boolean;
	pendingTargetPriceLabel: string | null;
	pendingChangeDate: string;
	pendingSubscriptionChange: PendingSubscriptionChangeResponse;
	pendingInitialPriceLabel: string | null;
	pendingRecurringPriceLabel: string | null;
	pendingCreditPriceLabel: string | null;
}

export const ScheduledSubscriptionChangeDescription = observer(
	({
		hasPendingListPriceSwitch,
		pendingTargetPriceLabel,
		pendingChangeDate,
		pendingSubscriptionChange,
		pendingInitialPriceLabel,
		pendingRecurringPriceLabel,
		pendingCreditPriceLabel,
	}: ScheduledSubscriptionChangeDescriptionProps) => {
		if (hasPendingListPriceSwitch) {
			if (pendingTargetPriceLabel) {
				return (
					<Trans comment="Plutonium subscription card line shown when a move to a lower price is scheduled. {pendingChangeDate} is a date and {pendingTargetPriceLabel} is a currency amount, both already formatted and localized by code; never write a date or an amount into the translation.">
						New price scheduled for{' '}
						<strong data-flx="app.plutonium.subscription-card.strong--13">{pendingChangeDate}</strong>. Renewals will be{' '}
						{pendingTargetPriceLabel} from then on.
					</Trans>
				);
			}
			return (
				<Trans comment="Plutonium subscription card line shown when a move to a lower price is scheduled but the new amount is unknown. {pendingChangeDate} is a date already formatted and localized by code; never write a date into the translation.">
					New price scheduled for{' '}
					<strong data-flx="app.plutonium.subscription-card.strong--13">{pendingChangeDate}</strong>.
				</Trans>
			);
		}
		if (pendingSubscriptionChange?.target_billing_cycle === 'yearly') {
			if (!pendingInitialPriceLabel || !pendingRecurringPriceLabel) {
				return (
					<Trans>
						Yearly upgrade scheduled for{' '}
						<strong data-flx="app.plutonium.subscription-card.strong--12">{pendingChangeDate}</strong>.
					</Trans>
				);
			}
			if (pendingCreditPriceLabel) {
				return (
					<Trans>
						Yearly upgrade scheduled for{' '}
						<strong data-flx="app.plutonium.subscription-card.strong--12">{pendingChangeDate}</strong>. First yearly
						charge: {pendingInitialPriceLabel} after a {pendingCreditPriceLabel} credit. Future yearly renewals:{' '}
						{pendingRecurringPriceLabel}/year.
					</Trans>
				);
			}
			return (
				<Trans>
					Yearly upgrade scheduled for{' '}
					<strong data-flx="app.plutonium.subscription-card.strong--12">{pendingChangeDate}</strong>. First yearly
					charge: {pendingInitialPriceLabel}. Future yearly renewals: {pendingRecurringPriceLabel}/year.
				</Trans>
			);
		}
		if (pendingInitialPriceLabel && pendingRecurringPriceLabel) {
			return (
				<Trans>
					Monthly billing starts on{' '}
					<strong data-flx="app.plutonium.subscription-card.strong--12">{pendingChangeDate}</strong>. First monthly
					charge: {pendingInitialPriceLabel}. Future monthly renewals: {pendingRecurringPriceLabel}
					/month.
				</Trans>
			);
		}
		return (
			<Trans>
				Monthly billing starts on{' '}
				<strong data-flx="app.plutonium.subscription-card.strong--12">{pendingChangeDate}</strong>.
			</Trans>
		);
	},
);
