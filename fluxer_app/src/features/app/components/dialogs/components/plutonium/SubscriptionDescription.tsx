// SPDX-License-Identifier: AGPL-3.0-or-later

import {PerksButton} from '@app/features/app/components/dialogs/components/PerksButton';
import styles from '@app/features/app/components/dialogs/components/plutonium/SubscriptionCard.module.css';
import {getStoreName} from '@app/features/premium/utils/PremiumUtils';
import type {GracePeriodInfo} from '@app/features/premium/utils/SubscriptionStatus';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {getFormattedLongDate} from '@fluxer/date_utils/src/DateFormatting';
import type {PremiumStoreSubscriptionState} from '@fluxer/schema/src/domains/premium/StoreBillingSchemas';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

interface SubscriptionDescriptionProps {
	locale: string;
	isFullyExpired: boolean;
	isInGracePeriod: boolean;
	graceEndDate: Date | null;
	gracePeriodInfo: GracePeriodInfo;
	isGiftGrace: boolean;
	storeSubscription: PremiumStoreSubscriptionState | null;
	premiumWillCancel: boolean;
	premiumUntil: Date | null;
	isGiftSubscription: boolean;
	perksDisabled: boolean;
	isVisionary: boolean;
	billingCycle: string | null;
	effectiveMonthlyPrice: string;
	effectiveYearlyPrice: string;
	isCurrentSubscriptionGrandfathered: boolean;
	grandfatheredTooltip: string;
	scrollToPerks: () => void;
}

export const SubscriptionDescription = observer(
	({
		locale,
		isFullyExpired,
		isInGracePeriod,
		graceEndDate,
		gracePeriodInfo,
		isGiftGrace,
		storeSubscription,
		premiumWillCancel,
		premiumUntil,
		isGiftSubscription,
		perksDisabled,
		isVisionary,
		billingCycle,
		effectiveMonthlyPrice,
		effectiveYearlyPrice,
		isCurrentSubscriptionGrandfathered,
		grandfatheredTooltip,
		scrollToPerks,
	}: SubscriptionDescriptionProps) => {
		if (isFullyExpired) {
			const expiredDate = graceEndDate ? getFormattedLongDate(graceEndDate, locale) : undefined;
			return (
				<Trans>
					Expired on <strong data-flx="app.plutonium.subscription-card.strong">{expiredDate}</strong>.{' '}
					<PerksButton
						onClick={scrollToPerks}
						data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks"
					/>{' '}
					are no longer active. You can resubscribe at any time.
				</Trans>
			);
		}
		if (storeSubscription && gracePeriodInfo.isPaymentRecovery) {
			const graceDate = graceEndDate ? getFormattedLongDate(graceEndDate, locale) : undefined;
			const storeName = getStoreName(storeSubscription.provider);
			return (
				<Trans comment="Plutonium subscription card text shown while a failed App Store or Google Play renewal payment is being retried. {storeName} is the store brand name, App Store or Google Play, and must not be translated. {graceDate} is a date already formatted and localized by code; never write a date into the translation.">
					Your {storeName} renewal payment failed but{' '}
					<PerksButton
						onClick={scrollToPerks}
						data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--10"
					/>{' '}
					stay active until <strong data-flx="app.plutonium.subscription-card.strong--17">{graceDate}</strong>. Update
					your {storeName} payment method before then to keep your subscription.
				</Trans>
			);
		}
		if (gracePeriodInfo.isPaymentRecovery) {
			const graceDate = graceEndDate ? getFormattedLongDate(graceEndDate, locale) : undefined;
			return (
				<Trans comment="Plutonium subscription card text shown while a failed renewal payment is being retried. {graceDate} is a date already formatted and localized by code; never write a date into the translation.">
					Your renewal payment failed but{' '}
					<PerksButton
						onClick={scrollToPerks}
						data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--9"
					/>{' '}
					stay active until <strong data-flx="app.plutonium.subscription-card.strong--16">{graceDate}</strong>. Update
					your payment method before then to keep your subscription.
				</Trans>
			);
		}
		if (isGiftGrace) {
			const graceDate = graceEndDate ? getFormattedLongDate(graceEndDate, locale) : undefined;
			return (
				<Trans comment="Plutonium subscription card text shown in the short grace period after gifted premium time ends. {graceDate} is a date already formatted and localized by code; never write a date into the translation.">
					Your gift time ended but{' '}
					<PerksButton
						onClick={scrollToPerks}
						data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--11"
					/>{' '}
					stay active until <strong data-flx="app.plutonium.subscription-card.strong--19">{graceDate}</strong>.
					Subscribe before then to keep them.
				</Trans>
			);
		}
		if (isInGracePeriod) {
			const graceDate = graceEndDate ? getFormattedLongDate(graceEndDate, locale) : undefined;
			return (
				<Trans>
					Your subscription ended but{' '}
					<PerksButton
						onClick={scrollToPerks}
						data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--2"
					/>{' '}
					stay active until <strong data-flx="app.plutonium.subscription-card.strong--2">{graceDate}</strong>.
					Resubscribe before then to keep your subscriber history.
				</Trans>
			);
		}
		if (storeSubscription && premiumWillCancel && premiumUntil) {
			const storeName = getStoreName(storeSubscription.provider);
			const endDate = getFormattedLongDate(premiumUntil, locale);
			return (
				<Trans comment="Plutonium subscription card text for an App Store or Google Play subscription that will not renew. {storeName} is the store brand name, App Store or Google Play, and must not be translated. {endDate} is a date already formatted and localized by code; never write a date into the translation.">
					Billed through {storeName}. Ends on{' '}
					<strong data-flx="app.plutonium.subscription-card.strong--18">{endDate}</strong>.
				</Trans>
			);
		}
		if (storeSubscription) {
			const storeName = getStoreName(storeSubscription.provider);
			return (
				<Trans comment="Plutonium subscription card text for an App Store or Google Play subscription. {storeName} is the store brand name, App Store or Google Play, and must not be translated.">
					Billed through {storeName}.
				</Trans>
			);
		}
		if (isGiftSubscription) {
			const giftEndDate = premiumUntil ? getFormattedLongDate(premiumUntil, locale) : undefined;
			return (
				<Trans>
					Gifted{' '}
					<PerksButton
						onClick={scrollToPerks}
						data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--3"
					/>{' '}
					until <strong data-flx="app.plutonium.subscription-card.strong--5">{giftEndDate}</strong>. Does not renew
					automatically. Redeem more gift codes to extend.
				</Trans>
			);
		}
		if (premiumWillCancel && premiumUntil) {
			const cancelDate = getFormattedLongDate(premiumUntil, locale);
			return (
				<Trans>
					Cancels on <strong data-flx="app.plutonium.subscription-card.strong--8">{cancelDate}</strong>.{' '}
					<PerksButton
						onClick={scrollToPerks}
						data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--4"
					/>{' '}
					remain active until then.
				</Trans>
			);
		}
		if (perksDisabled) {
			return <Trans>Your subscription is active but perks are temporarily paused.</Trans>;
		}
		if (isVisionary) {
			return (
				<Trans>
					All{' '}
					<PerksButton
						onClick={scrollToPerks}
						data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--5"
					/>
					, forever. No billing, no renewals.
				</Trans>
			);
		}
		if (billingCycle === 'monthly') {
			return (
				<>
					<Trans>
						All{' '}
						<PerksButton
							onClick={scrollToPerks}
							data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--6"
						/>{' '}
						for <strong data-flx="app.plutonium.subscription-card.strong--9">{effectiveMonthlyPrice}/month</strong>.
					</Trans>
					{isCurrentSubscriptionGrandfathered && (
						<Tooltip text={grandfatheredTooltip} data-flx="app.plutonium.subscription-card.tooltip">
							<span className={styles.legacyRateBadge} data-flx="app.plutonium.subscription-card.legacy-rate-badge">
								<Trans>Legacy rate</Trans>
							</span>
						</Tooltip>
					)}
				</>
			);
		}
		if (billingCycle === 'yearly') {
			return (
				<>
					<Trans>
						All{' '}
						<PerksButton
							onClick={scrollToPerks}
							data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--7"
						/>{' '}
						for <strong data-flx="app.plutonium.subscription-card.strong--10">{effectiveYearlyPrice}/year</strong>.
					</Trans>
					{isCurrentSubscriptionGrandfathered && (
						<Tooltip text={grandfatheredTooltip} data-flx="app.plutonium.subscription-card.tooltip--2">
							<span className={styles.legacyRateBadge} data-flx="app.plutonium.subscription-card.legacy-rate-badge--2">
								<Trans>Legacy rate</Trans>
							</span>
						</Tooltip>
					)}
				</>
			);
		}
		return (
			<Trans>
				All{' '}
				<PerksButton
					onClick={scrollToPerks}
					data-flx="app.plutonium.subscription-card.perks-button.scroll-to-perks--8"
				/>{' '}
				included with your subscription.
			</Trans>
		);
	},
);
