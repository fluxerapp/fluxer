// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import styles from '@app/features/app/components/dialogs/components/plutonium/BillingRows.module.css';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import {
	PURCHASE_TITLE_DESCRIPTOR,
	UNDELIVERED_LINE_DESCRIPTOR,
	useOperatorPassPurchases,
} from '@app/features/premium/components/operator_pass/OperatorPassPurchasesSection';
import {
	openOperatorPassWithdrawModal,
	PLUTONIUM_GIFT_CONTRACT_DESCRIPTOR,
	PLUTONIUM_SUBSCRIPTION_CONTRACT_DESCRIPTOR,
	WITHDRAW_FROM_CONTRACT_HERE_DESCRIPTOR,
} from '@app/features/premium/components/operator_pass/OperatorPassWithdrawModal';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import {
	formatOperatorPassCalendarDay,
	formatOperatorPassDate,
	isOperatorPassLastPurchase,
} from '@app/features/premium/utils/OperatorPassUtils';
import {REFUNDS_SECTION_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {getFormattedShortDate} from '@app/features/user/utils/DateFormatting';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {formatMinorUnitPrice} from '@fluxer/number_utils/src/PriceFormatting';
import type {
	PlutoniumWithdrawalResponse,
	SelfServeRefundEligibilityResponse,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useCallback, useEffect, useId, useState} from 'react';

const REFUND_YOUR_MOST_RECENT_PURCHASE_DESCRIPTOR = msg({
	message: 'Refund your most recent purchase?',
	comment: 'Question prompt in the Plutonium self serve refund section. Keep the tone plain and specific.',
});
const REFUND_YOUR_RECENT_PURCHASES_DESCRIPTOR = msg({
	message: 'Refund your recent purchases?',
	comment:
		'Title of the confirm dialog for a Plutonium self-serve refund paid on the web or in the desktop app. The refund covers every payment made in the last 3 days, for example a subscription and the upgrade paid after it. Keep the tone plain and specific.',
});
const REFUND_PURCHASE_DESCRIPTOR = msg({
	message: 'Refund purchase',
	comment: 'Short label in the Plutonium self serve refund section. Keep the tone plain and specific.',
});
const NOT_NOW_DESCRIPTOR = msg({
	message: 'Not now',
	comment: 'Short label in the Plutonium self serve refund section. Keep the tone plain and specific.',
});
const REFUND_ISSUED_DESCRIPTOR = msg({
	message: 'Your refund has been issued. It can take a few business days before you see the money.',
	comment:
		'Success toast shown after a self-serve Plutonium refund is issued. "Business days" means working days, not weekends or holidays.',
});
const REFUND_OUTSIDE_WINDOW_TITLE_DESCRIPTOR = msg({
	message: 'Outside the refund window',
	comment: 'Title of the error modal shown when a self-serve refund is no longer within the eligible window.',
});
const REFUND_OUTSIDE_WINDOW_MESSAGE_DESCRIPTOR = msg({
	message: 'Your most recent purchase is no longer within the refund window. Contact support for billing issues.',
	comment: 'Body of the error modal shown when a self-serve refund is no longer within the eligible window.',
});
const REFUND_COOLDOWN_TITLE_DESCRIPTOR = msg({
	message: 'You have refunded recently',
	comment: 'Title of the error modal shown when a self-serve refund is blocked by the cooldown period.',
});
const REFUND_COOLDOWN_MESSAGE_DESCRIPTOR = msg({
	message: "You've already used a self-serve refund recently. You can refund again once the cooldown ends.",
	comment: 'Body of the error modal shown when a self-serve refund is blocked by the cooldown period.',
});
const REFUND_NO_PURCHASE_TITLE_DESCRIPTOR = msg({
	message: 'Nothing to refund',
	comment: 'Title of the error modal shown when there is no refundable purchase on the account.',
});
const REFUND_NO_PURCHASE_MESSAGE_DESCRIPTOR = msg({
	message: "We couldn't find a recent purchase to refund on this account.",
	comment: 'Body of the error modal shown when there is no refundable purchase on the account.',
});
const REFUND_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't refund this purchase",
	comment: 'Title of the generic fallback error modal shown when a self-serve refund fails.',
});
const REFUND_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while issuing your refund. Contact support if this keeps happening.',
	comment: 'Body of the generic fallback error modal shown when a self-serve refund fails.',
});
const FULL_REFUND_TITLE_DESCRIPTOR = msg({
	message: 'Full refund',
	comment:
		'Title of the row in the Refunds section of the Payments settings where the user can get all the money back for their recent Plutonium purchases.',
});
const FULL_REFUND_AMOUNT_DESCRIPTOR = msg({
	message: 'You can get the full {amount} back until {date}.',
	comment:
		'Line under "Full refund" in the Payments settings. {amount} is the price paid, with currency, and {date} is the last day of the refund window, such as Nov 2, 2026. Both are filled in by code.',
});
const FULL_REFUND_AMOUNT_ENDS_SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'You can get the full {amount} back until {date}. This also ends your subscription right away.',
	comment:
		'Line under "Full refund" in the Payments settings when the refund also ends a Plutonium subscription. {amount} is the price paid, with currency, and {date} is the last day of the refund window, such as Nov 2, 2026. Both are filled in by code.',
});
const FULL_REFUND_DESCRIPTOR = msg({
	message: 'You can get a full refund until {date}.',
	comment:
		'Line under "Full refund" in the Payments settings when the amount is not known, for example for a Google Play purchase. {date} is the last day of the refund window, such as Nov 2, 2026.',
});
const FULL_REFUND_ENDS_SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'You can get a full refund until {date}. This also ends your subscription right away.',
	comment:
		'Line under "Full refund" in the Payments settings when the amount is not known and the refund also ends a Plutonium subscription. {date} is the last day of the refund window, such as Nov 2, 2026.',
});
const WITHDRAW_FULL_DESCRIPTOR = msg({
	message: 'You can withdraw until {date} and get {amount} back.',
	comment:
		'Line under a Plutonium subscription or gift in settings, next to the Withdraw from contract here button. "Withdraw" means using the consumer right of withdrawal to get money back, never a word for taking money out. {date} is the last day of the withdrawal period, such as Dec 4, 2026. {amount} is a price with currency.',
});
const WITHDRAW_PART_DESCRIPTOR = msg({
	message:
		"You paid {paid} on {paidDate}. If you withdraw now, we keep the part for the time you've had {premiumProductName} and refund {amount}. You can withdraw until {endDate}.",
	comment:
		'Line under a Plutonium subscription in the Payments settings, next to the Withdraw from contract here button, when the refund is less than what was paid. "Withdraw" means using the consumer right of withdrawal to get money back, never a word for taking money out. {paid} and {amount} are prices with currency: {paid} is what the user paid and {amount} is what they get back. {paidDate} is the payment date and {endDate} the last day of the withdrawal period, such as Dec 4, 2026. {premiumProductName} is the plan name, such as Plutonium, keep it as is.',
});

const logger = new Logger('RefundsSection');

function resolveRefundErrorContent(code: string | undefined): {title: string; message: string} {
	switch (code) {
		case APIErrorCodes.STRIPE_REFUND_OUTSIDE_WINDOW:
			return {
				title: i18n._(REFUND_OUTSIDE_WINDOW_TITLE_DESCRIPTOR),
				message: i18n._(REFUND_OUTSIDE_WINDOW_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.STRIPE_REFUND_COOLDOWN_ACTIVE:
			return {
				title: i18n._(REFUND_COOLDOWN_TITLE_DESCRIPTOR),
				message: i18n._(REFUND_COOLDOWN_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.STRIPE_NO_PURCHASE_HISTORY:
			return {
				title: i18n._(REFUND_NO_PURCHASE_TITLE_DESCRIPTOR),
				message: i18n._(REFUND_NO_PURCHASE_MESSAGE_DESCRIPTOR),
			};
		default:
			return {
				title: i18n._(REFUND_FAILED_TITLE_DESCRIPTOR),
				message: i18n._(REFUND_FAILED_MESSAGE_DESCRIPTOR),
			};
	}
}

function showRefundErrorModal(error: unknown): void {
	const {title, message} = resolveRefundErrorContent(failureCode(error));
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal title={title} message={message} data-flx="app.plutonium.refunds-section.generic-error-modal" />
		)),
	);
}

function formatShortDay(isoTimestamp: string | null): string | null {
	if (!isoTimestamp) return null;
	const date = new Date(isoTimestamp);
	if (Number.isNaN(date.getTime())) return null;
	return getFormattedShortDate(date);
}

function showsRefundRow(eligibility: SelfServeRefundEligibilityResponse | null): boolean {
	return (
		eligibility != null &&
		eligibility.reason !== 'no_refundable_purchase' &&
		eligibility.reason !== 'feature_unavailable'
	);
}

function isCoveredByFullRefund(
	withdrawal: Pick<PlutoniumWithdrawalResponse, 'purchased_at'>,
	eligibility: SelfServeRefundEligibilityResponse | null,
	now: number = Date.now(),
): boolean {
	if (!eligibility?.eligible || eligibility.provider === 'google_play') return false;
	if (!eligibility.paid_at || !eligibility.refund_window_expires_at) return false;
	const windowMs = Date.parse(eligibility.refund_window_expires_at) - Date.parse(eligibility.paid_at);
	return Date.parse(withdrawal.purchased_at) + windowMs > now;
}

function fullRefundLine(eligibility: SelfServeRefundEligibilityResponse, countryCode: string | null): string {
	const date = formatShortDay(eligibility.refund_window_expires_at) ?? '';
	const amount =
		eligibility.invoice_amount_paid_cents != null && eligibility.currency
			? formatMinorUnitPrice(
					eligibility.invoice_amount_paid_cents,
					eligibility.currency,
					getCurrentLocale(),
					countryCode ?? undefined,
				)
			: null;
	if (amount) {
		return i18n._(
			eligibility.cancels_subscription
				? FULL_REFUND_AMOUNT_ENDS_SUBSCRIPTION_DESCRIPTOR
				: FULL_REFUND_AMOUNT_DESCRIPTOR,
			{amount, date},
		);
	}
	return i18n._(eligibility.cancels_subscription ? FULL_REFUND_ENDS_SUBSCRIPTION_DESCRIPTOR : FULL_REFUND_DESCRIPTOR, {
		date,
	});
}

function withdrawalLine(withdrawal: PlutoniumWithdrawalResponse): string {
	const locale = getCurrentLocale();
	const amount = formatMinorUnitPrice(withdrawal.amount_minor, withdrawal.currency, locale);
	const endDate = formatOperatorPassCalendarDay(withdrawal.withdrawal_ends_on);
	if (withdrawal.amount_minor === withdrawal.paid_minor) {
		return i18n._(WITHDRAW_FULL_DESCRIPTOR, {date: endDate, amount});
	}
	return i18n._(WITHDRAW_PART_DESCRIPTOR, {
		paid: formatMinorUnitPrice(withdrawal.paid_minor, withdrawal.currency, locale),
		paidDate: formatOperatorPassDate(withdrawal.purchased_at),
		premiumProductName: getPremiumProductName(),
		amount,
		endDate,
	});
}

interface RefundRowProps {
	rowId: string;
	title: string;
	line: ReactNode;
	action: (describedBy: string) => ReactNode;
}

function RefundRow({rowId, title, line, action}: RefundRowProps) {
	const titleId = `${rowId}-title`;
	const lineId = `${rowId}-line`;
	return (
		<li className={styles.row} aria-labelledby={titleId} data-flx="app.plutonium.refunds-section.row">
			<div className={styles.text} data-flx="app.plutonium.refunds-section.row-text">
				<p id={titleId} className={styles.title} data-flx="app.plutonium.refunds-section.row-title">
					{title}
				</p>
				<p id={lineId} className={styles.line} data-flx="app.plutonium.refunds-section.row-line">
					{line}
				</p>
			</div>
			<div className={styles.actions} data-flx="app.plutonium.refunds-section.row-actions">
				{action(`${titleId} ${lineId}`)}
			</div>
		</li>
	);
}

interface RefundsSectionProps {
	eligibility: SelfServeRefundEligibilityResponse | null;
	countryCode: string | null;
	refreshPremiumState: () => Promise<unknown>;
}

export const RefundsSection = observer(function RefundsSection({
	eligibility,
	countryCode,
	refreshPremiumState,
}: RefundsSectionProps) {
	const {i18n} = useLingui();
	const baseId = useId();
	const [refunding, setRefunding] = useState(false);
	const [withdrawals, setWithdrawals] = useState<ReadonlyArray<PlutoniumWithdrawalResponse>>([]);
	const operatorPass = useOperatorPassPurchases();
	const reloadWithdrawals = useCallback(() => {
		PremiumCommands.fetchWithdrawals()
			.then(setWithdrawals)
			.catch((error) => logger.warn('Failed to load Plutonium purchases open for withdrawal', error));
	}, []);
	useEffect(reloadWithdrawals, [reloadWithdrawals]);
	const isGooglePlay = eligibility?.provider === 'google_play';
	const handleRefund = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(
						isGooglePlay ? REFUND_YOUR_MOST_RECENT_PURCHASE_DESCRIPTOR : REFUND_YOUR_RECENT_PURCHASES_DESCRIPTOR,
					)}
					description={
						isGooglePlay ? (
							<Trans>
								This refunds your most recent Google Play purchase and cancels any subscription it paid for. You won't
								be able to self-refund again for 30 days.
							</Trans>
						) : (
							<Trans comment="Body of the confirm dialog for a Plutonium self-serve refund paid on the web or in the desktop app. Every payment made in the last 3 days is refunded, for example a subscription and the upgrade paid after it.">
								This refunds every purchase you made in the last 3 days and cancels any subscription they paid for. You
								won't be able to self-refund again for 30 days.
							</Trans>
						)
					}
					primaryText={i18n._(REFUND_PURCHASE_DESCRIPTOR)}
					primaryVariant="danger"
					secondaryText={i18n._(NOT_NOW_DESCRIPTOR)}
					onPrimary={async () => {
						setRefunding(true);
						try {
							await PremiumCommands.refundLatestPurchase();
							ToastCommands.success(i18n._(REFUND_ISSUED_DESCRIPTOR));
							await refreshPremiumState();
							reloadWithdrawals();
						} catch (error) {
							logger.error('Refund failed', error);
							showRefundErrorModal(error);
						} finally {
							setRefunding(false);
						}
					}}
					data-flx="app.plutonium.refunds-section.handle-refund.confirm-modal"
				/>
			)),
		);
	}, [refreshPremiumState, reloadWithdrawals, i18n, isGooglePlay]);
	const openWithdrawals = withdrawals.filter((withdrawal) => !isCoveredByFullRefund(withdrawal, eligibility));
	const undeliveredPasses = operatorPass.purchases.filter(
		(purchase) => purchase.can_withdraw && purchase.delivered_at == null,
	);
	const refundRowShown =
		showsRefundRow(eligibility) && (eligibility?.eligible === true || openWithdrawals.length === 0);
	if (!refundRowShown && openWithdrawals.length === 0 && undeliveredPasses.length === 0) return null;
	let refundLine: ReactNode = null;
	if (eligibility && refundRowShown) {
		if (eligibility.eligible) {
			refundLine = fullRefundLine(eligibility, countryCode);
		} else if (eligibility.reason === 'outside_refund_window') {
			refundLine = <Trans>Your last purchase is outside the refund window. Contact support for billing issues.</Trans>;
		} else if (eligibility.reason === 'cooldown_active') {
			const cooldownEndsOn = formatShortDay(eligibility.cooldown_expires_at);
			refundLine = <Trans>You can refund again after {cooldownEndsOn}. Contact support for billing issues.</Trans>;
		} else {
			refundLine = <Trans>This purchase is not eligible for a refund.</Trans>;
		}
	}
	return (
		<SettingsSection
			id="refunds"
			title={i18n._(REFUNDS_SECTION_DESCRIPTOR)}
			data-flx="app.plutonium.refunds-section.section"
		>
			<ul className={styles.list} data-flx="app.plutonium.refunds-section.list">
				{eligibility && refundRowShown && (
					<RefundRow
						rowId={`${baseId}-refund`}
						title={i18n._(FULL_REFUND_TITLE_DESCRIPTOR)}
						line={refundLine}
						action={(describedBy) => (
							<Button
								small
								variant="danger"
								onClick={handleRefund}
								submitting={refunding}
								disabled={!eligibility.eligible}
								aria-describedby={describedBy}
								data-flx="app.plutonium.refunds-section.button.refund"
							>
								{i18n._(REFUND_PURCHASE_DESCRIPTOR)}
							</Button>
						)}
						data-flx="app.plutonium.refunds-section.refund-row"
					/>
				)}
				{openWithdrawals.map((withdrawal) => (
					<RefundRow
						key={withdrawal.id}
						rowId={`${baseId}-${withdrawal.id}`}
						title={i18n._(
							withdrawal.kind === 'gift'
								? PLUTONIUM_GIFT_CONTRACT_DESCRIPTOR
								: PLUTONIUM_SUBSCRIPTION_CONTRACT_DESCRIPTOR,
							{premiumProductName: getPremiumProductName(), date: formatOperatorPassDate(withdrawal.purchased_at)},
						)}
						line={withdrawalLine(withdrawal)}
						action={(describedBy) => (
							<Button
								small
								variant="secondary"
								aria-describedby={describedBy}
								onClick={() =>
									openOperatorPassWithdrawModal({
										kind: 'plutonium',
										withdrawal,
										onWithdrawn: () => {
											reloadWithdrawals();
											void refreshPremiumState();
										},
									})
								}
								data-flx="app.plutonium.refunds-section.button.withdraw"
							>
								{i18n._(WITHDRAW_FROM_CONTRACT_HERE_DESCRIPTOR)}
							</Button>
						)}
						data-flx="app.plutonium.refunds-section.withdrawal-row"
					/>
				))}
				{undeliveredPasses.map((purchase) => (
					<RefundRow
						key={purchase.id}
						rowId={`${baseId}-${purchase.id}`}
						title={i18n._(PURCHASE_TITLE_DESCRIPTOR)}
						line={
							purchase.purchased_at
								? i18n._(UNDELIVERED_LINE_DESCRIPTOR, {date: formatOperatorPassDate(purchase.purchased_at)})
								: null
						}
						action={(describedBy) => (
							<Button
								small
								variant="secondary"
								aria-describedby={describedBy}
								onClick={() =>
									openOperatorPassWithdrawModal({
										kind: 'purchase',
										purchase,
										lastPass: isOperatorPassLastPurchase(OperatorPassHoldings.passes, purchase.id),
										onWithdrawn: () => {
											operatorPass.reload();
											void OperatorPassCommands.fetchPasses().catch((error) =>
												logger.warn('Failed to refresh Operator Passes after purchase withdrawal', error),
											);
										},
									})
								}
								data-flx="app.plutonium.refunds-section.button.withdraw-operator-pass"
							>
								{i18n._(WITHDRAW_FROM_CONTRACT_HERE_DESCRIPTOR)}
							</Button>
						)}
						data-flx="app.plutonium.refunds-section.operator-pass-row"
					/>
				))}
			</ul>
		</SettingsSection>
	);
});
