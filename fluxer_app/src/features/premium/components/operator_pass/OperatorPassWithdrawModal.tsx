// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {isolateBidi} from '@app/features/i18n/utils/BidiIsolate';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import styles from '@app/features/premium/components/operator_pass/OperatorPassWithdraw.module.css';
import {
	formatOperatorPassCalendarDay,
	formatOperatorPassDate,
	formatOperatorPassDateTime,
	keepsOperatorsThroughVisionary,
} from '@app/features/premium/utils/OperatorPassUtils';
import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import Users from '@app/features/user/state/Users';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {formatMinorUnitPrice} from '@fluxer/number_utils/src/PriceFormatting';
import type {
	OperatorPassPurchaseResponse,
	OperatorPassUnitResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type {PlutoniumWithdrawalResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useRef, useState} from 'react';

export const WITHDRAW_FROM_CONTRACT_HERE_DESCRIPTOR = msg({
	message: 'Withdraw from contract here',
	comment:
		'Button on a paid Operator Pass purchase, shown while the buyer can still withdraw. Statutory wording from EU consumer law (Directive 2023/2673, article 11a, the withdrawal function). Use the exact words of your language version of the directive if it has one (sv-SE: Ångra avtalet här), otherwise translate the English faithfully. "Withdraw" means using the consumer right of withdrawal on an Operator Pass purchase for a full refund, before the certificate is delivered or, where the law keeps the right after delivery such as in Brazil, within the days it gives. Never a word for taking money out.',
});
const WITHDRAW_TITLE_DESCRIPTOR = msg({
	message: 'Withdraw from contract',
	comment:
		'Title of the dialog that withdraws from a paid Operator Pass purchase. "Withdraw" means using the consumer right of withdrawal on an Operator Pass purchase for a full refund, before the certificate is delivered or, where the law keeps the right after delivery such as in Brazil, within the days it gives. Never a word for taking money out.',
});
const WITHDRAW_BODY_DESCRIPTOR = msg({
	message: 'We refund {amount} to your original payment method.',
	comment:
		'Sentence in the withdrawal dialog. {amount} is a price with currency. "Withdraw" means using the consumer right of withdrawal on an Operator Pass purchase for a full refund, before the certificate is delivered or, where the law keeps the right after delivery such as in Brazil, within the days it gives. Never a word for taking money out.',
});
const PLUTONIUM_WITHDRAW_PART_BODY_DESCRIPTOR = msg({
	message:
		"You paid {paid}. We keep the part for the time you've had {premiumProductName}, so we refund {amount} to your original payment method.",
	comment:
		'Sentence in the dialog that withdraws from a paid Plutonium subscription, when the refund is less than what was paid. {paid} is what the user paid and {amount} is what they get back, both prices with currency. {premiumProductName} is the plan name, such as Plutonium, keep it as is. "Withdraw" means using the consumer right of withdrawal to get money back, never a word for taking money out.',
});
const PURCHASE_DELIVERED_BODY_DESCRIPTOR = msg({
	message:
		'Every pass from this order that is still on your account ends now, and our records show their certificate pages as canceled.',
	comment:
		'Sentence in the dialog that withdraws from a whole paid Operator Pass purchase whose certificate was already delivered, which the law allows in some countries such as Brazil. The certificate pages are the pages for these passes in the Operator certificate PDF. Never translate certificate as diploma, card, license or token.',
});
const WITHDRAW_OPEN_UNTIL_DESCRIPTOR = msg({
	message: 'Open until {date}.',
	comment:
		'Line in the dialog that withdraws from a paid Operator Pass purchase or pass. It is the last day the withdrawal can be sent. {date} is a date without a time, such as Dec 4, 2026.',
});
const ORDER_DESCRIPTOR = msg({
	message: 'Order {orderNumber}, bought on {date}.',
	comment:
		'Line in the dialog that withdraws from a paid Operator Pass purchase. This is the statutory line naming the contract being withdrawn from, required for the withdrawal function by EU consumer law (Directive 2023/2673, article 11a) and in Sweden by distansavtalslagen 2 kap. 10 a §. {orderNumber} is the order number from the purchase receipt email, such as 1A2B3C4D-0001, keep it as is. {date} is the purchase date, such as Dec 4, 2026.',
});
const ORDER_WITHOUT_NUMBER_DESCRIPTOR = msg({
	message: 'Operator Pass purchase made on {date}.',
	comment:
		'Line in the dialog that withdraws from a paid Operator Pass purchase, shown when the order number is not known. This is the statutory line naming the contract being withdrawn from, required for the withdrawal function by EU consumer law (Directive 2023/2673, article 11a) and in Sweden by distansavtalslagen 2 kap. 10 a §. {date} is the purchase date, such as Dec 4, 2026.',
});
const PASS_ORDER_DESCRIPTOR = msg({
	message: 'One pass from order {orderNumber}, bought on {date}.',
	comment:
		'Line in the dialog that withdraws from one Operator Pass of a purchase. This is the statutory line naming the contract being withdrawn from, required for the withdrawal function by EU consumer law (Directive 2023/2673, article 11a) and in Sweden by distansavtalslagen 2 kap. 10 a §. {orderNumber} is the order number from the purchase receipt email, such as 1A2B3C4D-0001, keep it as is. {date} is the purchase date, such as Dec 4, 2026.',
});
const PASS_ORDER_WITHOUT_NUMBER_DESCRIPTOR = msg({
	message: 'One pass from the Operator Pass purchase made on {date}.',
	comment:
		'Line in the dialog that withdraws from one Operator Pass of a purchase, shown when the order number is not known. This is the statutory line naming the contract being withdrawn from, required for the withdrawal function by EU consumer law (Directive 2023/2673, article 11a) and in Sweden by distansavtalslagen 2 kap. 10 a §. {date} is the purchase date, such as Dec 4, 2026.',
});
const RECEIPT_TO_EMAIL_DESCRIPTOR = msg({
	message: "We'll send your receipt to {email}.",
	comment:
		'Line in the Operator Pass withdrawal dialogs, right above the name field. This is the statutory line saying in which durable form the receipt for the withdrawal is sent, required by EU consumer law (Directive 2023/2673, article 11a) and in Sweden by distansavtalslagen 2 kap. 10 a §. The buyer confirms it by pressing Confirm withdrawal. {email} is the email address of the account, keep it as is. The receipt is an email that acknowledges we received the withdrawal, never a payment receipt.',
});
const RECEIPT_TO_CHECKOUT_EMAIL_DESCRIPTOR = msg({
	message: "We'll send your receipt to the email address you gave at checkout.",
	comment:
		'Line in the Operator Pass withdrawal dialogs, right above the name field, shown when the account has no email address. This is the statutory line saying in which durable form the receipt for the withdrawal is sent, required by EU consumer law (Directive 2023/2673, article 11a) and in Sweden by distansavtalslagen 2 kap. 10 a §. The buyer confirms it by pressing Confirm withdrawal. Checkout is the payment page where the buyer paid. The receipt is an email that acknowledges we received the withdrawal, never a payment receipt.',
});
const KEEP_PURCHASE_DESCRIPTOR = msg({
	message: 'Keep purchase',
	comment:
		'Button that closes the withdrawal dialog without withdrawing. "Withdraw" means using the consumer right of withdrawal on an Operator Pass purchase for a full refund, before the certificate is delivered or, where the law keeps the right after delivery such as in Brazil, within the days it gives. Never a word for taking money out.',
});
const CONFIRM_WITHDRAWAL_DESCRIPTOR = msg({
	message: 'Confirm withdrawal',
	comment:
		'Button that sends the withdrawal. Statutory wording from EU consumer law (Directive 2023/2673, article 11a, the confirmation function). Use the exact words of your language version of the directive if it has one (sv-SE: Bekräfta frånträde), otherwise translate the English faithfully. "Withdraw" means using the consumer right of withdrawal on an Operator Pass purchase for a full refund, before the certificate is delivered or, where the law keeps the right after delivery such as in Brazil, within the days it gives. Never a word for taking money out.',
});
const WITHDRAWN_TOAST_DESCRIPTOR = msg({
	message: "Withdrawal confirmed. We'll email your receipt.",
	comment:
		'Toast after the withdrawal went through. The receipt is the email that acknowledges we received the withdrawal, never a payment receipt, so use the same words as in "We\'ll send your receipt to {email}." The receipt email can go out some time after this toast shows, so keep it in the future tense. "Withdraw" means using the consumer right of withdrawal on an Operator Pass purchase for a full refund, before the certificate is delivered or, where the law keeps the right after delivery such as in Brazil, within the days it gives. Never a word for taking money out.',
});
const WITHDRAW_FAILED_DESCRIPTOR = msg({
	message: "Couldn't withdraw. Try again or contact support.",
	comment:
		'Error in the withdrawal dialog. "Withdraw" means using the consumer right of withdrawal on an Operator Pass purchase for a full refund, before the certificate is delivered or, where the law keeps the right after delivery such as in Brazil, within the days it gives. Never a word for taking money out.',
});
const WITHDRAW_UNAVAILABLE_DESCRIPTOR = msg({
	message: "This can't be withdrawn here anymore. Contact support.",
	comment:
		'Error in the withdrawal dialog when the purchase or pass changed, for example because the certificate was delivered or the withdrawal period passed. "Withdraw" means using the consumer right of withdrawal on an Operator Pass purchase for a full refund, before the certificate is delivered or, where the law keeps the right after delivery such as in Brazil, within the days it gives. Never a word for taking money out.',
});
const PASS_WITHDRAW_TITLE_DESCRIPTOR = msg({
	message: 'Withdraw from this pass?',
	comment:
		'Title of the dialog where a buyer withdraws from one Operator Pass of a purchase. "Withdraw" means using the consumer right of withdrawal on an Operator Pass purchase for a full refund, before the certificate is delivered or, where the law keeps the right after delivery such as in Brazil, within the days it gives. Never a word for taking money out.',
});
const PASS_WITHDRAW_BODY_DESCRIPTOR = msg({
	message:
		'You get a full refund to your original payment method. The pass ends now, and our records show its certificate page as canceled.',
	comment:
		'Sentence in the dialog that withdraws from one Operator Pass. The certificate page is the page for this pass in the Operator certificate PDF. "Withdraw" means using the consumer right of withdrawal on an Operator Pass purchase for a full refund, before the certificate is delivered or, where the law keeps the right after delivery such as in Brazil, within the days it gives. Never a word for taking money out.',
});
const LAST_PASS_DESCRIPTOR = msg({
	message: 'Your badge, wall listing and #operators go too.',
	comment:
		"Sentence in the Operator Pass withdrawal dialogs, shown only when the withdrawal ends the last Operator Pass on the account. The badge is the Operator badge on the profile, the wall listing is the spot on the Operators' wall, and #operators is the channel name in Fluxer Labs, kept as is. Together they stand for everything else that comes with the pass.",
});
const LAST_PASS_VISIONARY_DESCRIPTOR = msg({
	message: 'Your badge and wall listing go too. You keep #operators through your Visionary.',
	comment:
		"Sentence in the Operator Pass withdrawal dialogs, shown instead of the usual one when the withdrawal ends the last Operator Pass on the account and you have Visionary. The badge is the Operator badge on the profile and the wall listing is the spot on the Operators' wall. #operators is the channel name in Fluxer Labs and Visionary is a plan name, keep both as is. Visionary keeps access to #operators under section 13 of the Operator Pass terms.",
});
export const PLUTONIUM_SUBSCRIPTION_CONTRACT_DESCRIPTOR = msg({
	message: '{premiumProductName} subscription, paid on {date}.',
	comment:
		'Line naming a Plutonium subscription the buyer can withdraw from, shown in settings and in the withdrawal dialog. This is the statutory line naming the contract, required for the withdrawal function by EU consumer law (Directive 2023/2673, article 11a) and in Sweden by distansavtalslagen 2 kap. 10 a §. {premiumProductName} is the plan name, such as Plutonium, keep it as is. {date} is the date of the payment that started the withdrawal period, the first payment or a renewal, such as Dec 4, 2026.',
});
export const PLUTONIUM_GIFT_CONTRACT_DESCRIPTOR = msg({
	message: '{premiumProductName} gift, bought on {date}.',
	comment:
		'Line naming a Plutonium gift purchase the buyer can withdraw from, shown in settings and in the withdrawal dialog. This is the statutory line naming the contract, required for the withdrawal function by EU consumer law (Directive 2023/2673, article 11a) and in Sweden by distansavtalslagen 2 kap. 10 a §. {premiumProductName} is the plan name, such as Plutonium, keep it as is. {date} is the purchase date, such as Dec 4, 2026.',
});
const PLUTONIUM_SUBSCRIPTION_ENDS_DESCRIPTOR = msg({
	message: 'Your subscription ends now.',
	comment:
		'Sentence in the dialog that withdraws from a paid Plutonium subscription. Withdrawing refunds every payment for it and ends the subscription right away.',
});
const PLUTONIUM_GIFT_ENDS_DESCRIPTOR = msg({
	message: 'The gift code stops working, and if someone redeemed it, they lose the time it gave them.',
	comment:
		'Sentence in the dialog that withdraws from a paid Plutonium gift. Withdrawing refunds the buyer, so the gift code no longer works and whoever redeemed it loses the Plutonium time it gave.',
});
const KEEP_PASS_DESCRIPTOR = msg({
	message: 'Keep pass',
	comment: 'Button that closes the dialog that withdraws from one Operator Pass, without withdrawing.',
});
const FULL_NAME_DESCRIPTOR = msg({
	message: 'Full name',
	comment:
		'Label of the name field in the Operator Pass withdrawal and cancel dialogs. Consumer law asks for the name.',
});
const FULL_NAME_REQUIRED_DESCRIPTOR = msg({
	message: 'Enter your full name.',
	comment: 'Error under the name field of the Operator Pass withdrawal and cancel dialogs when it is empty.',
});

const WITHDRAWAL_NAME_INPUT_MAX_LENGTH = 200;

const logger = new Logger('OperatorPassWithdraw');

export type OperatorPassWithdrawTarget =
	| {kind: 'plutonium'; withdrawal: PlutoniumWithdrawalResponse; onWithdrawn: () => void}
	| {kind: 'purchase'; purchase: OperatorPassPurchaseResponse; lastPass: boolean; onWithdrawn: () => void}
	| {
			kind: 'pass';
			pass: OperatorPassUnitResponse;
			purchase: OperatorPassPurchaseResponse | undefined;
			lastPass: boolean;
			onWithdrawn: () => void;
	  };

function withdrawTargetPeriod(target: OperatorPassWithdrawTarget): {
	id: string;
	endsAt: string | null;
	endsOn: string | null;
} {
	switch (target.kind) {
		case 'plutonium':
			return {
				id: target.withdrawal.id,
				endsAt: target.withdrawal.withdrawal_ends_at,
				endsOn: target.withdrawal.withdrawal_ends_on,
			};
		case 'purchase':
			return {
				id: target.purchase.id,
				endsAt: target.purchase.withdrawal_ends_at,
				endsOn: target.purchase.withdrawal_ends_on ?? null,
			};
		case 'pass':
			return {
				id: target.pass.id,
				endsAt: target.pass.withdrawal_ends_at,
				endsOn: target.pass.withdrawal_ends_on ?? null,
			};
	}
}

const OperatorPassWithdrawModal = observer(function OperatorPassWithdrawModal({
	target,
}: {
	target: OperatorPassWithdrawTarget;
}) {
	const {i18n} = useLingui();
	const [name, setName] = useState('');
	const [submitting, setSubmitting] = useState(false);
	const [nameError, setNameError] = useState<string | undefined>(undefined);
	const [failure, setFailure] = useState<string | null>(null);
	const nameInputRef = useRef<HTMLInputElement | null>(null);
	const single = target.kind === 'pass';
	const plutonium = target.kind === 'plutonium' ? target.withdrawal : null;
	const operatorPassPurchase = target.kind === 'plutonium' ? undefined : target.purchase;
	const {id: targetId, endsAt, endsOn} = withdrawTargetPeriod(target);
	const orderNumber = operatorPassPurchase?.order_number ?? null;
	const boughtAt =
		operatorPassPurchase?.purchased_at ?? (target.kind === 'pass' ? (target.pass.purchased_at ?? null) : null);
	const accountEmail = Users.currentUser?.email ?? null;
	const handleConfirm = async () => {
		if (submitting) return;
		const trimmedName = name.trim();
		if (!trimmedName) {
			setNameError(i18n._(FULL_NAME_REQUIRED_DESCRIPTOR));
			requestAnimationFrame(() => nameInputRef.current?.focus());
			return;
		}
		setNameError(undefined);
		setFailure(null);
		setSubmitting(true);
		try {
			if (target.kind === 'plutonium') {
				await PremiumCommands.withdrawPurchase(target.withdrawal.id, {
					name: trimmedName,
					quoted_at: target.withdrawal.quoted_at,
				});
				target.onWithdrawn();
			} else if (target.kind === 'purchase') {
				await OperatorPassCommands.withdrawPurchase(target.purchase.id, {name: trimmedName});
				target.onWithdrawn();
			} else {
				await OperatorPassCommands.withdrawPass(target.pass.id, {name: trimmedName});
				target.onWithdrawn();
			}
			ToastCommands.success(i18n._(WITHDRAWN_TOAST_DESCRIPTOR));
			ModalCommands.popWithKey(WITHDRAW_MODAL_KEY);
		} catch (error) {
			logger.error('Operator Pass withdrawal failed', error);
			const unavailable =
				failureCode(error) === APIErrorCodes.STRIPE_REFUND_OUTSIDE_WINDOW ||
				(error instanceof HttpError && (error.status === HttpStatus.CONFLICT || error.status === HttpStatus.NOT_FOUND));
			setFailure(i18n._(unavailable ? WITHDRAW_UNAVAILABLE_DESCRIPTOR : WITHDRAW_FAILED_DESCRIPTOR));
			setSubmitting(false);
		}
	};
	let body: string;
	if (target.kind === 'pass') {
		body = i18n._(PASS_WITHDRAW_BODY_DESCRIPTOR);
	} else if (target.kind === 'plutonium') {
		const {amount_minor, paid_minor, currency} = target.withdrawal;
		const amount = formatMinorUnitPrice(amount_minor, currency, getCurrentLocale());
		body =
			amount_minor < paid_minor
				? i18n._(PLUTONIUM_WITHDRAW_PART_BODY_DESCRIPTOR, {
						paid: formatMinorUnitPrice(paid_minor, currency, getCurrentLocale()),
						premiumProductName: getPremiumProductName(),
						amount,
					})
				: i18n._(WITHDRAW_BODY_DESCRIPTOR, {amount});
	} else {
		body = i18n._(WITHDRAW_BODY_DESCRIPTOR, {
			amount: formatMinorUnitPrice(
				target.purchase.refundable_minor,
				target.purchase.currency,
				getCurrentLocale(),
				OperatorPassCommands.getCountryCode(),
			),
		});
	}
	return (
		<Modal.Root size="small" centered data-flx="premium.operator-pass-withdraw-modal.modal-root">
			<Modal.Header
				title={i18n._(single ? PASS_WITHDRAW_TITLE_DESCRIPTOR : WITHDRAW_TITLE_DESCRIPTOR)}
				data-flx="premium.operator-pass-withdraw-modal.modal-header"
			/>
			<form
				noValidate
				style={{display: 'contents'}}
				onSubmit={(event) => {
					event.preventDefault();
					void handleConfirm();
				}}
				data-flx="premium.operator-pass-withdraw-modal.form"
			>
				<Modal.Content data-flx="premium.operator-pass-withdraw-modal.modal-content">
					<Modal.ContentLayout data-flx="premium.operator-pass-withdraw-modal.content-layout">
						<Modal.Description data-flx="premium.operator-pass-withdraw-modal.body">{body}</Modal.Description>
						{endsOn && (
							<p
								className={styles.note}
								title={endsAt ? formatOperatorPassDateTime(endsAt) : undefined}
								data-flx="premium.operator-pass-withdraw-modal.open-until"
							>
								{i18n._(WITHDRAW_OPEN_UNTIL_DESCRIPTOR, {date: formatOperatorPassCalendarDay(endsOn)})}
							</p>
						)}
						{plutonium && (
							<p className={styles.note} data-flx="premium.operator-pass-withdraw-modal.contract">
								{i18n._(
									plutonium.kind === 'gift'
										? PLUTONIUM_GIFT_CONTRACT_DESCRIPTOR
										: PLUTONIUM_SUBSCRIPTION_CONTRACT_DESCRIPTOR,
									{premiumProductName: getPremiumProductName(), date: formatOperatorPassDate(plutonium.purchased_at)},
								)}
							</p>
						)}
						{plutonium && (
							<p className={styles.note} data-flx="premium.operator-pass-withdraw-modal.plutonium-ends">
								{i18n._(
									plutonium.kind === 'gift' ? PLUTONIUM_GIFT_ENDS_DESCRIPTOR : PLUTONIUM_SUBSCRIPTION_ENDS_DESCRIPTOR,
								)}
							</p>
						)}
						{boughtAt && (
							<p className={styles.note} data-flx="premium.operator-pass-withdraw-modal.contract">
								{orderNumber
									? i18n._(single ? PASS_ORDER_DESCRIPTOR : ORDER_DESCRIPTOR, {
											orderNumber: isolateBidi(orderNumber),
											date: formatOperatorPassDate(boughtAt),
										})
									: i18n._(single ? PASS_ORDER_WITHOUT_NUMBER_DESCRIPTOR : ORDER_WITHOUT_NUMBER_DESCRIPTOR, {
											date: formatOperatorPassDate(boughtAt),
										})}
							</p>
						)}
						{target.kind === 'purchase' && target.purchase.delivered_at != null && (
							<p className={styles.note} data-flx="premium.operator-pass-withdraw-modal.delivered-purchase">
								{i18n._(PURCHASE_DELIVERED_BODY_DESCRIPTOR)}
							</p>
						)}
						{target.kind !== 'plutonium' &&
							target.lastPass &&
							(target.kind === 'pass' || target.purchase.delivered_at != null) && (
								<p className={styles.note} data-flx="premium.operator-pass-withdraw-modal.last-pass">
									{i18n._(
										keepsOperatorsThroughVisionary(Users.currentUser)
											? LAST_PASS_VISIONARY_DESCRIPTOR
											: LAST_PASS_DESCRIPTOR,
									)}
								</p>
							)}
						<p className={styles.note} data-flx="premium.operator-pass-withdraw-modal.receipt">
							{accountEmail
								? i18n._(RECEIPT_TO_EMAIL_DESCRIPTOR, {email: isolateBidi(accountEmail)})
								: i18n._(RECEIPT_TO_CHECKOUT_EMAIL_DESCRIPTOR)}
						</p>
						<Input
							ref={nameInputRef}
							id={`operator-pass-withdraw-name-${targetId}`}
							label={i18n._(FULL_NAME_DESCRIPTOR)}
							value={name}
							onChange={(event) => setName(event.target.value)}
							error={nameError}
							autoComplete="name"
							required
							aria-required
							maxLength={WITHDRAWAL_NAME_INPUT_MAX_LENGTH}
							disabled={submitting}
							data-flx="premium.operator-pass-withdraw-modal.input.name"
						/>
						{failure && (
							<p className={styles.error} role="alert" data-flx="premium.operator-pass-withdraw-modal.error">
								{failure}
							</p>
						)}
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="premium.operator-pass-withdraw-modal.modal-footer">
					<Button
						variant="secondary"
						onClick={ModalCommands.pop}
						disabled={submitting}
						data-flx="premium.operator-pass-withdraw-modal.button.cancel"
					>
						{i18n._(single ? KEEP_PASS_DESCRIPTOR : KEEP_PURCHASE_DESCRIPTOR)}
					</Button>
					<Button
						variant="danger"
						type="submit"
						submitting={submitting}
						data-flx="premium.operator-pass-withdraw-modal.button.withdraw"
					>
						{i18n._(CONFIRM_WITHDRAWAL_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</form>
		</Modal.Root>
	);
});

const WITHDRAW_MODAL_KEY = 'operator-pass-withdraw';

export function openOperatorPassWithdrawModal(target: OperatorPassWithdrawTarget): void {
	ModalCommands.pushWithKey(
		modal(() => <OperatorPassWithdrawModal target={target} data-flx="premium.operator-pass-withdraw-modal.open" />),
		WITHDRAW_MODAL_KEY,
	);
}
