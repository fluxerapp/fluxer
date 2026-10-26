// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {OPERATOR_PASS_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import styles from '@app/features/premium/components/operator_pass/OperatorPassHoldingsSection.module.css';
import {openOperatorPassOfferModal} from '@app/features/premium/components/operator_pass/OperatorPassOfferModal';
import {
	openOperatorPassPassItOnModal,
	PASS_IT_ON_DESCRIPTOR,
} from '@app/features/premium/components/operator_pass/OperatorPassPassItOnModal';
import {
	UNDELIVERED_LINE_DESCRIPTOR,
	UNDELIVERED_TITLE_DESCRIPTOR,
	useUndeliveredOperatorPassPurchases,
} from '@app/features/premium/components/operator_pass/OperatorPassPurchasesSection';
import {
	openOperatorPassWithdrawModal,
	WITHDRAW_FROM_CONTRACT_HERE_DESCRIPTOR,
} from '@app/features/premium/components/operator_pass/OperatorPassWithdrawModal';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import {
	buildOperatorPassPassRows,
	isOperatorPassBenefitsRemoved,
	type OperatorPassPassRow,
} from '@app/features/premium/utils/OperatorPassPassRows';
import {
	OPERATOR_PASS_RECOGNITION_COUNT_DESCRIPTOR,
	OPERATOR_PASS_RECOGNITION_DESCRIPTOR,
} from '@app/features/premium/utils/OperatorPassRecognitionMessages';
import {
	formatOperatorPassCalendarDay,
	formatOperatorPassDate,
	formatOperatorPassLongDate,
	formatOperatorPassUnlockDay,
	getOperatorPassOfferUserName,
	isOperatorPassAvailable,
	isOperatorPassLastFreePass,
} from '@app/features/premium/utils/OperatorPassUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {OperatorPassOfferResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useEffect, useState} from 'react';

const PASS_COUNT_DESCRIPTOR = msg({
	message: '{count, plural, one {# Operator Pass} other {# Operator Passes}}',
	comment:
		'Heading of the list of Operator Passes in profile settings, and the count of passes an account has. Operator Pass is a product name, keep it as is.',
});
const OFFERS_TITLE_DESCRIPTOR = msg({
	message: 'Operator Pass offers',
	comment:
		'Heading of the list of Operator Passes other people want to pass on to this account. Operator Pass is a product name.',
});
const BOUGHT_DESCRIPTOR = msg({
	message: '{count, plural, =1 {Bought {date}} other {# passes, bought {date}}}',
	comment:
		'Row in the list of Operator Passes for passes the account bought in one purchase. {count} is how many passes of that purchase are in the row, {date} the purchase date without a time, such as Nov 20, 2026.',
});
const RECEIVED_FROM_DESCRIPTOR = msg({
	message: 'Received from {name}, {date}',
	comment:
		'Row in the list of Operator Passes for a pass another account passed on to this one. {name} is a display name, {date} a date without a time.',
});
const RECEIVED_DESCRIPTOR = msg({
	message: 'Received {date}',
	comment:
		'Row in the list of Operator Passes for a pass another account passed on to this one, when that account no longer exists. {date} is a date without a time.',
});
const ADDED_BY_TEAM_DESCRIPTOR = msg({
	message: 'Added by our team, {date}',
	comment:
		'Row in the list of Operator Passes for a pass our staff added to the account. {date} is a date without a time.',
});
const PAUSED_PASS_DESCRIPTOR = msg({
	message: 'Paused during a payment dispute',
	comment: 'Short muted line under an Operator Pass that is paused while a payment dispute about it is open.',
});
const LOCKED_UNTIL_DESCRIPTOR = msg({
	message: 'Can be passed on from {date}',
	comment:
		'Short muted line under an Operator Pass that can be passed on to another account only after a waiting period after payment. {date} is a date without a time.',
});
const PASSED_ON_TO_DESCRIPTOR = msg({
	message: 'Passed on to {name}',
	comment:
		'Row in the list of Operator Passes for a pass this account passed on to another account. {name} is a display name.',
});
const OFFERED_TO_DESCRIPTOR = msg({
	message: 'Offered to {name}',
	comment:
		'Short muted line under an Operator Pass this account offered to another account, waiting for them to accept. {name} is a display name.',
});
const MOVES_ON_DESCRIPTOR = msg({
	message: 'Moves to {name} on {date}',
	comment:
		'Short muted line under an Operator Pass offer the other account accepted, before the pass moves to them. {name} is a display name, {date} a date without a time.',
});
const FROM_DESCRIPTOR = msg({
	message: 'From {name}',
	comment: 'Row for an Operator Pass another account wants to pass on to this one. {name} is a display name.',
});
const REVIEW_OFFER_DESCRIPTOR = msg({
	message: 'Review',
	comment: 'Button that opens an incoming Operator Pass offer to accept or decline it.',
});
const CANCEL_UNTIL_DESCRIPTOR = msg({
	message: 'Cancel (until {date})',
	comment:
		'Small text link under an Operator Pass bought in Brazil. The buyer can cancel it within 7 days for a full refund. {date} is the last day, without a time. Use the consumer law word for withdrawing from a purchase (pt-BR: desistir), never a word for taking money out.',
});
const PASS_ONE_ON_DESCRIPTOR = msg({
	message: 'Pass one on',
	comment:
		'Button on a row of several Operator Passes from the same purchase. It offers one of them to another account. Pass it on is the name of the feature.',
});
const REMOVED_DESCRIPTOR = msg({
	message: 'Paused while your benefits are removed.',
	comment:
		"Muted line under the list of Operator Passes in profile settings while the account's Operator Pass benefits are removed under the Operator Pass terms. A link to the terms follows it.",
});
const TERMS_LINK_DESCRIPTOR = msg({
	message: 'Operator Pass terms',
	comment: 'Link to the Operator Pass terms. Operator Pass is a product name.',
});
const HOW_PASSES_WORK_DESCRIPTOR = msg({
	message: 'How passes work',
	comment:
		'Link under the list of Operator Passes in profile settings. It opens the questions on the Operator Pass page.',
});
const TAKE_BACK_OFFER_DESCRIPTOR = msg({
	message: 'Take back offer',
	comment: 'Button that takes back an Operator Pass offer before the pass moves to the other account.',
});
const OFFER_TAKEN_BACK_DESCRIPTOR = msg({
	message: 'Offer taken back. The pass stays with you.',
	comment: 'Toast after taking back an Operator Pass offer.',
});
const TAKE_BACK_FAILED_DESCRIPTOR = msg({
	message: "Couldn't take back the offer. It may have moved already.",
	comment: 'Toast when taking back an Operator Pass offer failed.',
});

const logger = new Logger('OperatorPassHoldingsSection');

function DateTooltip({iso, children}: {iso: string | null; children: ReactNode}) {
	if (!iso) return <>{children}</>;
	return (
		<Tooltip text={formatOperatorPassLongDate(iso)} data-flx="premium.operator-pass-holdings-section.date-tooltip">
			<span className={styles.dated} data-flx="premium.operator-pass-holdings-section.dated">
				{children}
			</span>
		</Tooltip>
	);
}

function TextLink({onClick, children, flx}: {onClick: () => void; children: ReactNode; flx: string}) {
	return (
		<FocusRing offset={-2} data-flx={`${flx}.focus-ring`}>
			<button type="button" className={styles.textLink} onClick={onClick} data-flx={flx}>
				{children}
			</button>
		</FocusRing>
	);
}

interface RowView {
	title: ReactNode;
	meta: Array<ReactNode>;
	action: ReactNode;
}

export const OperatorPassHoldingsSection = observer(function OperatorPassHoldingsSection() {
	const {i18n} = useLingui();
	const [takingBackId, setTakingBackId] = useState<string | null>(null);
	const {purchases, reload} = useUndeliveredOperatorPassPurchases();
	useEffect(() => {
		if (!isOperatorPassAvailable()) return;
		OperatorPassCommands.fetchPasses().catch((error) => logger.warn('Failed to load Operator Passes', error));
	}, []);
	const holdings = OperatorPassHoldings.passes;
	if (!holdings) return null;
	const rows = buildOperatorPassPassRows(holdings, purchases);
	if (rows.length === 0) return null;
	const removed = isOperatorPassBenefitsRemoved(holdings);
	const passCount = removed ? holdings.passes.length : holdings.count;
	const onlyPass = isOperatorPassLastFreePass(holdings);
	const lastPass = holdings.count <= 1;
	const handleTakeBack = async (offer: OperatorPassOfferResponse) => {
		setTakingBackId(offer.id);
		try {
			await OperatorPassCommands.withdrawOffer(offer.id);
			ToastCommands.success(i18n._(OFFER_TAKEN_BACK_DESCRIPTOR));
		} catch (error) {
			logger.error('Failed to take back the Operator Pass offer', error);
			ToastCommands.error(i18n._(TAKE_BACK_FAILED_DESCRIPTOR));
		} finally {
			setTakingBackId(null);
		}
	};
	const takeBackButton = (offer: OperatorPassOfferResponse) => (
		<Button
			small
			variant="secondary"
			onClick={() => void handleTakeBack(offer)}
			submitting={takingBackId === offer.id}
			data-flx="premium.operator-pass-holdings-section.button.take-back-offer"
		>
			{i18n._(TAKE_BACK_OFFER_DESCRIPTOR)}
		</Button>
	);
	const bought = (count: number, iso: string) => (
		<DateTooltip iso={iso} data-flx="premium.operator-pass.operator-pass-holdings-section.bought.date-tooltip">
			{i18n._(BOUGHT_DESCRIPTOR, {count, date: formatOperatorPassDate(iso)})}
		</DateTooltip>
	);
	const view = (row: Exclude<OperatorPassPassRow, {kind: 'incoming'}>): RowView => {
		switch (row.kind) {
			case 'undelivered':
				return {
					title: i18n._(UNDELIVERED_TITLE_DESCRIPTOR),
					meta: row.purchase.purchased_at
						? [
								<DateTooltip
									key="paid"
									iso={row.purchase.purchased_at}
									data-flx="premium.operator-pass.operator-pass-holdings-section.view.date-tooltip"
								>
									{i18n._(UNDELIVERED_LINE_DESCRIPTOR, {date: formatOperatorPassDate(row.purchase.purchased_at)})}
								</DateTooltip>,
							]
						: [],
					action: (
						<Button
							small
							variant="secondary"
							onClick={() =>
								openOperatorPassWithdrawModal({
									kind: 'purchase',
									purchase: row.purchase,
									onWithdrawn: () => {
										reload();
										void OperatorPassCommands.fetchPasses().catch((error) => {
											logger.warn(
												'Failed to refresh Operator Passes after purchase withdrawal',
												{purchaseId: row.purchase.id},
												error,
											);
										});
									},
								})
							}
							data-flx="premium.operator-pass-holdings-section.button.withdraw-purchase"
						>
							{i18n._(WITHDRAW_FROM_CONTRACT_HERE_DESCRIPTOR)}
						</Button>
					),
				};
			case 'held': {
				const first = row.passes[0];
				let title: ReactNode;
				if (first.source === 'transfer') {
					title = (
						<DateTooltip
							iso={row.date}
							data-flx="premium.operator-pass.operator-pass-holdings-section.title.date-tooltip"
						>
							{row.receivedFrom
								? i18n._(RECEIVED_FROM_DESCRIPTOR, {
										name: getOperatorPassOfferUserName(row.receivedFrom),
										date: formatOperatorPassDate(row.date),
									})
								: i18n._(RECEIVED_DESCRIPTOR, {date: formatOperatorPassDate(row.date)})}
						</DateTooltip>
					);
				} else if (first.source === 'admin') {
					title = (
						<DateTooltip
							iso={row.date}
							data-flx="premium.operator-pass.operator-pass-holdings-section.title.date-tooltip--2"
						>
							{i18n._(ADDED_BY_TEAM_DESCRIPTOR, {date: formatOperatorPassDate(row.date)})}
						</DateTooltip>
					);
				} else {
					title = bought(row.passes.length, row.date);
				}
				const meta: Array<ReactNode> = [];
				if (row.paused) meta.push(i18n._(PAUSED_PASS_DESCRIPTOR));
				if (row.lockEndsAt) {
					meta.push(
						<DateTooltip
							key="lock"
							iso={row.lockEndsAt}
							data-flx="premium.operator-pass.operator-pass-holdings-section.view.date-tooltip--2"
						>
							{i18n._(LOCKED_UNTIL_DESCRIPTOR, {date: formatOperatorPassUnlockDay(row.lockEndsAt)})}
						</DateTooltip>,
					);
				}
				const cancel = row.cancel;
				if (cancel?.withdrawal_ends_at && cancel.withdrawal_ends_on) {
					const endsAt = cancel.withdrawal_ends_at;
					const endsOn = cancel.withdrawal_ends_on;
					meta.push(
						<DateTooltip
							key="cancel"
							iso={endsAt}
							data-flx="premium.operator-pass.operator-pass-holdings-section.view.date-tooltip--3"
						>
							<TextLink
								onClick={() => openOperatorPassWithdrawModal({kind: 'pass', pass: cancel, lastPass})}
								flx="premium.operator-pass-holdings-section.link.cancel"
								data-flx="premium.operator-pass.operator-pass-holdings-section.view.text-link.open-operator-pass-withdraw-modal"
							>
								{i18n._(CANCEL_UNTIL_DESCRIPTOR, {date: formatOperatorPassCalendarDay(endsOn)})}
							</TextLink>
						</DateTooltip>,
					);
				}
				const passOn = row.passOn;
				const passable = row.passes.filter((pass) => pass.can_pass_on && !pass.paused).length;
				return {
					title,
					meta,
					action: passOn && (
						<Button
							small
							variant="secondary"
							disabled={row.lockEndsAt != null}
							onClick={() => openOperatorPassPassItOnModal(passOn, onlyPass)}
							data-flx="premium.operator-pass-holdings-section.button.pass-it-on"
						>
							{i18n._(passable > 1 ? PASS_ONE_ON_DESCRIPTOR : PASS_IT_ON_DESCRIPTOR)}
						</Button>
					),
				};
			}
			case 'offered': {
				const name = getOperatorPassOfferUserName(row.offer.recipient);
				return {
					title: bought(1, row.pass.delivered_at),
					meta: [
						row.offer.status === 'accepted' ? (
							<DateTooltip
								key="moves"
								iso={row.offer.arrives_at}
								data-flx="premium.operator-pass.operator-pass-holdings-section.view.date-tooltip--4"
							>
								{i18n._(MOVES_ON_DESCRIPTOR, {name, date: formatOperatorPassUnlockDay(row.offer.arrives_at)})}
							</DateTooltip>
						) : (
							i18n._(OFFERED_TO_DESCRIPTOR, {name})
						),
					],
					action: takeBackButton(row.offer),
				};
			}
			case 'passed_on':
				return {
					title: (
						<DateTooltip
							iso={row.offer.ended_at}
							data-flx="premium.operator-pass.operator-pass-holdings-section.view.date-tooltip--5"
						>
							{i18n._(PASSED_ON_TO_DESCRIPTOR, {name: getOperatorPassOfferUserName(row.offer.recipient)})}
						</DateTooltip>
					),
					meta: [],
					action: null,
				};
		}
	};
	const renderItem = (key: string, kind: string, {title, meta, action}: RowView) => (
		<li key={key} className={styles.item} data-kind={kind} data-flx="premium.operator-pass-holdings-section.pass">
			<div className={styles.itemText} data-flx="premium.operator-pass-holdings-section.pass-text">
				<p className={styles.line} data-flx="premium.operator-pass-holdings-section.row-title">
					{title}
				</p>
				{meta.map((part, index) => (
					<p key={index} className={styles.meta} data-flx="premium.operator-pass-holdings-section.meta">
						{part}
					</p>
				))}
			</div>
			{action && (
				<div className={styles.actions} data-flx="premium.operator-pass-holdings-section.pass-actions">
					{action}
				</div>
			)}
		</li>
	);
	const incoming = rows.filter(
		(row): row is Extract<OperatorPassPassRow, {kind: 'incoming'}> => row.kind === 'incoming',
	);
	const owned = rows.filter((row): row is Exclude<OperatorPassPassRow, {kind: 'incoming'}> => row.kind !== 'incoming');
	return (
		<div className={styles.section} data-flx="premium.operator-pass-holdings-section.section">
			{incoming.length > 0 && (
				<div className={styles.group} data-flx="premium.operator-pass-holdings-section.incoming">
					<h3 className={styles.title} data-flx="premium.operator-pass-holdings-section.incoming-title">
						{i18n._(OFFERS_TITLE_DESCRIPTOR)}
					</h3>
					<ul className={styles.list} data-flx="premium.operator-pass-holdings-section.incoming-list">
						{incoming.map((row) =>
							renderItem(row.key, row.kind, {
								title: i18n._(FROM_DESCRIPTOR, {name: getOperatorPassOfferUserName(row.offer.sender)}),
								meta: [],
								action: (
									<Button
										small
										variant="primary"
										onClick={() => openOperatorPassOfferModal(row.offer.id)}
										data-flx="premium.operator-pass-holdings-section.button.open-offer"
									>
										{i18n._(REVIEW_OFFER_DESCRIPTOR)}
									</Button>
								),
							}),
						)}
					</ul>
				</div>
			)}
			{owned.length > 0 && (
				<div className={styles.group} data-flx="premium.operator-pass-holdings-section.owned">
					<h3 className={styles.title} data-flx="premium.operator-pass-holdings-section.title">
						{passCount > 0 ? i18n._(PASS_COUNT_DESCRIPTOR, {count: passCount}) : OPERATOR_PASS_NAME}
					</h3>
					{holdings.recognition_count != null && (
						<p className={styles.meta} data-flx="premium.operator-pass-holdings-section.recognition-count">
							{i18n._(OPERATOR_PASS_RECOGNITION_COUNT_DESCRIPTOR, {count: holdings.recognition_count})}
						</p>
					)}
					<p className={styles.meta} data-flx="premium.operator-pass-holdings-section.recognition-description">
						{i18n._(OPERATOR_PASS_RECOGNITION_DESCRIPTOR)}
					</p>
					{removed && (
						<p className={styles.meta} data-flx="premium.operator-pass-holdings-section.removed">
							{i18n._(REMOVED_DESCRIPTOR)}{' '}
							<ExternalLink
								href={`${Routes.operatorPassTerms()}#8-when-we-remove-perks`}
								data-flx="premium.operator-pass-holdings-section.link.terms"
							>
								{i18n._(TERMS_LINK_DESCRIPTOR)}
							</ExternalLink>
						</p>
					)}
					<ul className={styles.list} data-flx="premium.operator-pass-holdings-section.passes">
						{owned.map((row) => renderItem(row.key, row.kind, view(row)))}
					</ul>
				</div>
			)}
			<ExternalLink
				href={`${Routes.operatorPassInfo()}#pass-it-on`}
				className={styles.howLink}
				data-flx="premium.operator-pass-holdings-section.link.how-passes-work"
			>
				{i18n._(HOW_PASSES_WORK_DESCRIPTOR)}
			</ExternalLink>
		</div>
	);
});
