// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {OPERATOR_PASS_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {COPY_LINK_DESCRIPTOR, DOWNLOAD_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import * as OperatorPassModalCommands from '@app/features/premium/commands/OperatorPassModalCommands';
import styles from '@app/features/premium/components/operator_pass/OperatorPassHoldingsSection.module.css';
import {openOperatorPassOfferModal} from '@app/features/premium/components/operator_pass/OperatorPassOfferModal';
import {
	openOperatorPassPassItOnModal,
	PASS_IT_ON_DESCRIPTOR,
} from '@app/features/premium/components/operator_pass/OperatorPassPassItOnModal';
import {
	CERTIFICATE_PAGE_DESCRIPTOR,
	DOWNLOAD_CERTIFICATE_DESCRIPTOR,
	OperatorPassPurchasesLoadFailed,
	openCertificateLinkResetModal,
	PURCHASE_TITLE_DESCRIPTOR,
	RESET_LINK_DESCRIPTOR,
	UNDELIVERED_LINE_DESCRIPTOR,
	useOperatorPassPurchases,
} from '@app/features/premium/components/operator_pass/OperatorPassPurchasesSection';
import {
	OPERATOR_PASS_TURN_ON_DESCRIPTOR,
	openOperatorPassVisionaryModal,
} from '@app/features/premium/components/operator_pass/OperatorPassVisionaryModal';
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
	isOperatorPassLastPurchase,
	OPERATOR_PASS_VISIONARY_DATE_TIME_ZONE,
} from '@app/features/premium/utils/OperatorPassUtils';
import {Button} from '@app/features/ui/button/Button';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import type {
	OperatorPassOfferResponse,
	OperatorPassPurchaseResponse,
	OperatorPassUnitResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useEffect, useId, useState} from 'react';

const PASS_COUNT_DESCRIPTOR = msg({
	message: '{count, plural, one {# Operator Pass} other {# Operator Passes}}',
	comment:
		'Heading of the list of Operator Passes in the Operator Pass settings, and the count of passes an account has. Operator Pass is a product name, keep it as is.',
});
const GET_ANOTHER_PASS_DESCRIPTOR = msg({
	message: 'Get another pass',
	comment:
		'Button next to the list of Operator Passes in the Operator Pass settings. It opens the Operator Pass checkout. Operator Pass is a product name.',
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
const CAME_WITH_VISIONARY_DESCRIPTOR = msg({
	message: 'Came with Visionary, {date}',
	comment:
		'Row in the list of Operator Passes for the pass that comes with Visionary. {date} is the date the account got Visionary, without a time. Visionary is a product name, keep it as is.',
});
const VISIONARY_CARD_DESCRIPTOR = msg({
	message:
		'Your Visionary comes with an Operator Pass. You pay nothing more to turn it on, and nothing about your Visionary changes.',
	comment:
		'Line in the Operator Pass settings for a Visionary who has not turned on the Operator Pass that comes with Visionary yet. A Turn on button follows it. Visionary and Operator Pass are product names, keep them as is. You pay nothing more means turning it on costs nothing on top of what they paid for Visionary, never use a word that means free or included.',
});
const VISIONARY_ENDED_DESCRIPTOR = msg({
	message: 'This pass ended because your Visionary ended.',
	comment:
		'Muted line in the Operator Pass settings when the Operator Pass that came with Visionary ended because the account no longer has Visionary.',
});
const PAUSED_PASS_DESCRIPTOR = msg({
	message: 'Paused during a payment dispute',
	comment: 'Short muted line under an Operator Pass that is paused while a payment dispute about it is open.',
});
const LOCKED_UNTIL_DESCRIPTOR = msg({
	message: 'If you pass it on, it moves on {date} at the earliest',
	comment:
		'Short muted line under an Operator Pass during the waiting period after payment. The pass can be offered to another account at any time, but it only moves to them once this waiting period is over. {date} is a date without a time.',
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
const WITHDRAW_UNTIL_DESCRIPTOR = msg({
	message: 'Withdraw from one pass (until {date})',
	comment:
		'Small text link under Operator Passes bought in a country whose withdrawal right lasts after delivery, such as Brazil with 7 days. It withdraws from one pass of the purchase for a refund of that pass, while the button next to it withdraws from the whole purchase. Only shown when the purchase has more than one pass. {date} is the last day, without a time. Use the consumer law word for withdrawing from a purchase (pt-BR: desistir), never a word for taking money out.',
});
const PASS_ONE_ON_DESCRIPTOR = msg({
	message: 'Pass one on',
	comment:
		'Button on a row of several Operator Passes from the same purchase. It offers one of them to another account. Pass it on is the name of the feature.',
});
const REMOVED_DESCRIPTOR = msg({
	message: 'Paused while your badge, wall listing and Operator role are removed.',
	comment:
		"Muted line under the list of Operator Passes in the Operator Pass settings while our team has removed the Operator badge, the Operators' wall listing and the Operator role from the account under the Operator Pass terms. Operator is the role name, keep it as is. A link to the terms follows it.",
});
const TERMS_LINK_DESCRIPTOR = msg({
	message: 'Operator Pass terms',
	comment: 'Link to the Operator Pass terms. Operator Pass is a product name.',
});
const HOW_PASSES_WORK_DESCRIPTOR = msg({
	message: 'How passes work',
	comment:
		'Link under the list of Operator Passes in the Operator Pass settings. It opens the questions on the Operator Pass page.',
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

function DateTooltip({iso, timeZone, children}: {iso: string | null; timeZone?: string; children: ReactNode}) {
	if (!iso) return <>{children}</>;
	return (
		<Tooltip
			text={formatOperatorPassLongDate(iso, timeZone)}
			data-flx="premium.operator-pass-holdings-section.date-tooltip"
		>
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
	links: Array<ReactNode>;
	pages?: Array<ReactNode>;
	action: ((describedBy: string) => ReactNode) | null;
}

export const OperatorPassHoldingsSection = observer(function OperatorPassHoldingsSection() {
	const {i18n} = useLingui();
	const [takingBackId, setTakingBackId] = useState<string | null>(null);
	const baseId = useId();
	const {purchases, failed, reload} = useOperatorPassPurchases();
	useEffect(() => {
		if (!isOperatorPassAvailable()) return;
		OperatorPassCommands.fetchPasses().catch((error) => logger.warn('Failed to load Operator Passes', error));
	}, []);
	const holdings = OperatorPassHoldings.passes;
	const loadFailed = failed ? (
		<OperatorPassPurchasesLoadFailed
			onRetry={reload}
			data-flx="premium.operator-pass-holdings-section.operator-pass-purchases-load-failed"
		/>
	) : null;
	if (!holdings) return null;
	const rows = buildOperatorPassPassRows(holdings, purchases);
	const visionaryState = holdings.visionary?.state;
	if (rows.length === 0 && visionaryState !== 'available' && visionaryState !== 'ended') return null;
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
	const takeBackButton = (offer: OperatorPassOfferResponse) => (describedBy: string) => (
		<Button
			small
			variant="secondary"
			onClick={() => void handleTakeBack(offer)}
			submitting={takingBackId === offer.id}
			aria-describedby={describedBy}
			data-flx="premium.operator-pass-holdings-section.button.take-back-offer"
		>
			{i18n._(TAKE_BACK_OFFER_DESCRIPTOR)}
		</Button>
	);
	const openPurchaseWithdrawal = (purchase: OperatorPassPurchaseResponse) =>
		openOperatorPassWithdrawModal({
			kind: 'purchase',
			purchase,
			lastPass: isOperatorPassLastPurchase(holdings, purchase.id),
			onWithdrawn: () => {
				reload();
				void OperatorPassCommands.fetchPasses().catch((error) => {
					logger.warn('Failed to refresh Operator Passes after purchase withdrawal', {purchaseId: purchase.id}, error);
				});
			},
		});
	const certificateLinks = (key: string, passId: string, url: string, onReset: () => void, label?: ReactNode) => (
		<span key={key} className={styles.linkGroup} data-flx="premium.operator-pass-holdings-section.certificate-links">
			{label}
			{label && (
				<TextLink
					onClick={() => void openExternalUrl(url)}
					flx="premium.operator-pass-holdings-section.link.download-page"
					data-flx="premium.operator-pass.operator-pass-holdings-section.certificate-links.text-link"
				>
					{i18n._(DOWNLOAD_DESCRIPTOR)}
				</TextLink>
			)}
			<TextLink
				onClick={() => void TextCopyCommands.copy(i18n, url)}
				flx="premium.operator-pass-holdings-section.link.copy-certificate-link"
				data-flx="premium.operator-pass.operator-pass-holdings-section.certificate-links.text-link--2"
			>
				{i18n._(COPY_LINK_DESCRIPTOR)}
			</TextLink>
			<TextLink
				onClick={() => openCertificateLinkResetModal(i18n, passId, onReset)}
				flx="premium.operator-pass-holdings-section.link.reset-certificate-link"
				data-flx="premium.operator-pass.operator-pass-holdings-section.certificate-links.text-link.open-certificate-link-reset-modal"
			>
				{i18n._(RESET_LINK_DESCRIPTOR)}
			</TextLink>
		</span>
	);
	const heldPurchaseIds = new Set(
		rows.flatMap((row) =>
			row.kind === 'held' && row.passes[0].source === 'purchase' && row.passes[0].purchase_id
				? [row.passes[0].purchase_id]
				: [],
		),
	);
	const passedOnPageLinks = (passId: string): Array<ReactNode> => {
		for (const purchase of purchases) {
			if (heldPurchaseIds.has(purchase.id) || !purchase.certificate_available) continue;
			const index = purchase.pages.findIndex((page) => page.pass_id === passId);
			const page = purchase.pages[index];
			if (!page?.certificate_url) continue;
			return [
				certificateLinks(
					`page-${page.pass_id}`,
					page.pass_id,
					page.certificate_url,
					reload,
					i18n._(CERTIFICATE_PAGE_DESCRIPTOR, {number: index + 1, name: page.name}),
				),
			];
		}
		return [];
	};
	const refreshPasses = () =>
		void OperatorPassCommands.fetchPasses().catch((error) => {
			logger.warn('Failed to refresh Operator Passes after a certificate link reset', error);
		});
	const boughtAt = (pass: OperatorPassUnitResponse, fallback: string) =>
		(pass.source === 'purchase' && pass.purchase_id
			? purchases.find((purchase) => purchase.id === pass.purchase_id)?.purchased_at
			: null) ?? fallback;
	const bought = (count: number, iso: string) => (
		<DateTooltip iso={iso} data-flx="premium.operator-pass.operator-pass-holdings-section.bought.date-tooltip">
			{i18n._(BOUGHT_DESCRIPTOR, {count, date: formatOperatorPassDate(iso)})}
		</DateTooltip>
	);
	const view = (row: Exclude<OperatorPassPassRow, {kind: 'incoming'}>): RowView => {
		switch (row.kind) {
			case 'undelivered':
				return {
					title: i18n._(PURCHASE_TITLE_DESCRIPTOR),
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
					links: [],
					action: (describedBy) => (
						<Button
							small
							variant="secondary"
							onClick={() => openPurchaseWithdrawal(row.purchase)}
							aria-describedby={describedBy}
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
				} else if (first.source === 'visionary') {
					title = (
						<DateTooltip
							iso={row.date}
							timeZone={OPERATOR_PASS_VISIONARY_DATE_TIME_ZONE}
							data-flx="premium.operator-pass.operator-pass-holdings-section.title.date-tooltip--visionary"
						>
							{i18n._(CAME_WITH_VISIONARY_DESCRIPTOR, {
								date: formatOperatorPassDate(row.date, OPERATOR_PASS_VISIONARY_DATE_TIME_ZONE),
							})}
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
					title = bought(row.passes.length, boughtAt(first, row.date));
				}
				const meta: Array<ReactNode> = [];
				const links: Array<ReactNode> = [];
				const pageLinks: Array<ReactNode> = [];
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
				const purchaseId = first.source === 'purchase' ? first.purchase_id : null;
				const rowPurchase = purchaseId ? purchases.find((purchase) => purchase.id === purchaseId) : undefined;
				const actionPurchase = row.purchaseActions ? rowPurchase : undefined;
				const certificatePurchase = actionPurchase?.certificate_available ? actionPurchase : undefined;
				const withdrawPurchase = actionPurchase?.can_withdraw ? actionPurchase : undefined;
				const purchasePassCount = purchaseId
					? holdings.passes.filter((pass) => pass.source === 'purchase' && pass.purchase_id === purchaseId).length
					: 0;
				const cancel = withdrawPurchase && purchasePassCount <= 1 ? null : row.cancel;
				if (cancel?.withdrawal_ends_at && cancel.withdrawal_ends_on) {
					const endsAt = cancel.withdrawal_ends_at;
					const endsOn = cancel.withdrawal_ends_on;
					links.push(
						<DateTooltip
							key="cancel"
							iso={endsAt}
							data-flx="premium.operator-pass.operator-pass-holdings-section.view.date-tooltip--3"
						>
							<TextLink
								onClick={() =>
									openOperatorPassWithdrawModal({
										kind: 'pass',
										pass: cancel,
										purchase: rowPurchase,
										lastPass,
										onWithdrawn: reload,
									})
								}
								flx="premium.operator-pass-holdings-section.link.cancel"
								data-flx="premium.operator-pass.operator-pass-holdings-section.view.text-link.open-operator-pass-withdraw-modal"
							>
								{i18n._(WITHDRAW_UNTIL_DESCRIPTOR, {date: formatOperatorPassCalendarDay(endsOn)})}
							</TextLink>
						</DateTooltip>,
					);
				}
				const pages = certificatePurchase?.pages ?? [];
				const singlePage = pages.length === 1 ? pages[0] : null;
				const visionaryPassId = first.source === 'visionary' ? first.id : null;
				const certificateUrl = visionaryPassId
					? (holdings.visionary?.certificate_url ?? null)
					: (singlePage?.certificate_url ?? null);
				const certificatePassId = visionaryPassId ?? singlePage?.pass_id ?? null;
				if (certificateUrl && certificatePassId) {
					links.push(
						certificateLinks(
							'certificate',
							certificatePassId,
							certificateUrl,
							visionaryPassId ? refreshPasses : reload,
						),
					);
				}
				if (pages.length > 1) {
					for (const [index, page] of pages.entries()) {
						if (!page.certificate_url) continue;
						pageLinks.push(
							certificateLinks(
								`page-${page.pass_id}`,
								page.pass_id,
								page.certificate_url,
								reload,
								i18n._(CERTIFICATE_PAGE_DESCRIPTOR, {number: index + 1, name: page.name}),
							),
						);
					}
				}
				const passOn = row.passOn;
				const passable = row.passes.filter((pass) => pass.can_pass_on && !pass.paused).length;
				const action =
					certificateUrl || passOn || withdrawPurchase
						? (describedBy: string) => (
								<>
									{certificateUrl && (
										<Button
											small
											variant="secondary"
											onClick={() => void openExternalUrl(certificateUrl)}
											aria-describedby={describedBy}
											data-flx="premium.operator-pass-holdings-section.button.download-certificate"
										>
											{i18n._(DOWNLOAD_CERTIFICATE_DESCRIPTOR)}
										</Button>
									)}
									{withdrawPurchase && (
										<Button
											small
											variant="secondary"
											onClick={() => openPurchaseWithdrawal(withdrawPurchase)}
											aria-describedby={describedBy}
											data-flx="premium.operator-pass-holdings-section.button.withdraw-held-purchase"
										>
											{i18n._(WITHDRAW_FROM_CONTRACT_HERE_DESCRIPTOR)}
										</Button>
									)}
									{passOn && (
										<Button
											small
											variant="secondary"
											onClick={() => openOperatorPassPassItOnModal(passOn, onlyPass)}
											aria-describedby={describedBy}
											data-flx="premium.operator-pass-holdings-section.button.pass-it-on"
										>
											{i18n._(passable > 1 ? PASS_ONE_ON_DESCRIPTOR : PASS_IT_ON_DESCRIPTOR)}
										</Button>
									)}
								</>
							)
						: null;
				return {title, meta, links, pages: pageLinks, action};
			}
			case 'offered': {
				const name = getOperatorPassOfferUserName(row.offer.recipient);
				return {
					title: bought(1, boughtAt(row.pass, row.pass.delivered_at)),
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
					links: passedOnPageLinks(row.pass.id),
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
					links: passedOnPageLinks(row.offer.pass_id),
					action: null,
				};
		}
	};
	const renderItem = (key: string, kind: string, {title, meta, links, pages = [], action}: RowView) => {
		const titleId = `${baseId}-${key}-title`;
		const metaIds = meta.map((_, index) => `${baseId}-${key}-meta-${index}`);
		return (
			<li key={key} className={styles.item} data-kind={kind} data-flx="premium.operator-pass-holdings-section.pass">
				<div className={styles.itemText} data-flx="premium.operator-pass-holdings-section.pass-text">
					<p id={titleId} className={styles.line} data-flx="premium.operator-pass-holdings-section.row-title">
						{title}
					</p>
					{meta.map((part, index) => (
						<p
							key={index}
							id={metaIds[index]}
							className={styles.meta}
							data-flx="premium.operator-pass-holdings-section.meta"
						>
							{part}
						</p>
					))}
					{links.length > 0 && (
						<div className={styles.links} data-flx="premium.operator-pass-holdings-section.links">
							{links.map((link, index) => (
								<p key={index} className={styles.meta} data-flx="premium.operator-pass-holdings-section.link-line">
									{link}
								</p>
							))}
						</div>
					)}
					{pages.length > 0 && (
						<div className={styles.pages} data-flx="premium.operator-pass-holdings-section.pages">
							{pages}
						</div>
					)}
				</div>
				{action && (
					<div className={styles.actions} data-flx="premium.operator-pass-holdings-section.pass-actions">
						{action([titleId, ...metaIds].join(' '))}
					</div>
				)}
			</li>
		);
	};
	const incoming = rows.filter(
		(row): row is Extract<OperatorPassPassRow, {kind: 'incoming'}> => row.kind === 'incoming',
	);
	const owned = rows.filter((row): row is Exclude<OperatorPassPassRow, {kind: 'incoming'}> => row.kind !== 'incoming');
	return (
		<SettingsSection
			id="operator-pass-passes"
			title={passCount > 0 ? i18n._(PASS_COUNT_DESCRIPTOR, {count: passCount}) : OPERATOR_PASS_NAME}
			actions={
				<Button
					small
					variant="secondary"
					onClick={() => OperatorPassModalCommands.openCheckoutModal()}
					data-flx="premium.operator-pass-holdings-section.button.get-another-pass"
				>
					{i18n._(GET_ANOTHER_PASS_DESCRIPTOR)}
				</Button>
			}
			data-flx="premium.operator-pass-holdings-section.section"
		>
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
								links: [],
								action: (describedBy) => (
									<Button
										small
										variant="primary"
										onClick={() => openOperatorPassOfferModal(row.offer.id)}
										aria-describedby={describedBy}
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
			{visionaryState === 'available' && (
				<div className={styles.group} data-flx="premium.operator-pass-holdings-section.visionary">
					<ul className={styles.list} data-flx="premium.operator-pass-holdings-section.visionary-list">
						{renderItem('visionary', 'visionary', {
							title: i18n._(VISIONARY_CARD_DESCRIPTOR),
							meta: [],
							links: [],
							action: (describedBy) => (
								<Button
									small
									variant="primary"
									onClick={openOperatorPassVisionaryModal}
									aria-describedby={describedBy}
									data-flx="premium.operator-pass-holdings-section.button.turn-on-visionary"
								>
									{i18n._(OPERATOR_PASS_TURN_ON_DESCRIPTOR)}
								</Button>
							),
						})}
					</ul>
				</div>
			)}
			{visionaryState === 'ended' && (
				<p className={styles.meta} data-flx="premium.operator-pass-holdings-section.visionary-ended">
					{i18n._(VISIONARY_ENDED_DESCRIPTOR)}
				</p>
			)}
			{owned.length > 0 && (
				<div className={styles.group} data-flx="premium.operator-pass-holdings-section.owned">
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
								href={`${Routes.operatorPassTerms()}#8-when-we-remove-the-bonus`}
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
			{loadFailed}
			<ExternalLink
				href={`${Routes.operatorPassInfo()}#pass-it-on`}
				className={styles.howLink}
				data-flx="premium.operator-pass-holdings-section.link.how-passes-work"
			>
				{i18n._(HOW_PASSES_WORK_DESCRIPTOR)}
			</ExternalLink>
		</SettingsSection>
	);
});
