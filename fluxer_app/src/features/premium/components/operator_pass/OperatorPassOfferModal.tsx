// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {OPERATOR_PASS_NAME, PRODUCT_LABS_COMMUNITY_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {isolateBidi, isolateChannelNames} from '@app/features/i18n/utils/BidiIsolate';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import {
	asksOperatorPassLabsGreeting,
	getStoredOperatorPassLabsGreeting,
	OperatorPassLabsGreetingField,
} from '@app/features/premium/components/operator_pass/OperatorPassLabsGreetingField';
import styles from '@app/features/premium/components/operator_pass/OperatorPassOfferModal.module.css';
import {OperatorPassWallNameField} from '@app/features/premium/components/operator_pass/OperatorPassWallNameField';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import {
	focusFirstInvalidOperatorPassField,
	focusOperatorPassField,
	formatOperatorPassDateTime,
	formatOperatorPassUnlockDay,
	getOperatorPassErrorCode,
	getOperatorPassOfferUserName,
	getOperatorPassWallNameProblem,
	hasOperatorPassTimePassed,
	normalizeOperatorPassWallName,
	type OperatorPassWallNameProblem,
} from '@app/features/premium/utils/OperatorPassUtils';
import {
	OPERATOR_PASS_LISTING_UNCHANGED_DESCRIPTOR,
	OPERATOR_PASS_OFFER_BONUS_DESCRIPTOR,
	OPERATOR_PASS_REQUIRED_BOX_DESCRIPTOR,
} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Spinner} from '@app/features/ui/components/Spinner';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import Users from '@app/features/user/state/Users';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {OperatorPassOfferResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';

const OFFER_TITLE_DESCRIPTOR = msg({
	message: '{operatorPassName} from {senderName}',
	comment:
		'Title of the dialog where you accept or decline an Operator Pass another person passed on to you. {senderName} is their display name. Operator Pass is a product name and stays untranslated.',
});
const OFFER_ARRIVES_DESCRIPTOR = msg({
	message: 'If you accept, it arrives on {date}, unless the sender takes the offer back first.',
	comment:
		'Line in the dialog for an Operator Pass someone passed on to you, while the waiting period after payment runs. It is the pass. {date} is a date such as Nov 23, 2026.',
});
const OFFER_ARRIVES_NOW_DESCRIPTOR = msg({
	message: 'It arrives as soon as you accept.',
	comment:
		'Line in the dialog for an Operator Pass someone passed on to you, once the waiting period after payment has ended. It is the pass.',
});
const ACCEPT_DESCRIPTOR = msg({
	message: 'Accept',
	comment: 'Button that accepts an incoming Operator Pass offer.',
});
const DECLINE_DESCRIPTOR = msg({
	message: 'Decline',
	comment: 'Button that declines an incoming Operator Pass offer.',
});
const ACCEPTED_DESCRIPTOR = msg({
	message: 'Accepted. It arrives on {date}, unless the sender takes the offer back first.',
	comment:
		'Toast after accepting an Operator Pass offer while the waiting period after payment runs. It is the pass. {date} is a date such as Nov 23, 2026.',
});
const ARRIVED_DESCRIPTOR = msg({
	message: 'The pass is yours now.',
	comment: 'Toast after accepting an Operator Pass offer that moved to your account right away.',
});
const DECLINED_DESCRIPTOR = msg({
	message: 'Pass declined.',
	comment: 'Toast after declining an Operator Pass someone offered you.',
});
const OFFER_GONE_DESCRIPTOR = msg({
	message: 'This offer is no longer open.',
	comment: 'Notice in the incoming Operator Pass offer dialog when the sender took the offer back or it ended.',
});
const OFFER_FAILED_DESCRIPTOR = msg({
	message: "Couldn't answer the offer. Try again.",
	comment: 'Notice when accepting or declining an incoming Operator Pass offer failed.',
});
const OFFER_LOAD_FAILED_DESCRIPTOR = msg({
	message: "Couldn't load the offer. Try again later.",
	comment: 'Notice when the incoming Operator Pass offer could not be loaded.',
});

const logger = new Logger('OperatorPassOfferModal');

const WALL_NAME_FIELD_ID = 'operator-pass-offer-wall-name';
const TERMS_BOX_ID = 'operator-pass-offer-terms';
const TERMS_ERROR_ID = 'operator-pass-offer-terms-error';
const LABS_GREETING_FIELD_ID = 'operator-pass-offer-labs-greeting';

type Notice = 'gone' | 'failed' | 'load_failed';

const NOTICE_DESCRIPTORS: Record<Notice, MessageDescriptor> = {
	gone: OFFER_GONE_DESCRIPTOR,
	failed: OFFER_FAILED_DESCRIPTOR,
	load_failed: OFFER_LOAD_FAILED_DESCRIPTOR,
};

const OperatorPassOfferModal = observer(function OperatorPassOfferModal({offerId}: {offerId: string}) {
	const {i18n} = useLingui();
	const [offer, setOffer] = useState<OperatorPassOfferResponse | null>(() => OperatorPassHoldings.findOffer(offerId));
	const [acceptedTerms, setAcceptedTerms] = useState(false);
	const [termsMissing, setTermsMissing] = useState(false);
	const [wallName, setWallName] = useState(() => Users.currentUser?.displayName ?? '');
	const [wallOptOut, setWallOptOut] = useState(false);
	const [wallNameProblem, setWallNameProblem] = useState<OperatorPassWallNameProblem | null>(null);
	const [labsGreeting, setLabsGreeting] = useState(getStoredOperatorPassLabsGreeting);
	const asksLabsGreeting = asksOperatorPassLabsGreeting();
	const [submitting, setSubmitting] = useState<'accept' | 'decline' | null>(null);
	const [notice, setNotice] = useState<Notice | null>(null);
	const [wallChosen, setWallChosen] = useState<boolean | null>(null);
	const [revealWallField, setRevealWallField] = useState(false);
	const isOperator = (OperatorPassHoldings.count ?? 0) > 0 || Users.currentUser?.operatorPassSince != null;
	const asksWallChoice = revealWallField || !(wallChosen ?? isOperator);
	useEffect(() => {
		let active = true;
		OperatorPassCommands.fetchState()
			.then((state) => {
				if (active) setWallChosen(state.holder?.wall_chosen ?? false);
			})
			.catch((error) => logger.warn('Failed to load the Operator Pass state for an offer', error));
		return () => {
			active = false;
		};
	}, []);
	useEffect(() => {
		let active = true;
		OperatorPassCommands.fetchOffer(offerId)
			.then((result) => {
				if (!active) return;
				setOffer(result);
				if (result.status !== 'pending') setNotice('gone');
			})
			.catch((error) => {
				logger.warn('Failed to load the Operator Pass offer', error);
				if (active) setNotice((current) => current ?? 'load_failed');
			});
		return () => {
			active = false;
		};
	}, [offerId]);
	const pending = offer?.status === 'pending';
	const handleError = (error: unknown) => {
		logger.error('Operator Pass offer answer failed', error);
		const code = getOperatorPassErrorCode(error);
		if (
			code === APIErrorCodes.OPERATOR_PASS_WALL_NAME_REQUIRED ||
			code === APIErrorCodes.OPERATOR_PASS_WALL_NAME_REJECTED
		) {
			setRevealWallField(true);
			setWallNameProblem(code === APIErrorCodes.OPERATOR_PASS_WALL_NAME_REJECTED ? 'rejected' : 'required');
			focusOperatorPassField(WALL_NAME_FIELD_ID);
			return;
		}
		if (code === APIErrorCodes.OPERATOR_PASS_TERMS_NOT_ACCEPTED) {
			setAcceptedTerms(false);
			setTermsMissing(true);
			focusOperatorPassField(TERMS_BOX_ID);
			return;
		}
		setNotice(
			code === APIErrorCodes.UNKNOWN_OPERATOR_PASS_OFFER ||
				code === APIErrorCodes.OPERATOR_PASS_OFFER_UNAVAILABLE ||
				code === APIErrorCodes.OPERATOR_PASS_RECIPIENT_NOT_FRIEND
				? 'gone'
				: 'failed',
		);
	};
	const handleAccept = async () => {
		if (!offer || submitting) return;
		const problem = asksWallChoice ? getOperatorPassWallNameProblem(wallName, wallOptOut) : null;
		setWallNameProblem(problem);
		setTermsMissing(!acceptedTerms);
		if (
			focusFirstInvalidOperatorPassField([
				{id: WALL_NAME_FIELD_ID, invalid: problem !== null},
				{id: TERMS_BOX_ID, invalid: !acceptedTerms},
			])
		) {
			return;
		}
		setSubmitting('accept');
		setNotice(null);
		try {
			const result = await OperatorPassCommands.acceptOffer(offer.id, {
				accept_terms: true,
				...(asksWallChoice
					? {wall_name: wallOptOut ? null : normalizeOperatorPassWallName(wallName), wall_opt_out: wallOptOut}
					: {}),
				...(asksLabsGreeting ? {labs_greeting: labsGreeting} : {}),
			});
			ToastCommands.success(
				result.status === 'completed'
					? i18n._(ARRIVED_DESCRIPTOR)
					: i18n._(ACCEPTED_DESCRIPTOR, {date: formatOperatorPassUnlockDay(result.arrives_at)}),
			);
			ModalCommands.popWithKey(`${OFFER_MODAL_KEY_PREFIX}${offerId}`);
		} catch (error) {
			handleError(error);
		} finally {
			setSubmitting(null);
		}
	};
	const handleDecline = async () => {
		if (!offer || submitting) return;
		setSubmitting('decline');
		setNotice(null);
		try {
			await OperatorPassCommands.declineOffer(offer.id);
			ToastCommands.success(i18n._(DECLINED_DESCRIPTOR));
			ModalCommands.popWithKey(`${OFFER_MODAL_KEY_PREFIX}${offerId}`);
		} catch (error) {
			handleError(error);
		} finally {
			setSubmitting(null);
		}
	};
	const noticeText = notice ? i18n._(NOTICE_DESCRIPTORS[notice]) : null;
	return (
		<Modal.Root size="small" centered data-flx="premium.operator-pass-offer-modal.modal-root">
			<Modal.Header
				title={
					offer
						? i18n._(OFFER_TITLE_DESCRIPTOR, {
								operatorPassName: OPERATOR_PASS_NAME,
								senderName: getOperatorPassOfferUserName(offer.sender),
							})
						: OPERATOR_PASS_NAME
				}
				data-flx="premium.operator-pass-offer-modal.modal-header"
			/>
			<Modal.Content data-flx="premium.operator-pass-offer-modal.modal-content">
				<Modal.ContentLayout data-flx="premium.operator-pass-offer-modal.content-layout">
					{noticeText && (
						<div role="alert" data-flx="premium.operator-pass-offer-modal.notice-alert">
							<WarningAlert data-flx="premium.operator-pass-offer-modal.notice">{noticeText}</WarningAlert>
						</div>
					)}
					{!offer && !notice && (
						<div className={styles.loading} data-flx="premium.operator-pass-offer-modal.loading">
							<Spinner data-flx="premium.operator-pass-offer-modal.spinner" />
						</div>
					)}
					{offer && (
						<>
							<Modal.Description
								title={
									hasOperatorPassTimePassed(offer.arrives_at) ? undefined : formatOperatorPassDateTime(offer.arrives_at)
								}
								data-flx="premium.operator-pass-offer-modal.body"
							>
								{`${isOperator ? '' : `${isolateChannelNames(i18n._(OPERATOR_PASS_OFFER_BONUS_DESCRIPTOR, {labsName: isolateBidi(PRODUCT_LABS_COMMUNITY_NAME)}))} `}${
									hasOperatorPassTimePassed(offer.arrives_at)
										? i18n._(OFFER_ARRIVES_NOW_DESCRIPTOR)
										: i18n._(OFFER_ARRIVES_DESCRIPTOR, {date: formatOperatorPassUnlockDay(offer.arrives_at)})
								}`}
							</Modal.Description>
							{pending &&
								(!asksWallChoice ? (
									<p className={styles.note} data-flx="premium.operator-pass-offer-modal.listing-unchanged">
										{i18n._(OPERATOR_PASS_LISTING_UNCHANGED_DESCRIPTOR)}
									</p>
								) : (
									<OperatorPassWallNameField
										id={WALL_NAME_FIELD_ID}
										name={wallName}
										optOut={wallOptOut}
										problem={wallNameProblem}
										disabled={submitting != null}
										context="offer"
										onNameChange={(value) => {
											setWallName(value);
											if (wallNameProblem) setWallNameProblem(getOperatorPassWallNameProblem(value, wallOptOut));
										}}
										onOptOutChange={(value) => {
											setWallOptOut(value);
											if (value) setWallNameProblem(null);
										}}
										data-flx="premium.operator-pass.operator-pass-offer-modal.operator-pass-wall-name-field"
									/>
								))}
							{pending && asksLabsGreeting && (
								<OperatorPassLabsGreetingField
									id={LABS_GREETING_FIELD_ID}
									checked={labsGreeting}
									disabled={submitting != null}
									onChange={setLabsGreeting}
									data-flx="premium.operator-pass-offer-modal.labs-greeting"
								/>
							)}
							{pending && (
								<div
									id={TERMS_BOX_ID}
									className={clsx(styles.consentBox, termsMissing && styles.consentBoxError)}
									data-flx="premium.operator-pass-offer-modal.terms"
								>
									<Checkbox
										className={styles.consentCheckbox}
										checked={acceptedTerms}
										onChange={(value) => {
											setAcceptedTerms(value);
											if (value) setTermsMissing(false);
										}}
										disabled={submitting != null}
										aria-describedby={termsMissing ? TERMS_ERROR_ID : undefined}
										data-flx="premium.operator-pass-offer-modal.checkbox.terms"
									>
										<span className={styles.consentLabel} data-flx="premium.operator-pass-offer-modal.terms-label">
											<Trans comment="Required checkbox next to Accept in the dialog for an Operator Pass someone passed on to you. Consumer law text, translate faithfully and keep every condition. The linked text names the Operator Pass terms document. {OPERATOR_PASS_NAME} is a product name and stays untranslated. The buyer is the person who paid for the pass. Reversed means charged back through a bank or card company.">
												I accept the{' '}
												<ExternalLink
													href={Routes.operatorPassTerms()}
													data-flx="premium.operator-pass-offer-modal.terms-link"
												>
													{OPERATOR_PASS_NAME} terms
												</ExternalLink>
												. Each pass can be passed on only once, so you can't pass this one on again. It ends if the
												buyer's payment for it is refunded or reversed, because the purchase stays theirs.
											</Trans>
										</span>
									</Checkbox>
									{termsMissing && (
										<p
											id={TERMS_ERROR_ID}
											className={styles.consentError}
											data-flx="premium.operator-pass-offer-modal.terms-error"
										>
											{i18n._(OPERATOR_PASS_REQUIRED_BOX_DESCRIPTOR)}
										</p>
									)}
								</div>
							)}
						</>
					)}
				</Modal.ContentLayout>
			</Modal.Content>
			{pending ? (
				<Modal.Footer data-flx="premium.operator-pass-offer-modal.modal-footer">
					<Button
						variant="secondary"
						onClick={() => void handleDecline()}
						submitting={submitting === 'decline'}
						disabled={submitting === 'accept'}
						data-flx="premium.operator-pass-offer-modal.button.decline"
					>
						{i18n._(DECLINE_DESCRIPTOR)}
					</Button>
					<Button
						variant="primary"
						onClick={() => void handleAccept()}
						submitting={submitting === 'accept'}
						disabled={submitting === 'decline'}
						data-flx="premium.operator-pass-offer-modal.button.accept"
					>
						{i18n._(ACCEPT_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			) : (
				<Modal.DismissFooter data-flx="premium.operator-pass-offer-modal.dismiss-footer" />
			)}
		</Modal.Root>
	);
});

const OFFER_MODAL_KEY_PREFIX = 'operator-pass-offer-';

export function openOperatorPassOfferModal(offerId: string): void {
	ModalCommands.pushWithKey(
		modal(() => <OperatorPassOfferModal offerId={offerId} data-flx="premium.operator-pass-offer-modal.open" />),
		`${OFFER_MODAL_KEY_PREFIX}${offerId}`,
	);
}
