// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {
	type RecipientItem,
	RecipientList,
	useRecipientItems,
} from '@app/features/app/components/dialogs/shared/RecipientList';
import selectorStyles from '@app/features/app/components/dialogs/shared/SelectorModalStyles.module.css';
import {SEARCH_FRIENDS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import styles from '@app/features/premium/components/operator_pass/OperatorPassPassItOnModal.module.css';
import {OPERATOR_PASS_RECOGNITION_DESCRIPTOR} from '@app/features/premium/utils/OperatorPassRecognitionMessages';
import {getOperatorPassErrorCode, getOperatorPassOfferUserName} from '@app/features/premium/utils/OperatorPassUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {OperatorPassUnitResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useMemo, useState} from 'react';

export const PASS_IT_ON_DESCRIPTOR = msg({
	message: 'Pass it on',
	comment:
		'Button that offers one of your Operator Passes to another person, and the title of the dialog where you pick that person. Pass it on is the name of the feature, translate it as a short natural phrase for handing something on.',
});
const PASS_IT_ON_BODY_DESCRIPTOR = msg({
	message: 'Pick a friend to pass it on to. They can accept or decline, and you can take it back until it moves.',
	comment:
		'Text in the dialog where you pick who gets one of your Operator Passes. Only friends can get a pass. They means the friend you pick. Take it back means canceling your offer. It moves means the pass moves to their account. Never word it as a present.',
});
const CHOOSE_DESCRIPTOR = msg({
	message: 'Choose',
	comment:
		'Button next to a person in the list of people an Operator Pass can be passed on to, and next to the username field in the same dialog.',
});
const CONFIRM_TITLE_DESCRIPTOR = msg({
	message: 'Pass it on to {name}?',
	comment:
		'Title of the confirmation before offering an Operator Pass to someone. {name} is the display name of that person.',
});
const CONFIRM_BODY_DESCRIPTOR = msg({
	message: 'It moves as soon as they accept. You can take it back until then.',
	comment:
		'Body of the confirmation before offering an Operator Pass to someone. It is the pass, they is the person you offer it to. Take it back means canceling your offer.',
});
const CONFIRM_LAST_TITLE_DESCRIPTOR = msg({
	message: 'Pass on your last pass?',
	comment: 'Title of the confirmation before offering the only Operator Pass you have left to someone else.',
});
const CONFIRM_LAST_BODY_DESCRIPTOR = msg({
	message: 'Your badge and wall listing go as soon as {name} accepts.',
	comment:
		"Body of the confirmation before offering your last Operator Pass. The badge is the Operator badge, the wall listing is your entry on the Operators' wall. {name} is the display name of the person you offer it to.",
});
const CONFIRM_LAST_PRIMARY_DESCRIPTOR = msg({
	message: 'Pass on my last pass',
	comment: 'Button that confirms offering the only Operator Pass you have left to someone else.',
});
const SEND_OFFER_DESCRIPTOR = msg({
	message: 'Send offer',
	comment: 'Button that confirms offering an Operator Pass to someone. They can then accept or decline it.',
});
const LAST_PASS_NOW_DESCRIPTOR = msg({
	message: 'Your other passes are all on offer. This is your last one.',
	comment:
		'Error after trying to pass on an Operator Pass when all your other passes are already offered to someone. A confirmation for passing on your last pass opens next.',
});
const OFFER_SENT_DESCRIPTOR = msg({
	message: 'Offer sent to {name}.',
	comment: 'Toast after offering an Operator Pass to someone. {name} is a display name.',
});
const NOT_TRANSFERABLE_DESCRIPTOR = msg({
	message: "Passes you received or got with Visionary can't be passed on.",
	comment:
		'Error when you try to pass on an Operator Pass that cannot move. Visionary is a product name and stays untranslated.',
});
const OFFER_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'This pass cannot be passed on to that account right now.',
	comment: 'Error when an Operator Pass cannot be offered to the chosen account right now.',
});
const NOT_FRIEND_DESCRIPTOR = msg({
	message: 'You can only pass this on to one of your friends on Fluxer.',
	comment: 'Error when you try to pass an Operator Pass on to someone you are not friends with.',
});
const NO_FRIENDS_DESCRIPTOR = msg({
	message: 'You can only pass a pass on to a friend. Add the person as a friend first, then come back here.',
	comment:
		'Shown in the Pass it on dialog when you have no friends yet. Operator Passes can only go to friends so they cannot be sent to strangers.',
});
const OFFER_FAILED_DESCRIPTOR = msg({
	message: "Couldn't send the offer. Try again.",
	comment: 'Error when offering an Operator Pass failed for an unknown reason.',
});

function offerErrorMessage(error: unknown): typeof OFFER_FAILED_DESCRIPTOR {
	switch (getOperatorPassErrorCode(error)) {
		case APIErrorCodes.OPERATOR_PASS_NOT_TRANSFERABLE:
			return NOT_TRANSFERABLE_DESCRIPTOR;
		case APIErrorCodes.OPERATOR_PASS_RECIPIENT_NOT_FRIEND:
			return NOT_FRIEND_DESCRIPTOR;
		case APIErrorCodes.OPERATOR_PASS_OFFER_UNAVAILABLE:
		case APIErrorCodes.UNKNOWN_USER:
			return OFFER_UNAVAILABLE_DESCRIPTOR;
		default:
			return OFFER_FAILED_DESCRIPTOR;
	}
}

const OperatorPassPassItOnModal = observer(function OperatorPassPassItOnModalComponent({
	pass,
	onlyPass,
}: {
	pass: OperatorPassUnitResponse;
	onlyPass: boolean;
}) {
	const {i18n} = useLingui();
	const allRecipients = useRecipientItems();
	const recipients = useMemo(() => allRecipients.filter((item) => item.type !== 'group_dm'), [allRecipients]);
	const [searchQuery, setSearchQuery] = useState('');
	const handleChoose = (item: RecipientItem) => {
		confirm(NicknameUtils.getDisplayName(item.user), item.user.id, onlyPass);
	};
	const confirm = (name: string, recipientId: string, lastPass: boolean) => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={lastPass ? i18n._(CONFIRM_LAST_TITLE_DESCRIPTOR) : i18n._(CONFIRM_TITLE_DESCRIPTOR, {name})}
					description={`${i18n._(lastPass ? CONFIRM_LAST_BODY_DESCRIPTOR : CONFIRM_BODY_DESCRIPTOR, {name})} ${i18n._(OPERATOR_PASS_RECOGNITION_DESCRIPTOR)}`}
					primaryText={i18n._(lastPass ? CONFIRM_LAST_PRIMARY_DESCRIPTOR : SEND_OFFER_DESCRIPTOR)}
					primaryVariant={lastPass ? 'danger' : 'primary'}
					onPrimary={async () => {
						try {
							const offer = await OperatorPassCommands.createOffer(pass.id, recipientId, lastPass);
							ToastCommands.success(
								i18n._(OFFER_SENT_DESCRIPTOR, {name: getOperatorPassOfferUserName(offer.recipient) || name}),
							);
							ModalCommands.popByType(OperatorPassPassItOnModal);
						} catch (error) {
							if (
								!lastPass &&
								getOperatorPassErrorCode(error) === APIErrorCodes.OPERATOR_PASS_LAST_PASS_CONFIRMATION_REQUIRED
							) {
								ToastCommands.error(i18n._(LAST_PASS_NOW_DESCRIPTOR));
								confirm(name, recipientId, true);
								return;
							}
							ToastCommands.error(i18n._(offerErrorMessage(error)));
						}
					}}
					data-flx="premium.operator-pass-pass-it-on-modal.confirm-modal"
				/>
			)),
		);
	};
	return (
		<Modal.Root
			size="small"
			centered
			className={styles.root}
			data-flx="premium.operator-pass-pass-it-on-modal.modal-root"
		>
			<Modal.Header
				title={i18n._(PASS_IT_ON_DESCRIPTOR)}
				data-flx="premium.operator-pass-pass-it-on-modal.modal-header"
			>
				<Modal.Description data-flx="premium.operator-pass-pass-it-on-modal.description">
					{i18n._(PASS_IT_ON_BODY_DESCRIPTOR)}
				</Modal.Description>
				<div className={selectorStyles.headerSearch} data-flx="premium.operator-pass-pass-it-on-modal.search">
					<Input
						value={searchQuery}
						onChange={(event) => setSearchQuery(event.target.value)}
						placeholder={i18n._(SEARCH_FRIENDS_DESCRIPTOR)}
						leftIcon={
							<MagnifyingGlassIcon
								size={remFromPx(20)}
								weight="bold"
								className={selectorStyles.searchIcon}
								data-flx="premium.operator-pass-pass-it-on-modal.search-icon"
							/>
						}
						className={selectorStyles.headerSearchInput}
						data-flx="premium.operator-pass-pass-it-on-modal.input.search"
					/>
				</div>
			</Modal.Header>
			<Modal.Content
				className={selectorStyles.selectorContent}
				data-flx="premium.operator-pass-pass-it-on-modal.modal-content"
			>
				<RecipientList
					recipients={recipients}
					sendingTo={new Set()}
					sentTo={new Map()}
					onSend={handleChoose}
					defaultButtonLabel={i18n._(CHOOSE_DESCRIPTOR)}
					scrollerKey="operator-pass-pass-it-on-modal-scroller"
					searchQuery={searchQuery}
					onSearchQueryChange={setSearchQuery}
					showSearchInput={false}
					emptyText={i18n._(NO_FRIENDS_DESCRIPTOR)}
					data-flx="premium.operator-pass-pass-it-on-modal.recipient-list"
				/>
			</Modal.Content>
			<Modal.DismissFooter data-flx="premium.operator-pass-pass-it-on-modal.dismiss-footer" />
		</Modal.Root>
	);
});

export function openOperatorPassPassItOnModal(pass: OperatorPassUnitResponse, onlyPass: boolean): void {
	ModalCommands.push(
		modal(() => (
			<OperatorPassPassItOnModal
				pass={pass}
				onlyPass={onlyPass}
				data-flx="premium.operator-pass-pass-it-on-modal.open"
			/>
		)),
	);
}
