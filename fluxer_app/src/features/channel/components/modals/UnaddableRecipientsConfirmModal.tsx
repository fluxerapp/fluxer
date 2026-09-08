// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {CANCEL_DESCRIPTOR, CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Users from '@app/features/user/state/Users';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useMemo} from 'react';

const USER_NOT_FOUND_DESCRIPTOR = msg({
	message: 'User not found',
	comment: 'Reason chip in the unaddable recipients alert when the user record is missing.',
});
const YOU_CAN_T_MESSAGE_THIS_USER_DESCRIPTOR = msg({
	message: "You can't message this user",
	comment: 'Reason chip in the unaddable recipients alert when blocked or DMs are not allowed.',
});
const NOT_ON_YOUR_FRIENDS_LIST_DESCRIPTOR = msg({
	message: 'Not on your friends list',
	comment: 'Reason chip in the unaddable recipients alert when the user requires a friend relationship for group DMs.',
});
const DOESN_T_ALLOW_BEING_ADDED_TO_GROUP_DMS_DESCRIPTOR = msg({
	message: "Doesn't allow being added to group DMs",
	comment: 'Reason chip in the unaddable recipients alert when the user disabled group DM additions.',
});
const SOME_USERS_CAN_T_BE_ADDED_DESCRIPTOR = msg({
	message: "Some users can't be added",
	comment: 'Title of the unaddable recipients confirmation alert shown when creating a group DM.',
});
const CREATE_WITHOUT_THEM_DESCRIPTOR = msg({
	message: 'Create without them',
	comment:
		'Confirm button label on the unaddable recipients alert. Creates the group DM excluding the unaddable users.',
});

export type UnaddableRecipientReason = 'unknown_user' | 'blocked' | 'not_friends' | 'group_dm_add_disabled';

export interface UnaddableRecipient {
	userId: string;
	reason: UnaddableRecipientReason;
}

interface UnaddableRecipientsConfirmModalProps {
	unaddableRecipients: Array<UnaddableRecipient>;
	addableCount: number;
	onConfirm: () => Promise<void> | void;
}

function useReasonLabel(): (reason: UnaddableRecipientReason) => string {
	const {i18n} = useLingui();
	return (reason) => {
		switch (reason) {
			case 'unknown_user':
				return i18n._(USER_NOT_FOUND_DESCRIPTOR);
			case 'blocked':
				return i18n._(YOU_CAN_T_MESSAGE_THIS_USER_DESCRIPTOR);
			case 'not_friends':
				return i18n._(NOT_ON_YOUR_FRIENDS_LIST_DESCRIPTOR);
			case 'group_dm_add_disabled':
				return i18n._(DOESN_T_ALLOW_BEING_ADDED_TO_GROUP_DMS_DESCRIPTOR);
		}
	};
}

export const UnaddableRecipientsConfirmModal = observer(
	({unaddableRecipients, addableCount, onConfirm}: UnaddableRecipientsConfirmModalProps) => {
		const {i18n} = useLingui();
		const reasonLabel = useReasonLabel();
		const rows = useMemo(
			() =>
				unaddableRecipients.map(({userId, reason}) => {
					const user = Users.getUser(userId);
					const name = user?.displayName ?? userId;
					return {userId, name, reasonText: reasonLabel(reason)};
				}),
			[unaddableRecipients, reasonLabel],
		);
		const description = (
			<>
				<p data-flx="channel.unaddable-recipients-confirm-modal.p">
					<Trans>The following people can't be added to this group DM:</Trans>
				</p>
				<ul data-flx="channel.unaddable-recipients-confirm-modal.ul">
					{rows.map((row) => (
						<li key={row.userId} data-flx="channel.unaddable-recipients-confirm-modal.li">
							<strong data-flx="channel.unaddable-recipients-confirm-modal.strong">{row.name}</strong>
							<span data-flx="channel.unaddable-recipients-confirm-modal.span">{`: ${row.reasonText}`}</span>
						</li>
					))}
				</ul>
				{addableCount > 0 ? (
					<p data-flx="channel.unaddable-recipients-confirm-modal.p--2">
						<Trans>Create the group DM with the remaining {addableCount} recipient(s) and skip the others?</Trans>
					</p>
				) : (
					<p data-flx="channel.unaddable-recipients-confirm-modal.p--3">
						<Trans>No remaining recipients to create a group DM with.</Trans>
					</p>
				)}
			</>
		);
		if (addableCount === 0) {
			return (
				<ConfirmModal
					title={i18n._(SOME_USERS_CAN_T_BE_ADDED_DESCRIPTOR)}
					description={description}
					secondaryText={i18n._(CLOSE_DESCRIPTOR)}
					size="small"
					data-flx="channel.unaddable-recipients-confirm-modal.confirm-modal"
				/>
			);
		}
		return (
			<ConfirmModal
				title={i18n._(SOME_USERS_CAN_T_BE_ADDED_DESCRIPTOR)}
				description={description}
				primaryText={i18n._(CREATE_WITHOUT_THEM_DESCRIPTOR)}
				primaryVariant="primary"
				secondaryText={i18n._(CANCEL_DESCRIPTOR)}
				size="small"
				onPrimary={onConfirm}
				disableAutoDismiss={true}
				data-flx="channel.unaddable-recipients-confirm-modal.confirm-modal--2"
			/>
		);
	},
);
