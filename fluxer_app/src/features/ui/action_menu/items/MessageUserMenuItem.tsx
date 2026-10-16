// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {OPEN_DM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Relationships from '@app/features/relationship/state/Relationships';
import {MessageUserIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import type {User} from '@app/features/user/models/User';
import {blockedDmConfirmModal, openDMChannelOrShowError} from '@app/features/user/utils/DMActionUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const MESSAGE_DESCRIPTOR = msg({
	message: 'Message',
	context: 'message-action',
	comment: 'Action that opens a DM conversation with the selected user.',
});

interface MessageUserMenuItemProps {
	user: User;
	onClose: () => void;
}

export const MessageUserMenuItem: React.FC<MessageUserMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const relationshipType = Relationships.getRelationship(user.id)?.type;
	const isBlocked = relationshipType === RelationshipTypes.BLOCKED;
	const displayName = NicknameUtils.getNickname(user);
	const openDmChannel = useCallback(async () => {
		await openDMChannelOrShowError(user.id);
	}, [user.id]);
	const handleMessageUser = useCallback(async () => {
		onClose();
		await openDmChannel();
	}, [onClose, openDmChannel]);
	const handleOpenBlockedDm = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			blockedDmConfirmModal(
				i18n,
				displayName,
				openDmChannel,
				'ui.action-menu.items.message-user-menu-item.handle-open-blocked-dm.confirm-modal',
			),
		);
	}, [onClose, openDmChannel, displayName, i18n]);
	if (RuntimeConfig.directMessagesDisabled) {
		return null;
	}
	return (
		<MenuItem
			icon={<MessageUserIcon size={16} data-flx="ui.action-menu.items.message-user-menu-item.message-user-icon" />}
			onClick={isBlocked ? handleOpenBlockedDm : handleMessageUser}
			data-flx="ui.action-menu.items.message-user-menu-item.menu-item.open-blocked-dm"
		>
			{isBlocked ? i18n._(OPEN_DM_DESCRIPTOR) : i18n._(MESSAGE_DESCRIPTOR)}
		</MenuItem>
	);
});
