// SPDX-License-Identifier: AGPL-3.0-or-later

import {showDmActionErrorModal} from '@app/features/app/components/alerts/DmActionErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {
	ADD_NOTE_DESCRIPTOR,
	BLOCKED_USER_DM_WARNING_DESCRIPTOR,
	CHANGE_FRIEND_NICKNAME_DESCRIPTOR,
	OPEN_DM_DESCRIPTOR,
	START_VOICE_CALL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ChangeFriendNicknameModal} from '@app/features/relationship/components/modals/ChangeFriendNicknameModal';
import {
	AddNoteIcon,
	ChangeNicknameIcon,
	MessageUserIcon,
	VoiceCallIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {hasActiveDirectCallWithUser} from '@app/features/voice/utils/PrivateCallMenuUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const ACTIONS_DESCRIPTOR = msg({
	message: 'Actions',
	comment: 'Submenu label that groups further actions.',
});
const MESSAGE_DESCRIPTOR = msg({
	message: 'Message',
	context: 'message-action',
	comment: 'Action that opens a DM conversation with the selected user.',
});
const logger = new Logger('ActionsSubmenu');

interface ActionsSubmenuProps {
	user: User;
	onClose: () => void;
	relationshipType?: number;
}

export const ActionsSubmenu: React.FC<ActionsSubmenuProps> = observer(({user, onClose, relationshipType}) => {
	const {i18n} = useLingui();
	const isBlocked = relationshipType === RelationshipTypes.BLOCKED;
	const displayName = NicknameUtils.getNickname(user);
	const openDmChannel = useCallback(async () => {
		try {
			await PrivateChannelCommands.openDMChannel(user.id);
		} catch (error) {
			logger.error('Failed to open DM channel:', error);
			showDmActionErrorModal(error);
		}
	}, [user.id]);
	const handleOpenDM = useCallback(async () => {
		onClose();
		await openDmChannel();
	}, [onClose, openDmChannel]);
	const handleOpenBlockedDm = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<ConfirmModal
					title={i18n._(OPEN_DM_DESCRIPTOR)}
					description={i18n._(BLOCKED_USER_DM_WARNING_DESCRIPTOR, {userName: displayName})}
					primaryText={i18n._(OPEN_DM_DESCRIPTOR)}
					primaryVariant="primary"
					onPrimary={openDmChannel}
					data-flx="ui.action-menu.items.actions-submenu.handle-open-blocked-dm.confirm-modal"
				/>
			)),
		);
	}, [onClose, openDmChannel, displayName, i18n]);
	const handleAddNote = useCallback(() => {
		ModalCommands.runAfterBottomSheetClose(onClose, () =>
			UserProfileCommands.openUserProfile(user.id, undefined, true),
		);
	}, [user.id, onClose]);
	const handleChangeFriendNickname = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<ChangeFriendNicknameModal
					user={user}
					data-flx="ui.action-menu.items.actions-submenu.handle-change-friend-nickname.change-friend-nickname-modal"
				/>
			)),
		);
	}, [user, onClose]);
	const handleStartVoiceCall = useCallback(async () => {
		onClose();
		try {
			await PrivateChannelCommands.ensureDMChannel(user.id);
			await PrivateChannelCommands.openDMChannel(user.id);
		} catch (error) {
			logger.error('Failed to start voice call:', error);
			showDmActionErrorModal(error);
		}
	}, [user.id, onClose]);
	return (
		<MenuItemSubmenu
			label={i18n._(ACTIONS_DESCRIPTOR)}
			render={() => (
				<MenuGroup data-flx="ui.action-menu.items.actions-submenu.menu-group">
					<MenuItem
						icon={<MessageUserIcon size={16} data-flx="ui.action-menu.items.actions-submenu.message-user-icon" />}
						onClick={isBlocked ? handleOpenBlockedDm : handleOpenDM}
						data-flx="ui.action-menu.items.actions-submenu.menu-item.open-blocked-dm"
					>
						{isBlocked ? i18n._(OPEN_DM_DESCRIPTOR) : i18n._(MESSAGE_DESCRIPTOR)}
					</MenuItem>
					<MenuItem
						icon={<AddNoteIcon size={16} data-flx="ui.action-menu.items.actions-submenu.add-note-icon" />}
						onClick={handleAddNote}
						data-flx="ui.action-menu.items.actions-submenu.menu-item.add-note"
					>
						{i18n._(ADD_NOTE_DESCRIPTOR)}
					</MenuItem>
					{relationshipType === RelationshipTypes.FRIEND && (
						<MenuItem
							icon={
								<ChangeNicknameIcon size={16} data-flx="ui.action-menu.items.actions-submenu.change-nickname-icon" />
							}
							onClick={handleChangeFriendNickname}
							data-flx="ui.action-menu.items.actions-submenu.menu-item.change-friend-nickname"
						>
							{i18n._(CHANGE_FRIEND_NICKNAME_DESCRIPTOR)}
						</MenuItem>
					)}
					{!user.bot && !hasActiveDirectCallWithUser(user.id) && (
						<MenuItem
							icon={<VoiceCallIcon size={16} data-flx="ui.action-menu.items.actions-submenu.voice-call-icon" />}
							onClick={handleStartVoiceCall}
							data-flx="ui.action-menu.items.actions-submenu.menu-item.start-voice-call"
						>
							{i18n._(START_VOICE_CALL_DESCRIPTOR)}
						</MenuItem>
					)}
				</MenuGroup>
			)}
			data-flx="ui.action-menu.items.actions-submenu.menu-item-submenu"
		/>
	);
});
