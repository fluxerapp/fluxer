// SPDX-License-Identifier: AGPL-3.0-or-later

import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import styles from '@app/features/channel/components/direct_message/GroupDMWelcomeSection.module.css';
import {EditGroupModal} from '@app/features/channel/components/modals/EditGroupModal';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {isGroupDmFull} from '@app/features/channel/utils/GroupDmUtils';
import {AddFriendsToGroupModal} from '@app/features/relationship/components/modals/AddFriendsToGroupModal';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {GroupDMContextMenu} from '@app/features/ui/action_menu/GroupDMContextMenu';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {useLingui} from '@lingui/react';
import {Trans} from '@lingui/react/macro';
import {NotePencilIcon, UserPlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

interface GroupDMWelcomeSectionProps {
	channel: Channel;
}

export const GroupDMWelcomeSection: React.FC<GroupDMWelcomeSectionProps> = observer(({channel}) => {
	useLingui();
	const displayName = ChannelUtils.getDMDisplayName(channel);
	const isGroupDMFull = isGroupDmFull(channel);
	const handleOpenEditGroup = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<EditGroupModal
					channelId={channel.id}
					data-flx="channel.direct-message.group-dm-welcome-section.handle-open-edit-group.edit-group-modal"
				/>
			)),
		);
	}, [channel.id]);
	const handleAddFriends = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<AddFriendsToGroupModal
					channelId={channel.id}
					data-flx="channel.direct-message.group-dm-welcome-section.handle-add-friends.add-friends-to-group-modal"
				/>
			)),
		);
	}, [channel.id]);
	const handleGroupContextMenu = useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<GroupDMContextMenu
					channel={channel}
					onClose={onClose}
					data-flx="channel.direct-message.group-dm-welcome-section.handle-group-context-menu.group-dm-context-menu"
				/>
			));
		},
		[channel],
	);
	return (
		<div className={styles.welcomeSection} data-flx="channel.direct-message.group-dm-welcome-section.welcome-section">
			<button
				type="button"
				className={styles.profileSection}
				onContextMenu={handleGroupContextMenu}
				aria-haspopup="menu"
				data-flx="channel.direct-message.group-dm-welcome-section.profile-section.group-context-menu.button"
			>
				<GroupDMAvatar
					channel={channel}
					size={80}
					data-flx="channel.direct-message.group-dm-welcome-section.group-dm-avatar"
				/>
				<span className={styles.groupName} data-flx="channel.direct-message.group-dm-welcome-section.group-name">
					{displayName}
				</span>
			</button>
			<p className={styles.welcomeText} data-flx="channel.direct-message.group-dm-welcome-section.welcome-text">
				<Trans>
					Welcome to <strong data-flx="channel.direct-message.group-dm-welcome-section.strong">{displayName}</strong>.
					Add friends to get the group going.
				</Trans>
			</p>
			<div className={styles.actions} data-flx="channel.direct-message.group-dm-welcome-section.actions">
				<Button
					variant="secondary"
					leftIcon={
						<NotePencilIcon
							size={remFromPx(18)}
							weight="bold"
							data-flx="channel.direct-message.group-dm-welcome-section.note-pencil-icon"
						/>
					}
					onClick={handleOpenEditGroup}
					fitContent
					data-flx="channel.direct-message.group-dm-welcome-section.button.open-edit-group"
				>
					<Trans>Edit group</Trans>
				</Button>
				{!isGroupDMFull && (
					<Button
						variant="primary"
						leftIcon={
							<UserPlusIcon
								size={remFromPx(18)}
								weight="bold"
								data-flx="channel.direct-message.group-dm-welcome-section.user-plus-icon"
							/>
						}
						onClick={handleAddFriends}
						fitContent
						data-flx="channel.direct-message.group-dm-welcome-section.button.add-friends"
					>
						<Trans>Add friends to group</Trans>
					</Button>
				)}
			</div>
		</div>
	);
});
