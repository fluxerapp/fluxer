// SPDX-License-Identifier: AGPL-3.0-or-later

import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import userProfileModalStyles from '@app/features/user/components/modals/UserProfileModal.module.css';
import type {Profile} from '@app/features/user/models/Profile';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {clsx} from 'clsx';
import type React from 'react';
import {useRef} from 'react';

interface MutualFriendItemProps {
	user: User;
	profile: Profile | null;
	onClick: () => void;
	onContextMenu: (event: React.MouseEvent) => void;
	isContextMenuOpen: (target: EventTarget | null) => boolean;
}

export const MutualFriendItem: React.FC<MutualFriendItemProps> = ({
	user,
	profile,
	onClick,
	onContextMenu,
	isContextMenuOpen,
}) => {
	const itemRef = useRef<HTMLDivElement>(null);
	const isActive = isContextMenuOpen(itemRef.current);
	return (
		<div
			ref={itemRef}
			className={clsx(userProfileModalStyles.mutualFriendItem, isActive && userProfileModalStyles.active)}
			onClick={onClick}
			onKeyDown={(event) => {
				if (!isKeyboardActivationKey(event.key)) return;
				event.preventDefault();
				onClick();
			}}
			onContextMenu={onContextMenu}
			role="button"
			tabIndex={0}
			data-flx="user.user-profile-modal.mutual-friend-item.button.click"
		>
			<StatusAwareAvatar
				size={40}
				user={user}
				data-flx="user.user-profile-modal.mutual-friend-item.status-aware-avatar"
			/>
			<div
				className={userProfileModalStyles.mutualFriendInfo}
				data-flx="user.user-profile-modal.mutual-friend-item.div"
			>
				<span
					className={userProfileModalStyles.mutualFriendName}
					data-flx="user.user-profile-modal.mutual-friend-item.span"
				>
					{NicknameUtils.getNickname(user, profile?.guildId ?? null)}
				</span>
				<span
					className={userProfileModalStyles.mutualFriendUsername}
					data-flx="user.user-profile-modal.mutual-friend-item.span--2"
				>
					{NicknameUtils.formatTagForStreamerMode(user.tag)}
				</span>
			</div>
		</div>
	);
};
