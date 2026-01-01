/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useLingui} from '@lingui/react/macro';
import {CrownIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {autorun} from 'mobx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {isOfflineStatus} from '~/Constants';
import {PreloadableUserPopout} from '~/components/channel/PreloadableUserPopout';
import {UserTag} from '~/components/channel/UserTag';
import {CustomStatusDisplay} from '~/components/common/CustomStatusDisplay/CustomStatusDisplay';
import {GroupDMMemberContextMenu} from '~/components/uikit/ContextMenu/GroupDMContextMenu';
import {GuildMemberContextMenu} from '~/components/uikit/ContextMenu/GuildMemberContextMenu';
import {FocusRingWrapper} from '~/components/uikit/FocusRingWrapper';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useMemberListCustomStatus} from '~/hooks/useMemberListCustomStatus';
import {useMemberListPresence} from '~/hooks/useMemberListPresence';
import type {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ContextMenuStore from '~/stores/ContextMenuStore';
import TypingStore from '~/stores/TypingStore';
import * as NicknameUtils from '~/utils/NicknameUtils';
import styles from './MemberListItem.module.css';

interface MemberListItemProps {
	user: UserRecord;
	channelId: string;
	guildId?: string;
	isOwner?: boolean;
	roleColor?: string;
	displayName?: string;
	disableBackdrop?: boolean;
}

export const MemberListItem: React.FC<MemberListItemProps> = observer((props) => {
	const {t} = useLingui();
	const {user, channelId, guildId, isOwner = false, roleColor, displayName, disableBackdrop = false} = props;

	const itemRef = React.useRef<HTMLButtonElement>(null);
	const status = useMemberListPresence({
		guildId: guildId ?? '',
		channelId,
		userId: user.id,
		enabled: guildId !== undefined,
	});
	const memberListCustomStatus = useMemberListCustomStatus({
		guildId: guildId ?? '',
		channelId,
		userId: user.id,
		enabled: guildId !== undefined,
	});
	const [contextMenuOpen, setContextMenuOpen] = React.useState(false);

	const isTyping = TypingStore.isTyping(channelId, user.id);
	const isCurrentUser = user.id === AuthenticationStore.currentUserId;

	React.useEffect(() => {
		const disposer = autorun(() => {
			const contextMenu = ContextMenuStore.contextMenu;
			const targetElement = contextMenu?.target.target;
			const isNodeTarget = typeof Node !== 'undefined' && targetElement instanceof Node;
			const isOpen = Boolean(contextMenu && isNodeTarget && itemRef.current?.contains(targetElement));
			setContextMenuOpen(isOpen);
		});

		return () => {
			disposer();
		};
	}, []);

	const handleContextMenu = React.useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();

			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<>
					{guildId ? (
						<GuildMemberContextMenu user={user} onClose={onClose} guildId={guildId} channelId={channelId} />
					) : (
						<GroupDMMemberContextMenu userId={user.id} channelId={channelId} onClose={onClose} />
					)}
				</>
			));
		},
		[user, guildId, channelId],
	);

	const ownerTitle = guildId ? t`Community Owner` : t`Group Owner`;
	const nickname = displayName || NicknameUtils.getNickname(user, guildId, channelId);

	const content = (
		<FocusRingWrapper focusRingClassName={styles.memberFocusRing}>
			<button
				type="button"
				className={clsx(
					styles.button,
					!isCurrentUser && isOfflineStatus(status) && !contextMenuOpen && styles.buttonOffline,
					contextMenuOpen && styles.buttonContextMenuOpen,
				)}
				onContextMenu={handleContextMenu}
			>
				<div className={styles.grid}>
					<span className={styles.content}>
						<div className={styles.avatarContainer}>
							<StatusAwareAvatar
								user={user}
								size={32}
								isTyping={isTyping}
								showOffline={isCurrentUser || isTyping}
								guildId={guildId}
								status={guildId ? status : undefined}
							/>
						</div>
						<div className={styles.userInfoContainer}>
							<div className={styles.nameContainer}>
								<span className={styles.name} style={roleColor ? {color: roleColor} : undefined}>
									{nickname}
								</span>
								{isOwner && (
									<div className={styles.ownerIcon}>
										<Tooltip text={ownerTitle}>
											<CrownIcon className={styles.crownIcon} />
										</Tooltip>
									</div>
								)}
								{user.bot && <UserTag className={styles.userTag} system={user.system} />}
							</div>
							<CustomStatusDisplay
								customStatus={memberListCustomStatus}
								userId={user.id}
								className={styles.memberCustomStatus}
								showTooltip
								constrained
								animateOnParentHover
							/>
						</div>
					</span>
				</div>
			</button>
		</FocusRingWrapper>
	);

	return (
		<PreloadableUserPopout
			ref={itemRef}
			user={user}
			isWebhook={false}
			guildId={guildId}
			channelId={channelId}
			key={user.id}
			disableBackdrop={disableBackdrop}
		>
			{content}
		</PreloadableUserPopout>
	);
});
