// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/bottomsheets/ChannelDetailsBottomSheet.module.css';
import {GROUP_OWNER_DESCRIPTOR} from '@app/features/channel/components/bottomsheets/channel_details_bottom_sheet/ChannelDetailsBottomSheetShared';
import {UserTag} from '@app/features/channel/components/ChannelUserTag';
import type {Channel} from '@app/features/channel/models/Channel';
import type {GroupDMMemberGroup} from '@app/features/member/utils/MemberListUtils';
import {ChevronRightIcon, NewGroupIcon, OwnerCrownIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import React from 'react';

const CREATE_GROUP_WITH_RECIPIENT_DESCRIPTOR = msg({
	message: 'Create a new group with {userName}',
	comment: 'Subtitle for an action row that creates a new group DM with the named recipient.',
});

interface DMMembersListProps {
	channel: Channel;
	currentUser: User | null | undefined;
	recipient: User | null | undefined;
	members: Array<GroupDMMemberGroup>;
	onOpenCreateGroupModal: () => void;
}

export const DMMembersList: React.FC<DMMembersListProps> = ({
	channel,
	currentUser,
	recipient,
	members,
	onOpenCreateGroupModal,
}) => {
	const {i18n} = useLingui();
	const isDM = channel.type === ChannelTypes.DM;
	const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
	const recipientDisplayName = recipient ? NicknameUtils.getNickname(recipient) : '';
	return (
		<div className={styles.dmMembersContainer} data-flx="channel.channel-details-bottom-sheet.dm-members-container">
			{isDM && recipient && (
				<button
					type="button"
					className={styles.newGroupButton}
					onClick={onOpenCreateGroupModal}
					data-flx="channel.channel-details-bottom-sheet.new-group-button.open-create-group-modal"
				>
					<div
						className={styles.newGroupIconContainer}
						data-flx="channel.channel-details-bottom-sheet.new-group-icon-container"
					>
						<NewGroupIcon
							className={`${styles.iconMedium} ${styles.newGroupIconWhite}`}
							data-flx="channel.channel-details-bottom-sheet.icon-medium"
						/>
					</div>
					<div className={styles.newGroupContent} data-flx="channel.channel-details-bottom-sheet.new-group-content">
						<p className={styles.newGroupTitle} data-flx="channel.channel-details-bottom-sheet.new-group-title">
							<Trans>New group</Trans>
						</p>
						<p className={styles.newGroupSubtitle} data-flx="channel.channel-details-bottom-sheet.new-group-subtitle">
							{i18n._(CREATE_GROUP_WITH_RECIPIENT_DESCRIPTOR, {userName: recipientDisplayName})}
						</p>
					</div>
					<ChevronRightIcon
						className={styles.iconMedium}
						data-flx="channel.channel-details-bottom-sheet.icon-medium--2"
					/>
				</button>
			)}
			{members.map((group) => (
				<div
					key={group.id}
					className={styles.memberGroupContainer}
					data-flx="channel.channel-details-bottom-sheet.member-group-container"
				>
					<div className={styles.memberGroupHeader} data-flx="channel.channel-details-bottom-sheet.member-group-header">
						{group.displayName}—{group.count}
					</div>
					<div className={styles.memberGroupList} data-flx="channel.channel-details-bottom-sheet.member-group-list">
						{group.users.map((user, index) => {
							const isCurrentUser = user.id === currentUser?.id;
							const isOwner = isGroupDM && channel.ownerId === user.id;
							const displayName = NicknameUtils.getNickname(user, undefined, channel.id);
							const handleUserClick = () => {
								UserProfileCommands.openUserProfile(user.id);
							};
							return (
								<React.Fragment key={user.id}>
									<button
										type="button"
										onClick={handleUserClick}
										className={styles.memberItemButton}
										data-flx="channel.channel-details-bottom-sheet.member-item-button.user-click"
									>
										<StatusAwareAvatar
											user={user}
											size={40}
											data-flx="channel.channel-details-bottom-sheet.status-aware-avatar--3"
										/>
										<div
											className={styles.memberItemContent}
											data-flx="channel.channel-details-bottom-sheet.member-item-content"
										>
											<span
												className={styles.memberItemName}
												data-flx="channel.channel-details-bottom-sheet.member-item-name"
											>
												{displayName}
												{isCurrentUser && (
													<span
														className={styles.memberItemYou}
														data-flx="channel.channel-details-bottom-sheet.member-item-you"
													>
														{' '}
														<Trans>(you)</Trans>
													</span>
												)}
											</span>
											{(user.bot || isOwner) && (
												<div
													className={styles.memberItemTags}
													data-flx="channel.channel-details-bottom-sheet.member-item-tags"
												>
													{user.bot && (
														<UserTag system={user.system} data-flx="channel.channel-details-bottom-sheet.user-tag" />
													)}
													{isOwner && (
														<Tooltip
															text={i18n._(GROUP_OWNER_DESCRIPTOR)}
															data-flx="channel.channel-details-bottom-sheet.tooltip"
														>
															<OwnerCrownIcon
																className={styles.ownerCrown}
																data-flx="channel.channel-details-bottom-sheet.owner-crown"
															/>
														</Tooltip>
													)}
												</div>
											)}
										</div>
									</button>
									{index < group.users.length - 1 && (
										<div
											className={styles.memberItemDivider}
											data-flx="channel.channel-details-bottom-sheet.member-item-divider"
										/>
									)}
								</React.Fragment>
							);
						})}
					</div>
				</div>
			))}
		</div>
	);
};
