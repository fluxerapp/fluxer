// SPDX-License-Identifier: AGPL-3.0-or-later

import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import {GO_BACK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {focusChannelTextareaAfterNavigation} from '@app/features/messaging/utils/ChannelTextareaFocusUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Scroller} from '@app/features/ui/components/Scroller';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import styles from '@app/features/user/components/modals/MutualItemsSheet.module.css';
import {getMutualItemsDescriptor} from '@app/features/user/components/modals/user_profile_modal/MutualItemsDescriptors';
import {getMutualCommunityDisplayItems} from '@app/features/user/components/modals/user_profile_modal/MutualItemsUtils';
import type {Profile} from '@app/features/user/models/Profile';
import {User} from '@app/features/user/models/User';
import UserProfileMobile from '@app/features/user/state/UserProfileMobile';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {msg, plural} from '@lingui/core/macro';
import {useLingui as useLinguiRuntime} from '@lingui/react';
import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const MUTUAL_FRIENDS_DESCRIPTOR = msg({
	message: 'Mutual friends',
	comment: 'Short label in the mutual items sheet. Keep it concise.',
});

type MutualItemsView = 'friends' | 'communities_groups';

interface MutualItemsSheetProps {
	isOpen: boolean;
	onClose: () => void;
	view: MutualItemsView;
	profile: Profile;
	mutualGroups: ReadonlyArray<Channel>;
}

export const MutualItemsSheet: React.FC<MutualItemsSheetProps> = observer(
	({isOpen, onClose, view, profile, mutualGroups}) => {
		const {i18n} = useLingui();
		const mutualCommunitiesCount = getMutualCommunityDisplayItems(profile.mutualGuilds ?? []).length;
		const mutualGroupsCount = mutualGroups.length;
		const title = (() => {
			switch (view) {
				case 'friends':
					return i18n._(MUTUAL_FRIENDS_DESCRIPTOR);
				case 'communities_groups':
					return i18n._(
						getMutualItemsDescriptor({
							mutualCommunitiesCount,
							mutualGroupsCount,
							includeCount: false,
						}),
					);
			}
		})();
		return (
			<BottomSheet
				isOpen={isOpen}
				onClose={onClose}
				snapPoints={[0, 1]}
				initialSnap={1}
				disablePadding={true}
				surface="primary"
				showCloseButton={false}
				leadingAction={
					<button
						type="button"
						onClick={onClose}
						className={styles.backButton}
						aria-label={i18n._(GO_BACK_DESCRIPTOR)}
						data-flx="user.mutual-items-sheet.back-button.close"
					>
						<ArrowLeftIcon className={styles.backIcon} weight="bold" data-flx="user.mutual-items-sheet.back-icon" />
					</button>
				}
				title={title}
				data-flx="user.mutual-items-sheet.bottom-sheet"
			>
				<Scroller className={styles.container} data-flx="user.mutual-items-sheet.container">
					<div className={styles.list} data-flx="user.mutual-items-sheet.list">
						{view === 'friends' && (
							<MutualFriendsList
								profile={profile}
								onClose={onClose}
								data-flx="user.mutual-items-sheet.mutual-friends-list"
							/>
						)}
						{view === 'communities_groups' && (
							<MutualCommunitiesGroupsList
								profile={profile}
								groups={mutualGroups}
								onClose={onClose}
								data-flx="user.mutual-items-sheet.mutual-communities-groups-list"
							/>
						)}
					</div>
				</Scroller>
			</BottomSheet>
		);
	},
);
const MutualFriendsList: React.FC<{profile: Profile; onClose: () => void}> = observer(({profile, onClose}) => {
	const friends = profile.mutualFriends ?? [];
	return (
		<>
			{friends.map((friend: UserPartial) => {
				const friendRecord = new User(friend);
				return (
					<button
						key={friendRecord.id}
						type="button"
						className={styles.item}
						onClick={() => {
							ModalCommands.runAfterBottomSheetClose(onClose, () =>
								UserProfileMobile.open(friendRecord.id, profile.guildId ?? undefined),
							);
						}}
						data-flx="user.mutual-items-sheet.mutual-friends-list.item.close.button"
					>
						<StatusAwareAvatar
							size={40}
							user={friendRecord}
							data-flx="user.mutual-items-sheet.mutual-friends-list.status-aware-avatar"
						/>
						<div className={styles.itemInfo} data-flx="user.mutual-items-sheet.mutual-friends-list.item-info">
							<span className={styles.itemName} data-flx="user.mutual-items-sheet.mutual-friends-list.item-name">
								{NicknameUtils.getNickname(friendRecord, profile.guildId ?? null)}
							</span>
							<span className={styles.itemDetail} data-flx="user.mutual-items-sheet.mutual-friends-list.item-detail">
								{NicknameUtils.formatTagForStreamerMode(friendRecord.tag)}
							</span>
						</div>
					</button>
				);
			})}
		</>
	);
});
const MutualCommunitiesGroupsList: React.FC<{
	profile: Profile;
	groups: ReadonlyArray<Channel>;
	onClose: () => void;
}> = observer(({profile, groups, onClose}) => {
	useLinguiRuntime();
	const mutualCommunityDisplayItems = getMutualCommunityDisplayItems(profile.mutualGuilds ?? []);
	return (
		<>
			{groups.map((group) => {
				const memberCount = group.recipientIds.length + 1;
				const memberLabel = plural(
					{count: memberCount},
					{
						one: '# member',
						other: '# members',
					},
				);
				return (
					<button
						key={group.id}
						type="button"
						className={styles.item}
						onClick={() => {
							onClose();
							UserProfileMobile.close();
							NavigationCommands.selectChannel(ME, group.id);
							focusChannelTextareaAfterNavigation(group.id);
						}}
						data-flx="user.mutual-items-sheet.mutual-communities-groups-list.group-item.close.button"
					>
						<div
							className={styles.iconFrame}
							data-flx="user.mutual-items-sheet.mutual-communities-groups-list.group-icon-frame"
						>
							<GroupDMAvatar
								channel={group}
								size={40}
								data-flx="user.mutual-items-sheet.mutual-communities-groups-list.group-dm-avatar"
							/>
						</div>
						<div
							className={styles.itemInfo}
							data-flx="user.mutual-items-sheet.mutual-communities-groups-list.group-item-info"
						>
							<span
								className={styles.itemName}
								data-flx="user.mutual-items-sheet.mutual-communities-groups-list.group-item-name"
							>
								{ChannelUtils.getDMDisplayName(group)}
							</span>
							<span
								className={styles.itemDetail}
								data-flx="user.mutual-items-sheet.mutual-communities-groups-list.group-item-detail"
							>
								{memberLabel}
							</span>
						</div>
					</button>
				);
			})}
			{groups.length > 0 && mutualCommunityDisplayItems.length > 0 && (
				<div
					className={styles.sectionDivider}
					data-flx="user.mutual-items-sheet.mutual-communities-groups-list.section-divider"
				/>
			)}
			{mutualCommunityDisplayItems.map(({guild, nick}) => (
				<button
					key={guild.id}
					type="button"
					className={styles.item}
					onClick={() => {
						onClose();
						UserProfileMobile.close();
						const selectedChannel = SelectedChannel.selectedChannelIds.get(guild.id);
						NavigationCommands.selectGuild(guild.id, selectedChannel);
					}}
					data-flx="user.mutual-items-sheet.mutual-communities-groups-list.guild-item.close.button"
				>
					<div
						className={styles.iconFrame}
						data-flx="user.mutual-items-sheet.mutual-communities-groups-list.guild-icon-frame"
					>
						<GuildIcon
							id={guild.id}
							name={guild.name}
							icon={guild.icon}
							className={styles.guildIcon}
							sizePx={40}
							data-flx="user.mutual-items-sheet.mutual-communities-groups-list.guild-icon"
						/>
					</div>
					<div
						className={styles.itemInfo}
						data-flx="user.mutual-items-sheet.mutual-communities-groups-list.guild-item-info"
					>
						<span
							className={styles.itemName}
							data-flx="user.mutual-items-sheet.mutual-communities-groups-list.guild-item-name"
						>
							{guild.name}
						</span>
						{nick && (
							<span
								className={styles.itemDetail}
								data-flx="user.mutual-items-sheet.mutual-communities-groups-list.guild-item-detail"
							>
								{nick}
							</span>
						)}
					</div>
				</button>
			))}
		</>
	);
});
