// SPDX-License-Identifier: AGPL-3.0-or-later

import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import styles from '@app/features/channel/components/bottomsheets/ChannelDetailsBottomSheet.module.css';
import {ChannelTopicSection} from '@app/features/channel/components/bottomsheets/channel_details_bottom_sheet/ChannelTopicSection';
import {UserTag} from '@app/features/channel/components/ChannelUserTag';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {PERSONAL_NOTES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import * as Sheet from '@app/features/ui/sheet/Sheet';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {plural} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';

interface ChannelInfoHeaderProps {
	channel: Channel;
	currentUser: User | null | undefined;
	recipient: User | null | undefined;
	channelTypeLabel: string;
	onClose: () => void;
}

export const ChannelInfoHeader: React.FC<ChannelInfoHeaderProps> = ({
	channel,
	currentUser,
	recipient,
	channelTypeLabel,
	onClose,
}) => {
	const {i18n} = useLingui();
	const isDM = channel.type === ChannelTypes.DM;
	const isPersonalNotes = channel.type === ChannelTypes.DM_PERSONAL_NOTES;
	const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
	const recipientDisplayName = recipient ? NicknameUtils.getNickname(recipient, null, channel.id) : '';
	return (
		<div className={styles.channelInfoSection} data-flx="channel.channel-details-bottom-sheet.channel-info-section">
			<Sheet.CloseButton
				onClick={onClose}
				className={styles.closeButton}
				data-flx="channel.channel-details-bottom-sheet.close-button"
			/>
			<div
				className={styles.channelInfoContainer}
				data-flx="channel.channel-details-bottom-sheet.channel-info-container"
			>
				{isDM && recipient ? (
					<StatusAwareAvatar
						user={recipient}
						size={48}
						data-flx="channel.channel-details-bottom-sheet.status-aware-avatar"
					/>
				) : isGroupDM ? (
					<GroupDMAvatar channel={channel} size={48} data-flx="channel.channel-details-bottom-sheet.group-dm-avatar" />
				) : isPersonalNotes && currentUser ? (
					<StatusAwareAvatar
						user={currentUser}
						size={48}
						data-flx="channel.channel-details-bottom-sheet.status-aware-avatar--2"
					/>
				) : (
					<div className={styles.channelAvatar} data-flx="channel.channel-details-bottom-sheet.channel-avatar">
						{ChannelUtils.getIcon(channel, {className: styles.iconLarge})}
					</div>
				)}
				<div className={styles.channelInfoContent} data-flx="channel.channel-details-bottom-sheet.channel-info-content">
					{isDM && recipient ? (
						<>
							<div
								className={styles.channelInfoUserContainer}
								data-flx="channel.channel-details-bottom-sheet.channel-info-user-container"
							>
								<span
									className={styles.channelInfoUsername}
									data-flx="channel.channel-details-bottom-sheet.channel-info-username"
								>
									{recipientDisplayName}
								</span>
								<span
									className={styles.channelInfoDiscriminator}
									data-flx="channel.channel-details-bottom-sheet.channel-info-discriminator"
								>
									{NicknameUtils.formatUserTagForStreamerMode(recipient)}
								</span>
							</div>
							{recipient.bot && (
								<UserTag
									className={styles.channelInfoTag}
									system={recipient.system}
									data-flx="channel.channel-details-bottom-sheet.channel-info-tag"
								/>
							)}
						</>
					) : isGroupDM ? (
						<>
							<h2
								className={styles.channelInfoTitle}
								data-flx="channel.channel-details-bottom-sheet.channel-info-title"
							>
								{ChannelUtils.getDMDisplayName(channel)}
							</h2>
							<p
								className={styles.channelInfoSubtitle}
								data-flx="channel.channel-details-bottom-sheet.channel-info-subtitle"
							>
								{plural(
									{count: channel.recipientIds.length + 1},
									{
										one: 'Group DM · # member',
										other: 'Group DM · # members',
									},
								)}
							</p>
						</>
					) : isPersonalNotes ? (
						<>
							<h2
								className={styles.channelInfoTitle}
								data-flx="channel.channel-details-bottom-sheet.channel-info-title--2"
							>
								{i18n._(PERSONAL_NOTES_DESCRIPTOR)}
							</h2>
							<p
								className={styles.channelInfoSubtitle}
								data-flx="channel.channel-details-bottom-sheet.channel-info-subtitle--2"
							>
								<Trans>Your private space</Trans>
							</p>
						</>
					) : (
						<>
							<h2
								className={styles.channelInfoTitle}
								data-flx="channel.channel-details-bottom-sheet.channel-info-title--3"
							>
								<span
									className={styles.channelNameWithIcon}
									data-flx="channel.channel-details-bottom-sheet.channel-name-with-icon"
								>
									{ChannelUtils.getIcon(channel, {className: styles.channelNameIcon})}
									{channel.name}
								</span>
							</h2>
							<p
								className={styles.channelInfoSubtitle}
								data-flx="channel.channel-details-bottom-sheet.channel-info-subtitle--3"
							>
								{channelTypeLabel}
							</p>
						</>
					)}
				</div>
			</div>
			{channel.topic && !isDM && !isPersonalNotes && (
				<ChannelTopicSection
					channelId={channel.id}
					topic={channel.topic}
					data-flx="channel.channel-details-bottom-sheet.channel-info-header.channel-topic-section"
				/>
			)}
		</div>
	);
};
