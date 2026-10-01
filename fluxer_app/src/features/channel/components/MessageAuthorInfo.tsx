// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserTag} from '@app/features/channel/components/ChannelUserTag';
import {MessageAvatar} from '@app/features/channel/components/MessageAvatar';
import {MessageTimeoutIndicator} from '@app/features/channel/components/MessageTimeoutIndicator';
import {MessageUsername} from '@app/features/channel/components/MessageUsername';
import {TimestampWithTooltip} from '@app/features/channel/components/TimestampWithTooltip';
import type {Guild} from '@app/features/guild/models/Guild';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import styles from '@app/features/theme/styles/Message.module.css';
import type {User} from '@app/features/user/models/User';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {observer} from 'mobx-react-lite';
import {useMemo} from 'react';

interface MessageAuthorInfoProps {
	message: Message;
	author: User;
	guild?: Guild;
	member?: GuildMember;
	shouldGroup: boolean;
	shouldAppearAuthorless: boolean;
	mobileLayoutEnabled: boolean;
	isHovering: boolean;
	formattedDate: string;
	previewContext?: keyof typeof MessagePreviewContext;
	previewOverrides?: {
		usernameColor?: string;
		displayName?: string;
	};
}

export const MessageAuthorInfo = observer((props: MessageAuthorInfoProps) => {
	const {
		message,
		author,
		guild,
		member,
		shouldGroup,
		shouldAppearAuthorless,
		mobileLayoutEnabled,
		isHovering,
		formattedDate,
		previewContext,
		previewOverrides,
	} = props;
	const isPreview = useMemo(() => Boolean(previewContext), [previewContext]);
	const timeoutIndicator = (
		<MessageTimeoutIndicator
			guildId={message.guildId}
			userId={author.id}
			data-flx="channel.message-author-info.message-timeout-indicator"
		/>
	);
	const username = useMemo(
		() => (
			<MessageUsername
				user={author}
				message={message}
				guild={guild}
				member={member}
				className={styles.messageUsername}
				isPreview={isPreview}
				previewColor={previewOverrides?.usernameColor}
				previewName={previewOverrides?.displayName}
				data-flx="channel.message-author-info.username.message-username"
			/>
		),
		[author, message, guild, member, isPreview, previewOverrides?.usernameColor, previewOverrides?.displayName],
	);
	if (shouldAppearAuthorless) return null;
	if (!shouldGroup) {
		const displayName =
			previewOverrides?.displayName || NicknameUtils.getNickname(author, guild?.id, message.channelId);
		const headerAriaLabel = `${displayName}, ${formattedDate}`;
		return (
			<>
				<div className={styles.messageGutterLeft} data-flx="channel.message-author-info.message-gutter-left" />
				<MessageAvatar
					user={author}
					message={message}
					guildId={guild?.id}
					size={40}
					className={styles.messageAvatar}
					isHovering={isHovering}
					isPreview={isPreview}
					data-flx="channel.message-author-info.message-avatar"
				/>
				<div className={styles.messageGutterRight} data-flx="channel.message-author-info.message-gutter-right" />
				<h3
					className={styles.messageAuthorInfo}
					aria-label={headerAriaLabel}
					data-flx="channel.message-author-info.message-author-info"
				>
					<span
						className={styles.messageAuthorRow}
						aria-hidden="true"
						data-flx="channel.message-author-info.message-author-row"
					>
						<span className={styles.messageAuthorPart} data-flx="channel.message-author-info.message-author-part">
							{timeoutIndicator}
							{username}
							{(author.bot || message.isCrosspostCopy) && (
								<UserTag
									className={styles.userTagOffset}
									system={author.system}
									variant={message.isCrosspostCopy ? 'community' : undefined}
									data-flx="channel.message-author-info.user-tag-offset"
								/>
							)}
						</span>
						<TimestampWithTooltip
							date={message.timestamp}
							className={styles.messageTimestamp}
							data-flx="channel.message-author-info.message-timestamp"
						>
							<span className={styles.authorDashSeparator} data-flx="channel.message-author-info.author-dash-separator">
								{' \u2014 '}
							</span>
							{formattedDate}
						</TimestampWithTooltip>
					</span>
				</h3>
			</>
		);
	}
	if (mobileLayoutEnabled) return null;
	return (
		<>
			<div className={styles.messageGutterLeft} data-flx="channel.message-author-info.message-gutter-left--2" />
			<TimestampWithTooltip
				date={message.timestamp}
				className={styles.messageTimestampHover}
				copyHidden
				data-flx="channel.message-author-info.message-timestamp-hover"
			>
				<span className={styles.textSeparator} data-flx="channel.message-author-info.text-separator">
					[
				</span>
				{DateUtils.getFormattedTime(message.timestamp)}
				<span className={styles.textSeparator} data-flx="channel.message-author-info.text-separator--2">
					]
				</span>
			</TimestampWithTooltip>
			<div className={styles.messageGutterRight} data-flx="channel.message-author-info.message-gutter-right--2" />
		</>
	);
});
