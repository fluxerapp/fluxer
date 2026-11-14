// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {UserTag} from '@app/features/channel/components/ChannelUserTag';
import {CompactAuthorPrefix, CompactMessageLayout} from '@app/features/channel/components/CompactMessageLayout';
import {MessageAvatar} from '@app/features/channel/components/MessageAvatar';
import {MessageUsername} from '@app/features/channel/components/MessageUsername';
import {useMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import {TimestampWithTooltip} from '@app/features/channel/components/TimestampWithTooltip';
import Guilds from '@app/features/guild/state/Guilds';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {compactMarkdownProps} from '@app/features/theme/layout/MessageLayoutAttributes';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import styles from '@app/features/theme/styles/Message.module.css';
import Users from '@app/features/user/state/Users';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {WarningCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

const UPDATE_PRODUCT_TO_VIEW_MESSAGE_DESCRIPTOR = msg({
	message: 'Update {productName} to view this message.',
	comment: 'Message placeholder shown when the app cannot render a newer message type.',
});
export const UnknownMessage = observer(() => {
	const {i18n} = useLingui();
	const {message, channel, shouldGroup, isHovering, messageDisplayCompact, previewContext, previewOverrides} =
		useMessageViewContext();
	const userAuthor = Users.getUser(message.author.id);
	const author = message.webhookId != null ? message.author : (userAuthor ?? message.author);
	const formattedDate = DateUtils.getRelativeDateString(message.timestamp, i18n);
	const guild = Guilds.getGuild(channel.guildId ?? '');
	const member = GuildMembers.getMember(guild?.id ?? '', author?.id ?? '');
	const updateMessage = i18n._(UPDATE_PRODUCT_TO_VIEW_MESSAGE_DESCRIPTOR, {productName: RuntimeConfig.productName});
	if (messageDisplayCompact) {
		return (
			<CompactMessageLayout
				message={message}
				shouldGroup={shouldGroup}
				mobileLayoutEnabled={false}
				data-flx="channel.unknown-message.compact-message-layout"
			>
				{(showMetadata) => (
					<div
						className={clsx(markupStyles.markup, styles.compactInlineContent, styles.unknownMessageCompactContent)}
						data-flx="channel.unknown-message.compact-inline-content"
						{...compactMarkdownProps()}
					>
						{showMetadata && (
							<CompactAuthorPrefix
								message={message}
								author={author}
								guild={guild}
								member={member ?? undefined}
								showAvatar={false}
								showTimeoutIndicator={false}
								isHovering={isHovering}
								previewContext={previewContext}
								previewOverrides={previewOverrides}
								data-flx="channel.unknown-message.compact-author-prefix"
							/>
						)}
						<div className={styles.unknownMessageWarning} data-flx="channel.unknown-message.unknown-message-warning">
							<WarningCircleIcon size={16} weight="fill" data-flx="channel.unknown-message.warning-circle-icon" />
							<span data-flx="channel.unknown-message.span">{updateMessage}</span>
						</div>
					</div>
				)}
			</CompactMessageLayout>
		);
	}
	return (
		<>
			{!shouldGroup && (
				<>
					<div className={styles.messageGutterLeft} data-flx="channel.unknown-message.message-gutter-left" />
					<MessageAvatar
						user={author}
						message={message}
						guildId={guild?.id}
						size={40}
						className={styles.messageAvatar}
						isHovering={isHovering}
						isPreview={!!previewContext}
						data-flx="channel.unknown-message.message-avatar"
					/>
					<div className={styles.messageGutterRight} data-flx="channel.unknown-message.message-gutter-right" />
				</>
			)}
			<div className={styles.messageContent} data-flx="channel.unknown-message.message-content">
				<h3 className={styles.messageAuthorInfo} data-flx="channel.unknown-message.message-author-info">
					<span className={styles.messageAuthorRow} data-flx="channel.unknown-message.message-author-row">
						<span className={styles.messageAuthorPart} data-flx="channel.unknown-message.message-author-part">
							<MessageUsername
								user={author}
								message={message}
								guild={guild}
								member={member ?? undefined}
								className={styles.messageUsername}
								isPreview={!!previewContext}
								previewColor={previewOverrides?.usernameColor}
								previewName={previewOverrides?.displayName}
								data-flx="channel.unknown-message.message-username"
							/>
							{author.bot && (
								<UserTag
									className={styles.userTagOffset}
									system={author.system}
									data-flx="channel.unknown-message.user-tag-offset"
								/>
							)}
						</span>
						<span
							aria-hidden="true"
							className={styles.authorDashSeparator}
							data-flx="channel.unknown-message.author-dash-separator"
						>
							{' \u2014 '}
						</span>
						<TimestampWithTooltip
							date={message.timestamp}
							className={styles.messageTimestamp}
							data-flx="channel.unknown-message.message-timestamp"
						>
							{formattedDate}
						</TimestampWithTooltip>
					</span>
				</h3>
				<div className={styles.messageText} data-flx="channel.unknown-message.message-text">
					<div className={styles.unknownMessageWarning} data-flx="channel.unknown-message.unknown-message-warning--2">
						<WarningCircleIcon size={16} weight="fill" data-flx="channel.unknown-message.warning-circle-icon--2" />
						<span data-flx="channel.unknown-message.span--2">{updateMessage}</span>
					</div>
				</div>
			</div>
		</>
	);
});
