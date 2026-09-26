// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageReactions} from '@app/features/channel/components/MessageReactions';
import {TimestampWithTooltip} from '@app/features/channel/components/TimestampWithTooltip';
import {useMessageReactions as useMessageReactionsSnapshot} from '@app/features/messaging/hooks/useMessageReactionStore';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import styles from '@app/features/theme/styles/Message.module.css';
import UserSettings from '@app/features/user/state/UserSettings';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {useLingui} from '@lingui/react/macro';
import type {Icon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

export const SystemMessage = observer(
	({
		icon: Icon,
		iconWeight,
		iconClassname,
		message,
		messageContent,
		additionalContent,
	}: {
		icon: Icon;
		iconWeight: 'bold' | 'fill';
		iconClassname?: string;
		message: Message;
		messageContent: React.ReactNode;
		additionalContent?: React.ReactNode;
	}) => {
		const {i18n} = useLingui();
		const messageDisplayCompact = UserSettings.getMessageDisplayCompact();
		const reactions = useMessageReactionsSnapshot(message.id);
		const formattedDate = useMemo(
			() =>
				messageDisplayCompact
					? DateUtils.getFormattedTime(message.timestamp)
					: DateUtils.getRelativeDateString(message.timestamp, i18n),
			[messageDisplayCompact, message.timestamp, i18n.locale],
		);
		const showReactions = UserSettings.getRenderReactions() && reactions.length > 0;
		if (messageDisplayCompact) {
			return (
				<div
					className={styles.systemMessageCompactContent}
					data-flx="channel.system-message.system-message-compact-content"
				>
					<TimestampWithTooltip
						date={message.timestamp}
						className={styles.messageTimestampCompact}
						data-flx="channel.system-message.message-timestamp-compact"
					>
						{formattedDate}
					</TimestampWithTooltip>
					<div
						className={styles.systemMessageIconCompact}
						data-flx="channel.system-message.system-message-icon-compact"
					>
						<Icon
							weight={iconWeight}
							className={clsx(styles.systemMessageIconSvg, iconClassname)}
							data-flx="channel.system-message.system-message-icon-svg"
						/>
					</div>
					<div
						className={styles.systemMessageContentWrapper}
						data-flx="channel.system-message.system-message-content-wrapper"
					>
						<div
							className={styles.systemMessageContent}
							data-search-highlight-scope="message"
							data-flx="channel.system-message.system-message-content"
						>
							{messageContent}
						</div>
						{showReactions && (
							<div className={styles.container} data-flx="channel.system-message.container">
								<MessageReactions message={message} data-flx="channel.system-message.message-reactions" />
							</div>
						)}
					</div>
				</div>
			);
		}
		return (
			<>
				<div className={styles.messageGutterLeft} data-flx="channel.system-message.message-gutter-left" />
				<div className={styles.systemMessageIconWrapper} data-flx="channel.system-message.system-message-icon-wrapper">
					<Icon
						weight={iconWeight}
						className={clsx(styles.systemMessageIconSvg, iconClassname)}
						data-flx="channel.system-message.system-message-icon-svg--2"
					/>
				</div>
				<div className={styles.messageGutterRight} data-flx="channel.system-message.message-gutter-right" />
				<div className={styles.systemMessageContent} data-flx="channel.system-message.system-message-content--2">
					<span data-search-highlight-scope="message" data-flx="channel.system-message.span">
						{messageContent}
					</span>{' '}
					<TimestampWithTooltip
						date={message.timestamp}
						className={clsx(styles.messageTimestamp, styles.systemMessageTimestamp)}
						data-flx="channel.system-message.message-timestamp"
					>
						{formattedDate}
					</TimestampWithTooltip>
				</div>
				{(showReactions || additionalContent) && (
					<div
						className={styles.container}
						style={{
							rowGap: 0,
						}}
						data-flx="channel.system-message.container--2"
					>
						{additionalContent}
						{showReactions && (
							<MessageReactions message={message} data-flx="channel.system-message.message-reactions--2" />
						)}
					</div>
				)}
			</>
		);
	},
);
