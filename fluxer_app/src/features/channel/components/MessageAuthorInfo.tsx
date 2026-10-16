// SPDX-License-Identifier: AGPL-3.0-or-later

import {TimestampWithTooltip} from '@app/features/channel/components/TimestampWithTooltip';

import type {Message} from '@app/features/messaging/models/MessagingMessage';
import styles from '@app/features/theme/styles/Message.module.css';

import * as DateUtils from '@app/features/user/utils/DateFormatting';

import {observer} from 'mobx-react-lite';

interface MessageAuthorInfoProps {
	message: Message;
	mobileLayoutEnabled: boolean;
}

export const MessageAuthorInfo = observer((props: MessageAuthorInfoProps) => {
	const {message, mobileLayoutEnabled} = props;

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
