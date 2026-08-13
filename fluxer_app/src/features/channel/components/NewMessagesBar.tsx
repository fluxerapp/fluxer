// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ChannelMessages.module.css';
import {MARK_AS_READ_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {isSameDay as isSameDayBase} from '@fluxer/date_utils/src/DateComparison';
import {msg} from '@lingui/core/macro';
import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {CheckIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const JUMP_TO_FIRST_UNREAD_MESSAGE_DESCRIPTOR = msg({
	message: 'Jump to first unread message',
	comment: 'Label in the channel and chat new messages bar.',
});
const NEW_SINCE_DESCRIPTOR = msg({
	message: '{unreadCount}+ new since {shortTime}',
	comment:
		'Label in the channel and chat new messages bar. Preserve {unreadCount}, {shortTime}; they are inserted by code.',
});
const NEW_MESSAGES_SINCE_DESCRIPTOR = msg({
	message: '{unreadCount}+ new messages since {compactTime}',
	comment:
		'Label in the channel and chat new messages bar. Preserve {unreadCount}, {compactTime}; they are inserted by code.',
});
const NEW_SINCE_2_DESCRIPTOR = msg({
	message: '{unreadCount} new since {shortTime}',
	comment:
		'Label in the channel and chat new messages bar. Preserve {unreadCount}, {shortTime}; they are inserted by code.',
});
const MARK_READ_DESCRIPTOR = msg({
	message: 'Mark read',
	comment: 'Button or menu action label in the channel and chat new messages bar. Keep it concise.',
});
export const NewMessagesBar = observer(
	({
		unreadCount,
		oldestUnreadTimestamp,
		isEstimated,
		onJumpToOldestUnread,
		onJumpToNewMessages,
	}: {
		unreadCount: number;
		oldestUnreadTimestamp: number;
		isEstimated: boolean;
		onJumpToOldestUnread: () => void;
		onJumpToNewMessages: () => void;
	}) => {
		const {i18n} = useLingui();
		const isMobile = MobileLayout.isMobileLayout();
		const sameDay = isSameDayBase(oldestUnreadTimestamp);
		const compactTime = DateUtils.getFormattedCompactDateTime(oldestUnreadTimestamp);
		const shortTime = sameDay ? DateUtils.getFormattedTime(oldestUnreadTimestamp) : compactTime;
		return (
			<div className={styles.newMessagesBarBanner} data-flx="channel.new-messages-bar.new-messages-bar">
				<button
					type="button"
					className={styles.newMessagesBarBannerText}
					onClick={onJumpToOldestUnread}
					aria-label={i18n._(JUMP_TO_FIRST_UNREAD_MESSAGE_DESCRIPTOR)}
					data-flx="channel.new-messages-bar.new-messages-bar-text.jump-to-oldest-unread.button"
				>
					{isEstimated ? (
						isMobile ? (
							i18n._(NEW_SINCE_DESCRIPTOR, {unreadCount, shortTime})
						) : (
							i18n._(NEW_MESSAGES_SINCE_DESCRIPTOR, {unreadCount, compactTime})
						)
					) : isMobile ? (
						i18n._(NEW_SINCE_2_DESCRIPTOR, {unreadCount, shortTime})
					) : (
						<Trans>
							<Plural
								value={unreadCount}
								one="# new message"
								other="# new messages"
								data-flx="channel.new-messages-bar.plural"
							/>{' '}
							since {compactTime}
						</Trans>
					)}
				</button>
				<button
					type="button"
					className={styles.newMessagesBarBannerAction}
					onClick={onJumpToNewMessages}
					data-flx="channel.new-messages-bar.new-messages-bar-action.jump-to-new-messages.button"
				>
					<span data-flx="channel.new-messages-bar.span">
						{isMobile ? i18n._(MARK_READ_DESCRIPTOR) : i18n._(MARK_AS_READ_DESCRIPTOR)}
					</span>
					<CheckIcon weight="bold" size={16} data-flx="channel.new-messages-bar.check-icon" />
				</button>
			</div>
		);
	},
);
