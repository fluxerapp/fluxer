// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import Authentication from '@app/features/auth/state/Authentication';
import styles from '@app/features/channel/components/ChannelReplyBar.module.css';
import wrapperStyles from '@app/features/channel/components/textarea/InputWrapper.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import Guilds from '@app/features/guild/state/Guilds';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import {
	getReplyMentionPreferenceConflict,
	resolveMentionReplyPreference,
} from '@app/features/notification/utils/MentionReplyPreferenceUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {AtIcon, XCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

const OVERRIDE_MENTION_PREFERENCE_DESCRIPTOR = msg({
	message: 'Override mention preference?',
	comment:
		'Title of the confirmation alert shown when sending a reply against the recipient mention-on-reply preference.',
});
const PREFERS_TO_BE_MENTIONED_ON_REPLIES_SEND_WITHOUT_DESCRIPTOR = msg({
	message: '{authorNickname} prefers to be @mentioned on replies. Send without the mention anyway?',
	comment:
		'Confirmation body when the recipient prefers @mentions on replies but the user disabled it. authorNickname is the recipient display name. The @ character is literal.',
});
const PREFERS_REPLIES_WITHOUT_AN_MENTION_SEND_WITH_THE_DESCRIPTOR = msg({
	message: '{authorNickname} prefers replies without an @mention. Send with the mention anyway?',
	comment:
		'Confirmation body when the recipient prefers no @mention on replies but the user enabled it. authorNickname is the recipient display name. The @ character is literal.',
});
const IGNORE_MENTION_PREFERENCE_DESCRIPTOR = msg({
	message: 'Ignore preference',
	comment: 'Confirm button label on the override-mention-preference alert.',
});
const CLICK_TO_DISABLE_PINGING_THE_USER_YOU_RE_DESCRIPTOR = msg({
	message: "Click to disable pinging the user you're replying to.",
	comment: 'Tooltip on the reply bar mention toggle when mention-on-reply is currently on.',
});
const CLICK_TO_ENABLE_PINGING_THE_USER_YOU_RE_DESCRIPTOR = msg({
	message: "Click to enable pinging the user you're replying to.",
	comment: 'Tooltip on the reply bar mention toggle when mention-on-reply is currently off.',
});
const MENTION_REPLIED_USER_DESCRIPTOR = msg({
	message: 'Mention replied user',
	comment: 'Label next to the mention toggle in the reply bar.',
});
const ON_DESCRIPTOR = msg({
	message: 'On',
	comment: 'On state suffix appended to the reply bar mention toggle label.',
});
const OFF_DESCRIPTOR = msg({
	message: 'Off',
	comment: 'Off state suffix appended to the reply bar mention toggle label.',
});
const CANCEL_REPLY_DESCRIPTOR = msg({
	message: 'Cancel reply',
	comment: 'Accessible label and tooltip for the cancel-reply button in the reply bar.',
});
const JUMP_TO_REPLIED_MESSAGE_FROM_DESCRIPTOR = msg({
	message: 'Jump to the message from {authorNickname}',
	comment: 'Accessible label for the reply bar target button. Preserve {authorNickname}; it is inserted by code.',
});

interface ReplyBarProps {
	replyingMessageObject: Message;
	shouldReplyMention: boolean;
	setShouldReplyMention: (mentioning: boolean) => void;
	channel: Channel;
}

export const ReplyBar = observer(function ReplyBar({
	replyingMessageObject,
	shouldReplyMention: initialShouldMention,
	setShouldReplyMention,
	channel,
}: ReplyBarProps) {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(channel.guildId ?? '');
	const currentUserId = Authentication.currentUserId;
	const isOwnMessage = replyingMessageObject.author.id === currentUserId;
	const isInGuild = channel.guildId != null;
	const isWebhook = replyingMessageObject.webhookId != null;
	const canMention = !isOwnMessage && isInGuild && !isWebhook;
	const shouldMention = initialShouldMention && canMention;
	const authorNickname = NicknameUtils.getNickname(replyingMessageObject.author, guild?.id);
	const handleStopReply = () => {
		MessageCommands.stopReply(channel.id);
	};
	const handleJumpToReplyTarget = () => {
		goToMessage(replyingMessageObject.channelId, replyingMessageObject.id);
	};
	const toggleMention = () => {
		const next = !shouldMention;
		const preference = resolveMentionReplyPreference({
			authorId: replyingMessageObject.author.id,
			guildId: channel.guildId,
		});
		const conflict = getReplyMentionPreferenceConflict(next, preference);
		if (conflict) {
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(OVERRIDE_MENTION_PREFERENCE_DESCRIPTOR)}
						description={
							conflict === 'prefers_mention'
								? i18n._(PREFERS_TO_BE_MENTIONED_ON_REPLIES_SEND_WITHOUT_DESCRIPTOR, {authorNickname})
								: i18n._(PREFERS_REPLIES_WITHOUT_AN_MENTION_SEND_WITH_THE_DESCRIPTOR, {authorNickname})
						}
						primaryText={i18n._(IGNORE_MENTION_PREFERENCE_DESCRIPTOR)}
						onPrimary={() => setShouldReplyMention(next)}
						data-flx="channel.reply-bar.toggle-mention.confirm-modal"
					/>
				)),
			);
			return;
		}
		setShouldReplyMention(next);
	};
	return (
		<div
			className={clsx(
				wrapperStyles.box,
				wrapperStyles.wrapperSides,
				wrapperStyles.roundedTop,
				wrapperStyles.noBottomBorder,
			)}
			data-flx="channel.reply-bar.top-border"
		>
			<div
				className={clsx(wrapperStyles.barInner)}
				style={{gridTemplateColumns: '1fr auto'}}
				data-flx="channel.reply-bar.div"
			>
				<FocusRing offset={-2} data-flx="channel.reply-bar.reply-target.focus-ring">
					<button
						type="button"
						className={clsx(styles.text, styles.replyTargetButton)}
						onClick={handleJumpToReplyTarget}
						aria-label={i18n._(JUMP_TO_REPLIED_MESSAGE_FROM_DESCRIPTOR, {authorNickname})}
						data-flx="channel.reply-bar.reply-target.button"
					>
						<Trans>
							Replying to{' '}
							<span className={styles.authorName} data-flx="channel.reply-bar.author-name">
								{authorNickname}
							</span>
						</Trans>
					</button>
				</FocusRing>
				<div className={styles.controls} data-flx="channel.reply-bar.controls">
					{canMention && (
						<Tooltip
							text={
								shouldMention
									? i18n._(CLICK_TO_DISABLE_PINGING_THE_USER_YOU_RE_DESCRIPTOR)
									: i18n._(CLICK_TO_ENABLE_PINGING_THE_USER_YOU_RE_DESCRIPTOR)
							}
							data-flx="channel.reply-bar.tooltip"
						>
							<FocusRing offset={-2} data-flx="channel.reply-bar.focus-ring">
								<button
									type="button"
									className={clsx(
										styles.mentionToggle,
										shouldMention ? styles.mentionToggleOn : styles.mentionToggleOff,
									)}
									role="switch"
									aria-checked={shouldMention}
									aria-label={i18n._(MENTION_REPLIED_USER_DESCRIPTOR)}
									onClick={toggleMention}
									data-flx="channel.reply-bar.switch.toggle-mention"
								>
									<AtIcon weight="bold" className={styles.mentionIcon} data-flx="channel.reply-bar.mention-icon" />
									{shouldMention ? i18n._(ON_DESCRIPTOR) : i18n._(OFF_DESCRIPTOR)}
								</button>
							</FocusRing>
						</Tooltip>
					)}
					<FocusRing offset={-2} data-flx="channel.reply-bar.focus-ring--2">
						<button
							type="button"
							className={styles.closeButton}
							onClick={handleStopReply}
							aria-label={i18n._(CANCEL_REPLY_DESCRIPTOR)}
							data-flx="channel.reply-bar.close-button.stop-reply"
						>
							<XCircleIcon className={styles.closeIcon} data-flx="channel.reply-bar.close-icon" />
						</button>
					</FocusRing>
				</div>
			</div>
			<div className={wrapperStyles.separator} data-flx="channel.reply-bar.div--2" />
		</div>
	);
});
