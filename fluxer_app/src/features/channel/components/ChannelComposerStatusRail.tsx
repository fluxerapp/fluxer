// SPDX-License-Identifier: AGPL-3.0-or-later

import {EditBar} from '@app/features/channel/components/ChannelEditBar';
import {ReplyBar} from '@app/features/channel/components/ChannelReplyBar';
import {SlowmodeIndicator} from '@app/features/channel/components/SlowmodeIndicator';
import {TypingUsers, usePresentableTypingUsers} from '@app/features/channel/components/TypingUsers';
import wrapperStyles from '@app/features/channel/components/textarea/InputWrapper.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface ChannelComposerStatusRailProps {
	channel: Channel;
	editingMessage: Message | null;
	referencedMessage: Message | null;
	shouldReplyMention: boolean;
	setShouldReplyMention: (mentioning: boolean) => void;
	autocompleteVisible: boolean;
	showTypingStatus: boolean;
	showSlowmodeStatus: boolean;
	slowmodeEnabled: boolean;
	slowmodeRemaining: number;
	slowmodeImmune: boolean;
	mobile: boolean;
	onCancelEdit: () => void;
}

export const ChannelComposerStatusRail = observer(function ChannelComposerStatusRail({
	channel,
	editingMessage,
	referencedMessage,
	shouldReplyMention,
	setShouldReplyMention,
	autocompleteVisible,
	showTypingStatus,
	showSlowmodeStatus,
	slowmodeEnabled,
	slowmodeRemaining,
	slowmodeImmune,
	mobile,
	onCancelEdit,
}: ChannelComposerStatusRailProps) {
	const presentableTypingUsers = usePresentableTypingUsers(channel);
	const mobileEditVisible = editingMessage !== null && mobile;
	const replyVisible = !mobileEditVisible && referencedMessage !== null;
	const leadingContentVisible = mobileEditVisible || replyVisible;
	const typingVisible = showTypingStatus && !autocompleteVisible && presentableTypingUsers.length > 0;
	const slowmodeVisible = showSlowmodeStatus && slowmodeEnabled;
	let topBar: React.ReactNode = null;
	if (mobileEditVisible) {
		topBar = <EditBar channel={channel} onCancel={onCancelEdit} data-flx="channel.composer-status-rail.edit-bar" />;
	} else if (referencedMessage !== null) {
		topBar = (
			<ReplyBar
				replyingMessageObject={referencedMessage}
				shouldReplyMention={shouldReplyMention}
				setShouldReplyMention={setShouldReplyMention}
				channel={channel}
				data-flx="channel.composer-status-rail.reply-bar"
			/>
		);
	}
	return (
		<>
			<div className={wrapperStyles.statusRail} data-flx="channel.composer-status-rail.container">
				<div className={wrapperStyles.statusRailLeft} data-flx="channel.composer-status-rail.left">
					{typingVisible && (
						<div className={wrapperStyles.statusTypingSlot} data-flx="channel.composer-status-rail.typing-slot">
							<TypingUsers channel={channel} showAvatars={true} data-flx="channel.composer-status-rail.typing-users" />
						</div>
					)}
				</div>
				{slowmodeVisible && (
					<div className={wrapperStyles.statusSlowmodeSlot} data-flx="channel.composer-status-rail.slowmode-slot">
						<SlowmodeIndicator
							slowmodeRemaining={slowmodeRemaining}
							slowmodeDuration={channel.rateLimitPerUser * 1000}
							isImmune={slowmodeImmune}
							data-flx="channel.composer-status-rail.slowmode-indicator"
						/>
					</div>
				)}
			</div>
			{leadingContentVisible && (
				<div className={wrapperStyles.composerActionStack} data-flx="channel.composer-status-rail.action-stack">
					<div className={wrapperStyles.composerActionRow} data-flx="channel.composer-status-rail.action-row">
						{topBar}
					</div>
				</div>
			)}
		</>
	);
});
