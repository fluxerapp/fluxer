// SPDX-License-Identifier: AGPL-3.0-or-later

import {Message} from '@app/features/channel/components/ChannelMessage';
import Channels from '@app/features/channel/state/Channels';
import {Message as MessageModel} from '@app/features/messaging/models/MessagingMessage';
import styles from '@app/features/moderation/components/report_flow/ReportFlowPreview.module.css';
import {Avatar} from '@app/features/ui/components/Avatar';
import {Scroller} from '@app/features/ui/components/Scroller';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

const PREVIEW_BEHAVIOR_OVERRIDES = {
	isEditing: false,
	isHighlight: false,
	disableContextMenu: true,
	disableContextMenuTracking: true,
	contextMenuOpen: false,
};

export const ReportFlowMessagePreview: React.FC<{message: MessageModel}> = observer(({message}) => {
	const snapshot = useMemo(
		() =>
			new MessageModel(message.toJSON(), {
				skipUserCache: true,
				missingReactions: 'preserve',
				skipReactionHydration: true,
				instanceId: message.instanceId,
			}),
		[message],
	);
	const channel = Channels.getChannel(snapshot.channelId);
	if (!channel) return null;
	return (
		<Scroller
			className={styles.messageScroller}
			scrollbar="thin"
			data-flx="moderation.report-flow.report-flow-preview.report-flow-message-preview.message-scroller"
		>
			<div
				className={styles.messageContent}
				data-flx="moderation.report-flow.report-flow-preview.report-flow-message-preview.message-content"
			>
				<Message
					channel={channel}
					message={snapshot}
					previewContext={MessagePreviewContext.LIST_POPOUT}
					removeTopSpacing={true}
					behaviorOverrides={PREVIEW_BEHAVIOR_OVERRIDES}
					data-flx="moderation.report-flow.report-flow-preview.report-flow-message-preview.message"
				/>
			</div>
		</Scroller>
	);
});

export const ReportFlowUserPreview: React.FC<{user: User; guildId?: string}> = observer(({user, guildId}) => (
	<div
		className={styles.userCard}
		data-flx="moderation.report-flow.report-flow-preview.report-flow-user-preview.user-card"
	>
		<Avatar
			user={user}
			size={40}
			guildId={guildId ?? null}
			data-flx="moderation.report-flow.report-flow-preview.report-flow-user-preview.avatar"
		/>
		<div
			className={styles.userText}
			data-flx="moderation.report-flow.report-flow-preview.report-flow-user-preview.user-text"
		>
			<span
				className={styles.userName}
				data-flx="moderation.report-flow.report-flow-preview.report-flow-user-preview.user-name"
			>
				{NicknameUtils.getDisplayName(user)}
			</span>
			<span
				className={styles.userTag}
				data-flx="moderation.report-flow.report-flow-preview.report-flow-user-preview.user-tag"
			>
				{NicknameUtils.formatUserTagForStreamerMode(user)}
			</span>
		</div>
	</div>
));
