// SPDX-License-Identifier: AGPL-3.0-or-later

import {Message} from '@app/features/channel/components/ChannelMessage';
import Channels from '@app/features/channel/state/Channels';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import {Message as MessageModel} from '@app/features/messaging/models/MessagingMessage';
import type {ReportedGuild} from '@app/features/moderation/components/report_flow/ReportFlowModal';
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
		className={styles.targetCard}
		data-flx="moderation.report-flow.report-flow-preview.report-flow-user-preview.user-card"
	>
		<Avatar
			user={user}
			size={40}
			guildId={guildId ?? null}
			data-flx="moderation.report-flow.report-flow-preview.report-flow-user-preview.avatar"
		/>
		<div
			className={styles.targetText}
			data-flx="moderation.report-flow.report-flow-preview.report-flow-user-preview.user-text"
		>
			<span
				className={styles.targetName}
				data-flx="moderation.report-flow.report-flow-preview.report-flow-user-preview.user-name"
			>
				{NicknameUtils.getDisplayName(user)}
			</span>
			<span
				className={styles.targetTag}
				data-flx="moderation.report-flow.report-flow-preview.report-flow-user-preview.user-tag"
			>
				{NicknameUtils.formatUserTagForStreamerMode(user)}
			</span>
		</div>
	</div>
));

export const ReportFlowGuildPreview: React.FC<{guild: ReportedGuild}> = observer(({guild}) => (
	<div
		className={styles.targetCard}
		data-flx="moderation.report-flow.report-flow-preview.report-flow-guild-preview.target-card"
	>
		<GuildIcon
			id={guild.id}
			name={guild.name}
			icon={guild.icon}
			sizePx={40}
			containerProps={{'aria-hidden': true}}
			data-flx="moderation.report-flow.report-flow-preview.report-flow-guild-preview.guild-icon"
		/>
		<span
			className={styles.targetName}
			data-flx="moderation.report-flow.report-flow-preview.report-flow-guild-preview.target-name"
		>
			{guild.name}
		</span>
	</div>
));
