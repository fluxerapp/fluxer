// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ChannelWelcomeSection.module.css';
import {DMWelcomeSection} from '@app/features/channel/components/direct_message/DMWelcomeSection';
import {GroupDMWelcomeSection} from '@app/features/channel/components/direct_message/GroupDMWelcomeSection';
import {PersonalNotesWelcomeSection} from '@app/features/channel/components/direct_message/PersonalNotesWelcomeSection';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import Guilds from '@app/features/guild/state/Guilds';
import {ThreadWelcomeSection} from '@app/features/threads/components/ThreadWelcomeSection';
import Users from '@app/features/user/state/Users';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import {Trans} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

interface ChannelWelcomeSectionProps {
	channel: Channel;
}

export const ChannelWelcomeSection = observer(({channel}: ChannelWelcomeSectionProps) => {
	const recipient = Users.getUser(channel.recipientIds[0]);
	if (channel.type === ChannelTypes.DM && recipient) {
		return (
			<DMWelcomeSection
				userId={recipient.id}
				channel={channel}
				data-flx="channel.channel-welcome-section.dm-welcome-section"
			/>
		);
	}
	if (channel.type === ChannelTypes.DM_PERSONAL_NOTES && recipient) {
		return (
			<PersonalNotesWelcomeSection
				userId={recipient.id}
				data-flx="channel.channel-welcome-section.personal-notes-welcome-section"
			/>
		);
	}
	if (channel.type === ChannelTypes.GROUP_DM) {
		return (
			<GroupDMWelcomeSection channel={channel} data-flx="channel.channel-welcome-section.group-dm-welcome-section" />
		);
	}
	if (channel.isThread()) {
		return <ThreadWelcomeSection thread={channel} data-flx="channel.channel-welcome-section.thread-welcome-section" />;
	}
	const channelDisplayName = `#${channel.name ?? ''}`;
	const visitor = Users.currentUser;
	const visitHistoryLimited =
		channel.guildId != null &&
		Guilds.getGuild(channel.guildId)?.visiting === true &&
		visitor != null &&
		visitor.accountTier !== 'verified' &&
		channel.createdAt.getTime() < Date.now() - visitor.guildVisitHistoryDays * MS_PER_DAY;
	return (
		<div className={styles.container} data-flx="channel.channel-welcome-section.container">
			<div
				className={clsx('pointer-events-none', styles.channelIcon)}
				data-flx="channel.channel-welcome-section.pointer-events-none"
			>
				{ChannelUtils.getIcon(channel, {className: styles.iconSize})}
			</div>
			<h1 className={styles.heading} data-flx="channel.channel-welcome-section.heading">
				<Trans>Welcome to {channelDisplayName}</Trans>
			</h1>
			<p className={styles.description} data-flx="channel.channel-welcome-section.description">
				{visitHistoryLimited ? (
					<Trans comment="Shown at the top of a channel you are previewing before joining the community, where older messages are hidden until you join.">
						Older messages are visible after you join.
					</Trans>
				) : (
					<Trans>This is the start of {channelDisplayName}.</Trans>
				)}
			</p>
		</div>
	);
});
