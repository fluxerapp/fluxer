// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ChannelWelcomeSection.module.css';
import {DMWelcomeSection} from '@app/features/channel/components/direct_message/DMWelcomeSection';
import {GroupDMWelcomeSection} from '@app/features/channel/components/direct_message/GroupDMWelcomeSection';
import {PersonalNotesWelcomeSection} from '@app/features/channel/components/direct_message/PersonalNotesWelcomeSection';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {ThreadWelcomeSection} from '@app/features/threads/components/ThreadWelcomeSection';
import Users from '@app/features/user/state/Users';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
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
				<Trans>In the beginning, there was nothing. Then, there was {channelDisplayName}. And it was good.</Trans>
			</p>
		</div>
	);
});
