// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {DMChannelView} from '@app/features/channel/components/channel_view/DMChannelView';
import styles from '@app/features/channel/components/direct_message/DirectMessageLayout.module.css';
import {DMList} from '@app/features/channel/components/direct_message/DirectMessageList';
import {DMFriendsView} from '@app/features/channel/components/direct_message/DMFriendsView';
import {EmptyStateView} from '@app/features/channel/components/friends/EmptyStateView';
import {RecentMentionsPage} from '@app/features/messaging/components/pages/RecentMentionsPage';
import {SavedMessagesPage} from '@app/features/messaging/components/pages/SavedMessagesPage';
import {useLocation, useParams} from '@app/features/platform/components/router/RouterReact';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Users from '@app/features/user/state/Users';
import VoiceCallFullscreen from '@app/features/voice/state/VoiceCallFullscreen';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';

const NO_COMMUNITIES_TITLE_DESCRIPTOR = msg({
	message: 'No communities yet',
	comment:
		'Heading on the home page when the instance turns off direct messages and friends and the user is in no community.',
});
const NO_COMMUNITIES_SUBTITLE_DESCRIPTOR = msg({
	message: 'Open an invite link to join a community.',
	comment:
		'Text under the heading on the home page when the instance turns off direct messages and friends and the user is in no community.',
});

interface DMLayoutProps {
	children?: ReactNode;
}

export const DMLayout = observer(({children}: DMLayoutProps) => {
	const {i18n} = useLingui();
	const {channelId} = useParams() as {channelId?: string};
	const location = useLocation();
	const mobileLayout = MobileLayout;
	const directMessagesDisabled = RuntimeConfig.directMessagesDisabled;
	const isVoiceCallFullscreenActive = VoiceCallFullscreen.isActive;
	const viewAccountKey = Users.viewAccountKey;
	const renderContent = () => {
		if (location.pathname === Routes.BOOKMARKS) {
			return <SavedMessagesPage data-flx="channel.direct-message.dm-layout.render-content.saved-messages-page" />;
		}
		if (location.pathname === Routes.MENTIONS) {
			return <RecentMentionsPage data-flx="channel.direct-message.dm-layout.render-content.recent-mentions-page" />;
		}
		if (channelId) {
			return (
				<DMChannelView
					channelId={channelId}
					data-flx="channel.direct-message.dm-layout.render-content.dm-channel-view"
				/>
			);
		}
		if (children) {
			return children;
		}
		if (RuntimeConfig.friendsDisabled) {
			return directMessagesDisabled ? (
				<EmptyStateView
					title={i18n._(NO_COMMUNITIES_TITLE_DESCRIPTOR)}
					subtitle={i18n._(NO_COMMUNITIES_SUBTITLE_DESCRIPTOR)}
					data-flx="channel.direct-message.dm-layout.render-content.no-communities"
				/>
			) : null;
		}
		return (
			<DMFriendsView key={viewAccountKey} data-flx="channel.direct-message.dm-layout.render-content.dm-friends-view" />
		);
	};
	if (isVoiceCallFullscreenActive) {
		return (
			<div
				className={clsx(styles.dmLayoutContainer, styles.dmLayoutContainerFullscreen)}
				data-flx="channel.direct-message.dm-layout.dm-layout-container.fullscreen"
			>
				<div
					key="content"
					className={clsx(styles.contentColumn, styles.contentColumnFullscreen)}
					data-flx="channel.direct-message.dm-layout.content-column.fullscreen"
				>
					<div
						className={clsx(styles.contentInner, styles.contentInnerFullscreen)}
						data-flx="channel.direct-message.dm-layout.content-inner.fullscreen"
					>
						{renderContent()}
					</div>
				</div>
			</div>
		);
	}
	if (mobileLayout.enabled) {
		if (!channelId && !children) {
			return (
				<div className={styles.dmListColumn} data-flx="channel.direct-message.dm-layout.dm-list-column">
					<DMList data-flx="channel.direct-message.dm-layout.dm-list" />
				</div>
			);
		}
		return (
			<div className={styles.contentColumn} data-flx="channel.direct-message.dm-layout.content-column">
				<div className={styles.contentInner} data-flx="channel.direct-message.dm-layout.content-inner">
					{renderContent()}
				</div>
			</div>
		);
	}
	return (
		<div className={styles.dmLayoutContainer} data-flx="channel.direct-message.dm-layout.dm-layout-container">
			<div className={styles.dmListColumn} data-flx="channel.direct-message.dm-layout.dm-list-column--2">
				<DMList data-flx="channel.direct-message.dm-layout.dm-list--2" />
			</div>
			<div className={styles.contentColumn} data-flx="channel.direct-message.dm-layout.content-column--2">
				<div className={styles.contentInner} data-flx="channel.direct-message.dm-layout.content-inner--2">
					{renderContent()}
				</div>
			</div>
		</div>
	);
});
