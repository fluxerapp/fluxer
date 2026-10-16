// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type RememberedSkeletonActiveNowCard,
	reportSkeletonActiveNowLayout,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import styles from '@app/features/channel/components/active_now/ActiveNowSidebar.module.css';
import {useActiveFriendVoiceStates} from '@app/features/channel/components/active_now/useActiveFriendVoiceStates';
import {ACTIVE_NOW_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {VoiceActivityCard} from '@app/features/user/components/profile/VoiceActivityCard';
import PrivacyPreferences from '@app/features/user/state/PrivacyPreferences';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo} from 'react';

const IT_S_QUIET_FOR_NOW_DESCRIPTOR = msg({
	message: "It's quiet for now...",
	comment: 'Label in the channel and chat active now sidebar.',
});
const WHEN_FRIENDS_ARE_ACTIVE_IN_VOICE_CHANNELS_THEIR_DESCRIPTOR = msg({
	message: 'When friends are active in voice channels, their activity will appear here.',
	comment: 'Description text in the channel and chat active now sidebar.',
});
export const ActiveNowSidebar: React.FC = observer(function ActiveNowSidebar() {
	const {i18n} = useLingui();
	const showActiveNow = PrivacyPreferences.getShowActiveNow();
	const activeChannels = useActiveFriendVoiceStates();
	const activeNowCards = useMemo<ReadonlyArray<RememberedSkeletonActiveNowCard>>(
		() =>
			activeChannels.map((activity) => ({
				participantCount: activity.participantUsers.length,
				streaming: activity.isStreaming && activity.streamKey != null,
			})),
		[activeChannels],
	);
	useEffect(() => {
		reportSkeletonActiveNowLayout(showActiveNow, activeNowCards);
	}, [activeNowCards, showActiveNow]);
	if (!showActiveNow) {
		return null;
	}
	return (
		<aside
			className={styles.sidebar}
			aria-label={i18n._(ACTIVE_NOW_DESCRIPTOR)}
			data-flx="channel.active-now.active-now-sidebar.sidebar"
		>
			<div className={styles.header} data-flx="channel.active-now.active-now-sidebar.header">
				<h2 className={styles.headerTitle} data-flx="channel.active-now.active-now-sidebar.header-title">
					{i18n._(ACTIVE_NOW_DESCRIPTOR)}
				</h2>
			</div>
			{activeChannels.length === 0 ? (
				<div className={styles.emptyState} data-flx="channel.active-now.active-now-sidebar.empty-state">
					<svg
						stroke="currentColor"
						fill="none"
						strokeWidth="2"
						viewBox="0 0 24 24"
						strokeLinecap="round"
						strokeLinejoin="round"
						className={styles.emptyIcon}
						xmlns="http://www.w3.org/2000/svg"
						data-flx="channel.active-now.active-now-sidebar.empty-icon"
					>
						<path d="M4 12h6l-6 8h6" data-flx="channel.active-now.active-now-sidebar.path" />
						<path d="M14 4h6l-6 8h6" data-flx="channel.active-now.active-now-sidebar.path--2" />
					</svg>
					<span className={styles.emptyTitle} data-flx="channel.active-now.active-now-sidebar.empty-title">
						{i18n._(IT_S_QUIET_FOR_NOW_DESCRIPTOR)}
					</span>
					<span className={styles.emptyDescription} data-flx="channel.active-now.active-now-sidebar.empty-description">
						{i18n._(WHEN_FRIENDS_ARE_ACTIVE_IN_VOICE_CHANNELS_THEIR_DESCRIPTOR)}
					</span>
				</div>
			) : (
				<div className={styles.content} data-flx="channel.active-now.active-now-sidebar.content">
					{activeChannels.map((activity) => (
						<VoiceActivityCard
							key={activity.channelId}
							activity={activity}
							data-flx="channel.active-now.active-now-sidebar.voice-activity-card"
						/>
					))}
				</div>
			)}
		</aside>
	);
});
