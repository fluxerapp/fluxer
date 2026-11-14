// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ChannelIndexPage.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import {CompactVoiceCallView} from '@app/features/voice/components/CompactVoiceCallView';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface ChannelCompactCallSurfaceProps {
	channel: Channel;
	isExpanded: boolean;
	onToggleExpanded: () => void;
	unreadCount: number;
	mediaMode?: 'live' | 'placeholder';
	avatarFallback?: React.ReactNode;
	avatarFallbackFullBleed?: boolean;
	audioOnly?: boolean;
	hideControlBar?: boolean;
	onFullscreenRequest?: () => void;
	reserveHeaderChrome?: boolean;
}

export const ChannelCompactCallSurface = observer(function ChannelCompactCallSurface({
	channel,
	isExpanded,
	onToggleExpanded,
	unreadCount,
	mediaMode = 'live',
	avatarFallback,
	avatarFallbackFullBleed = false,
	audioOnly = false,
	hideControlBar = false,
	onFullscreenRequest,
	reserveHeaderChrome = false,
}: ChannelCompactCallSurfaceProps) {
	return (
		<div
			className={clsx(styles.compactCallWrapper, isExpanded && styles.compactCallWrapperExpanded)}
			data-flx="channel.channel-view.channel-compact-call-surface.compact-call-wrapper"
		>
			<CompactVoiceCallView
				channel={channel}
				className={clsx(styles.compactVoiceCallView, isExpanded && styles.compactVoiceCallViewExpanded)}
				fillHeight={isExpanded}
				mediaMode={mediaMode}
				audioOnly={audioOnly}
				hideControlBar={hideControlBar}
				heightToggle={{
					isExpanded,
					onToggle: onToggleExpanded,
					unreadCount,
				}}
				avatarFallback={avatarFallback}
				avatarFallbackFullBleed={avatarFallbackFullBleed}
				onFullscreenRequest={onFullscreenRequest}
				reserveHeaderChrome={reserveHeaderChrome}
				data-flx="channel.channel-view.channel-compact-call-surface.compact-voice-call-view"
			/>
		</div>
	);
});
