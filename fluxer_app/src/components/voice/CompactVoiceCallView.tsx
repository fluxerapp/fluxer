/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	isTrackReference,
	ParticipantContext,
	TrackRefContext,
	type TrackReference,
	useConnectionState,
	useParticipants,
	useTracks,
} from '@livekit/components-react';
import {clsx} from 'clsx';
import {ConnectionState, Track} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';
import type {ChannelRecord} from '~/records/ChannelRecord';
import styles from './CompactVoiceCallView.module.css';
import {VoiceControlBar} from './VoiceControlBar';
import {VoiceParticipantTile} from './VoiceParticipantTile';

interface CompactVoiceCallViewProps {
	channel: ChannelRecord;
	className?: string;
	hideHeader?: boolean;
	hideControlBar?: boolean;
	controlBar?: React.ReactNode;
}

const MAX_TILES = 4;

function getConnectionLabel(state: ConnectionState, participantCount: number, t: (m: MessageDescriptor) => string) {
	switch (state) {
		case ConnectionState.Connecting:
			return t(msg({message: 'Connecting…'}));
		case ConnectionState.Reconnecting:
			return t(msg({message: 'Reconnecting…'}));
		case ConnectionState.Disconnected:
			return t(msg({message: 'Disconnected'}));
		default:
			return participantCount === 1
				? t(msg({message: 'Voice Connected'}))
				: t(msg({message: `${participantCount} in call`}));
	}
}

function trackSortKey(tr: TrackReference) {
	const sourceRank = tr.source === Track.Source.ScreenShare ? 0 : 1;
	return `${sourceRank}:${tr.participant?.identity ?? ''}:${tr.publication?.trackSid ?? ''}`;
}

export const CompactVoiceCallView: React.FC<CompactVoiceCallViewProps> = observer(function CompactVoiceCallView({
	channel,
	className,
	hideHeader = false,
	hideControlBar = false,
	controlBar,
}) {
	const {t} = useLingui();
	const participants = useParticipants();
	const connectionState = useConnectionState();

	const tracks = useTracks(
		[
			{source: Track.Source.ScreenShare, withPlaceholder: false},
			{source: Track.Source.Camera, withPlaceholder: false},
		],
		{onlySubscribed: true},
	);

	const trackRefs = useMemo(() => tracks.filter(isTrackReference) as Array<TrackReference>, [tracks]);

	const sortedTrackRefs = useMemo(() => {
		return [...trackRefs].sort((a, b) => trackSortKey(a).localeCompare(trackSortKey(b)));
	}, [trackRefs]);

	const visibleTracks = useMemo(() => sortedTrackRefs.slice(0, MAX_TILES), [sortedTrackRefs]);
	const overflowCount = Math.max(0, sortedTrackRefs.length - MAX_TILES);

	const participantCount = participants.length;
	const statusText = useMemo(
		() => getConnectionLabel(connectionState, participantCount, t),
		[connectionState, participantCount, t],
	);

	const ariaLabel = useMemo(() => {
		if (connectionState !== ConnectionState.Connected) return statusText;
		return t`Voice call. ${statusText}.`;
	}, [connectionState, statusText, t]);

	const containerClassName = clsx(styles.container, className, hideHeader && styles.containerNoHeader);
	const controlBarContent = hideControlBar ? null : (controlBar ?? <VoiceControlBar />);

	const showVideoStrip = visibleTracks.length > 0;

	return (
		<section className={containerClassName} aria-label={ariaLabel}>
			{showVideoStrip && (
				<div className={styles.videoSection}>
					<div className={styles.videoContainer} role="list" aria-label={t(msg({message: 'Video previews'}))}>
						{visibleTracks.map((trackRef) => {
							const key = `${trackRef.publication?.trackSid ?? 'pub'}:${trackRef.participant.identity}:${trackRef.source}`;

							return (
								<div key={key} className={styles.videoTile} role="listitem">
									<TrackRefContext.Provider value={trackRef}>
										<ParticipantContext.Provider value={trackRef.participant}>
											<VoiceParticipantTile trackRef={trackRef} guildId={channel.guildId} channelId={channel.id} />
										</ParticipantContext.Provider>
									</TrackRefContext.Provider>
								</div>
							);
						})}

						{overflowCount > 0 && (
							<div className={styles.moreVideos} role="listitem" aria-label={t`${overflowCount} more videos`}>
								<span className={styles.moreVideosText}>{t`+${overflowCount} more`}</span>
							</div>
						)}
					</div>
				</div>
			)}

			{controlBarContent && <footer className={styles.controlBarSection}>{controlBarContent}</footer>}
		</section>
	);
});
