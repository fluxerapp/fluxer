// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/music/components/MusicPanel.module.css';
import MusicStore from '@app/features/music/state/MusicStore';
import {Avatar} from '@app/features/ui/components/Avatar';
import Users from '@app/features/user/state/Users';
import type {MusicState} from '@fluxer/music_protocol/src/Types';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useRef, useState} from 'react';

const NOTHING_PLAYING_DESCRIPTOR = msg({
	message: 'Nothing is playing',
	comment: 'Shown in the music panel when the queue is empty.',
});
const ADDED_BY_DESCRIPTOR = msg({
	message: 'Added by {name}',
	comment: 'Under the current song in the music panel. {name} is the user who added the song.',
});
const SOMEONE_DESCRIPTOR = msg({
	message: 'someone',
	comment: 'Fallback name when the user who added a song is unknown.',
});
const SEEK_DESCRIPTOR = msg({
	message: 'Seek',
	comment: 'Aria label of the song progress bar in the music panel.',
});

export function formatDuration(ms: number): string {
	const total = Math.max(0, Math.floor(ms / 1000));
	const h = Math.floor(total / 3600);
	const m = Math.floor((total % 3600) / 60);
	const s = total % 60;
	const ss = String(s).padStart(2, '0');
	return h > 0 ? `${h}:${String(m).padStart(2, '0')}:${ss}` : `${m}:${ss}`;
}

interface NowPlayingProps {
	state: MusicState;
}

export const NowPlaying = observer(function NowPlaying({state}: NowPlayingProps) {
	const {i18n} = useLingui();
	const [now, setNow] = useState(() => Date.now());
	const [dragFraction, setDragFraction] = useState<number | null>(null);
	const barRef = useRef<HTMLDivElement>(null);
	useEffect(() => {
		const id = window.setInterval(() => setNow(Date.now()), 500);
		return () => window.clearInterval(id);
	}, []);
	const track = state.current;
	if (!track) {
		return (
			<p className={styles.notice} data-flx="music.now-playing.empty">
				{i18n._(NOTHING_PLAYING_DESCRIPTOR)}
			</p>
		);
	}
	const duration = Math.max(1, track.durationMs);
	const positionMs = Math.min(duration, MusicStore.livePositionMs(now));
	const fraction = dragFraction ?? positionMs / duration;
	const adder = Users.getUser(track.addedBy);
	const fractionFromEvent = (clientX: number): number => {
		const rect = barRef.current?.getBoundingClientRect();
		if (!rect || rect.width === 0) return 0;
		return Math.min(1, Math.max(0, (clientX - rect.left) / rect.width));
	};
	return (
		<div className={styles.nowPlaying} data-flx="music.now-playing.container">
			<div className={styles.nowPlayingRow} data-flx="music.now-playing.row">
				{track.thumbnailUrl && (
					<img
						className={styles.thumbnail}
						src={track.thumbnailUrl}
						alt=""
						referrerPolicy="no-referrer"
						data-flx="music.now-playing.thumbnail"
					/>
				)}
				<div className={styles.trackText} data-flx="music.now-playing.text">
					<a
						className={styles.trackTitle}
						href={track.url}
						target="_blank"
						rel="noopener noreferrer"
						data-flx="music.now-playing.title"
					>
						{track.title}
					</a>
					<span className={styles.trackMeta} data-flx="music.now-playing.author">
						{track.author}
					</span>
					<span className={styles.addedBy} data-flx="music.now-playing.added-by">
						{adder && <Avatar user={adder} size={16} data-flx="music.now-playing.adder-avatar" />}
						{i18n._(ADDED_BY_DESCRIPTOR, {name: adder?.displayName ?? i18n._(SOMEONE_DESCRIPTOR)})}
					</span>
				</div>
			</div>
			<div
				ref={barRef}
				className={styles.progress}
				role="slider"
				tabIndex={0}
				aria-label={i18n._(SEEK_DESCRIPTOR)}
				aria-valuemin={0}
				aria-valuemax={duration}
				aria-valuenow={Math.round(fraction * duration)}
				onPointerDown={(e) => {
					e.currentTarget.setPointerCapture(e.pointerId);
					setDragFraction(fractionFromEvent(e.clientX));
				}}
				onPointerMove={(e) => {
					if (dragFraction !== null) setDragFraction(fractionFromEvent(e.clientX));
				}}
				onPointerUp={(e) => {
					const f = fractionFromEvent(e.clientX);
					setDragFraction(null);
					void MusicStore.seek(Math.round(f * duration));
				}}
				onPointerCancel={() => setDragFraction(null)}
				data-flx="music.now-playing.progress"
			>
				<div
					className={styles.progressFill}
					style={{width: `${fraction * 100}%`}}
					data-flx="music.now-playing.progress-fill"
				/>
			</div>
			<div className={styles.times} data-flx="music.now-playing.times">
				<span data-flx="music.now-playing.position">{formatDuration(fraction * duration)}</span>
				<span data-flx="music.now-playing.duration">{formatDuration(track.durationMs)}</span>
			</div>
		</div>
	);
});
