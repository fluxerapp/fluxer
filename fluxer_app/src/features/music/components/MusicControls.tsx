// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/music/components/MusicPanel.module.css';
import MusicStore from '@app/features/music/state/MusicStore';
import type {LoopMode, MusicState} from '@fluxer/music_protocol/src/Types';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	PauseIcon,
	PlayIcon,
	RepeatIcon,
	RepeatOnceIcon,
	ShuffleIcon,
	SkipForwardIcon,
	StopIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

const PLAY_DESCRIPTOR = msg({message: 'Play', comment: 'Music control button label.'});
const PAUSE_DESCRIPTOR = msg({message: 'Pause', comment: 'Music control button label.'});
const SKIP_DESCRIPTOR = msg({message: 'Skip', comment: 'Music control button label that skips the current song.'});
const SHUFFLE_DESCRIPTOR = msg({message: 'Shuffle', comment: 'Music control button label that shuffles the queue.'});
const LOOP_OFF_DESCRIPTOR = msg({message: 'Loop: off', comment: 'Music loop button label when looping is off.'});
const LOOP_TRACK_DESCRIPTOR = msg({
	message: 'Loop: song',
	comment: 'Music loop button label when the current song repeats.',
});
const LOOP_QUEUE_DESCRIPTOR = msg({
	message: 'Loop: queue',
	comment: 'Music loop button label when the whole queue repeats.',
});
const STOP_DESCRIPTOR = msg({
	message: 'Stop and disconnect',
	comment: 'Music control button label that stops the music and makes the bot leave.',
});

const LOOP_LABELS = {off: LOOP_OFF_DESCRIPTOR, track: LOOP_TRACK_DESCRIPTOR, queue: LOOP_QUEUE_DESCRIPTOR};
const NEXT_LOOP: Record<LoopMode, LoopMode> = {off: 'track', track: 'queue', queue: 'off'};

interface MusicControlsProps {
	state: MusicState;
}

export const MusicControls = observer(function MusicControls({state}: MusicControlsProps) {
	const {i18n} = useLingui();
	const paused = state.status === 'paused';
	const loopLabel = LOOP_LABELS[state.loop];
	const LoopIcon = state.loop === 'track' ? RepeatOnceIcon : RepeatIcon;
	return (
		<div className={styles.controls} data-flx="music.music-controls.container">
			<button
				type="button"
				className={styles.iconButton}
				aria-label={i18n._(paused ? PLAY_DESCRIPTOR : PAUSE_DESCRIPTOR)}
				onClick={() => void (paused ? MusicStore.resume() : MusicStore.pause())}
				data-flx="music.music-controls.play-pause"
			>
				{paused ? (
					<PlayIcon weight="fill" data-flx="music.music-controls.play-icon" />
				) : (
					<PauseIcon weight="fill" data-flx="music.music-controls.pause-icon" />
				)}
			</button>
			<button
				type="button"
				className={styles.iconButton}
				aria-label={i18n._(SKIP_DESCRIPTOR)}
				onClick={() => void MusicStore.skip()}
				data-flx="music.music-controls.skip"
			>
				<SkipForwardIcon weight="fill" data-flx="music.music-controls.skip-icon" />
			</button>
			<button
				type="button"
				className={styles.iconButton}
				aria-label={i18n._(SHUFFLE_DESCRIPTOR)}
				onClick={() => void MusicStore.shuffle()}
				data-flx="music.music-controls.shuffle"
			>
				<ShuffleIcon weight="bold" data-flx="music.music-controls.shuffle-icon" />
			</button>
			<button
				type="button"
				className={clsx(styles.iconButton, state.loop !== 'off' && styles.iconButtonActive)}
				aria-label={i18n._(loopLabel)}
				aria-pressed={state.loop !== 'off'}
				onClick={() => void MusicStore.setLoop(NEXT_LOOP[state.loop])}
				data-flx="music.music-controls.loop"
			>
				<LoopIcon weight="bold" data-flx="music.music-controls.loop-icon" />
			</button>
			<button
				type="button"
				className={styles.iconButton}
				aria-label={i18n._(STOP_DESCRIPTOR)}
				onClick={() => void MusicStore.stop()}
				data-flx="music.music-controls.stop"
			>
				<StopIcon weight="fill" data-flx="music.music-controls.stop-icon" />
			</button>
		</div>
	);
});
