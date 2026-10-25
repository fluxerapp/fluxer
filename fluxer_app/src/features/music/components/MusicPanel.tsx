// SPDX-License-Identifier: AGPL-3.0-or-later

import {MusicControls} from '@app/features/music/components/MusicControls';
import styles from '@app/features/music/components/MusicPanel.module.css';
import {MusicQueue} from '@app/features/music/components/MusicQueue';
import {MusicSearch} from '@app/features/music/components/MusicSearch';
import {NowPlaying} from '@app/features/music/components/NowPlaying';
import MusicStore from '@app/features/music/state/MusicStore';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Users from '@app/features/user/state/Users';
import {useStoreVersion} from '@app/features/voice/engine/Store';
import type {LastAction} from '@fluxer/music_protocol/src/Types';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const MUSIC_TITLE_DESCRIPTOR = msg({
	message: 'Music',
	comment: 'Heading of the music panel in the voice call view.',
});
const CLOSE_PANEL_DESCRIPTOR = msg({
	message: 'Close music panel',
	comment: 'Aria label of the button that closes the music panel.',
});
const UNDEAFEN_DESCRIPTOR = msg({
	message: 'Undeafen to use music',
	comment: 'Shown in the music panel while the user is deafened, since music cannot be heard or controlled.',
});
const SOMEONE_DESCRIPTOR = msg({
	message: 'someone',
	comment: 'Fallback name in the music activity line when the user who did something is unknown.',
});
const A_SONG_DESCRIPTOR = msg({
	message: 'a song',
	comment: 'Fallback in the music activity line when the song title is unknown.',
});

const ACTIVITY_DESCRIPTORS: Record<LastAction['kind'], MessageDescriptor> = {
	add: msg({message: '➕ {name} added {title}', comment: 'Music activity line. {name} is a user, {title} a song.'}),
	pause: msg({message: '⏸ {name} paused the music', comment: 'Music activity line. {name} is a user.'}),
	resume: msg({message: '▶ {name} resumed the music', comment: 'Music activity line. {name} is a user.'}),
	skip: msg({message: '⏭ {name} skipped {title}', comment: 'Music activity line. {name} is a user, {title} a song.'}),
	seek: msg({
		message: '⏩ {name} jumped within {title}',
		comment: 'Music activity line. {name} is a user, {title} a song.',
	}),
	remove: msg({
		message: '✕ {name} removed {title}',
		comment: 'Music activity line. {name} is a user, {title} a song.',
	}),
	move: msg({message: '↕ {name} moved {title}', comment: 'Music activity line. {name} is a user, {title} a song.'}),
	clear: msg({message: '🗑 {name} cleared the queue', comment: 'Music activity line. {name} is a user.'}),
	shuffle: msg({message: '🔀 {name} shuffled the queue', comment: 'Music activity line. {name} is a user.'}),
	loop: msg({message: '🔁 {name} changed the loop mode', comment: 'Music activity line. {name} is a user.'}),
	stop: msg({message: '⏹ {name} stopped the music', comment: 'Music activity line. {name} is a user.'}),
	failed: msg({message: "⚠ Couldn't play {title}", comment: 'Music activity line. {title} is a song.'}),
	unmatched: msg({message: '⚠ No YouTube match for {title}', comment: 'Music activity line. {title} is a song.'}),
	blocked: msg({
		message: '⚠ YouTube may be blocking the bot',
		comment: 'Music activity line shown when YouTube refuses requests.',
	}),
};

export const MusicPanel = observer(function MusicPanel() {
	const {i18n} = useLingui();
	useStoreVersion(MusicStore);
	const isMobile = MobileLayout.isMobileLayout();
	if (!MusicStore.panelOpen) return null;
	const state = MusicStore.state;
	const hasBot = MusicStore.botIdentity !== null && state !== null;
	let body: React.ReactNode;
	if (MusicStore.isDeafened) {
		body = (
			<p className={styles.notice} data-flx="music.music-panel.deafened">
				{i18n._(UNDEAFEN_DESCRIPTOR)}
			</p>
		);
	} else if (!hasBot) {
		body = <MusicSearch startMode data-flx="music.music-panel.search-start" />;
	} else {
		const action = state.lastAction;
		const activity = action ? activityText(action) : null;
		body = (
			<>
				<NowPlaying state={state} data-flx="music.music-panel.now-playing" />
				<MusicControls state={state} data-flx="music.music-panel.controls" />
				<MusicSearch data-flx="music.music-panel.search" />
				<MusicQueue state={state} data-flx="music.music-panel.queue" />
				{activity && (
					<p className={styles.activity} data-flx="music.music-panel.activity">
						{activity}
					</p>
				)}
			</>
		);
	}
	return (
		<aside className={clsx(styles.panel, isMobile && styles.panelMobile)} data-flx="music.music-panel.panel">
			<div className={styles.header} data-flx="music.music-panel.header">
				<h2 className={styles.title} data-flx="music.music-panel.title">
					{i18n._(MUSIC_TITLE_DESCRIPTOR)}
				</h2>
				<button
					type="button"
					className={styles.iconButton}
					aria-label={i18n._(CLOSE_PANEL_DESCRIPTOR)}
					onClick={() => MusicStore.togglePanel()}
					data-flx="music.music-panel.close"
				>
					<XIcon weight="bold" data-flx="music.music-panel.close-icon" />
				</button>
			</div>
			<div className={styles.body} data-flx="music.music-panel.body">
				{body}
			</div>
		</aside>
	);

	function activityText(action: LastAction): string {
		const user = action.by === null ? undefined : Users.getUser(action.by);
		const name = user?.displayName ?? i18n._(SOMEONE_DESCRIPTOR);
		const title = action.trackTitle ?? i18n._(A_SONG_DESCRIPTOR);
		return i18n._(ACTIVITY_DESCRIPTORS[action.kind], {name, title});
	}
});
