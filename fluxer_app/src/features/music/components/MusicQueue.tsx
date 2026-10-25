// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import styles from '@app/features/music/components/MusicPanel.module.css';
import {formatDuration} from '@app/features/music/components/NowPlaying';
import MusicStore from '@app/features/music/state/MusicStore';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import Users from '@app/features/user/state/Users';
import type {MusicState, Track} from '@fluxer/music_protocol/src/Types';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';
import {useDrag, useDrop} from 'react-dnd';

const QUEUE_DND_TYPE = 'music-queue-track';

const QUEUE_DESCRIPTOR = msg({message: 'Up next', comment: 'Heading of the music queue list.'});
const EMPTY_DESCRIPTOR = msg({
	message: 'The queue is empty',
	comment: 'Shown in the music panel when no songs are queued.',
});
const LOAD_MORE_DESCRIPTOR = msg({message: 'Load more', comment: 'Button that loads more songs of the music queue.'});
const CLEAR_DESCRIPTOR = msg({message: 'Clear', comment: 'Button that clears the music queue.'});
const CLEAR_TITLE_DESCRIPTOR = msg({
	message: 'Clear the queue?',
	comment: 'Title of the confirmation dialog for clearing the music queue.',
});
const CLEAR_BODY_DESCRIPTOR = msg({
	message: 'This removes every upcoming song. The current song keeps playing.',
	comment: 'Body of the confirmation dialog for clearing the music queue.',
});
const REMOVE_DESCRIPTOR = msg({
	message: 'Remove from queue',
	comment: 'Aria label of the button that removes a song from the queue.',
});
const ADDED_BY_DESCRIPTOR = msg({
	message: 'added by {name}',
	comment: 'Under a queued song. {name} is the user who added it.',
});
const SOMEONE_DESCRIPTOR = msg({
	message: 'someone',
	comment: 'Fallback name when the user who added a song is unknown.',
});

interface QueueRowProps {
	track: Track;
	index: number;
}

const QueueRow = observer(function QueueRow({track, index}: QueueRowProps) {
	const {i18n} = useLingui();
	const [{isDragging}, dragRef] = useDrag(
		() => ({
			type: QUEUE_DND_TYPE,
			item: {id: track.id},
			collect: (monitor) => ({isDragging: monitor.isDragging()}),
		}),
		[track.id],
	);
	const [{isOver}, dropRef] = useDrop(
		() => ({
			accept: QUEUE_DND_TYPE,
			drop: (item: {id: string}) => {
				if (item.id !== track.id) void MusicStore.move(item.id, index);
			},
			collect: (monitor) => ({isOver: monitor.isOver()}),
		}),
		[track.id, index],
	);
	const adder = Users.getUser(track.addedBy);
	return (
		<li
			ref={(node) => {
				dragRef(node);
				dropRef(node);
			}}
			className={clsx(styles.row, styles.queueRow, isDragging && styles.rowDragging, isOver && styles.rowOver)}
			data-flx="music.music-queue.row"
		>
			<div className={styles.trackText} data-flx="music.music-queue.row-text">
				<span className={styles.trackTitle} data-flx="music.music-queue.row-title">
					{track.title}
				</span>
				<span className={styles.trackMeta} data-flx="music.music-queue.row-meta">
					{formatDuration(track.durationMs)} ·{' '}
					{i18n._(ADDED_BY_DESCRIPTOR, {name: adder?.displayName ?? i18n._(SOMEONE_DESCRIPTOR)})}
				</span>
			</div>
			<button
				type="button"
				className={styles.iconButton}
				aria-label={i18n._(REMOVE_DESCRIPTOR)}
				onClick={() => void MusicStore.remove(track.id)}
				data-flx="music.music-queue.remove"
			>
				<XIcon weight="bold" data-flx="music.music-queue.remove-icon" />
			</button>
		</li>
	);
});

interface MusicQueueProps {
	state: MusicState;
}

export const MusicQueue = observer(function MusicQueue({state}: MusicQueueProps) {
	const {i18n} = useLingui();
	const [loaded, setLoaded] = useState<{key: string; tracks: Array<Track>}>({key: '', tracks: []});
	const [loading, setLoading] = useState(false);
	const queueKey = `${state.current?.id ?? ''}:${state.queueLength}`;
	const extra = loaded.key === queueKey ? loaded.tracks : [];
	const seen = new Set(state.upcoming.map((t) => t.id));
	const shown = [...state.upcoming, ...extra.filter((t) => !seen.has(t.id))];
	const loadMore = async () => {
		setLoading(true);
		try {
			const page = await MusicStore.queuePage(shown.length);
			setLoaded({key: queueKey, tracks: [...extra, ...page.tracks]});
		} finally {
			setLoading(false);
		}
	};
	const confirmClear = () => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(CLEAR_TITLE_DESCRIPTOR)}
					description={i18n._(CLEAR_BODY_DESCRIPTOR)}
					primaryText={i18n._(CLEAR_DESCRIPTOR)}
					primaryVariant="danger"
					onPrimary={() => MusicStore.clear()}
					data-flx="music.music-queue.clear-confirm"
				/>
			)),
		);
	};
	return (
		<div className={styles.queue} data-flx="music.music-queue.container">
			<div className={styles.queueHeader} data-flx="music.music-queue.header">
				<h3 className={styles.sectionTitle} data-flx="music.music-queue.title">
					{i18n._(QUEUE_DESCRIPTOR)}
				</h3>
				{state.queueLength > 0 && (
					<button type="button" className={styles.textButton} onClick={confirmClear} data-flx="music.music-queue.clear">
						{i18n._(CLEAR_DESCRIPTOR)}
					</button>
				)}
			</div>
			{shown.length === 0 ? (
				<p className={styles.notice} data-flx="music.music-queue.empty">
					{i18n._(EMPTY_DESCRIPTOR)}
				</p>
			) : (
				<ul className={styles.list} data-flx="music.music-queue.list">
					{shown.map((track, index) => (
						<QueueRow key={track.id} track={track} index={index} data-flx="music.music-queue.queue-row" />
					))}
				</ul>
			)}
			{shown.length < state.queueLength && (
				<button
					type="button"
					className={styles.textButton}
					disabled={loading}
					onClick={() => void loadMore()}
					data-flx="music.music-queue.load-more"
				>
					{i18n._(LOAD_MORE_DESCRIPTOR)}
				</button>
			)}
		</div>
	);
});
