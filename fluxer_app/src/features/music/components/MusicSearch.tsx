// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/music/components/MusicPanel.module.css';
import {formatDuration} from '@app/features/music/components/NowPlaying';
import MusicStore from '@app/features/music/state/MusicStore';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Paste a link or search for a song',
	comment: 'Placeholder of the search box in the music panel.',
});
const START_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Type a song name or paste a link to start the music bot',
	comment: 'Placeholder of the search box in the music panel when no music bot is in the call yet.',
});
const SEARCHING_DESCRIPTOR = msg({message: 'Searching...', comment: 'Shown while music search results load.'});
const ADD_DESCRIPTOR = msg({message: 'Add', comment: 'Button that adds a search result to the music queue.'});
const PLAY_NEXT_DESCRIPTOR = msg({
	message: 'Play next',
	comment: 'Button that adds a search result to the front of the music queue.',
});

function isUrl(input: string): boolean {
	return /^https?:\/\//i.test(input);
}

interface MusicSearchProps {
	startMode?: boolean;
}

export const MusicSearch = observer(function MusicSearch({startMode = false}: MusicSearchProps) {
	const {i18n} = useLingui();
	const [input, setInput] = useState('');
	const submit = () => {
		const text = input.trim();
		if (!text) return;
		if (startMode) {
			void MusicStore.startWithChat(text);
			setInput('');
		} else if (isUrl(text)) {
			void MusicStore.add(text);
			setInput('');
		} else if (!MusicStore.searching) {
			void MusicStore.search(text);
		}
	};
	const results = startMode ? [] : MusicStore.searchResults;
	return (
		<div className={styles.search} data-flx="music.music-search.container">
			<input
				className={styles.searchInput}
				type="text"
				value={input}
				placeholder={i18n._(startMode ? START_PLACEHOLDER_DESCRIPTOR : PLACEHOLDER_DESCRIPTOR)}
				aria-label={i18n._(startMode ? START_PLACEHOLDER_DESCRIPTOR : PLACEHOLDER_DESCRIPTOR)}
				onChange={(e) => setInput(e.target.value)}
				onKeyDown={(e) => {
					if (e.key === 'Enter') submit();
				}}
				data-flx="music.music-search.input"
			/>
			{MusicStore.searching && !startMode && (
				<p className={styles.notice} data-flx="music.music-search.searching">
					{i18n._(SEARCHING_DESCRIPTOR)}
				</p>
			)}
			{results.length > 0 && (
				<ul className={styles.list} data-flx="music.music-search.results">
					{results.map((result) => (
						<li key={result.url} className={styles.row} data-flx="music.music-search.result">
							<div className={styles.trackText} data-flx="music.music-search.result-text">
								<span className={styles.trackTitle} data-flx="music.music-search.result-title">
									{result.title}
								</span>
								<span className={styles.trackMeta} data-flx="music.music-search.result-meta">
									{result.author} · {formatDuration(result.durationMs)}
								</span>
							</div>
							<button
								type="button"
								className={styles.textButton}
								onClick={() => void MusicStore.add(result.url)}
								data-flx="music.music-search.add"
							>
								{i18n._(ADD_DESCRIPTOR)}
							</button>
							<button
								type="button"
								className={styles.textButton}
								onClick={() => void MusicStore.add(result.url, true)}
								data-flx="music.music-search.play-next"
							>
								{i18n._(PLAY_NEXT_DESCRIPTOR)}
							</button>
						</li>
					))}
				</ul>
			)}
		</div>
	);
});
