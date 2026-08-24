// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAnimatedMediaVideoPlayback} from '@app/features/app/hooks/useAnimatedMediaPlayback';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import styles from '@app/features/channel/components/AutocompleteEmoji.module.css';
import type {FavoriteMeme} from '@app/features/expressions/models/FavoriteMeme';
import {buildStaticGifPreviewURL} from '@app/features/messaging/utils/MediaProxyUtils';
import {MusicNoteIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useRef} from 'react';

const AutocompleteMemeVideo = ({src}: {src: string}) => {
	const videoRef = useRef<HTMLVideoElement>(null);
	const playbackAllowed = useAnimatedMediaVideoPlayback(videoRef);
	return (
		<video
			ref={videoRef}
			className={styles.memeVideo}
			muted
			autoPlay={playbackAllowed}
			loop
			playsInline
			preload="auto"
			src={src}
			data-flx="channel.autocomplete-meme-preview.meme-video"
		/>
	);
};
export const AutocompleteMemePreview = observer(({meme}: {meme: FavoriteMeme}) => {
	const contentType = meme.contentType.toLowerCase();
	const isAudioMeme = contentType.startsWith('audio/');
	const isVideoMeme = contentType.startsWith('video/');
	const isAnimatedImageMeme = !isVideoMeme && !isAudioMeme && contentType.includes('gif');
	const motionAllowed = useShouldAnimate({kind: 'gif', isAnimated: isVideoMeme || isAnimatedImageMeme});
	if (isAudioMeme) {
		return (
			<div className={styles.audioIconWrapper} data-flx="channel.autocomplete-meme-preview.audio-icon-wrapper">
				<MusicNoteIcon
					className={styles.audioIcon}
					weight="fill"
					data-flx="channel.autocomplete-meme-preview.audio-icon"
				/>
			</div>
		);
	}
	if (isVideoMeme && motionAllowed) {
		return (
			<AutocompleteMemeVideo src={meme.url} data-flx="channel.autocomplete-meme-preview.autocomplete-meme-video" />
		);
	}
	const needsStillFrame = (isVideoMeme || isAnimatedImageMeme) && !motionAllowed;
	return (
		<img
			draggable={false}
			className={styles.memeIcon}
			src={needsStillFrame ? buildStaticGifPreviewURL(meme.url) : meme.url}
			alt={meme.name}
			aria-hidden={true}
			data-flx="channel.autocomplete-meme-preview.meme-icon"
		/>
	);
});
