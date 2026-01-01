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

import {useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon, PlayIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {type FC, useCallback, useEffect, useState} from 'react';
import {thumbHashToDataURL} from 'thumbhash';
import type {MessageEmbed} from '~/records/MessageRecord';
import {createCalculator} from '~/utils/DimensionUtils';
import * as ImageCacheUtils from '~/utils/ImageCacheUtils';
import {openExternalUrl} from '~/utils/NativeUtils';
import styles from './EmbedYouTube.module.css';
import {OverlayActionButton, OverlayPlayButton} from './MediaButtons';

const YOUTUBE_CONFIG = {
	DEFAULT_WIDTH: 400,
	ANIMATION_DURATION: 0.3,
	BUTTON_DELAY: 0.1,
} as const;

const youtubeCalculator = createCalculator({
	maxWidth: YOUTUBE_CONFIG.DEFAULT_WIDTH,
	responsive: true,
});

interface EmbedYouTubeProps {
	embed: MessageEmbed;
	width?: number;
}

interface ThumbnailProps {
	posterSrc: string;
	thumbHashURL?: string;
	posterLoaded: boolean;
	title?: string;
	onPlay: (event: React.MouseEvent | React.KeyboardEvent) => void;
	onOpenInNewTab: (event: React.MouseEvent | React.KeyboardEvent) => void;
}

const Thumbnail: FC<ThumbnailProps> = observer(
	({posterSrc, thumbHashURL, posterLoaded, title, onPlay, onOpenInNewTab}) => {
		const {t} = useLingui();

		return (
			<div
				className={styles.thumbnail}
				onClick={onPlay}
				role="button"
				tabIndex={0}
				onKeyDown={(e) => {
					if (e.key === 'Enter' || e.key === ' ') {
						e.preventDefault();
						onPlay(e);
					}
				}}
			>
				<AnimatePresence>
					{thumbHashURL && !posterLoaded && (
						<>
							<motion.img
								key="placeholder"
								initial={{opacity: 1}}
								exit={{opacity: 0}}
								transition={{duration: 0.2}}
								src={thumbHashURL}
								alt={t`Video thumbnail`}
								className={styles.thumbnailPlaceholder}
							/>
							<motion.div
								key="overlay"
								initial={{opacity: 1}}
								exit={{opacity: 0}}
								transition={{duration: 0.2}}
								className={styles.overlay}
							/>
						</>
					)}
				</AnimatePresence>

				<motion.img
					src={posterSrc}
					alt={title || 'Video thumbnail'}
					className={styles.posterImage}
					initial={{opacity: 0}}
					animate={{opacity: posterLoaded ? 1 : 0}}
					transition={{duration: 0.2}}
				/>

				<div className={styles.controlsContainer}>
					<div className={styles.buttonGroup}>
						<OverlayPlayButton
							onClick={onPlay}
							icon={<PlayIcon size={28} aria-hidden="true" />}
							ariaLabel={t`Play video`}
						/>
						<OverlayActionButton
							onClick={onOpenInNewTab}
							icon={<ArrowSquareOutIcon size={24} aria-hidden="true" />}
							ariaLabel={t`Open in new tab`}
						/>
					</div>
				</div>
			</div>
		);
	},
);

export const EmbedYouTube: FC<EmbedYouTubeProps> = observer(({embed, width = YOUTUBE_CONFIG.DEFAULT_WIDTH}) => {
	const {t} = useLingui();
	const [hasInteracted, setHasInteracted] = useState(false);
	const [posterLoaded, setPosterLoaded] = useState(false);

	const posterSrc = embed.thumbnail?.proxy_url || '';

	useEffect(() => {
		if (posterSrc) {
			ImageCacheUtils.loadImage(
				posterSrc,
				() => setPosterLoaded(true),
				() => setPosterLoaded(false),
			);
		}
	}, [posterSrc]);

	const handleInitialPlay = useCallback((event: React.MouseEvent | React.KeyboardEvent) => {
		event.stopPropagation();
		setHasInteracted(true);
	}, []);

	const handleOpenInNewTab = useCallback(
		(event: React.MouseEvent | React.KeyboardEvent) => {
			event.stopPropagation();
			if (embed.url) {
				void openExternalUrl(embed.url);
			}
		},
		[embed.url],
	);

	if (!(embed.video && embed.thumbnail && embed.thumbnail.proxy_url)) {
		return null;
	}

	const thumbHashUrl = embed.thumbnail.placeholder
		? thumbHashToDataURL(Uint8Array.from(atob(embed.thumbnail.placeholder), (c) => c.charCodeAt(0)))
		: undefined;

	const {style: containerStyle, dimensions} = youtubeCalculator.calculate(
		{
			width: embed.video.width!,
			height: embed.video.height!,
		},
		{maxWidth: width, forceScale: true},
	);

	const aspectRatio = `${dimensions.width} / ${dimensions.height}`;

	if (!hasInteracted) {
		return (
			<div
				className={styles.container}
				style={{
					...containerStyle,
					minHeight: `${dimensions.height}px`,
					aspectRatio,
					maxWidth: '100%',
				}}
			>
				<Thumbnail
					posterSrc={posterSrc}
					thumbHashURL={thumbHashUrl}
					posterLoaded={posterLoaded}
					title={embed.title}
					onPlay={handleInitialPlay}
					onOpenInNewTab={handleOpenInNewTab}
				/>
			</div>
		);
	}

	const videoUrl = embed.video.url ?? embed.url;
	if (!videoUrl) return null;

	const embedVideoUrl = new URL(videoUrl);
	embedVideoUrl.searchParams.set('autoplay', '1');
	embedVideoUrl.searchParams.set('auto_play', '1');

	return (
		<div
			className={styles.videoContainer}
			style={{
				...containerStyle,
				minHeight: `${dimensions.height}px`,
				aspectRatio,
				maxWidth: '100%',
			}}
		>
			<iframe
				allow="autoplay; fullscreen"
				sandbox="allow-forms allow-modals allow-popups allow-popups-to-escape-sandbox allow-same-origin allow-scripts"
				src={embedVideoUrl.toString()}
				className={styles.iframe}
				title={embed.title || t`YouTube video`}
			/>
		</div>
	);
});
