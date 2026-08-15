// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {YOUTUBE_PROVIDER_NAME} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/channel/components/embeds/media/EmbedYouTube.module.css';
import {OverlayActionButton, OverlayPlayButton} from '@app/features/channel/components/embeds/media/MediaButtons';
import {useNearViewport} from '@app/features/messaging/hooks/useNearViewport';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {decodeThumbHashDataURL} from '@app/features/messaging/utils/ThumbHashUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {createCalculator} from '@app/features/ui/utils/DimensionUtils';
import type {MessageEmbed} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon, PlayIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {type FC, useCallback, useEffect, useMemo, useState} from 'react';

const VIDEO_THUMBNAIL_DESCRIPTOR = msg({
	message: 'Video thumbnail',
	comment: 'Short label in the channel and chat embed you tube. Keep it concise.',
});
const PLAY_VIDEO_DESCRIPTOR = msg({
	message: 'Play video',
	comment: 'Short label in the channel and chat embed you tube. Keep it concise.',
});
const OPEN_IN_NEW_TAB_DESCRIPTOR = msg({
	message: 'Open in new tab',
	comment: 'Button or menu action label in the channel and chat embed you tube. Keep it concise.',
});
const VIDEO_DESCRIPTOR = msg({
	message: '{youtubeProviderName} video',
	comment:
		'Short label in the channel and chat embed you tube. Keep it concise. Preserve {youtubeProviderName}; it is inserted by code.',
});
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
	posterSrc?: string;
	thumbHashURL?: string;
	posterLoaded: boolean;
	posterCachedOnMount: boolean;
	title?: string;
	onPlay: (event: React.MouseEvent | React.KeyboardEvent) => void;
	onOpenInNewTab: (event: React.MouseEvent | React.KeyboardEvent) => void;
}

const Thumbnail: FC<ThumbnailProps> = observer(
	({posterSrc, thumbHashURL, posterLoaded, posterCachedOnMount, title, onPlay, onOpenInNewTab}) => {
		const {i18n} = useLingui();
		return (
			<div className={styles.thumbnail} data-flx="channel.embeds.media.embed-you-tube.thumbnail.thumbnail">
				<AnimatePresence data-flx="channel.embeds.media.embed-you-tube.thumbnail.animate-presence">
					{thumbHashURL && !posterLoaded && (
						<>
							<motion.img
								key="placeholder"
								initial={{opacity: 1}}
								exit={{opacity: 0}}
								transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2}}
								src={thumbHashURL}
								alt={i18n._(VIDEO_THUMBNAIL_DESCRIPTOR)}
								className={styles.thumbnailPlaceholder}
								data-flx="channel.embeds.media.embed-you-tube.thumbnail.thumbnail-placeholder"
							/>
							<motion.div
								key="overlay"
								initial={{opacity: 1}}
								exit={{opacity: 0}}
								transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2}}
								className={styles.overlay}
								data-flx="channel.embeds.media.embed-you-tube.thumbnail.overlay"
							/>
						</>
					)}
				</AnimatePresence>
				{posterSrc && (
					<motion.img
						src={posterSrc}
						alt={title || 'Video thumbnail'}
						className={styles.posterImage}
						initial={{opacity: posterCachedOnMount ? 1 : 0}}
						animate={{opacity: posterLoaded ? 1 : 0}}
						transition={{duration: posterCachedOnMount || Accessibility.useReducedMotion ? 0 : 0.2}}
						data-flx="channel.embeds.media.embed-you-tube.thumbnail.poster-image"
					/>
				)}
				<button
					type="button"
					className={styles.thumbnailClickTarget}
					onClick={onPlay}
					aria-label={i18n._(PLAY_VIDEO_DESCRIPTOR)}
					data-flx="channel.embeds.media.embed-you-tube.thumbnail.click-target"
				/>
				<div
					className={styles.controlsContainer}
					data-flx="channel.embeds.media.embed-you-tube.thumbnail.controls-container"
				>
					<div className={styles.buttonGroup} data-flx="channel.embeds.media.embed-you-tube.thumbnail.button-group">
						<OverlayPlayButton
							onClick={onPlay}
							icon={
								<PlayIcon
									size={remFromPx(28)}
									aria-hidden="true"
									data-flx="channel.embeds.media.embed-you-tube.thumbnail.play-icon"
								/>
							}
							ariaLabel={i18n._(PLAY_VIDEO_DESCRIPTOR)}
							data-flx="channel.embeds.media.embed-you-tube.thumbnail.overlay-play-button"
						/>
						<OverlayActionButton
							onClick={onOpenInNewTab}
							icon={
								<ArrowSquareOutIcon
									size={remFromPx(24)}
									aria-hidden="true"
									data-flx="channel.embeds.media.embed-you-tube.thumbnail.arrow-square-out-icon"
								/>
							}
							ariaLabel={i18n._(OPEN_IN_NEW_TAB_DESCRIPTOR)}
							data-flx="channel.embeds.media.embed-you-tube.thumbnail.overlay-action-button.open-in-new-tab"
						/>
					</div>
				</div>
			</div>
		);
	},
);
export const EmbedYouTube: FC<EmbedYouTubeProps> = observer(({embed, width = YOUTUBE_CONFIG.DEFAULT_WIDTH}) => {
	const {i18n} = useLingui();
	const [hasInteracted, setHasInteracted] = useState(false);
	const posterSrc = embed.thumbnail?.proxy_url || '';
	const {ref: visibilityRef, isNearViewport} = useNearViewport<HTMLDivElement>({rememberKey: posterSrc});
	const [posterCacheAtMount] = useState(() => ({src: posterSrc, cached: ImageCacheUtils.hasImage(posterSrc)}));
	const posterCachedOnMount = posterCacheAtMount.src === posterSrc && posterCacheAtMount.cached;
	const [loadedPosterSrc, setLoadedPosterSrc] = useState<string | null>(() => {
		if (posterSrc.length > 0 && ImageCacheUtils.hasImage(posterSrc)) return posterSrc;
		return null;
	});
	const posterLoaded = posterSrc.length > 0 && loadedPosterSrc === posterSrc;
	useEffect(() => {
		if (!isNearViewport) return;
		if (posterSrc.length === 0 || loadedPosterSrc === posterSrc) return;
		if (ImageCacheUtils.hasImage(posterSrc)) {
			setLoadedPosterSrc(posterSrc);
			return;
		}
		const cleanup = ImageCacheUtils.loadImage(
			posterSrc,
			() => setLoadedPosterSrc(posterSrc),
			() => setLoadedPosterSrc((currentSource) => (currentSource === posterSrc ? null : currentSource)),
		);
		return cleanup;
	}, [isNearViewport, loadedPosterSrc, posterSrc]);
	const handleInitialPlay = useCallback((event: React.MouseEvent | React.KeyboardEvent) => {
		event.stopPropagation();
		setHasInteracted(true);
	}, []);
	const handleOpenInNewTab = useCallback(
		(event: React.MouseEvent | React.KeyboardEvent) => {
			event.stopPropagation();
			if (embed.url) {
				openExternalUrlWithWarning(embed.url);
			}
		},
		[embed.url],
	);
	const thumbHashUrl = useMemo(
		() => decodeThumbHashDataURL(embed.thumbnail?.placeholder),
		[embed.thumbnail?.placeholder],
	);
	if (!(embed.video && embed.thumbnail && embed.thumbnail.proxy_url)) {
		return null;
	}
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
				ref={visibilityRef}
				className={styles.container}
				style={{
					...containerStyle,
					width: remFromPx(dimensions.width),
					aspectRatio,
					maxWidth: '100%',
				}}
				data-flx="channel.embeds.media.embed-you-tube.container"
			>
				<Thumbnail
					posterSrc={isNearViewport ? posterSrc : undefined}
					thumbHashURL={thumbHashUrl}
					posterLoaded={isNearViewport && posterLoaded}
					posterCachedOnMount={isNearViewport && posterCachedOnMount}
					title={embed.title}
					onPlay={handleInitialPlay}
					onOpenInNewTab={handleOpenInNewTab}
					data-flx="channel.embeds.media.embed-you-tube.thumbnail"
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
			ref={visibilityRef}
			className={styles.videoContainer}
			style={{
				...containerStyle,
				width: `${dimensions.width}px`,
				aspectRatio,
				maxWidth: '100%',
			}}
			data-flx="channel.embeds.media.embed-you-tube.video-container"
		>
			{/* biome-ignore lint/a11y/useIframeTitle: project policy forbids the native title attribute (NoNativeTitleAttribute test); aria-label provides the accessible name */}
			<iframe
				allow="autoplay; fullscreen"
				allowFullScreen
				sandbox="allow-forms allow-modals allow-popups allow-popups-to-escape-sandbox allow-same-origin allow-scripts"
				src={embedVideoUrl.toString()}
				className={styles.iframe}
				data-embed-media="true"
				aria-label={embed.title || i18n._(VIDEO_DESCRIPTOR, {youtubeProviderName: YOUTUBE_PROVIDER_NAME})}
				data-flx="channel.embeds.media.embed-you-tube.iframe"
			/>
		</div>
	);
});
