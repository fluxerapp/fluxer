// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAnimatedMediaVideoPlayback} from '@app/features/app/hooks/useAnimatedMediaPlayback';
import {type AutocompleteOption, isGif} from '@app/features/channel/components/Autocomplete';
import styles from '@app/features/channel/components/AutocompleteGif.module.css';
import * as KlipyUtils from '@app/features/expressions/utils/KlipyUtils';
import {GIFS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo, useRef} from 'react';

const NO_GIFS_FOUND_DESCRIPTOR = msg({
	message: 'No GIFs match',
	comment: 'Empty-state text in the channel and chat autocomplete gif.',
});
const AutocompleteGifVideo = ({src}: {src: string}) => {
	const videoRef = useRef<HTMLVideoElement>(null);
	const playbackAllowed = useAnimatedMediaVideoPlayback(videoRef);
	return (
		<video
			ref={videoRef}
			src={src}
			className={styles.gifVideo}
			muted
			autoPlay={playbackAllowed}
			loop
			playsInline
			data-flx="channel.autocomplete-gif.gif-video"
		/>
	);
};
export const AutocompleteGif = observer(
	({
		onSelect,
		keyboardFocusIndex,
		hoverIndex,
		options,
		onMouseEnter,
		onMouseLeave,
		rowRefs,
		getOptionId,
	}: {
		onSelect: (option: AutocompleteOption) => void;
		keyboardFocusIndex: number;
		hoverIndex: number;
		options: Array<AutocompleteOption>;
		onMouseEnter: (index: number) => void;
		onMouseLeave: () => void;
		rowRefs?: React.MutableRefObject<Array<HTMLButtonElement | null>>;
		getOptionId?: (index: number) => string;
	}) => {
		const {i18n} = useLingui();
		const gifs = useMemo(() => options.filter(isGif), [options]);
		const scrollerRef = useRef<ScrollerHandle>(null);
		useEffect(() => {
			const selectedElement = rowRefs?.current[keyboardFocusIndex] ?? null;
			if (!selectedElement) {
				return undefined;
			}
			const frameId = requestAnimationFrame(() => {
				if (!scrollerRef.current) {
					return;
				}
				scrollerRef.current.scrollIntoViewNode({
					node: selectedElement,
					shouldScrollToStart: false,
					padding: 0,
				});
			});
			return () => cancelAnimationFrame(frameId);
		}, [gifs.length, keyboardFocusIndex, rowRefs]);
		if (gifs.length === 0) {
			return (
				<div className={styles.empty} data-flx="channel.autocomplete-gif.empty">
					{i18n._(NO_GIFS_FOUND_DESCRIPTOR)}
				</div>
			);
		}
		return (
			<div className={styles.container} data-flx="channel.autocomplete-gif.container">
				<div className={styles.heading} data-flx="channel.autocomplete-gif.heading">
					<span data-flx="channel.autocomplete-gif.span">{i18n._(GIFS_DESCRIPTOR)}</span>
				</div>
				<Scroller
					ref={scrollerRef}
					className={styles.scroller}
					orientation="horizontal"
					fade={true}
					key="autocomplete-gif-scroller"
					data-flx="channel.autocomplete-gif.scroller"
				>
					{gifs.map((option, index) => {
						const gif = option.gif;
						const title = gif.title || KlipyUtils.parseTitleFromUrl(gif.url);
						const isActive = index === keyboardFocusIndex || index === hoverIndex;
						return (
							<button
								type="button"
								key={gif.id}
								id={getOptionId?.(index)}
								ref={(node) => {
									if (rowRefs) {
										rowRefs.current[index] = node;
									}
								}}
								className={`${styles.gifButton} ${isActive ? styles.gifButtonSelected : ''}`}
								onClick={() => onSelect(option)}
								onMouseEnter={() => onMouseEnter(index)}
								onMouseLeave={onMouseLeave}
								aria-label={title}
								role="option"
								aria-selected={index === keyboardFocusIndex}
								tabIndex={-1}
								data-flx="channel.autocomplete-gif.gif-button.select"
							>
								<div className={styles.gifVideoWrapper} data-flx="channel.autocomplete-gif.gif-video-wrapper">
									<AutocompleteGifVideo
										src={gif.proxy_src}
										data-flx="channel.autocomplete-gif.autocomplete-gif-video"
									/>
								</div>
							</button>
						);
					})}
				</Scroller>
			</div>
		);
	},
);
