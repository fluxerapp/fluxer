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
import {observer} from 'mobx-react-lite';
import React from 'react';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import * as TenorUtils from '~/utils/TenorUtils';
import {type AutocompleteOption, isGif} from './Autocomplete';

import styles from './AutocompleteGif.module.css';

export const AutocompleteGif = observer(
	({
		onSelect,
		keyboardFocusIndex,
		hoverIndex,
		options,
		onMouseEnter,
		onMouseLeave,
		rowRefs,
	}: {
		onSelect: (option: AutocompleteOption) => void;
		keyboardFocusIndex: number;
		hoverIndex: number;
		options: Array<AutocompleteOption>;
		onMouseEnter: (index: number) => void;
		onMouseLeave: () => void;
		rowRefs?: React.MutableRefObject<Array<HTMLButtonElement | null>>;
	}) => {
		const {t} = useLingui();
		const gifs = options.filter(isGif);
		const scrollerRef = React.useRef<ScrollerHandle>(null);

		const handleKeyDown = React.useCallback((event: React.KeyboardEvent) => {
			switch (event.key) {
				case 'ArrowDown':
				case 'ArrowUp': {
					event.preventDefault();
					break;
				}
			}
		}, []);

		React.useLayoutEffect(() => {
			const selectedElement = rowRefs?.current[keyboardFocusIndex];
			if (selectedElement && scrollerRef.current) {
				scrollerRef.current.scrollIntoViewNode({
					node: selectedElement,
					shouldScrollToStart: false,
					padding: 0,
				});
			}
		}, [keyboardFocusIndex, rowRefs?.current[keyboardFocusIndex]]);

		if (gifs.length === 0) {
			return <div className={styles.empty}>{t`No GIFs found`}</div>;
		}

		return (
			<div className={styles.container} onKeyDown={handleKeyDown} role="application">
				<div className={styles.heading}>{t`GIFs`}</div>

				<Scroller
					ref={scrollerRef}
					className={styles.scroller}
					orientation="horizontal"
					fade={false}
					key="autocomplete-gif-scroller"
				>
					{gifs.map((option, index) => {
						const gif = option.gif;
						const title = gif.title || TenorUtils.parseTitleFromUrl(gif.url);
						const isActive = index === keyboardFocusIndex || index === hoverIndex;
						return (
							<button
								type="button"
								key={gif.id}
								ref={(node) => {
									if (rowRefs) {
										rowRefs.current[index] = node;
									}
								}}
								className={`${styles.gifButton} ${isActive ? styles.gifButtonSelected : ''}`}
								onClick={() => onSelect(option)}
								onMouseEnter={() => onMouseEnter(index)}
								onMouseLeave={onMouseLeave}
								aria-label={`${title} - ${t`From Tenor`}`}
							>
								<div className={styles.gifVideoWrapper}>
									<video src={gif.proxy_src} className={styles.gifVideo} muted autoPlay loop playsInline />
								</div>
							</button>
						);
					})}
				</Scroller>
			</div>
		);
	},
);
