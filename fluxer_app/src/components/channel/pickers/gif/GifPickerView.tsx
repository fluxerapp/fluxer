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
import {SmileySadIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import styles from '~/components/channel/GifPicker.module.css';
import {useGifVideoPool} from '~/components/channel/GifVideoPool';
import {PickerEmptyState} from '~/components/channel/shared/PickerEmptyState';
import {
	ExpressionPickerHeaderPortal,
	useExpressionPickerHeaderPortal,
} from '~/components/popouts/ExpressionPickerPopout';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import {Spinner} from '~/components/uikit/Spinner';
import {useSearchInputAutofocus} from '~/hooks/useSearchInputAutofocus';
import {useWindowFocusVideoControl} from '~/hooks/useWindowFocusVideoControl';
import AccessibilityStore from '~/stores/AccessibilityStore';
import {useScrollerViewport} from '../shared/useScrollerViewport';
import type {GifPickerProps} from './GifPicker';
import {GifPickerGrid} from './GifPickerGrid';
import {GifPickerHeader} from './GifPickerHeader';
import {GifPickerStore} from './GifPickerStore';

export const GifPickerView = observer(({onClose}: GifPickerProps = {}) => {
	const {t} = useLingui();
	const storeRef = React.useRef<GifPickerStore | null>(null);
	if (!storeRef.current) storeRef.current = new GifPickerStore();
	const store = storeRef.current;

	const headerPortalContext = useExpressionPickerHeaderPortal();
	const hasPortal = Boolean(headerPortalContext?.headerPortalElement);

	const autoSendTenorGifs = AccessibilityStore.autoSendTenorGifs;
	const gifAutoPlay = true;
	const videoPool = useGifVideoPool();

	const scrollerRef = React.useRef<ScrollerHandle>(null);
	const searchInputRef = React.useRef<HTMLInputElement>(null);

	useSearchInputAutofocus(searchInputRef);

	const {viewportSize, scrollTop, handleScroll, handleResize, scrollToTop} = useScrollerViewport(scrollerRef);

	useWindowFocusVideoControl({scrollerRef, videoPool, gifAutoPlay});

	React.useEffect(() => {
		store.ensureFeaturedLoaded();
		return () => store.dispose();
	}, [store]);

	React.useEffect(() => {
		scrollToTop();
	}, [store.view, scrollToTop]);

	React.useEffect(() => {
		if (
			(!store.loading && store.shouldRenderSearchResults) ||
			(!store.searchTerm.trim() && !store.shouldRenderSearchResults)
		) {
			scrollToTop();
		}
	}, [store.loading, store.shouldRenderSearchResults, store.searchTerm, scrollToTop]);

	const header = <GifPickerHeader store={store} inputRef={searchInputRef} />;

	const headerElement = hasPortal ? (
		<ExpressionPickerHeaderPortal>{header}</ExpressionPickerHeaderPortal>
	) : (
		<div className={styles.mobileHeaderWrapper}>{header}</div>
	);

	if (store.initialFeaturedLoading && store.isLandingPage) {
		return (
			<div className={styles.gifPickerContainer}>
				{headerElement}
				<div className={styles.gifPickerMain} style={{display: 'grid', placeItems: 'center'}}>
					<Spinner size="large" />
				</div>
			</div>
		);
	}

	if (store.shouldShowNoResults) {
		return (
			<div className={styles.gifPickerContainer}>
				{headerElement}
				<div className={styles.gifPickerMain}>
					<PickerEmptyState
						icon={SmileySadIcon}
						title={t`No search results`}
						description={t`Try another search term`}
					/>
				</div>
			</div>
		);
	}

	return (
		<div className={styles.gifPickerContainer}>
			{headerElement}

			<div className={styles.gifPickerMain}>
				<div className={styles.autoSizerWrapper}>
					<Scroller
						ref={scrollerRef}
						className={styles.virtualList}
						onScroll={handleScroll}
						onResize={handleResize}
						fade={false}
						style={{height: '100%', width: '100%'}}
						reserveScrollbarTrack={false}
					>
						{viewportSize.width > 0 && viewportSize.height > 0 && (
							<GifPickerGrid
								store={store}
								onClose={onClose}
								autoSendTenorGifs={autoSendTenorGifs}
								gifAutoPlay={gifAutoPlay}
								viewportWidth={viewportSize.width}
								viewportHeight={viewportSize.height}
								scrollTop={scrollTop}
							/>
						)}
					</Scroller>
				</div>
			</div>
		</div>
	);
});
