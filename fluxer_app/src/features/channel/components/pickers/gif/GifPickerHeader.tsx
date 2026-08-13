// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/channel/components/GifPicker.module.css';
import type {GifPickerState} from '@app/features/channel/components/pickers/gif/GifPickerState';
import {PickerSearchInput} from '@app/features/channel/components/shared/PickerSearchInput';
import {GIFS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, DownloadSimpleIcon, UploadSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const TRENDING_GIFS_DESCRIPTOR = msg({
	message: 'Trending GIFs',
	comment: 'Short label in the channel and chat gif picker header. Keep it concise.',
});
const FAVORITE_GIFS_DESCRIPTOR = msg({
	message: 'Favorite GIFs',
	comment: 'Short label in the channel and chat gif picker header. Keep it concise.',
});
const BACK_TO_GIF_CATEGORIES_DESCRIPTOR = msg({
	message: 'Back to GIF categories',
	comment: 'Button or menu action label in the channel and chat gif picker header. Keep it concise.',
});
const IMPORT_GIFS_DESCRIPTOR = msg({
	message: 'Import GIFs',
	comment: 'Short label in the channel and chat gif picker header. Keep it concise.',
});
const EXPORT_GIF_URLS_DESCRIPTOR = msg({
	message: 'Export GIF URLs',
	comment: 'Short label in the channel and chat gif picker header. Downloads favorite GIF URLs as a text file.',
});
const SEARCH_DESCRIPTOR = msg({
	message: 'Search {gifProviderName}',
	comment:
		'Button or menu action label in the channel and chat gif picker header. Keep it concise. Preserve {gifProviderName}; it is inserted by code.',
});
export const GifPickerHeader = observer(
	({
		store,
		inputRef,
		onOpenImport,
		onExportFavorites,
		canExportFavorites = false,
	}: {
		store: GifPickerState;
		inputRef: React.RefObject<HTMLInputElement | null> | React.RefObject<HTMLInputElement>;
		onOpenImport?: () => void;
		onExportFavorites?: () => void;
		canExportFavorites?: boolean;
	}) => {
		const {i18n} = useLingui();
		const gifProviderName = RuntimeConfig.gifProviderDisplayName;
		if (store.view !== 'default') {
			const title = (() => {
				if (store.view === 'trending') return i18n._(TRENDING_GIFS_DESCRIPTOR);
				if (store.view === 'favorites') return i18n._(FAVORITE_GIFS_DESCRIPTOR);
				return i18n._(GIFS_DESCRIPTOR);
			})();
			return (
				<div
					className={styles.searchBarContainer}
					data-flx="channel.pickers.gif.gif-picker-header.search-bar-container"
				>
					<div
						className={styles.searchBarTitleWrapper}
						data-flx="channel.pickers.gif.gif-picker-header.search-bar-title-wrapper"
					>
						<div
							className={styles.searchBarTitleLeft}
							data-flx="channel.pickers.gif.gif-picker-header.search-bar-title-left"
						>
							<FocusRing offset={-2} data-flx="channel.pickers.gif.gif-picker-header.focus-ring">
								<button
									type="button"
									className={styles.searchBarBackButton}
									onClick={store.goToDefaultView}
									aria-label={i18n._(BACK_TO_GIF_CATEGORIES_DESCRIPTOR)}
									data-flx="channel.pickers.gif.gif-picker-header.search-bar-back-button.go-to-default-view"
								>
									<ArrowLeftIcon
										size={remFromPx(20)}
										weight="regular"
										data-flx="channel.pickers.gif.gif-picker-header.arrow-left-icon"
									/>
								</button>
							</FocusRing>
							<div className={styles.searchBarTitle} data-flx="channel.pickers.gif.gif-picker-header.search-bar-title">
								{title}
							</div>
						</div>
						{store.view === 'favorites' && (onOpenImport != null || onExportFavorites != null) && (
							<div className={styles.searchBarActions} data-flx="channel.pickers.gif.gif-picker-header.actions">
								{onOpenImport != null && (
									<Tooltip
										text={i18n._(IMPORT_GIFS_DESCRIPTOR)}
										position="top"
										data-flx="channel.pickers.gif.gif-picker-header.import-tooltip"
									>
										<FocusRing offset={-2} data-flx="channel.pickers.gif.gif-picker-header.focus-ring--2">
											<button
												type="button"
												className={styles.searchBarBackButton}
												onClick={onOpenImport}
												aria-label={i18n._(IMPORT_GIFS_DESCRIPTOR)}
												data-flx="channel.pickers.gif.gif-picker-header.search-bar-back-button.open-import"
											>
												<UploadSimpleIcon
													size={20}
													weight="regular"
													data-flx="channel.pickers.gif.gif-picker-header.upload-simple-icon"
												/>
											</button>
										</FocusRing>
									</Tooltip>
								)}
								{onExportFavorites != null && (
									<Tooltip
										text={i18n._(EXPORT_GIF_URLS_DESCRIPTOR)}
										position="top"
										data-flx="channel.pickers.gif.gif-picker-header.export-tooltip"
									>
										<FocusRing offset={-2} data-flx="channel.pickers.gif.gif-picker-header.focus-ring--3">
											<button
												type="button"
												className={styles.searchBarBackButton}
												onClick={onExportFavorites}
												disabled={!canExportFavorites}
												aria-label={i18n._(EXPORT_GIF_URLS_DESCRIPTOR)}
												data-flx="channel.pickers.gif.gif-picker-header.search-bar-back-button.export-favorites"
											>
												<DownloadSimpleIcon
													size={20}
													weight="regular"
													data-flx="channel.pickers.gif.gif-picker-header.download-simple-icon"
												/>
											</button>
										</FocusRing>
									</Tooltip>
								)}
							</div>
						)}
					</div>
				</div>
			);
		}
		const handleKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
			if (isIMEComposing(event)) {
				return;
			}
			if (event.key === 'Enter') {
				event.preventDefault();
				store.flushSearch();
			}
		};
		return (
			<PickerSearchInput
				value={store.searchTerm}
				onChange={store.setSearchTerm}
				placeholder={i18n._(SEARCH_DESCRIPTOR, {gifProviderName})}
				inputRef={inputRef}
				showBackButton={!!store.searchTerm.trim()}
				onBackButtonClick={() => store.setSearchTerm('')}
				onKeyDown={handleKeyDown}
				data-flx="channel.pickers.gif.gif-picker-header.picker-search-input.set-search-term"
			/>
		);
	},
);
