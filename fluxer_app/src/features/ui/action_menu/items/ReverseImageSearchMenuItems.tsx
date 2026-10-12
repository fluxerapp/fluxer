// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR,
	REVERSE_IMAGE_SEARCH_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {SearchProviderPickerModal} from '@app/features/search/components/modals/SearchProviderPickerModal';
import ReverseImageSearch from '@app/features/search/state/ReverseImageSearch';
import {CopyLinkIcon, OpenMediaLinkIcon, SearchIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {SearchProviderContextMenuItems} from '@app/features/ui/action_menu/items/SearchProviderMenuItems';
import {getSearchProviderMenuState} from '@app/features/ui/action_menu/items/SearchProviderMenuUtils';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const COPY_IMAGE_URL_DESCRIPTOR = msg({
	message: 'Copy image URL',
	comment: 'Action that copies the image URL to the clipboard.',
});
const OPEN_IMAGE_IN_BROWSER_DESCRIPTOR = msg({
	message: 'Open image in browser',
	comment: 'Action that opens the image URL in an external browser.',
});

interface ReverseImageSearchMenuItemsProps {
	imageUrl: string | null | undefined;
	onClose?: () => void;
	defaultLabel?: string;
	includeCopyAndOpen?: boolean;
	copyLabel?: string;
	openLabel?: string;
}

export const ReverseImageSearchMenuItems: React.FC<ReverseImageSearchMenuItemsProps> = observer(
	({imageUrl, onClose, defaultLabel, includeCopyAndOpen = false, copyLabel, openLabel}) => {
		const {i18n} = useLingui();
		const renderDefaultLabel = defaultLabel ?? i18n._(REVERSE_IMAGE_SEARCH_DESCRIPTOR);
		const renderCopyLabel = copyLabel ?? i18n._(COPY_IMAGE_URL_DESCRIPTOR);
		const renderOpenLabel = openLabel ?? i18n._(OPEN_IMAGE_IN_BROWSER_DESCRIPTOR);
		const openWith = useCallback((engineId: string, url: string) => {
			const target = ReverseImageSearch.buildSearchUrl(engineId, url);
			if (target) {
				void openExternalUrl(target);
			}
		}, []);
		const handleSearch = useCallback(
			(engineId: string) => {
				if (!imageUrl) {
					onClose?.();
					return;
				}
				openWith(engineId, imageUrl);
				onClose?.();
			},
			[imageUrl, onClose, openWith],
		);
		const handleDefaultSearch = useCallback(() => {
			if (!imageUrl) {
				onClose?.();
				return;
			}
			const defaultEngine = ReverseImageSearch.defaultEngine;
			if (defaultEngine) {
				openWith(defaultEngine.id, imageUrl);
				onClose?.();
				return;
			}
			const targetImageUrl = imageUrl;
			const openPicker = () =>
				ModalCommands.push(
					modal(() => (
						<SearchProviderPickerModal
							mode="image"
							onPick={(engineId) => openWith(engineId, targetImageUrl)}
							data-flx="ui.action-menu.items.reverse-image-search-menu-items.handle-default-search.search-provider-picker-modal"
						/>
					)),
				);
			if (onClose) {
				ModalCommands.runAfterBottomSheetClose(onClose, openPicker);
				return;
			}
			openPicker();
		}, [imageUrl, onClose, openWith]);
		const handleCopy = useCallback(async () => {
			if (!imageUrl) {
				onClose?.();
				return;
			}
			await TextCopyCommands.copy(i18n, imageUrl, true);
			ToastCommands.createToast({
				type: 'success',
				children: i18n._(LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR),
			});
			onClose?.();
		}, [imageUrl, onClose, i18n]);
		const handleOpen = useCallback(() => {
			if (!imageUrl) {
				onClose?.();
				return;
			}
			void openExternalUrl(imageUrl);
			onClose?.();
		}, [imageUrl, onClose]);
		if (!imageUrl) {
			return null;
		}
		const state = getSearchProviderMenuState(ReverseImageSearch);
		const searchItems =
			state.enabledEngines.length > 0 ? (
				<SearchProviderContextMenuItems
					state={state}
					defaultLabel={renderDefaultLabel}
					renderIcon={() => (
						<SearchIcon size={20} data-flx="ui.action-menu.items.reverse-image-search-menu-items.search-icon" />
					)}
					onDefaultSearch={handleDefaultSearch}
					onSearchWithEngine={(engine) => handleSearch(engine.id)}
					data-flx="ui.action-menu.items.reverse-image-search-menu-items.search-provider-context-menu-items"
				/>
			) : null;
		const copyAndOpen = includeCopyAndOpen ? (
			<>
				<MenuItem
					icon={
						<CopyLinkIcon size={20} data-flx="ui.action-menu.items.reverse-image-search-menu-items.copy-link-icon" />
					}
					onClick={handleCopy}
					data-flx="ui.action-menu.items.reverse-image-search-menu-items.menu-item.copy"
				>
					{renderCopyLabel}
				</MenuItem>
				<MenuItem
					icon={
						<OpenMediaLinkIcon
							size={20}
							data-flx="ui.action-menu.items.reverse-image-search-menu-items.open-media-link-icon"
						/>
					}
					onClick={handleOpen}
					data-flx="ui.action-menu.items.reverse-image-search-menu-items.menu-item.open"
				>
					{renderOpenLabel}
				</MenuItem>
			</>
		) : null;
		if (!searchItems && !copyAndOpen) {
			return null;
		}
		const content = (
			<>
				{copyAndOpen}
				{searchItems}
			</>
		);
		return <MenuGroup data-flx="ui.action-menu.items.reverse-image-search-menu-items.menu-group">{content}</MenuGroup>;
	},
);
