// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR,
	REVERSE_IMAGE_SEARCH_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {SearchProviderPickerModal} from '@app/features/search/components/modals/SearchProviderPickerModal';
import ReverseImageSearch from '@app/features/search/state/ReverseImageSearch';
import {CopyLinkIcon, OpenMediaLinkIcon, SearchIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {
	buildSearchProviderSheetItems,
	getSearchProviderMenuState,
} from '@app/features/ui/action_menu/items/SearchProviderMenuUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {
	MenuGroupType,
	MenuItemType,
	MenuSubmenuItemType,
} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const COPY_IMAGE_URL_DESCRIPTOR = msg({
	message: 'Copy image URL',
	comment: 'Action that copies the image URL to the clipboard.',
});
const OPEN_IMAGE_IN_BROWSER_DESCRIPTOR = msg({
	message: 'Open image in browser',
	comment: 'Action that opens the image URL in an external browser.',
});
const DEFAULT_DESCRIPTOR = msg({
	message: 'Default',
	comment: 'Option label representing the default value.',
});

interface ReverseImageSearchMenuOptions {
	i18n: I18n;
	onClose: () => void;
	defaultLabel?: string;
	includeCopyAndOpen?: boolean;
	copyLabel?: string;
	openLabel?: string;
}

const openReverseImageSearchWith = (engineId: string, url: string) => {
	const target = ReverseImageSearch.buildSearchUrl(engineId, url);
	if (target) {
		void openExternalUrl(target);
	}
};

export function buildReverseImageSearchMenuGroups(
	imageUrl: string | null | undefined,
	options: ReverseImageSearchMenuOptions,
): Array<MenuGroupType> {
	if (!imageUrl) return [];
	const {i18n, onClose, includeCopyAndOpen = false} = options;
	const defaultLabel = options.defaultLabel ?? i18n._(REVERSE_IMAGE_SEARCH_DESCRIPTOR);
	const copyLabel = options.copyLabel ?? i18n._(COPY_IMAGE_URL_DESCRIPTOR);
	const openLabel = options.openLabel ?? i18n._(OPEN_IMAGE_IN_BROWSER_DESCRIPTOR);
	const state = getSearchProviderMenuState(ReverseImageSearch);
	const items: Array<MenuItemType | MenuSubmenuItemType> = [];
	if (includeCopyAndOpen) {
		items.push({
			icon: (
				<CopyLinkIcon
					size={20}
					data-flx="ui.action-menu.items.search-menu-data.build-reverse-image-search-menu-groups.copy-link-icon"
				/>
			),
			label: copyLabel,
			onClick: async () => {
				await TextCopyCommands.copy(i18n, imageUrl, true);
				ToastCommands.createToast({
					type: 'success',
					children: i18n._(LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR),
				});
				onClose();
			},
		});
		items.push({
			icon: (
				<OpenMediaLinkIcon
					size={20}
					data-flx="ui.action-menu.items.search-menu-data.build-reverse-image-search-menu-groups.open-media-link-icon"
				/>
			),
			label: openLabel,
			onClick: () => {
				void openExternalUrl(imageUrl);
				onClose();
			},
		});
	}
	if (state.enabledEngines.length > 0) {
		const handleDefault = () => {
			const defaultEngine = ReverseImageSearch.defaultEngine;
			if (defaultEngine) {
				openReverseImageSearchWith(defaultEngine.id, imageUrl);
				onClose();
				return;
			}
			const targetImageUrl = imageUrl;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<SearchProviderPickerModal
						mode="image"
						onPick={(engineId) => openReverseImageSearchWith(engineId, targetImageUrl)}
						data-flx="ui.action-menu.items.search-menu-data.handle-default.search-provider-picker-modal"
					/>
				)),
			);
		};
		items.push(
			...buildSearchProviderSheetItems(state, {
				defaultLabel,
				defaultSubtext: i18n._(DEFAULT_DESCRIPTOR),
				renderIcon: () => (
					<SearchIcon size={20} data-flx="ui.action-menu.items.search-menu-data.render-icon.search-icon" />
				),
				onDefaultSearch: handleDefault,
				onSearchWithEngine: (engine) => {
					openReverseImageSearchWith(engine.id, imageUrl);
					onClose();
				},
			}),
		);
	}
	if (items.length === 0) return [];
	return [{items}];
}
