// SPDX-License-Identifier: AGPL-3.0-or-later

import * as StickerPickerCommands from '@app/features/emoji/commands/StickerPickerCommands';
import StickerPicker from '@app/features/emoji/state/StickerPicker';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import Guilds from '@app/features/guild/state/Guilds';
import {LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {CloneStickerMenuItem} from '@app/features/ui/action_menu/items/CloneStickerMenuItem';
import styles from '@app/features/ui/action_menu/items/MenuItems.module.css';
import {ReverseImageSearchMenuItems} from '@app/features/ui/action_menu/items/ReverseImageSearchMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ClipboardIcon, StarIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const UNFAVORITE_STICKER_DESCRIPTOR = msg({
	message: 'Unfavorite sticker',
	comment: 'Sticker context menu action that removes the sticker from favorites.',
});
const FAVORITE_STICKER_DESCRIPTOR = msg({
	message: 'Favorite sticker',
	comment: 'Sticker context menu action that adds the sticker to favorites.',
});
const COPY_STICKER_ID_DESCRIPTOR = msg({
	message: 'Copy sticker ID',
	comment: 'Developer-mode action that copies the sticker ID to the clipboard.',
});
const COPY_STICKER_URL_DESCRIPTOR = msg({
	message: 'Copy sticker URL',
	comment: 'Action that copies the sticker URL to the clipboard.',
});
const OPEN_STICKER_IN_BROWSER_DESCRIPTOR = msg({
	message: 'Open sticker in browser',
	comment: 'Action that opens the sticker URL in an external browser.',
});
const COPY_IMAGE_LINK_DESCRIPTOR = msg({
	message: 'Copy image link',
	comment: 'Image context menu action that copies the image URL to the clipboard.',
});
const OPEN_IMAGE_LINK_DESCRIPTOR = msg({
	message: 'Open image link',
	comment: 'Image context menu action that opens the image URL in an external browser.',
});
const MORE_STICKER_ACTIONS_DESCRIPTOR = msg({
	message: 'More sticker actions',
	comment: 'Submenu label that contains additional sticker context menu actions.',
});

export type StickerContextMenuSticker = Readonly<
	Pick<GuildSticker, 'id' | 'guildId' | 'name' | 'description' | 'tags' | 'animated' | 'url' | 'user'>
>;

interface StickerContextMenuItemsProps {
	sticker: StickerContextMenuSticker;
	onClose: () => void;
}

const useStickerHandlers = (sticker: StickerContextMenuSticker, onClose: () => void) => {
	const {i18n} = useLingui();
	const stickerImageUrl = AvatarUtils.getStickerURL({id: sticker.id, animated: sticker.animated, size: 320});
	const canFavorite = Boolean(sticker.guildId && Guilds.getGuild(sticker.guildId));
	const isFavorite = canFavorite ? StickerPicker.isFavorite(sticker) : false;
	const handleToggleFavorite = useCallback(() => {
		StickerPickerCommands.toggleFavorite(sticker);
	}, [sticker]);
	const handleCopyId = useCallback(() => {
		TextCopyCommands.copy(i18n, sticker.id);
		onClose();
	}, [i18n, sticker.id, onClose]);
	const handleCopyUrl = useCallback(async () => {
		await TextCopyCommands.copy(i18n, stickerImageUrl, true);
		ToastCommands.createToast({
			type: 'success',
			children: i18n._(LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR),
		});
		onClose();
	}, [i18n, stickerImageUrl, onClose]);
	const handleOpenInBrowser = useCallback(() => {
		void openExternalUrl(stickerImageUrl);
		onClose();
	}, [stickerImageUrl, onClose]);
	return {
		stickerImageUrl,
		canFavorite,
		isFavorite,
		handleToggleFavorite,
		handleCopyId,
		handleCopyUrl,
		handleOpenInBrowser,
	};
};
export const StickerContextMenuItems = observer(({sticker, onClose}: StickerContextMenuItemsProps) => {
	const {i18n} = useLingui();
	const {stickerImageUrl, canFavorite, isFavorite, handleToggleFavorite, handleCopyId} = useStickerHandlers(
		sticker,
		onClose,
	);
	return (
		<>
			<MenuGroup data-flx="ui.action-menu.items.sticker-context-menu-items.menu-group">
				{canFavorite && (
					<MenuItem
						icon={
							<StarIcon
								className={styles.iconSmall}
								weight={isFavorite ? 'fill' : 'bold'}
								data-flx="ui.action-menu.items.sticker-context-menu-items.icon-small"
							/>
						}
						onClick={handleToggleFavorite}
						data-flx="ui.action-menu.items.sticker-context-menu-items.menu-item.toggle-favorite"
					>
						{isFavorite ? i18n._(UNFAVORITE_STICKER_DESCRIPTOR) : i18n._(FAVORITE_STICKER_DESCRIPTOR)}
					</MenuItem>
				)}
				<MenuItem
					icon={
						<ClipboardIcon
							className={styles.iconSmall}
							data-flx="ui.action-menu.items.sticker-context-menu-items.icon-small--2"
						/>
					}
					onClick={handleCopyId}
					data-flx="ui.action-menu.items.sticker-context-menu-items.menu-item.copy-id"
				>
					{i18n._(COPY_STICKER_ID_DESCRIPTOR)}
				</MenuItem>
			</MenuGroup>
			<CloneStickerMenuItem
				sticker={sticker}
				onClose={onClose}
				data-flx="ui.action-menu.items.sticker-context-menu-items.clone-sticker-menu-item"
			/>
			<ReverseImageSearchMenuItems
				imageUrl={stickerImageUrl}
				onClose={onClose}
				wrapInGroup
				includeCopyAndOpen
				copyLabel={i18n._(COPY_STICKER_URL_DESCRIPTOR)}
				openLabel={i18n._(OPEN_STICKER_IN_BROWSER_DESCRIPTOR)}
				data-flx="ui.action-menu.items.sticker-context-menu-items.reverse-image-search-menu-items"
			/>
		</>
	);
});

StickerContextMenuItems.displayName = 'StickerContextMenuItems';

export const StickerInlineMenuItems = observer(({sticker, onClose}: StickerContextMenuItemsProps) => {
	const {i18n} = useLingui();
	const {
		stickerImageUrl,
		canFavorite,
		isFavorite,
		handleToggleFavorite,
		handleCopyId,
		handleCopyUrl,
		handleOpenInBrowser,
	} = useStickerHandlers(sticker, onClose);
	return (
		<MenuGroup data-flx="ui.action-menu.items.sticker-context-menu-items.sticker-inline-menu-items.menu-group">
			{canFavorite && (
				<MenuItem
					onClick={handleToggleFavorite}
					data-flx="ui.action-menu.items.sticker-context-menu-items.sticker-inline-menu-items.menu-item.toggle-favorite"
				>
					{isFavorite ? i18n._(UNFAVORITE_STICKER_DESCRIPTOR) : i18n._(FAVORITE_STICKER_DESCRIPTOR)}
				</MenuItem>
			)}
			<MenuItem
				onClick={handleCopyUrl}
				data-flx="ui.action-menu.items.sticker-context-menu-items.sticker-inline-menu-items.menu-item.copy-url"
			>
				{i18n._(COPY_IMAGE_LINK_DESCRIPTOR)}
			</MenuItem>
			<MenuItem
				onClick={handleOpenInBrowser}
				data-flx="ui.action-menu.items.sticker-context-menu-items.sticker-inline-menu-items.menu-item.open-in-browser"
			>
				{i18n._(OPEN_IMAGE_LINK_DESCRIPTOR)}
			</MenuItem>
			<MenuItemSubmenu
				label={i18n._(MORE_STICKER_ACTIONS_DESCRIPTOR)}
				render={() => (
					<>
						<MenuGroup data-flx="ui.action-menu.items.sticker-context-menu-items.sticker-inline-menu-items.menu-group--2">
							<MenuItem
								icon={
									<ClipboardIcon
										className={styles.iconSmall}
										data-flx="ui.action-menu.items.sticker-context-menu-items.sticker-inline-menu-items.icon-small"
									/>
								}
								onClick={handleCopyId}
								data-flx="ui.action-menu.items.sticker-context-menu-items.sticker-inline-menu-items.menu-item.copy-id"
							>
								{i18n._(COPY_STICKER_ID_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
						<CloneStickerMenuItem
							sticker={sticker}
							onClose={onClose}
							data-flx="ui.action-menu.items.sticker-context-menu-items.sticker-inline-menu-items.clone-sticker-menu-item"
						/>
						<ReverseImageSearchMenuItems
							imageUrl={stickerImageUrl}
							onClose={onClose}
							wrapInGroup
							data-flx="ui.action-menu.items.sticker-context-menu-items.sticker-inline-menu-items.reverse-image-search-menu-items"
						/>
					</>
				)}
				data-flx="ui.action-menu.items.sticker-context-menu-items.sticker-inline-menu-items.menu-item-submenu"
			/>
		</MenuGroup>
	);
});

StickerInlineMenuItems.displayName = 'StickerInlineMenuItems';
