// SPDX-License-Identifier: AGPL-3.0-or-later
import {LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {SubMenu as MenuItemSubmenu} from '@app/features/ui/action_menu/ContextMenu';
import {CloneExpressionMenuItem} from '@app/features/ui/action_menu/items/CloneExpressionMenuItem';
import {
	COPY_IMAGE_LINK_DESCRIPTOR,
	type ExpressionItem,
	type ExpressionMenuConfig,
	OPEN_IMAGE_LINK_DESCRIPTOR,
} from '@app/features/ui/action_menu/items/ExpressionMenuConfig';
import styles from '@app/features/ui/action_menu/items/MenuItems.module.css';
import {ReverseImageSearchMenuItems} from '@app/features/ui/action_menu/items/ReverseImageSearchMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {useLingui} from '@lingui/react/macro';
import {ClipboardIcon, StarIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

interface ExpressionMenuItemsProps<T extends ExpressionItem> {
	item: T;
	config: ExpressionMenuConfig<T>;
	onClose: () => void;
}
const useExpressionHandlers = <T extends ExpressionItem>(
	emoji: T,
	config: ExpressionMenuConfig<T>,
	onClose: () => void,
) => {
	const {i18n} = useLingui();
	const canFavorite = config.canFavorite(emoji);
	const isFavorite = canFavorite ? config.isFavorite(emoji) : false;
	const reverseImageSearchUrl = config.getURL(emoji);
	const handleToggleFavorite = useCallback(() => {
		config.toggleFavorite(emoji);
	}, [emoji, config]);
	const handleCopyId = useCallback(() => {
		if (!emoji.id) return;
		TextCopyCommands.copy(i18n, emoji.id);
		onClose();
	}, [i18n, emoji.id, onClose]);
	const handleCopyUrl = useCallback(async () => {
		if (!reverseImageSearchUrl) return;
		await TextCopyCommands.copy(i18n, reverseImageSearchUrl, true);
		ToastCommands.createToast({
			type: 'success',
			children: i18n._(LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR),
		});
		onClose();
	}, [i18n, reverseImageSearchUrl, onClose]);
	const handleOpenInBrowser = useCallback(() => {
		if (!reverseImageSearchUrl) return;
		void openExternalUrl(reverseImageSearchUrl);
		onClose();
	}, [reverseImageSearchUrl, onClose]);
	return {
		canFavorite,
		isFavorite,
		reverseImageSearchUrl,
		handleToggleFavorite,
		handleCopyId,
		handleCopyUrl,
		handleOpenInBrowser,
	};
};
export const ExpressionContextMenuItems = observer(
	<T extends ExpressionItem>({item: emoji, config, onClose}: ExpressionMenuItemsProps<T>) => {
		const {i18n} = useLingui();
		const {canFavorite, isFavorite, reverseImageSearchUrl, handleToggleFavorite, handleCopyId} = useExpressionHandlers(
			emoji,
			config,
			onClose,
		);
		const shouldShowPrimaryGroup = canFavorite || Boolean(emoji.id);
		return (
			<>
				{shouldShowPrimaryGroup && (
					<MenuGroup data-flx="ui.action-menu.items.expression-context-menu-items.menu-group">
						{canFavorite && (
							<MenuItem
								icon={
									<StarIcon
										className={styles.iconSmall}
										weight={isFavorite ? 'fill' : 'bold'}
										data-flx="ui.action-menu.items.expression-context-menu-items.icon-small"
									/>
								}
								onClick={handleToggleFavorite}
								data-flx="ui.action-menu.items.expression-context-menu-items.menu-item.toggle-favorite"
							>
								{isFavorite ? i18n._(config.unfavoriteLabel) : i18n._(config.favoriteLabel)}
							</MenuItem>
						)}
						{emoji.id && (
							<MenuItem
								icon={
									<ClipboardIcon
										className={styles.iconSmall}
										data-flx="ui.action-menu.items.expression-context-menu-items.icon-small--2"
									/>
								}
								onClick={handleCopyId}
								data-flx="ui.action-menu.items.expression-context-menu-items.menu-item.copy-id"
							>
								{i18n._(config.copyIdLabel)}
							</MenuItem>
						)}
					</MenuGroup>
				)}
				{emoji.id && (
					<CloneExpressionMenuItem
						item={emoji}
						config={config.clone}
						onClose={onClose}
						data-flx="ui.action-menu.items.expression-context-menu-items.clone-emoji-menu-item"
					/>
				)}
				{reverseImageSearchUrl && (
					<ReverseImageSearchMenuItems
						imageUrl={reverseImageSearchUrl}
						onClose={onClose}
						includeCopyAndOpen
						copyLabel={i18n._(config.copyUrlLabel)}
						openLabel={i18n._(config.openLabel)}
						data-flx="ui.action-menu.items.expression-context-menu-items.reverse-image-search-menu-items"
					/>
				)}
			</>
		);
	},
);

ExpressionContextMenuItems.displayName = 'EmojiContextMenuItems';

export const ExpressionInlineMenuItems = observer(
	<T extends ExpressionItem>({item: emoji, config, onClose}: ExpressionMenuItemsProps<T>) => {
		const {i18n} = useLingui();
		const {
			canFavorite,
			isFavorite,
			reverseImageSearchUrl,
			handleToggleFavorite,
			handleCopyId,
			handleCopyUrl,
			handleOpenInBrowser,
		} = useExpressionHandlers(emoji, config, onClose);
		if (!canFavorite && !emoji.id && !reverseImageSearchUrl) {
			return null;
		}
		const showSubmenu = Boolean(emoji.id) || Boolean(reverseImageSearchUrl);
		return (
			<MenuGroup data-flx="ui.action-menu.items.expression-context-menu-items.emoji-inline-menu-items.menu-group">
				{canFavorite && (
					<MenuItem
						onClick={handleToggleFavorite}
						data-flx="ui.action-menu.items.expression-context-menu-items.emoji-inline-menu-items.menu-item.toggle-favorite"
					>
						{isFavorite ? i18n._(config.unfavoriteLabel) : i18n._(config.favoriteLabel)}
					</MenuItem>
				)}
				{reverseImageSearchUrl && (
					<MenuItem
						onClick={handleCopyUrl}
						data-flx="ui.action-menu.items.expression-context-menu-items.emoji-inline-menu-items.menu-item.copy-url"
					>
						{i18n._(COPY_IMAGE_LINK_DESCRIPTOR)}
					</MenuItem>
				)}
				{reverseImageSearchUrl && (
					<MenuItem
						onClick={handleOpenInBrowser}
						data-flx="ui.action-menu.items.expression-context-menu-items.emoji-inline-menu-items.menu-item.open-in-browser"
					>
						{i18n._(OPEN_IMAGE_LINK_DESCRIPTOR)}
					</MenuItem>
				)}
				{showSubmenu && (
					<MenuItemSubmenu
						label={i18n._(config.moreLabel)}
						render={() => (
							<>
								{emoji.id && (
									<MenuGroup data-flx="ui.action-menu.items.expression-context-menu-items.emoji-inline-menu-items.menu-group--2">
										<MenuItem
											icon={
												<ClipboardIcon
													className={styles.iconSmall}
													data-flx="ui.action-menu.items.expression-context-menu-items.emoji-inline-menu-items.icon-small"
												/>
											}
											onClick={handleCopyId}
											data-flx="ui.action-menu.items.expression-context-menu-items.emoji-inline-menu-items.menu-item.copy-id"
										>
											{i18n._(config.copyIdLabel)}
										</MenuItem>
									</MenuGroup>
								)}
								{emoji.id && (
									<CloneExpressionMenuItem
										item={emoji}
										config={config.clone}
										onClose={onClose}
										data-flx="ui.action-menu.items.expression-context-menu-items.emoji-inline-menu-items.clone-emoji-menu-item"
									/>
								)}
								{reverseImageSearchUrl && (
									<ReverseImageSearchMenuItems
										imageUrl={reverseImageSearchUrl}
										onClose={onClose}
										data-flx="ui.action-menu.items.expression-context-menu-items.emoji-inline-menu-items.reverse-image-search-menu-items"
									/>
								)}
							</>
						)}
						data-flx="ui.action-menu.items.expression-context-menu-items.emoji-inline-menu-items.menu-item-submenu"
					/>
				)}
			</MenuGroup>
		);
	},
);

ExpressionInlineMenuItems.displayName = 'EmojiInlineMenuItems';
