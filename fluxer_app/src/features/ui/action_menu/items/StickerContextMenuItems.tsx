// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ExpressionContextMenuItems,
	ExpressionInlineMenuItems,
} from '@app/features/ui/action_menu/items/ExpressionContextMenuItems';
import type {StickerContextMenuSticker} from '@app/features/ui/action_menu/items/ExpressionMenuConfig';
import {STICKER_MENU_CONFIG} from '@app/features/ui/action_menu/items/ExpressionMenuConfig';

interface Props {
	sticker: StickerContextMenuSticker;
	onClose: () => void;
}
export function StickerContextMenuItems({sticker, onClose}: Props) {
	return (
		<ExpressionContextMenuItems
			item={sticker}
			config={STICKER_MENU_CONFIG}
			onClose={onClose}
			data-flx="ui.action-menu.items.sticker-context-menu-items.expression-context-menu-items"
		/>
	);
}
export function StickerInlineMenuItems({sticker, onClose}: Props) {
	return (
		<ExpressionInlineMenuItems
			item={sticker}
			config={STICKER_MENU_CONFIG}
			onClose={onClose}
			data-flx="ui.action-menu.items.sticker-context-menu-items.expression-inline-menu-items"
		/>
	);
}
