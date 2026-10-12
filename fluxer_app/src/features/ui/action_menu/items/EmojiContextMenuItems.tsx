// SPDX-License-Identifier: AGPL-3.0-or-later
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {
	ExpressionContextMenuItems,
	ExpressionInlineMenuItems,
} from '@app/features/ui/action_menu/items/ExpressionContextMenuItems';
import {EMOJI_MENU_CONFIG} from '@app/features/ui/action_menu/items/ExpressionMenuConfig';

interface Props {
	emoji: FlatEmoji;
	onClose: () => void;
}
export function EmojiContextMenuItems({emoji, onClose}: Props) {
	return (
		<ExpressionContextMenuItems
			item={emoji}
			config={EMOJI_MENU_CONFIG}
			onClose={onClose}
			data-flx="ui.action-menu.items.emoji-context-menu-items.expression-context-menu-items"
		/>
	);
}
export function EmojiInlineMenuItems({emoji, onClose}: Props) {
	return (
		<ExpressionInlineMenuItems
			item={emoji}
			config={EMOJI_MENU_CONFIG}
			onClose={onClose}
			data-flx="ui.action-menu.items.emoji-context-menu-items.expression-inline-menu-items"
		/>
	);
}
