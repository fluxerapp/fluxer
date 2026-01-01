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

import UnicodeEmojis from '~/lib/UnicodeEmojis';
import UserSettingsStore from '~/stores/UserSettingsStore';
import {NodeType} from '../parser/types/enums';
import type {Node, TextNode} from '../parser/types/nodes';

export function shouldRenderJumboEmojis(nodes: Array<Node>): boolean {
	if (UserSettingsStore.getMessageDisplayCompact()) {
		return false;
	}

	const emojiCount = nodes.filter((node) => {
		return (
			node.type === NodeType.Emoji ||
			(node.type === NodeType.Text && UnicodeEmojis.EMOJI_NAME_RE.test((node as TextNode).content))
		);
	}).length;

	return (
		emojiCount > 0 &&
		emojiCount <= 6 &&
		nodes.every((node) => {
			return (
				node.type === NodeType.Emoji ||
				(node.type === NodeType.Text &&
					((node as TextNode).content.trim() === '' || UnicodeEmojis.EMOJI_NAME_RE.test((node as TextNode).content)))
			);
		})
	);
}
