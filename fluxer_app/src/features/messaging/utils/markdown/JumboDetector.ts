// SPDX-License-Identifier: AGPL-3.0-or-later

import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import {NodeType} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {Node, TextNode} from '@app/features/messaging/utils/markdown/parser/Nodes';
import UserSettings from '@app/features/user/state/UserSettings';

const MAX_JUMBO_EMOJI_COUNT = 30;

export function shouldRenderAsJumboEmojis(nodes: ReadonlyArray<Node>): boolean {
	if (UserSettings.getMessageDisplayCompact()) {
		return false;
	}
	let emojiCount = 0;
	for (let i = 0; i < nodes.length; i++) {
		const node = nodes[i];
		if (node.type === NodeType.Emoji) {
			emojiCount++;
			if (emojiCount > MAX_JUMBO_EMOJI_COUNT) return false;
			continue;
		}
		if (node.type !== NodeType.Text) {
			return false;
		}
		const content = (node as TextNode).content;
		if (UnicodeEmojis.EMOJI_SHORTCODE_RE.test(content)) {
			emojiCount++;
			if (emojiCount > MAX_JUMBO_EMOJI_COUNT) return false;
			continue;
		}
		if (content.trim() !== '') return false;
	}
	return emojiCount > 0;
}
