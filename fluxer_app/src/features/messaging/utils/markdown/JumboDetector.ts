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
		let remaining = (node as TextNode).content;
		let nameMatch = UnicodeEmojis.EMOJI_NAME_RE.exec(remaining);
		while (nameMatch) {
			emojiCount++;
			if (emojiCount > MAX_JUMBO_EMOJI_COUNT) return false;
			remaining = remaining.slice(nameMatch[0].length).trimStart();
			nameMatch = UnicodeEmojis.EMOJI_NAME_RE.exec(remaining);
		}
		if (remaining.trim() !== '') return false;
	}
	return emojiCount > 0;
}
