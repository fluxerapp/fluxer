// SPDX-License-Identifier: AGPL-3.0-or-later

import Emoji from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {
	buildCustomEmojiURL,
	CUSTOM_EMOJI_ENLARGED_IMAGE_RUNG,
	CUSTOM_EMOJI_IMAGE_RUNG,
} from '@app/features/expressions/utils/CustomEmojiImageUrl';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import {EmojiKind} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {EmojiNode} from '@app/features/messaging/utils/markdown/parser/Nodes';

export interface EmojiRenderData {
	name: string;
	isAnimated: boolean;
	isAnimatable: boolean;
	surrogateUrl: string | null;
	id?: string;
	emoji?: FlatEmoji;
}

export function getEmojiRenderData(emojiNode: EmojiNode, disableAnimatedEmoji = false): EmojiRenderData {
	const {kind} = emojiNode;
	if (kind.kind === EmojiKind.Standard) {
		return {
			name: `:${kind.name}:`,
			isAnimated: false,
			isAnimatable: false,
			surrogateUrl: EmojiUtils.getTwemojiURL(kind.codepoints),
		};
	}
	const {id} = kind;
	const emoji = Emoji.getEmojiById(id);
	const isAnimated = emoji?.animated ?? kind.animated;
	return {
		name: `:${emoji?.name || kind.name}:`,
		isAnimated,
		isAnimatable: isAnimated && !disableAnimatedEmoji,
		surrogateUrl: null,
		id,
		emoji,
	};
}

interface EmojiRenderUrlOptions {
	id?: string;
	surrogateUrl: string | null;
	isAnimatable: boolean;
	animated: boolean;
	jumbo: boolean;
}

export function getEmojiRenderUrl({
	id,
	surrogateUrl,
	isAnimatable,
	animated,
	jumbo,
}: EmojiRenderUrlOptions): string | null {
	if (id == null) {
		return surrogateUrl;
	}
	return buildCustomEmojiURL({
		id,
		animated: animated && isAnimatable,
		size: jumbo ? CUSTOM_EMOJI_ENLARGED_IMAGE_RUNG : CUSTOM_EMOJI_IMAGE_RUNG,
	});
}
