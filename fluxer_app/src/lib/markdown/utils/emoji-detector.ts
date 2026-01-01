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

import ChannelStore from '~/stores/ChannelStore';
import EmojiStore from '~/stores/EmojiStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import * as EmojiUtils from '~/utils/EmojiUtils';
import {EmojiKind} from '../parser/types/enums';
import type {EmojiNode} from '../parser/types/nodes';

interface EmojiRenderData {
	url: string | null;
	name: string;
	isAnimated: boolean;
	id?: string;
}

export function getEmojiRenderData(
	emojiNode: EmojiNode,
	guildId?: string,
	disableAnimatedEmoji = false,
): EmojiRenderData {
	const {kind} = emojiNode;
	const emojiName = `:${kind.name}:`;

	if (kind.kind === EmojiKind.Standard) {
		return {
			url: EmojiUtils.getTwemojiURL(kind.codepoints),
			name: emojiName,
			isAnimated: false,
		};
	}

	const {id, animated} = kind;
	const shouldAnimate = !disableAnimatedEmoji && animated;

	const channel = guildId ? ChannelStore.getChannel(guildId) : undefined;
	const disambiguatedEmoji = EmojiStore.getDisambiguatedEmojiContext(channel?.guildId).getById(id);
	const finalEmojiName = `:${disambiguatedEmoji?.name || kind.name}:`;

	return {
		url: AvatarUtils.getEmojiURL({id, animated: shouldAnimate}),
		name: finalEmojiName,
		isAnimated: shouldAnimate,
		id,
	};
}
