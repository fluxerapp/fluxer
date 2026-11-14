// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Node} from '@app/features/messaging/utils/markdown/parser/Nodes';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import type {ChannelMention} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {I18n} from '@lingui/core';
import type React from 'react';

export const MarkdownContext = {
	STANDARD_WITH_JUMBO: 0,
	RESTRICTED_INLINE_REPLY: 1,
	RESTRICTED_USER_BIO: 2,
	RESTRICTED_EMBED_DESCRIPTION: 3,
	STANDARD_WITHOUT_JUMBO: 4,
	RESTRICTED_INLINE_PREVIEW: 5,
} as const;

export type MarkdownContext = ValueOf<typeof MarkdownContext>;

export function isRestrictedInlineContext(context: MarkdownContext): boolean {
	return context === MarkdownContext.RESTRICTED_INLINE_REPLY || context === MarkdownContext.RESTRICTED_INLINE_PREVIEW;
}

export interface MarkdownParseOptions {
	context: MarkdownContext;
	disableAnimatedEmoji?: boolean;
	disableInteractions?: boolean;
	disableEmojiInteractions?: boolean;
	disableEmojiInfoCard?: boolean;
	channelId?: string;
	messageId?: string;
	guildId?: string;
	mentionChannels?: ReadonlyArray<ChannelMention>;
}

export interface MarkdownRenderOptions extends MarkdownParseOptions {
	shouldJumboEmojis: boolean;
	i18n: I18n;
}

export interface RendererProps<T extends Node = Node> {
	node: T;
	id: string;
	renderChildren: (nodes: Array<Node>, optionOverrides?: Partial<MarkdownRenderOptions>) => React.ReactNode;
	options: MarkdownRenderOptions;
}
