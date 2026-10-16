// SPDX-License-Identifier: AGPL-3.0-or-later

import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {TABLE_PARSING_FLAG} from '@app/features/messaging/utils/markdown/MarkdownTableParsingConfig';
import {ParserFlags} from '@app/features/messaging/utils/markdown/parser/Enums';

const STANDARD_FLAGS =
	ParserFlags.ALLOW_SPOILERS |
	ParserFlags.ALLOW_HEADINGS |
	ParserFlags.ALLOW_LISTS |
	ParserFlags.ALLOW_CODE_BLOCKS |
	ParserFlags.ALLOW_MASKED_LINKS |
	ParserFlags.ALLOW_COMMAND_MENTIONS |
	ParserFlags.ALLOW_GUILD_NAVIGATIONS |
	ParserFlags.ALLOW_USER_MENTIONS |
	ParserFlags.ALLOW_ROLE_MENTIONS |
	ParserFlags.ALLOW_CHANNEL_MENTIONS |
	ParserFlags.ALLOW_EVERYONE_MENTIONS |
	ParserFlags.ALLOW_BLOCKQUOTES |
	ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES |
	ParserFlags.ALLOW_SUBTEXT |
	TABLE_PARSING_FLAG |
	ParserFlags.ALLOW_ALERTS |
	ParserFlags.ALLOW_AUTOLINKS;
const RESTRICTED_INLINE_REPLY_FLAGS =
	STANDARD_FLAGS &
	~(
		ParserFlags.ALLOW_BLOCKQUOTES |
		ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES |
		ParserFlags.ALLOW_TABLES |
		ParserFlags.ALLOW_ALERTS |
		ParserFlags.ALLOW_HEADINGS |
		ParserFlags.ALLOW_LISTS
	);
const RESTRICTED_INLINE_PREVIEW_FLAGS =
	RESTRICTED_INLINE_REPLY_FLAGS | ParserFlags.ALLOW_HEADINGS | ParserFlags.ALLOW_LISTS;
const RESTRICTED_USER_BIO_FLAGS =
	STANDARD_FLAGS &
	~(
		ParserFlags.ALLOW_HEADINGS |
		ParserFlags.ALLOW_CODE_BLOCKS |
		ParserFlags.ALLOW_ROLE_MENTIONS |
		ParserFlags.ALLOW_EVERYONE_MENTIONS |
		ParserFlags.ALLOW_SUBTEXT |
		ParserFlags.ALLOW_TABLES |
		ParserFlags.ALLOW_ALERTS
	);
const RESTRICTED_EMBED_DESCRIPTION_FLAGS = STANDARD_FLAGS & ~ParserFlags.ALLOW_TABLES;

export function getParserFlagsForContext(context: MarkdownContext): number {
	switch (context) {
		case MarkdownContext.RESTRICTED_INLINE_REPLY:
			return RESTRICTED_INLINE_REPLY_FLAGS;
		case MarkdownContext.RESTRICTED_INLINE_PREVIEW:
			return RESTRICTED_INLINE_PREVIEW_FLAGS;
		case MarkdownContext.RESTRICTED_USER_BIO:
			return RESTRICTED_USER_BIO_FLAGS;
		case MarkdownContext.RESTRICTED_EMBED_DESCRIPTION:
			return RESTRICTED_EMBED_DESCRIPTION_FLAGS;
		default:
			return STANDARD_FLAGS;
	}
}
