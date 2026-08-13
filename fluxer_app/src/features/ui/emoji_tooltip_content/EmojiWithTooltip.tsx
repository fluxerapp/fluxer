// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmojiInfoContent} from '@app/features/emoji/components/emojis/EmojiInfoContent';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {EmojiTooltipContent} from '@app/features/ui/emoji_tooltip_content/EmojiTooltipContent';
import {HoverFloatingTooltipSurface} from '@app/features/ui/tooltip/HoverFloatingTooltipSurface';
import {HoverFloatingTooltipTrigger} from '@app/features/ui/tooltip/HoverFloatingTooltipTrigger';
import {useHoverFloatingTooltip} from '@app/features/ui/tooltip/useHoverFloatingTooltip';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface EmojiWithTooltipProps {
	children: React.ReactElement<Record<string, unknown> & {ref?: React.Ref<HTMLElement>}>;
	emojiUrl: string | null;
	emojiName: string;
	emojiForSubtext: FlatEmoji;
}

export const EmojiWithTooltip = observer(({children, emojiUrl, emojiName, emojiForSubtext}: EmojiWithTooltipProps) => {
	const tooltip = useHoverFloatingTooltip(500);
	return (
		<>
			<HoverFloatingTooltipTrigger tooltip={tooltip}>{children}</HoverFloatingTooltipTrigger>
			<HoverFloatingTooltipSurface
				tooltip={tooltip}
				portalDataFlx="ui.emoji-with-tooltip.floating-portal"
				presenceDataFlx="ui.emoji-with-tooltip.animate-presence"
				data-flx="ui.emoji-with-tooltip.surface"
			>
				<EmojiTooltipContent
					emojiUrl={emojiUrl}
					emojiAlt={emojiName}
					primaryContent={emojiName}
					subtext={<EmojiInfoContent emoji={emojiForSubtext} />}
				/>
			</HoverFloatingTooltipSurface>
		</>
	);
});
