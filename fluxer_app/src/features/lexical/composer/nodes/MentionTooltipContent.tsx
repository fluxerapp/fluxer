// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/lexical/composer/nodes/MentionTooltipContent.module.css';
import {HoverFloatingTooltipSurface} from '@app/features/ui/tooltip/HoverFloatingTooltipSurface';
import {HoverFloatingTooltipTrigger} from '@app/features/ui/tooltip/HoverFloatingTooltipTrigger';
import {useHoverFloatingTooltip} from '@app/features/ui/tooltip/useHoverFloatingTooltip';
import {UserIdentityRow} from '@app/features/user/components/UserIdentityRow';
import Users from '@app/features/user/state/Users';
import {flxElementClassName} from '@app/lib/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface MentionWithTooltipProps {
	children: React.ReactElement<Record<string, unknown> & {ref?: React.Ref<HTMLElement>}>;
	userId: string;
	guildId?: string;
	channelId?: string;
}

export const MentionWithTooltip = observer(({children, userId, guildId, channelId}: MentionWithTooltipProps) => {
	const tooltip = useHoverFloatingTooltip(300);
	const user = Users.getUser(userId);
	if (!user) {
		return children;
	}
	return (
		<>
			<HoverFloatingTooltipTrigger tooltip={tooltip}>{children}</HoverFloatingTooltipTrigger>
			<HoverFloatingTooltipSurface
				tooltip={tooltip}
				portalDataFlx="lexical.mention-tooltip-content.floating-portal"
				presenceDataFlx="lexical.mention-tooltip-content.animate-presence"
				data-flx="lexical.mention-tooltip-content.surface"
			>
				<flx-lexical-mention-tooltip-card className={flxElementClassName(styles.card)}>
					<UserIdentityRow user={user} guildId={guildId} channelId={channelId} avatarSize={40} />
				</flx-lexical-mention-tooltip-card>
			</HoverFloatingTooltipSurface>
		</>
	);
});
