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

import {observer} from 'mobx-react-lite';
import {PreloadableUserPopout} from '~/components/channel/PreloadableUserPopout';
import {Avatar} from '~/components/uikit/Avatar';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import type {MessageRecord} from '~/records/MessageRecord';
import type {UserRecord} from '~/records/UserRecord';

export const MessageAvatar = observer(
	({
		user,
		message,
		guildId,
		size,
		className,
		isHovering,
	}: {
		user: UserRecord;
		message: MessageRecord;
		guildId?: string;
		size: 16 | 24 | 32 | 40 | 48 | 80 | 120;
		className: string;
		isHovering: boolean;
		isPreview: boolean;
	}) => {
		return (
			<PreloadableUserPopout
				user={user}
				isWebhook={message.webhookId != null}
				guildId={guildId}
				channelId={message.channelId}
				enableLongPressActions={false}
			>
				<FocusRing>
					<Avatar
						user={user}
						size={size}
						className={className}
						forceAnimate={isHovering}
						guildId={guildId}
						data-user-id={user.id}
						data-guild-id={guildId}
					/>
				</FocusRing>
			</PreloadableUserPopout>
		);
	},
);
