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

import React from 'react';
import {PreloadableUserPopout} from '~/components/channel/PreloadableUserPopout';
import type {GuildRecord} from '~/records/GuildRecord';
import type {MessageRecord} from '~/records/MessageRecord';
import type {UserRecord} from '~/records/UserRecord';
import GuildMemberStore from '~/stores/GuildMemberStore';
import styles from '~/styles/Message.module.css';
import * as NicknameUtils from '~/utils/NicknameUtils';

export const SystemMessageUsername = React.forwardRef<
	HTMLElement,
	{
		author: UserRecord;
		guild?: GuildRecord;
		message: MessageRecord;
	}
>(({author, guild, message}, ref) => {
	const member = GuildMemberStore.getMember(guild?.id ?? '', author.id);
	return (
		<PreloadableUserPopout ref={ref} user={author} isWebhook={false} guildId={guild?.id} channelId={message.channelId}>
			<span
				className={styles.systemMessageLink}
				style={{color: member?.getColorString()}}
				data-user-id={author.id}
				data-guild-id={guild?.id}
			>
				{NicknameUtils.getNickname(author, guild?.id)}
			</span>
		</PreloadableUserPopout>
	);
});
