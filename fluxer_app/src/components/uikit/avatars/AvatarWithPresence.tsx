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

import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {UserRecord} from '~/records/UserRecord';
import GuildMemberStore from '~/stores/GuildMemberStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import styles from './AvatarWithPresence.module.css';

interface Props {
	user: UserRecord;
	size: number;
	speaking?: boolean;
	className?: string;
	title?: string;
	borderClassName?: string;
	guildId?: string | null;
}

export const AvatarWithPresence: React.FC<Props> = observer(function AvatarWithPresence({
	user,
	size,
	speaking,
	className,
	title,
	borderClassName,
	guildId,
}) {
	const guildMember = GuildMemberStore.getMember(guildId || '', user.id);
	const src =
		guildId && guildMember?.avatar
			? (AvatarUtils.getGuildMemberAvatarURL({
					guildId,
					userId: user.id,
					avatar: guildMember.avatar,
					animated: false,
				}) ?? AvatarUtils.getUserAvatarURL(user, false))
			: AvatarUtils.getUserAvatarURL(user, false);

	return (
		<div
			className={clsx(styles.container, borderClassName, className)}
			style={{width: size, height: size}}
			title={title ?? user.username}
		>
			<div className={clsx(styles.imageWrapper, speaking && styles.imageWrapperSpeaking)}>
				<img src={src} alt={user.username} draggable={false} loading="lazy" decoding="async" className={styles.image} />
			</div>
		</div>
	);
});
