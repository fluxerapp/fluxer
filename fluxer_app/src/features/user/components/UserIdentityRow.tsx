// SPDX-License-Identifier: AGPL-3.0-or-later

import {Avatar} from '@app/features/ui/components/Avatar';
import styles from '@app/features/user/components/UserIdentityRow.module.css';
import type {User} from '@app/features/user/models/User';
import * as DisplayNameUtils from '@app/features/user/utils/DisplayNameUtils';
import {flxElementClassName} from '@app/lib/react';
import {observer} from 'mobx-react-lite';

interface UserIdentityRowProps {
	user: User;
	guildId?: string | null;
	channelId?: string;
	avatarSize?: number;
	className?: string;
}

export const UserIdentityRow = observer(
	({user, guildId, channelId, avatarSize = 40, className}: UserIdentityRowProps) => {
		const displayName = DisplayNameUtils.getNickname(user, guildId, channelId);
		return (
			<>
				<Avatar user={user} size={avatarSize} guildId={guildId === null ? undefined : guildId} className={className} />
				<flx-user-identity-row-info className={flxElementClassName(styles.info)}>
					<span className={styles.name}>{displayName}</span>
					<span className={styles.tag}>{user.tag}</span>
				</flx-user-identity-row-info>
			</>
		);
	},
);
