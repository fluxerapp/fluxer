// SPDX-License-Identifier: AGPL-3.0-or-later

import {getUserAccentColor} from '@app/features/theme/utils/AccentColorUtils';
import {Avatar} from '@app/features/ui/components/Avatar';
import styles from '@app/features/user/components/UserCard.module.css';
import type {User} from '@app/features/user/models/User';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface UserCardProps {
	user: User;
	guildId: string;
	children: React.ReactNode;
}

export const UserCard: React.FC<UserCardProps> = observer(({user, guildId, children}) => (
	<div className={styles.card} data-flx="user.user-card.card">
		<div
			className={styles.banner}
			style={{
				backgroundColor: getUserAccentColor(user, user.bannerColor),
				...(user.banner
					? {backgroundImage: `url(${AvatarUtils.getUserBannerURL({id: user.id, banner: user.banner})})`}
					: {}),
			}}
			data-flx="user.user-card.banner"
		/>
		<div className={styles.body} data-flx="user.user-card.body">
			<Avatar user={user} size={52} guildId={guildId} data-flx="user.user-card.avatar" />
			<div className={styles.text} data-flx="user.user-card.text">
				<span className={styles.name} data-flx="user.user-card.name">
					{NicknameUtils.getNickname(user, guildId)}
				</span>
				<span className={styles.tag} data-flx="user.user-card.tag">
					{NicknameUtils.formatTagForStreamerMode(user.tag)}
				</span>
				<span className={styles.userId} data-flx="user.user-card.user-id">
					{user.id}
				</span>
			</div>
			{children}
		</div>
	</div>
));
