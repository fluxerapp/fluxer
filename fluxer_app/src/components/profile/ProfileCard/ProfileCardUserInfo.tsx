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
import {UserTag} from '~/components/channel/UserTag';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import type {UserRecord} from '~/records/UserRecord';
import styles from './ProfileCardUserInfo.module.css';

interface ProfileCardUserInfoProps {
	displayName: string;
	user: UserRecord;
	pronouns?: string | null;
	showUsername?: boolean;
	isClickable?: boolean;
	isWebhook?: boolean;
	onDisplayNameClick?: () => void;
	onUsernameClick?: () => void;
	actions?: React.ReactNode;
	usernameActions?: React.ReactNode;
}

export const ProfileCardUserInfo: React.FC<ProfileCardUserInfoProps> = observer(
	({
		displayName,
		user,
		pronouns,
		showUsername = true,
		isClickable = true,
		isWebhook = false,
		onDisplayNameClick,
		onUsernameClick,
		actions,
		usernameActions,
	}) => {
		return (
			<div className={styles.userInfoContainer}>
				<div className={styles.nameRow}>
					<FocusRing offset={-2}>
						<button
							type="button"
							onClick={onDisplayNameClick}
							className={clsx(styles.nameButton, isClickable && styles.nameButtonClickable)}
						>
							{displayName}
						</button>
					</FocusRing>

					<div className={styles.badgeContainer}>
						{(user.bot || isWebhook) && <UserTag className={styles.userTagWrapper} system={user.system} />}
					</div>

					{actions && <div className={styles.actionsContainer}>{actions}</div>}
				</div>

				{showUsername && (
					<div className={styles.usernameRow}>
						<FocusRing offset={-2}>
							<button type="button" onClick={onUsernameClick} className={styles.usernameButton}>
								{user.tag}
							</button>
						</FocusRing>
						{usernameActions}
					</div>
				)}

				{pronouns && <div className={styles.pronouns}>{pronouns}</div>}
			</div>
		);
	},
);
