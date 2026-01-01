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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import UserStore from '~/stores/UserStore';
import styles from './PersonalNotesWelcomeSection.module.css';

interface PersonalNotesWelcomeSectionProps {
	userId: string;
}

export const PersonalNotesWelcomeSection: React.FC<PersonalNotesWelcomeSectionProps> = observer(({userId}) => {
	const {t} = useLingui();
	const user = UserStore.getUser(userId);

	if (!user) {
		return null;
	}

	return (
		<div className={styles.welcomeSection}>
			<div className={styles.avatarContainer}>
				<div className={styles.avatarBackground} />
				<StatusAwareAvatar user={user} size={80} disablePresence={true} className={styles.avatar} />
			</div>

			<h1 className={styles.title}>
				<Trans>Personal Notes</Trans>
			</h1>

			<div className={styles.dividerContainer}>
				<svg
					width="120"
					height="8"
					viewBox="0 0 120 8"
					className={styles.dividerSvg}
					role="img"
					aria-label={t`Decorative divider`}
				>
					<path
						d="M0,4 C10,0 15,8 25,4 C35,0 40,8 50,4 C60,0 65,8 75,4 C85,0 90,8 100,4 C110,0 115,8 120,4"
						stroke="currentColor"
						strokeWidth="1.5"
						fill="none"
					/>
				</svg>
			</div>

			<p className={styles.description}>
				<Trans>Your private space for thoughts and reminders</Trans>
			</p>
		</div>
	);
});
