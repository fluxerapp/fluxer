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

import {Trans} from '@lingui/react/macro';
import {BookmarkSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import styles from './NotificationsPage.module.css';

interface NotificationsPageProps {
	onBookmarksClick: () => void;
}

export const NotificationsPage = observer(({onBookmarksClick}: NotificationsPageProps) => {
	return (
		<div className={styles.container}>
			<div className={styles.header}>
				<h1 className={styles.title}>
					<Trans>Notifications</Trans>
				</h1>
				<button type="button" onClick={onBookmarksClick} className={styles.bookmarkButton}>
					<BookmarkSimpleIcon weight="fill" className={styles.bookmarkIcon} />
				</button>
			</div>
			<div className={styles.emptyContainer}>
				<div className={styles.emptyContent}>
					<p className={styles.emptyTitle}>
						<Trans>No notifications</Trans>
					</p>
					<p className={styles.emptyText}>
						<Trans>You're all caught up!</Trans>
					</p>
				</div>
			</div>
		</div>
	);
});
