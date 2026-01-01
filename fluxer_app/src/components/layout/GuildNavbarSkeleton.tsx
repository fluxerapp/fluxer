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
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import styles from './GuildNavbarSkeleton.module.css';

export const GuildNavbarSkeleton = observer(() => {
	const mobileLayout = MobileLayoutStore;

	return (
		<div className={clsx(styles.skeletonContainer, mobileLayout.enabled && styles.skeletonContainerMobile)}>
			<div className={styles.skeletonHeader}>
				<div className={styles.skeletonHeaderPill} />
			</div>

			<div className={styles.skeletonContent}>
				<div className={styles.skeletonCategory}>
					<div className={styles.skeletonCategoryPill} />
				</div>
				<div className={styles.skeletonChannel}>
					<div className={styles.skeletonChannelPill} />
				</div>
				<div className={styles.skeletonChannel}>
					<div className={styles.skeletonChannelPill} />
				</div>
				<div className={styles.skeletonChannel}>
					<div className={styles.skeletonChannelPill} />
				</div>

				<div className={styles.skeletonCategory}>
					<div className={styles.skeletonCategoryPill} />
				</div>
				<div className={styles.skeletonChannel}>
					<div className={styles.skeletonChannelPill} />
				</div>
				<div className={styles.skeletonChannel}>
					<div className={styles.skeletonChannelPill} />
				</div>

				<div className={styles.skeletonCategory}>
					<div className={styles.skeletonCategoryPill} />
				</div>
				<div className={styles.skeletonChannel}>
					<div className={styles.skeletonChannelPill} />
				</div>
				<div className={styles.skeletonChannel}>
					<div className={styles.skeletonChannelPill} />
				</div>
				<div className={styles.skeletonChannel}>
					<div className={styles.skeletonChannelPill} />
				</div>
				<div className={styles.skeletonChannel}>
					<div className={styles.skeletonChannelPill} />
				</div>
			</div>
		</div>
	);
});
