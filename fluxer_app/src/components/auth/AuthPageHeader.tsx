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

import {useLingui} from '@lingui/react/macro';
import {SealCheckIcon} from '@phosphor-icons/react';
import type {ReactNode} from 'react';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import styles from './AuthPageStyles.module.css';

interface AuthPageHeaderStatProps {
	value: string | number;
	dot?: 'online' | 'offline';
}

interface AuthPageHeaderProps {
	icon: ReactNode;
	title: string;
	subtitle: string;
	verified?: boolean;
	stats?: Array<AuthPageHeaderStatProps>;
}

export function AuthPageHeader({icon, title, subtitle, verified, stats}: AuthPageHeaderProps) {
	const {t} = useLingui();
	return (
		<div className={styles.entityHeader}>
			{icon}
			<div className={styles.entityDetails}>
				<p className={styles.entityText}>{title}</p>
				<div className={styles.entityTitleWrapper}>
					<h2 className={styles.entityTitle}>{subtitle}</h2>
					{verified && (
						<Tooltip text={t`Verified Community`} position="top">
							<SealCheckIcon className={styles.verifiedIcon} />
						</Tooltip>
					)}
				</div>
				{stats && stats.length > 0 && (
					<div className={styles.entityStats}>
						{stats.map((stat, index) => (
							<div key={index} className={styles.entityStat}>
								{stat.dot === 'online' && <div className={styles.onlineDot} />}
								{stat.dot === 'offline' && <div className={styles.offlineDot} />}
								<span className={styles.statText}>{stat.value}</span>
							</div>
						))}
					</div>
				)}
			</div>
		</div>
	);
}
