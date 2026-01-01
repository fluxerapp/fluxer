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

import clsx from 'clsx';
import type React from 'react';
import styles from './ApplicationDetail.module.css';

interface SectionCardProps {
	title: React.ReactNode;
	subtitle?: React.ReactNode;
	actions?: React.ReactNode;
	tone?: 'default' | 'danger';
	children: React.ReactNode;
}

export const SectionCard: React.FC<SectionCardProps> = ({title, subtitle, actions, children, tone = 'default'}) => {
	return (
		<section className={clsx(styles.card, tone === 'danger' && styles.cardDanger)}>
			<div className={styles.cardHeader}>
				<div>
					<h3 className={styles.cardTitle}>{title}</h3>
					{subtitle && <p className={styles.cardSubtitle}>{subtitle}</p>}
				</div>
				{actions && <div className={styles.cardActions}>{actions}</div>}
			</div>
			<div className={styles.cardBody}>{children}</div>
		</section>
	);
};
