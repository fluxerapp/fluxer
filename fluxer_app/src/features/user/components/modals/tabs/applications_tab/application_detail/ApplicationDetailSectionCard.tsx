// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetail.module.css';
import clsx from 'clsx';
import type React from 'react';

interface SectionCardProps {
	title: React.ReactNode;
	subtitle?: React.ReactNode;
	tone?: 'default' | 'danger';
	children: React.ReactNode;
}

export const SectionCard: React.FC<SectionCardProps> = ({title, subtitle, children, tone = 'default'}) => {
	return (
		<section
			className={clsx(styles.card, tone === 'danger' && styles.cardDanger)}
			data-flx="user.applications-tab.application-detail.section-card.card"
		>
			<div className={styles.cardHeader} data-flx="user.applications-tab.application-detail.section-card.card-header">
				<div data-flx="user.applications-tab.application-detail.section-card.div">
					<h3 className={styles.cardTitle} data-flx="user.applications-tab.application-detail.section-card.card-title">
						{title}
					</h3>
					{subtitle && (
						<p
							className={styles.cardSubtitle}
							data-flx="user.applications-tab.application-detail.section-card.card-subtitle"
						>
							{subtitle}
						</p>
					)}
				</div>
			</div>
			<div className={styles.cardBody} data-flx="user.applications-tab.application-detail.section-card.card-body">
				{children}
			</div>
		</section>
	);
};
