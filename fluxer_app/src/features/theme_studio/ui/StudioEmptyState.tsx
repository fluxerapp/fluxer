// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/theme_studio/ui/StudioEmptyState.module.css';
import type React from 'react';
import type {ReactNode} from 'react';

interface StudioEmptyStateProps {
	icon?: ReactNode;
	title: ReactNode;
	description?: ReactNode;
	actions?: ReactNode;
}

export const StudioEmptyState: React.FC<StudioEmptyStateProps> = ({icon, title, description, actions}) => (
	<div className={styles.empty} data-flx="theme-studio.ui.studio-empty-state.empty">
		{icon ? (
			<div className={styles.icon} data-flx="theme-studio.ui.studio-empty-state.icon">
				{icon}
			</div>
		) : null}
		<h4 className={styles.title} data-flx="theme-studio.ui.studio-empty-state.title">
			{title}
		</h4>
		{description ? (
			<p className={styles.description} data-flx="theme-studio.ui.studio-empty-state.description">
				{description}
			</p>
		) : null}
		{actions ? (
			<div className={styles.actions} data-flx="theme-studio.ui.studio-empty-state.actions">
				{actions}
			</div>
		) : null}
	</div>
);
