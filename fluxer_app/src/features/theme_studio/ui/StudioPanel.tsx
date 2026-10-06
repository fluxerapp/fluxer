// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/theme_studio/ui/StudioPanel.module.css';
import {clsx} from 'clsx';
import type React from 'react';
import type {ReactNode} from 'react';

interface StudioPanelProps {
	title?: ReactNode;
	description?: ReactNode;
	actions?: ReactNode;
	padded?: boolean;
	className?: string;
	bodyClassName?: string;
	children: ReactNode;
}

export const StudioPanel: React.FC<StudioPanelProps> = ({
	title,
	description,
	actions,
	padded = false,
	className,
	bodyClassName,
	children,
}) => {
	const hasHeader = title !== undefined || actions !== undefined;
	return (
		<section className={clsx(styles.panel, className)} data-flx="theme-studio.ui.studio-panel.panel">
			{hasHeader ? (
				<div className={styles.panelHeader} data-flx="theme-studio.ui.studio-panel.panel-header">
					<div data-flx="theme-studio.ui.studio-panel.div">
						{title ? (
							<h4 className={styles.panelTitle} data-flx="theme-studio.ui.studio-panel.panel-title">
								{title}
							</h4>
						) : null}
						{description ? (
							<p className={styles.panelDescription} data-flx="theme-studio.ui.studio-panel.panel-description">
								{description}
							</p>
						) : null}
					</div>
					{actions ? (
						<div className={styles.panelActions} data-flx="theme-studio.ui.studio-panel.panel-actions">
							{actions}
						</div>
					) : null}
				</div>
			) : null}
			<div
				className={clsx(styles.panelBody, padded && styles.panelBodyPadded, bodyClassName)}
				data-flx="theme-studio.ui.studio-panel.panel-body"
			>
				{children}
			</div>
		</section>
	);
};
