// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/theme_studio/ui/StudioPanel.module.css';
import type React from 'react';
import type {ReactNode} from 'react';

interface StudioPanelProps {
	title?: ReactNode;
	children: ReactNode;
}

export const StudioPanel: React.FC<StudioPanelProps> = ({title, children}) => {
	const hasHeader = title !== undefined;
	return (
		<section className={styles.panel} data-flx="theme-studio.ui.studio-panel.panel">
			{hasHeader ? (
				<div className={styles.panelHeader} data-flx="theme-studio.ui.studio-panel.panel-header">
					<div data-flx="theme-studio.ui.studio-panel.div">
						{title ? (
							<h4 className={styles.panelTitle} data-flx="theme-studio.ui.studio-panel.panel-title">
								{title}
							</h4>
						) : null}
					</div>
				</div>
			) : null}
			<div className={styles.panelBody} data-flx="theme-studio.ui.studio-panel.panel-body">
				{children}
			</div>
		</section>
	);
};
