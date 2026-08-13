// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/modals/channel_tabs/ChannelOverviewTab.module.css';
import {clsx} from 'clsx';
import type React from 'react';

interface SettingsControlRowProps {
	label: string;
	description?: string;
	dataFlx: string;
	stacked?: boolean;
	children: React.ReactNode;
}

export const SettingsControlRow: React.FC<SettingsControlRowProps> = ({
	label,
	description,
	dataFlx,
	stacked = false,
	children,
}) => {
	return (
		<div
			className={clsx(styles.settingsControlRow, stacked && styles.settingsControlRowStacked)}
			data-flx={`${dataFlx}.row`}
		>
			<div className={styles.settingsControlText} data-flx={`${dataFlx}.text`}>
				<div className={styles.settingsControlTitleRow} data-flx={`${dataFlx}.title-row`}>
					<span className={styles.settingsControlLabel} data-flx={`${dataFlx}.label`}>
						{label}
					</span>
				</div>
				{description != null && (
					<p className={styles.settingsControlDescription} data-flx={`${dataFlx}.description`}>
						{description}
					</p>
				)}
			</div>
			{children}
		</div>
	);
};
