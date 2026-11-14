// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ChannelPageHeaderTitle.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import type {Icon} from '@phosphor-icons/react';
import type React from 'react';

interface ChannelPageHeaderTitleProps {
	readonly icon: Icon;
	readonly label: string;
}

export const ChannelPageHeaderTitle: React.FC<ChannelPageHeaderTitleProps> = ({icon: HeaderIcon, label}) => (
	<div className={styles.headerLeftContent} data-flx="channel.channel-page-header-title.header-left-content">
		<HeaderIcon
			className={styles.headerIcon}
			size={remFromPx(20)}
			data-flx="channel.channel-page-header-title.header-icon"
		/>
		<span className={styles.headerLabel} data-flx="channel.channel-page-header-title.header-label">
			{label}
		</span>
	</div>
);
