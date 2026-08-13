// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/DiscoveryGuildHeader.module.css';
import guildHeaderStyles from '@app/features/app/components/layout/GuildHeader.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CompassIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';

const EXPLORE_DESCRIPTOR = msg({
	message: 'Explore',
	comment: 'Short label in the app layout discovery guild header.',
});

export function DiscoveryGuildHeader() {
	const {i18n} = useLingui();
	return (
		<div
			className={clsx(guildHeaderStyles.headerContainer, guildHeaderStyles.headerContainerNoBanner)}
			style={{height: remFromPx(56)}}
			data-flx="app.discovery-guild-header.div"
		>
			<div
				className={guildHeaderStyles.headerContent}
				style={{cursor: 'default'}}
				data-flx="app.discovery-guild-header.div--2"
			>
				<div className={styles.headerIconContainer} data-flx="app.discovery-guild-header.header-icon-container">
					<CompassIcon
						weight="fill"
						className={clsx(guildHeaderStyles.verifiedIconDefault, styles.headerIcon)}
						data-flx="app.discovery-guild-header.header-icon"
					/>
					<span className={guildHeaderStyles.guildNameDefault} data-flx="app.discovery-guild-header.span">
						{i18n._(EXPLORE_DESCRIPTOR)}
					</span>
				</div>
			</div>
		</div>
	);
}
