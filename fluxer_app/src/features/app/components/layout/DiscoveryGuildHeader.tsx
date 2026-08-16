// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/DiscoveryGuildHeader.module.css';
import guildHeaderStyles from '@app/features/app/components/layout/GuildHeader.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';

const EXPLORE_DESCRIPTOR = msg({
	message: 'Explore',
	comment: 'Short label in the app layout discovery guild header.',
});

export function DiscoveryGuildHeader() {
	const {i18n} = useLingui();
	return (
		<div
			className={guildHeaderStyles.headerContainer}
			style={{height: remFromPx(56)}}
			data-flx="app.discovery-guild-header.div"
		>
			<div
				className={guildHeaderStyles.headerContent}
				style={{cursor: 'default'}}
				data-flx="app.discovery-guild-header.div--2"
			>
				<h1 className={styles.headerTitle} data-flx="app.discovery-guild-header.header-title">
					{i18n._(EXPLORE_DESCRIPTOR)}
				</h1>
			</div>
		</div>
	);
}
