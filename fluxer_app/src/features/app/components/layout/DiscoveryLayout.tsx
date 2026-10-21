// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {DiscoveryGuildHeader} from '@app/features/app/components/layout/DiscoveryGuildHeader';
import styles from '@app/features/app/components/layout/GuildLayout.module.css';
import {GuildSidebar} from '@app/features/app/components/layout/GuildSidebar';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {DiscoveryPage} from '@app/features/discovery/discovery/DiscoveryPage';
import {DiscoverySidebar} from '@app/features/discovery/discovery/DiscoverySidebar';
import Discovery from '@app/features/discovery/state/Discovery';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';

export const DiscoveryLayout = observer(function DiscoveryLayout() {
	const mobileLayout = MobileLayout;
	const available = RuntimeConfig.discoveryAvailable;
	useEffect(() => {
		if (!available) {
			RouterUtils.replaceWith(Routes.ME);
			return;
		}
		void Discovery.loadCategories();
		void Discovery.search({offset: 0});
		return () => {
			Discovery.reset();
		};
	}, [available]);
	if (!available) {
		return null;
	}
	if (mobileLayout.enabled) {
		return <DiscoveryPage data-flx="app.discovery-layout.discovery-page.mobile" />;
	}
	return (
		<div className={styles.guildLayoutContainer} data-flx="app.discovery-layout.guild-layout-container">
			<div className={styles.guildLayoutContent} data-flx="app.discovery-layout.guild-layout-content">
				<GuildSidebar
					header={<DiscoveryGuildHeader data-flx="app.discovery-layout.discovery-guild-header" />}
					content={<DiscoverySidebar data-flx="app.discovery-layout.discovery-sidebar" />}
					data-flx="app.discovery-layout.guild-sidebar"
				/>
				<div className={styles.guildMainContent} data-flx="app.discovery-layout.guild-main-content">
					<DiscoveryPage data-flx="app.discovery-layout.discovery-page" />
				</div>
			</div>
		</div>
	);
});
