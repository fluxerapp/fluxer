// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/discovery/discovery/DiscoverySidebar.module.css';
import {COMMUNITIES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Navigation from '@app/features/navigation/state/Navigation';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Scroller} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {AppWindowIcon, type Icon, PaletteIcon, UsersThreeIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

const DISCOVERY_NAVIGATION_DESCRIPTOR = msg({
	message: 'Discovery navigation',
	comment: 'Accessible label for the section navigation in the Discovery page sidebar.',
});
const APPS_DESCRIPTOR = msg({
	message: 'Apps',
	comment: 'Short label in the Discovery page sidebar. Keep it concise.',
});
const THEMES_DESCRIPTOR = msg({
	message: 'Themes',
	comment: 'Short label in the Discovery page sidebar. Keep it concise.',
});
const COMING_SOON_DESCRIPTOR = msg({
	message: 'Coming soon',
	comment: 'Small badge on disabled Discovery sidebar navigation items for sections not available yet.',
});
const COMING_SOON_NAV_ITEM_DESCRIPTOR = msg({
	message: '{itemLabel}, coming soon',
	comment: 'Accessible label for a disabled Discovery sidebar navigation item. {itemLabel} is the section name.',
});

interface DiscoveryNavigationItem {
	label: string;
	icon: Icon;
	active?: boolean;
	disabled?: boolean;
}

export const DiscoverySidebar = observer(function DiscoverySidebar() {
	const {i18n} = useLingui();
	const comingSoonLabel = i18n._(COMING_SOON_DESCRIPTOR);
	const items: Array<DiscoveryNavigationItem> = [
		{label: i18n._(COMMUNITIES_DESCRIPTOR), icon: UsersThreeIcon, active: true},
		{label: i18n._(APPS_DESCRIPTOR), icon: AppWindowIcon, disabled: true},
		{label: i18n._(THEMES_DESCRIPTOR), icon: PaletteIcon, disabled: true},
	];
	return (
		<div className={styles.container} data-flx="discovery.discovery.discovery-sidebar.container">
			<Scroller
				showTrack={false}
				role="navigation"
				aria-label={i18n._(DISCOVERY_NAVIGATION_DESCRIPTOR)}
				data-flx="discovery.discovery.discovery-sidebar.scroller"
			>
				<div className={styles.navList} role="list" data-flx="discovery.discovery.discovery-sidebar.nav-list">
					{items.map((item) => {
						const ItemIcon = item.icon;
						return (
							<FocusRing key={item.label} offset={-2} data-flx="discovery.discovery.discovery-sidebar.focus-ring">
								<button
									type="button"
									className={clsx(
										styles.navItem,
										item.active && styles.navItemActive,
										item.disabled && styles.navItemDisabled,
									)}
									disabled={item.disabled}
									onClick={item.active ? () => Navigation.navigateToDiscover('replace') : undefined}
									aria-current={item.active ? 'page' : undefined}
									aria-label={
										item.disabled ? i18n._(COMING_SOON_NAV_ITEM_DESCRIPTOR, {itemLabel: item.label}) : undefined
									}
									data-flx="discovery.discovery.discovery-sidebar.nav-item"
								>
									<ItemIcon
										size={remFromPx(18)}
										weight={item.active ? 'fill' : 'bold'}
										className={styles.navItemIcon}
										aria-hidden
										data-flx="discovery.discovery.discovery-sidebar.nav-item-icon"
									/>
									<span className={styles.navItemLabel} data-flx="discovery.discovery.discovery-sidebar.nav-item-label">
										{item.label}
									</span>
									{item.disabled && (
										<span
											className={styles.comingSoonBadge}
											aria-hidden
											data-flx="discovery.discovery.discovery-sidebar.coming-soon-badge"
										>
											{comingSoonLabel}
										</span>
									)}
								</button>
							</FocusRing>
						);
					})}
				</div>
			</Scroller>
		</div>
	);
});
