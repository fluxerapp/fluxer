// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/theme_studio/ui/StudioSidebar.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {clsx} from 'clsx';
import type React from 'react';
import type {ReactNode} from 'react';

interface StudioSidebarProps {
	ariaLabel: string;
	footer?: ReactNode;
	children: ReactNode;
}

export const StudioSidebar: React.FC<StudioSidebarProps> = ({ariaLabel, footer, children}) => (
	<aside className={styles.sidebar} aria-label={ariaLabel} data-flx="theme-studio.ui.studio-sidebar.sidebar">
		<nav className={styles.nav} data-flx="theme-studio.ui.studio-sidebar.nav">
			{children}
		</nav>
		{footer ? (
			<div className={styles.footer} data-flx="theme-studio.ui.studio-sidebar.footer">
				{footer}
			</div>
		) : null}
	</aside>
);

interface StudioSidebarItemProps {
	icon: ReactNode;
	label: ReactNode;
	badge?: ReactNode;
	active?: boolean;
	onClick: () => void;
}

export const StudioSidebarItem: React.FC<StudioSidebarItemProps> = ({icon, label, badge, active, onClick}) => (
	<FocusRing offset={-2} data-flx="theme-studio.ui.studio-sidebar.studio-sidebar-item.focus-ring">
		<button
			type="button"
			className={clsx(styles.item, active && styles.itemActive)}
			aria-current={active ? 'page' : undefined}
			onClick={onClick}
			data-flx="theme-studio.ui.studio-sidebar.studio-sidebar-item.item.click.button"
		>
			<span className={styles.itemIcon} data-flx="theme-studio.ui.studio-sidebar.studio-sidebar-item.item-icon">
				{icon}
			</span>
			<span className={styles.itemLabel} data-flx="theme-studio.ui.studio-sidebar.studio-sidebar-item.item-label">
				{label}
			</span>
			{badge !== undefined && badge !== null ? (
				<span className={styles.itemBadge} data-flx="theme-studio.ui.studio-sidebar.studio-sidebar-item.item-badge">
					{badge}
				</span>
			) : null}
		</button>
	</FocusRing>
);
export const StudioSidebarGroupLabel: React.FC<{children: ReactNode}> = ({children}) => (
	<div className={styles.groupLabel} data-flx="theme-studio.ui.studio-sidebar.studio-sidebar-group-label.group-label">
		{children}
	</div>
);
export const StudioSidebarFooterText: React.FC<{children: ReactNode}> = ({children}) => (
	<span className={styles.footerText} data-flx="theme-studio.ui.studio-sidebar.studio-sidebar-footer-text.footer-text">
		{children}
	</span>
);
