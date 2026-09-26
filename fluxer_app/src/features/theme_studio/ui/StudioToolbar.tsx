// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/theme_studio/ui/StudioToolbar.module.css';
import {clsx} from 'clsx';
import type React from 'react';
import type {ReactNode} from 'react';

interface StudioToolbarProps {
	leading?: ReactNode;
	title?: ReactNode;
	subtitle?: ReactNode;
	dirty?: ReactNode;
	trailing?: ReactNode;
	className?: string;
	draggable?: boolean;
	trailingEdgeToEdge?: boolean;
}

export const StudioToolbar: React.FC<StudioToolbarProps> = ({
	leading,
	title,
	subtitle,
	dirty,
	trailing,
	className,
	draggable = false,
	trailingEdgeToEdge = false,
}) => {
	const hasLeading = leading !== undefined && leading !== null && leading !== false;
	const hasTitle = title !== undefined && title !== null && title !== false;
	const hasSubtitle = subtitle !== undefined && subtitle !== null && subtitle !== false;
	const hasDirty = dirty !== undefined && dirty !== null && dirty !== false;
	const hasTrailing = trailing !== undefined && trailing !== null && trailing !== false;
	const hasCenter = hasTitle || hasSubtitle || hasDirty;
	return (
		<header
			className={clsx(styles.toolbar, className)}
			data-draggable={draggable ? 'true' : undefined}
			data-trailing-edge-to-edge={trailingEdgeToEdge ? 'true' : undefined}
			data-flx="theme-studio.ui.studio-toolbar.toolbar"
		>
			{hasLeading ? (
				<div className={styles.leading} data-flx="theme-studio.ui.studio-toolbar.leading">
					{leading}
				</div>
			) : null}
			{hasCenter ? (
				<div className={styles.center} data-flx="theme-studio.ui.studio-toolbar.center">
					{hasTitle ? (
						<span className={styles.title} data-flx="theme-studio.ui.studio-toolbar.title">
							{title}
						</span>
					) : null}
					{hasSubtitle ? (
						<span className={styles.subtitle} data-flx="theme-studio.ui.studio-toolbar.subtitle">
							{subtitle}
						</span>
					) : null}
					{hasDirty ? (
						<span className={styles.dirtyPill} data-flx="theme-studio.ui.studio-toolbar.dirty-pill">
							<span className={styles.dirtyDot} data-flx="theme-studio.ui.studio-toolbar.dirty-dot" />
							{dirty}
						</span>
					) : null}
				</div>
			) : (
				<div className={styles.spacer} data-flx="theme-studio.ui.studio-toolbar.spacer" />
			)}
			{hasTrailing ? (
				<div className={styles.trailing} data-flx="theme-studio.ui.studio-toolbar.trailing">
					{trailing}
				</div>
			) : null}
		</header>
	);
};
