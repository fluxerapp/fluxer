// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/theme_studio/ui/StudioToolbar.module.css';
import type React from 'react';
import type {ReactNode} from 'react';

interface StudioToolbarProps {
	trailing?: ReactNode;
	draggable?: boolean;
	trailingEdgeToEdge?: boolean;
}

export const StudioToolbar: React.FC<StudioToolbarProps> = ({
	trailing,
	draggable = false,
	trailingEdgeToEdge = false,
}) => {
	const hasTrailing = trailing !== undefined && trailing !== null && trailing !== false;
	return (
		<header
			className={styles.toolbar}
			data-draggable={draggable ? 'true' : undefined}
			data-trailing-edge-to-edge={trailingEdgeToEdge ? 'true' : undefined}
			data-flx="theme-studio.ui.studio-toolbar.toolbar"
		>
			<div className={styles.spacer} data-flx="theme-studio.ui.studio-toolbar.spacer" />
			{hasTrailing ? (
				<div className={styles.trailing} data-flx="theme-studio.ui.studio-toolbar.trailing">
					{trailing}
				</div>
			) : null}
		</header>
	);
};
