// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/theme_studio/ui/StudioDivider.module.css';
import {clsx} from 'clsx';
import type React from 'react';

interface StudioDividerProps {
	orientation?: 'horizontal' | 'vertical';
	className?: string;
}

export const StudioDivider: React.FC<StudioDividerProps> = ({orientation = 'horizontal', className}) => (
	<div
		aria-hidden="true"
		className={clsx(styles.divider, styles[orientation], className)}
		data-flx="theme-studio.ui.studio-divider.divider"
	/>
);
