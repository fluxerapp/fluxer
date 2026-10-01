// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/badge/components/BadgeIcon.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {clsx} from 'clsx';

interface BadgeIconProps {
	icon: string;
	label: string;
	size?: number;
	className?: string;
}

export function BadgeIcon({icon, label, size, className}: BadgeIconProps) {
	return (
		<span
			role="img"
			aria-label={label}
			style={size === undefined ? undefined : {width: remFromPx(size), height: remFromPx(size)}}
			className={clsx(styles.icon, className)}
			dangerouslySetInnerHTML={{__html: icon}}
			data-flx="badge.badge-icon.span"
		/>
	);
}
