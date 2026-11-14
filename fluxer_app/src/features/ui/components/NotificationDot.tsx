// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/ui/components/NotificationDot.module.css';
import {clsx} from 'clsx';

interface NotificationDotProps {
	className?: string;
	label?: string;
	'data-flx'?: string;
}

export function NotificationDot({className, label, 'data-flx': dataFlx}: NotificationDotProps) {
	return (
		<>
			<span
				className={clsx(styles.dot, className)}
				aria-hidden="true"
				data-flx={dataFlx ?? 'ui.notification-dot.dot'}
			/>
			{label && (
				<span className={styles.srOnly} data-flx="ui.notification-dot.label">
					{label}
				</span>
			)}
		</>
	);
}
