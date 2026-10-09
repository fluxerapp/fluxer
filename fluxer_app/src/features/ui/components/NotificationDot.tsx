// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/ui/components/NotificationDot.module.css';
import {clsx} from 'clsx';

interface NotificationDotProps {
	className?: string;
	label?: string;
}

export function NotificationDot({className, label}: NotificationDotProps) {
	return (
		<>
			<span className={clsx(styles.dot, className)} aria-hidden="true" data-flx="ui.notification-dot.dot" />
			{label && (
				<span className={styles.srOnly} data-flx="ui.notification-dot.label">
					{label}
				</span>
			)}
		</>
	);
}
