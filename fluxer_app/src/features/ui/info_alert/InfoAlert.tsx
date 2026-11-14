// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/ui/info_alert/InfoAlert.module.css';
import {InfoIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';

interface InfoAlertProps {
	children: React.ReactNode;
	className?: string;
	'data-flx'?: string;
}

export const InfoAlert: React.FC<InfoAlertProps> = ({children, className, 'data-flx': dataFlx}) => (
	<div className={clsx(styles.alert, className)} role="note" data-flx={dataFlx ?? 'ui.info-alert.info-alert.alert'}>
		<InfoIcon
			size={remFromPx(20)}
			weight="fill"
			className={styles.icon}
			aria-hidden={true}
			data-flx="ui.info-alert.info-alert.icon"
		/>
		<p className={styles.text} data-flx="ui.info-alert.info-alert.text">
			{children}
		</p>
	</div>
);
