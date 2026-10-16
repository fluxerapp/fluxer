// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/flow/InstanceSelector.module.css';
import {InstanceDiscoveryStatus} from '@app/features/auth/flow/instance_selector/InstanceSelectorTypes';
import {flxElementClassName} from '@app/lib/react';

interface InstanceSelectorStatusRowProps {
	readonly status: InstanceDiscoveryStatus;
	readonly statusId: string;
	readonly statusMessage: string | null;
}

function resolveStatusRowRole(status: InstanceDiscoveryStatus): 'alert' | 'status' {
	if (status === InstanceDiscoveryStatus.ERROR) {
		return 'alert';
	}
	return 'status';
}

export function InstanceSelectorStatusRow({status, statusId, statusMessage}: InstanceSelectorStatusRowProps) {
	if (statusMessage == null || statusMessage.length === 0) {
		return null;
	}
	return (
		<flx-auth-instance-selector-status-row
			className={flxElementClassName(styles.statusRow)}
			data-flx="auth.flow.instance-selector.instance-selector-status-row.status-row"
		>
			<flx-auth-instance-selector-status-text
				id={statusId}
				className={flxElementClassName(
					styles.statusText,
					status === InstanceDiscoveryStatus.ERROR && styles.statusTextError,
				)}
				role={resolveStatusRowRole(status)}
				data-flx="auth.flow.instance-selector.instance-selector-status-row.status-text"
			>
				{statusMessage}
			</flx-auth-instance-selector-status-text>
		</flx-auth-instance-selector-status-row>
	);
}
