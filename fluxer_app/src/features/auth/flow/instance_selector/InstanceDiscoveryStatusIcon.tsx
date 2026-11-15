// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/flow/InstanceSelector.module.css';
import {InstanceDiscoveryStatus} from '@app/features/auth/flow/instance_selector/InstanceSelectorTypes';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Spinner, SpinnerSize} from '@app/features/ui/components/Spinner';
import {WarningCircleIcon} from '@phosphor-icons/react';

interface InstanceDiscoveryStatusIconProps {
	readonly status: InstanceDiscoveryStatus;
}

export function InstanceDiscoveryStatusIcon({status}: InstanceDiscoveryStatusIconProps) {
	if (status === InstanceDiscoveryStatus.DISCOVERING) {
		return (
			<Spinner
				size={SpinnerSize.SMALL}
				className={styles.statusSpinner}
				data-flx="auth.flow.instance-selector.instance-discovery-status-icon.status-spinner"
			/>
		);
	}
	if (status === InstanceDiscoveryStatus.ERROR) {
		return (
			<WarningCircleIcon
				weight="fill"
				className={styles.statusError}
				size={remFromPx(18)}
				data-flx="auth.flow.instance-selector.instance-discovery-status-icon.status-error"
			/>
		);
	}
	return null;
}
