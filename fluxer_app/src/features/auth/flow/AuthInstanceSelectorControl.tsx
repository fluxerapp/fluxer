// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {type InstanceDiscoveredEvent, InstanceSelector} from '@app/features/auth/flow/InstanceSelector';
import {InstanceDiscoveryStatus} from '@app/features/auth/flow/instance_selector/InstanceSelectorTypes';
import {flxElementClassName} from '@app/lib/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useState} from 'react';

interface AuthInstanceSelectorControlProps {
	readonly className: string | null;
	readonly initialInstanceUrl: string | null;
	readonly onInstanceDiscovered: ((snapshot: RuntimeConfigSnapshot) => void) | null;
	readonly onBackActionChange: ((action: (() => void) | null) => void) | null;
	readonly suppressInlineBackButton: boolean;
}

export const AuthInstanceSelectorControl = observer(function AuthInstanceSelectorControl({
	className,
	initialInstanceUrl,
	onInstanceDiscovered,
	onBackActionChange,
	suppressInlineBackButton,
}: AuthInstanceSelectorControlProps) {
	const sourceInstanceUrl = initialInstanceUrl ?? '';
	const [instanceUrl, setInstanceUrl] = useState(sourceInstanceUrl);
	const [discoveryStatus, setDiscoveryStatus] = useState<InstanceDiscoveryStatus>(InstanceDiscoveryStatus.IDLE);
	const [appliedSourceInstanceUrl, setAppliedSourceInstanceUrl] = useState(sourceInstanceUrl);
	if (appliedSourceInstanceUrl !== sourceInstanceUrl && discoveryStatus !== InstanceDiscoveryStatus.DISCOVERING) {
		setAppliedSourceInstanceUrl(sourceInstanceUrl);
		setInstanceUrl(sourceInstanceUrl);
	}
	const handleInstanceDiscovered = useCallback(
		({domain, snapshot}: InstanceDiscoveredEvent) => {
			if (onInstanceDiscovered == null) {
				setInstanceUrl(domain);
				return;
			}
			onInstanceDiscovered(snapshot);
		},
		[onInstanceDiscovered],
	);
	return (
		<flx-auth-instance-selector-control
			className={flxElementClassName(className)}
			data-flx="auth.flow.auth-instance-selector-control.flx-auth-instance-selector-control"
		>
			<InstanceSelector
				value={instanceUrl}
				onChange={setInstanceUrl}
				onInstanceDiscovered={handleInstanceDiscovered}
				onDiscoveryStatusChange={setDiscoveryStatus}
				onBackActionChange={onBackActionChange}
				className={null}
				disabled={false}
				suppressInlineBackButton={suppressInlineBackButton}
				data-flx="auth.flow.auth-instance-selector-control.instance-selector.set-instance-url"
			/>
		</flx-auth-instance-selector-control>
	);
});
