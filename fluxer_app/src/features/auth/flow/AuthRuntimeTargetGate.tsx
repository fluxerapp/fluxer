// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {AuthLoginInstanceStep} from '@app/features/auth/flow/auth_login_core/AuthLoginInstanceStep';
import {useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';

const CHOOSE_INSTANCE_DESCRIPTOR = msg({
	message: 'Choose your instance',
	comment: 'Heading for an authentication step that requires an instance before continuing.',
});

interface AuthRuntimeTargetGateProps {
	readonly children: (runtimeSnapshot: RuntimeConfigSnapshot) => ReactNode;
}

export const AuthRuntimeTargetGate = observer(function AuthRuntimeTargetGate({children}: AuthRuntimeTargetGateProps) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const runtimeSnapshot = runtimeTarget.snapshot;
	if (runtimeSnapshot != null) {
		return children(runtimeSnapshot);
	}
	return (
		<AuthLoginInstanceStep
			extraTopContent={null}
			title={i18n._(CHOOSE_INSTANCE_DESCRIPTOR)}
			initialInstanceUrl={null}
			onContinue={runtimeTarget.select}
			onBackActionChange={null}
			suppressInlineBackButton={false}
			data-flx="auth.flow.auth-runtime-target-gate.instance-step"
		/>
	);
});
