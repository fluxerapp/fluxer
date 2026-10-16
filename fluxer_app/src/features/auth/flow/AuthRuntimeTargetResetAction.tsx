// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';

const CHOOSE_A_DIFFERENT_INSTANCE_DESCRIPTOR = msg({
	message: 'Choose a different instance',
	comment: 'Button on a short link error screen that returns to the instance chooser.',
});

export function AuthRuntimeTargetResetAction() {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	if (runtimeTarget.initialSnapshot != null) {
		return null;
	}
	return (
		<Button
			type="button"
			variant="secondary"
			onClick={runtimeTarget.reset}
			data-flx="auth.flow.auth-runtime-target-reset-action.button.reset"
		>
			{i18n._(CHOOSE_A_DIFFERENT_INSTANCE_DESCRIPTOR)}
		</Button>
	);
}
