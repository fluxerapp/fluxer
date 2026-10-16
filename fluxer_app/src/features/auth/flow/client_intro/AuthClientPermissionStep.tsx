// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/flow/client_intro/AuthClientPreferencesStep.module.css';
import {MacPermissionPrompt} from '@app/features/permissions/system/components/MacPermissionPrompt';
import type {MacPermissionKind} from '@app/features/permissions/system/state/MacPermissions';
import {flxElementClassName} from '@app/lib/react';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import {useId} from 'react';

const PERMISSION_PROGRESS_DESCRIPTOR = msg({
	message: 'Step {current} of {total}',
	comment:
		'Screen reader text for the progress dots under a macOS permission screen in the desktop first-run setup. {current} and {total} are numbers.',
});
interface AuthClientPermissionStepProps {
	readonly kind: MacPermissionKind;
	readonly index: number;
	readonly count: number;
	readonly onAllowed: () => void;
	readonly onNotNow: () => void;
}

export function AuthClientPermissionStep({kind, index, count, onAllowed, onNotNow}: AuthClientPermissionStepProps) {
	const {i18n} = useLingui();
	const titleId = useId();
	return (
		<section
			className={styles.clientPermission}
			aria-labelledby={titleId}
			data-flx="auth.flow.client-intro.auth-client-permission-step.section"
		>
			<MacPermissionPrompt
				kind={kind}
				titleId={titleId}
				onAllowed={onAllowed}
				onNotNow={onNotNow}
				data-flx="auth.flow.client-intro.auth-client-permission-step.mac-permission-prompt"
			/>
			<flx-auth-client-permission-step-progress
				role="img"
				aria-label={i18n._(PERMISSION_PROGRESS_DESCRIPTOR, {current: index + 1, total: count})}
				className={flxElementClassName(styles.clientPermissionProgress)}
				data-flx="auth.flow.client-intro.auth-client-permission-step.progress"
			>
				{Array.from({length: count}, (_, dot) => (
					<flx-auth-client-permission-step-dot
						key={dot}
						className={flxElementClassName(
							clsx(styles.clientPermissionDot, dot === index && styles.clientPermissionDotCurrent),
						)}
						data-flx="auth.flow.client-intro.auth-client-permission-step.progress-dot"
					/>
				))}
			</flx-auth-client-permission-step-progress>
		</section>
	);
}
