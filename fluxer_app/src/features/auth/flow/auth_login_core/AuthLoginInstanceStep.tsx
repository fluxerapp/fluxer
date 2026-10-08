// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import loginStyles from '@app/features/auth/components/pages/LoginPage.module.css';
import {AuthInstanceSelectorControl} from '@app/features/auth/flow/AuthInstanceSelectorControl';
import styles from '@app/features/auth/flow/auth_login_core/AuthLoginInstanceStep.module.css';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {flxElementClassName} from '@app/lib/react';
import type {ReactNode} from 'react';

export function resolveShouldShowInstanceSelector(shouldShowInstanceSelector: boolean | null | undefined): boolean {
	return shouldShowInstanceSelector ?? hasDesktopAppStore();
}

export function shouldRenderInstanceStep(shouldShowInstanceSelector: boolean | null | undefined): boolean {
	return hasDesktopAppStore() && resolveShouldShowInstanceSelector(shouldShowInstanceSelector);
}

function hasDesktopAppStore(): boolean {
	return getElectronAPI()?.capabilities?.appStore === true;
}

interface AuthLoginInstanceStepProps {
	readonly extraTopContent: ReactNode;
	readonly title: ReactNode;
	readonly initialInstanceUrl: string | null;
	readonly onContinue: (snapshot: RuntimeConfigSnapshot) => void;
	readonly onBackActionChange: ((action: (() => void) | null) => void) | null;
	readonly suppressInlineBackButton: boolean;
}

export function AuthLoginInstanceStep({
	extraTopContent,
	title,
	initialInstanceUrl,
	onContinue,
	onBackActionChange,
	suppressInlineBackButton,
}: AuthLoginInstanceStepProps) {
	return (
		<flx-auth-login-instance-step
			className="flx-element"
			data-flx="auth.flow.auth-login-core.auth-login-instance-step.flx-element"
		>
			{extraTopContent}
			<h1 className={loginStyles.title} data-flx="auth.flow.auth-login-core.auth-login-instance-step.h1">
				{title}
			</h1>
			<flx-auth-login-instance-step-body
				className={flxElementClassName(styles.inlineStep)}
				data-flx="auth.flow.auth-login-core.auth-login-instance-step.inline-step"
			>
				<AuthInstanceSelectorControl
					className={styles.instanceSection}
					initialInstanceUrl={initialInstanceUrl}
					onInstanceDiscovered={onContinue}
					onBackActionChange={onBackActionChange}
					suppressInlineBackButton={suppressInlineBackButton}
					data-flx="auth.flow.auth-login-core.auth-login-instance-step.instance-section"
				/>
			</flx-auth-login-instance-step-body>
		</flx-auth-login-instance-step>
	);
}
