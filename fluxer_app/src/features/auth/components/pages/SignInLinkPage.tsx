// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/components/pages/LoginPage.module.css';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {useAuthSingleUseRequest} from '@app/features/auth/flow/AuthSingleUseRequest';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {toLoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {useAuthRuntimeTarget} from '@app/features/auth/state/AuthRuntimeTarget';
import {BACK_TO_SIGN_IN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {failureValidationErrors} from '@app/features/platform/utils/ResponseInspection';
import {Spinner} from '@app/features/ui/components/Spinner';
import * as FormUtils from '@app/lib/forms';
import {flxElementClassName} from '@app/lib/react';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const SIGN_IN_LINK_EXPIRED_DESCRIPTOR = msg({
	message: 'This sign-in link expired or was already used.',
	comment: 'Shown when someone opens a one-time sign-in link that no longer works.',
});

function isExpiredLink(error: unknown): boolean {
	return (
		failureValidationErrors(error)?.some(
			(fault) => fault.code === ValidationErrorCodes.INVALID_OR_EXPIRED_AUTHORIZATION_TOKEN,
		) === true
	);
}

interface SignInLinkContentProps {
	readonly token: string;
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
	readonly onBack: () => void;
}

const SignInLinkContent = observer(function SignInLinkContent({
	token,
	runtimeSnapshot,
	onBack,
}: SignInLinkContentProps) {
	const {i18n} = useLingui();
	const runtimeTarget = useAuthRuntimeTarget();
	const [error, setError] = useState<string | null>(null);
	useAuthSingleUseRequest(token, async (request) => {
		try {
			const response = await AuthenticationCommands.redeemSignInLink(
				token,
				instanceTargetFromSnapshot(runtimeSnapshot),
			);
			await AuthenticationCommands.completeLogin(
				{...toLoginSuccessPayload(response), runtimeSnapshot},
				{redirectPath: Routes.ME},
			);
			runtimeTarget.reset();
		} catch (failure) {
			if (AuthenticationCommands.showAccountRestriction(failure) || !request.isCurrent()) {
				return;
			}
			setError(
				isExpiredLink(failure) ? i18n._(SIGN_IN_LINK_EXPIRED_DESCRIPTOR) : FormUtils.extractErrorMessage(i18n, failure),
			);
		}
	});
	if (error != null) {
		return (
			<flx-auth-sign-in-link-page
				className={flxElementClassName(styles.loginContainer)}
				data-flx="auth.sign-in-link-page.login-container"
			>
				<h1 className={styles.title} data-flx="auth.sign-in-link-page.title">
					<Trans comment="Heading when a one-time sign-in link could not sign the person in.">
						Couldn't sign you in
					</Trans>
				</h1>
				<flx-auth-sign-in-link-page-notice
					aria-live="assertive"
					className={flxElementClassName(styles.loginNotice)}
					data-flx="auth.sign-in-link-page.login-notice"
				>
					{error}
				</flx-auth-sign-in-link-page-notice>
				<flx-auth-sign-in-link-page-actions
					className={flxElementClassName(styles.ssoCallbackActions)}
					data-flx="auth.sign-in-link-page.actions"
				>
					<button
						type="button"
						onClick={onBack}
						className={styles.ssoBackButton}
						data-flx="auth.sign-in-link-page.back-button"
					>
						{i18n._(BACK_TO_SIGN_IN_DESCRIPTOR)}
					</button>
				</flx-auth-sign-in-link-page-actions>
			</flx-auth-sign-in-link-page>
		);
	}
	return (
		<flx-auth-sign-in-link-page
			className={flxElementClassName(styles.loginContainer)}
			data-flx="auth.sign-in-link-page.login-container--2"
		>
			<h1 className={styles.title} data-flx="auth.sign-in-link-page.title--2">
				<Trans>Completing sign-in…</Trans>
			</h1>
			<p className={styles.ssoProcessingHint} role="status" data-flx="auth.sign-in-link-page.processing-hint">
				<Spinner data-flx="auth.sign-in-link-page.spinner" />
			</p>
		</flx-auth-sign-in-link-page>
	);
});

interface SignInLinkPageProps {
	readonly token: string;
	readonly onBack: () => void;
}

export const SignInLinkPage = observer(function SignInLinkPage({token, onBack}: SignInLinkPageProps) {
	useAuthPresentation({variant: AuthCardVariant.COMPACT});
	return (
		<AuthRuntimeTargetGate data-flx="auth.sign-in-link-page.runtime-target-gate">
			{(runtimeSnapshot) => (
				<SignInLinkContent
					token={token}
					runtimeSnapshot={runtimeSnapshot}
					onBack={onBack}
					data-flx="auth.sign-in-link-page.content"
				/>
			)}
		</AuthRuntimeTargetGate>
	);
});
