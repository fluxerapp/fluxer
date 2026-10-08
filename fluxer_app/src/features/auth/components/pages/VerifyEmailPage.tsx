// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/components/pages/VerifyEmailPage.module.css';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {useAuthTokenVerification} from '@app/features/auth/flow/useAuthTokenVerification';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {authRequestTargetFromSnapshot} from '@app/features/auth/state/AuthRequestTarget';
import {type VerificationError, VerificationErrorType} from '@app/features/auth/types/VerificationError';
import {VERIFY_EMAIL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Spinner} from '@app/features/ui/components/Spinner';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

const renderErrorMessage = (error: VerificationError | null) => {
	if (!error) return null;
	switch (error.type) {
		case VerificationErrorType.LINK_EXPIRED:
			return <Trans>Link expired. Your email was likely already verified.</Trans>;
		default:
			return <Trans>Something went wrong. Reload the page or request a new verification email.</Trans>;
	}
};

interface VerifyEmailPageContentProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

const VerifyEmailPageContent = observer(function VerifyEmailPageContent({
	runtimeSnapshot,
}: VerifyEmailPageContentProps) {
	const requestTarget = useMemo(() => authRequestTargetFromSnapshot(runtimeSnapshot), [runtimeSnapshot]);
	const verify = useCallback(
		(token: string) => AuthenticationCommands.verifyEmail(token, requestTarget),
		[requestTarget],
	);
	const {isLoading, isSuccess, error} = useAuthTokenVerification(verify);
	if (isLoading) {
		return (
			<div className={styles.container} data-flx="auth.verify-email-page.verify-page.container">
				<div className={styles.iconContainer} data-flx="auth.verify-email-page.verify-page.icon-container">
					<div className={styles.spinnerWrapper} data-flx="auth.verify-email-page.verify-page.spinner-wrapper">
						<Spinner data-flx="auth.verify-email-page.verify-page.spinner" />
					</div>
				</div>
				<div className={styles.loadingPlaceholder} data-flx="auth.verify-email-page.verify-page.loading-placeholder" />
				<div
					className={styles.descriptionPlaceholder}
					data-flx="auth.verify-email-page.verify-page.description-placeholder"
				/>
			</div>
		);
	}
	return (
		<div className={styles.container} data-flx="auth.verify-email-page.verify-page.container--2">
			<div className={styles.iconContainer} data-flx="auth.verify-email-page.verify-page.icon-container--2">
				<div
					className={clsx(styles.iconCircle, isSuccess ? styles.iconCircleSuccess : styles.iconCircleError)}
					data-flx="auth.verify-email-page.verify-page.icon-circle"
				>
					{isSuccess ? (
						<CheckIcon className={styles.icon} weight="bold" data-flx="auth.verify-email-page.verify-page.icon" />
					) : (
						<XIcon className={styles.icon} weight="bold" data-flx="auth.verify-email-page.verify-page.icon--2" />
					)}
				</div>
			</div>
			{isSuccess ? (
				<>
					<h1 className={styles.title} data-flx="auth.verify-email-page.verify-page.title">
						<Trans>Email verified successfully</Trans>
					</h1>
					<p className={styles.description} data-flx="auth.verify-email-page.verify-page.description">
						<Trans>Your email has been verified. You can now sign in to your account.</Trans>
					</p>
					<div className={styles.footer} data-flx="auth.verify-email-page.verify-page.footer">
						<AuthRouterLink to="/login" className={styles.link} data-flx="auth.verify-email-page.verify-page.link">
							<Trans>Go to sign-in</Trans>
						</AuthRouterLink>
					</div>
				</>
			) : (
				<>
					<h1 className={styles.title} data-flx="auth.verify-email-page.verify-page.title--2">
						<Trans>Verification failed</Trans>
					</h1>
					<p className={styles.description} data-flx="auth.verify-email-page.verify-page.description--2">
						{renderErrorMessage(error)}
					</p>
					<div className={styles.footer} data-flx="auth.verify-email-page.verify-page.footer--2">
						<div data-flx="auth.verify-email-page.verify-page.div">
							<AuthRouterLink to="/login" className={styles.link} data-flx="auth.verify-email-page.verify-page.link--2">
								<Trans>Go to sign-in</Trans>
							</AuthRouterLink>
						</div>
						<div data-flx="auth.verify-email-page.verify-page.div--2">
							<AuthRouterLink
								to="/register"
								className={styles.secondaryLink}
								data-flx="auth.verify-email-page.verify-page.secondary-link"
							>
								<Trans>Create new account</Trans>
							</AuthRouterLink>
						</div>
					</div>
				</>
			)}
		</div>
	);
});

const VerifyPage = observer(function VerifyPage() {
	const {i18n} = useLingui();
	useFluxerDocumentTitle(i18n._(VERIFY_EMAIL_DESCRIPTOR));
	useAuthPresentation({variant: AuthCardVariant.COMPACT});
	return (
		<AuthRuntimeTargetGate data-flx="auth.verify-email-page.runtime-target-gate">
			{(runtimeSnapshot) => (
				<VerifyEmailPageContent runtimeSnapshot={runtimeSnapshot} data-flx="auth.verify-email-page.content" />
			)}
		</AuthRuntimeTargetGate>
	);
});

export default VerifyPage;
