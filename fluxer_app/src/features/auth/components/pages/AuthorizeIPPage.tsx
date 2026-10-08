// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/components/pages/AuthorizeIPPage.module.css';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {useAuthTokenVerification} from '@app/features/auth/flow/useAuthTokenVerification';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {authRequestTargetFromSnapshot} from '@app/features/auth/state/AuthRequestTarget';
import {type VerificationError, VerificationErrorType} from '@app/features/auth/types/VerificationError';
import {Spinner} from '@app/features/ui/components/Spinner';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

const AUTHORIZE_IP_DESCRIPTOR = msg({
	message: 'Authorize IP',
	comment: 'Short label in the authentication authorize IP page. Keep the tone plain and specific.',
});
const renderErrorMessage = (error: VerificationError | null) => {
	if (!error) return null;
	switch (error.type) {
		case VerificationErrorType.LINK_EXPIRED:
			return <Trans>Link expired. Your IP address was likely already authorized.</Trans>;
		default:
			return <Trans>Something went wrong. Reload the page or sign in again.</Trans>;
	}
};

interface AuthorizeIPPageContentProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

const AuthorizeIPPageContent = observer(function AuthorizeIPPageContent({
	runtimeSnapshot,
}: AuthorizeIPPageContentProps) {
	const requestTarget = useMemo(() => authRequestTargetFromSnapshot(runtimeSnapshot), [runtimeSnapshot]);
	const verify = useCallback(
		(token: string) => AuthenticationCommands.authorizeIp(token, requestTarget),
		[requestTarget],
	);
	const {isLoading, isSuccess, error} = useAuthTokenVerification(verify);
	if (isLoading) {
		return (
			<div className={styles.container} data-flx="auth.authorize-ip-page.container">
				<div className={styles.iconContainer} data-flx="auth.authorize-ip-page.icon-container">
					<div className={styles.iconCircle} data-flx="auth.authorize-ip-page.icon-circle">
						<Spinner data-flx="auth.authorize-ip-page.spinner" />
					</div>
				</div>
				<div className={styles.loadingPlaceholder} data-flx="auth.authorize-ip-page.loading-placeholder" />
				<div className={styles.descriptionPlaceholder} data-flx="auth.authorize-ip-page.description-placeholder" />
			</div>
		);
	}
	return (
		<div className={styles.container} data-flx="auth.authorize-ip-page.container--2">
			<div className={styles.iconContainer} data-flx="auth.authorize-ip-page.icon-container--2">
				{isSuccess ? (
					<div
						className={clsx(styles.iconCircle, styles.iconCircleSuccess)}
						data-flx="auth.authorize-ip-page.icon-circle--2"
					>
						<CheckIcon className={styles.icon} weight="bold" data-flx="auth.authorize-ip-page.icon" />
					</div>
				) : (
					<div
						className={clsx(styles.iconCircle, styles.iconCircleError)}
						data-flx="auth.authorize-ip-page.icon-circle--3"
					>
						<XIcon className={styles.icon} weight="bold" data-flx="auth.authorize-ip-page.icon--2" />
					</div>
				)}
			</div>
			{isSuccess ? (
				<>
					<h1 className={styles.title} data-flx="auth.authorize-ip-page.title">
						<Trans>IP address authorized</Trans>
					</h1>
					<p className={styles.description} data-flx="auth.authorize-ip-page.description">
						<Trans>Your IP address has been successfully authorized. You can close this page or sign in again.</Trans>
					</p>
					<div className={styles.footer} data-flx="auth.authorize-ip-page.footer--2">
						<div data-flx="auth.authorize-ip-page.div--2">
							<AuthRouterLink to="/login" className={styles.link} data-flx="auth.authorize-ip-page.link--2">
								<Trans>Go to sign-in</Trans>
							</AuthRouterLink>
						</div>
					</div>
				</>
			) : (
				<>
					<h1 className={styles.title} data-flx="auth.authorize-ip-page.title--2">
						<Trans>Authorization failed</Trans>
					</h1>
					<p className={styles.description} data-flx="auth.authorize-ip-page.description--2">
						{renderErrorMessage(error)}
					</p>
					<div className={styles.footer} data-flx="auth.authorize-ip-page.footer">
						<div data-flx="auth.authorize-ip-page.div">
							<AuthRouterLink to="/login" className={styles.link} data-flx="auth.authorize-ip-page.link">
								<Trans>Go to sign-in</Trans>
							</AuthRouterLink>
						</div>
					</div>
				</>
			)}
		</div>
	);
});

const AuthorizeIPPage = observer(function AuthorizeIPPage() {
	const {i18n} = useLingui();
	useFluxerDocumentTitle(i18n._(AUTHORIZE_IP_DESCRIPTOR));
	useAuthPresentation({variant: AuthCardVariant.COMPACT});
	return (
		<AuthRuntimeTargetGate data-flx="auth.authorize-ip-page.runtime-target-gate">
			{(runtimeSnapshot) => (
				<AuthorizeIPPageContent runtimeSnapshot={runtimeSnapshot} data-flx="auth.authorize-ip-page.content" />
			)}
		</AuthRuntimeTargetGate>
	);
});

export default AuthorizeIPPage;
