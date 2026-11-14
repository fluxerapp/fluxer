// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import styles from '@app/features/auth/components/pages/ForgotPasswordPage.module.css';
import {resolveForgotPasswordRedirect} from '@app/features/auth/flow/AccountRecoveryRedirects';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {AuthRuntimeTargetGate} from '@app/features/auth/flow/AuthRuntimeTargetGate';
import {ForgotPasswordForm} from '@app/features/auth/flow/ForgotPasswordForm';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {BACK_TO_SIGN_IN_DESCRIPTOR, REGISTER_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';

const FORGOT_PASSWORD_DESCRIPTOR = msg({
	message: 'Forgot password',
	comment: 'Short label in the authentication forgot password page. Keep the tone plain and specific.',
});
interface ForgotPasswordPageContentProps {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

const ForgotPasswordPageContent = observer(function ForgotPasswordPageContent({
	runtimeSnapshot,
}: ForgotPasswordPageContentProps) {
	const {i18n} = useLingui();
	const redirect = resolveForgotPasswordRedirect(runtimeSnapshot);
	useEffect(() => {
		if (redirect !== null) RouterUtils.replaceWith(redirect);
	}, [redirect]);
	if (redirect !== null) return null;
	return (
		<ForgotPasswordForm
			runtimeSnapshot={runtimeSnapshot}
			sentFooter={
				<div className={styles.footer} data-flx="auth.forgot-password-page.footer">
					<AuthRouterLink to="/login" className={styles.primaryLink} data-flx="auth.forgot-password-page.primary-link">
						<Trans>Return to sign-in</Trans>
					</AuthRouterLink>
				</div>
			}
			formFooter={
				<div className={styles.footer} data-flx="auth.forgot-password-page.footer--2">
					<div data-flx="auth.forgot-password-page.div">
						<AuthRouterLink to="/login" className={styles.link} data-flx="auth.forgot-password-page.link">
							{i18n._(BACK_TO_SIGN_IN_DESCRIPTOR)}
						</AuthRouterLink>
					</div>
					<div data-flx="auth.forgot-password-page.div--2">
						<span className={styles.footerLabel} data-flx="auth.forgot-password-page.footer-label">
							<Trans>Don't have an account?</Trans>{' '}
						</span>
						<AuthRouterLink
							to="/register"
							className={styles.primaryLink}
							data-flx="auth.forgot-password-page.primary-link--2"
						>
							{i18n._(REGISTER_DESCRIPTOR)}
						</AuthRouterLink>
					</div>
				</div>
			}
			data-flx="auth.forgot-password-page.forgot-password-page-content.forgot-password-form"
		/>
	);
});

const ForgotPasswordPage = observer(function ForgotPasswordPage() {
	const {i18n} = useLingui();
	useFluxerDocumentTitle(i18n._(FORGOT_PASSWORD_DESCRIPTOR));
	useAuthPresentation({variant: AuthCardVariant.STANDARD});
	return (
		<AuthRuntimeTargetGate data-flx="auth.forgot-password-page.runtime-target-gate">
			{(runtimeSnapshot) => (
				<ForgotPasswordPageContent runtimeSnapshot={runtimeSnapshot} data-flx="auth.forgot-password-page.content" />
			)}
		</AuthRuntimeTargetGate>
	);
});

export default ForgotPasswordPage;
