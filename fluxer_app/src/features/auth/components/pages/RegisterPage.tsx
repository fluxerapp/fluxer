// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuthBottomLink} from '@app/features/auth/flow/AuthBottomLink';
import sharedStyles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRegisterFormCore} from '@app/features/auth/flow/AuthRegisterFormCore';
import {AuthSsoPanel, isRuntimeSsoEnforced} from '@app/features/auth/flow/AuthSsoPanel';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {REGISTER_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const RegisterPageContent = observer(function RegisterPageContent() {
	const location = useLocation();
	const params = new URLSearchParams(location.search);
	const rawRedirect = params['get']('redirect_to');
	const safeRedirect = safeRedirectTarget(rawRedirect);
	const redirectTo = safeRedirect ?? '/';
	const loginPath = safeRedirect ? setPathQueryParams('/login', {redirect_to: safeRedirect}) : '/login';
	if (isRuntimeSsoEnforced()) {
		return (
			<div className={sharedStyles.container} data-flx="auth.register-page.register-page-content.sso-container">
				<AuthSsoPanel
					redirectPath={redirectTo}
					dataFlx="auth.register-page.register-page-content.sso-panel"
					data-flx="auth.register-page.register-page-content.auth-sso-panel"
				/>
			</div>
		);
	}
	return (
		<>
			<h1 className={sharedStyles.title} data-flx="auth.register-page.register-page-content.h1">
				<Trans>Create an account</Trans>
			</h1>
			<div className={sharedStyles.container} data-flx="auth.register-page.register-page-content.div">
				<AuthRegisterFormCore
					fields={{
						showEmail: true,
						showPassword: true,
						showPasswordConfirmation: true,
						showUsernameValidation: true,
					}}
					submitLabel={<Trans>Create account</Trans>}
					redirectPath={redirectTo}
					data-flx="auth.register-page.register-page-content.auth-register-form-core"
				/>
				<AuthBottomLink
					variant="login"
					to={loginPath}
					data-flx="auth.register-page.register-page-content.auth-bottom-link"
				/>
			</div>
		</>
	);
});
const RegisterPage = observer(function RegisterPage() {
	const {i18n} = useLingui();
	useFluxerDocumentTitle(i18n._(REGISTER_DESCRIPTOR));
	return <RegisterPageContent data-flx="auth.register-page.register-page-content" />;
});

export default RegisterPage;
