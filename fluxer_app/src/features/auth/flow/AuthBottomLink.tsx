// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {AuthRouterLink} from '@app/features/auth/flow/AuthRouterLink';
import {
	ALREADY_HAVE_ACCOUNT_DESCRIPTOR,
	NEED_ACCOUNT_DESCRIPTOR,
	REGISTER_DESCRIPTOR,
	SIGN_IN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useLingui} from '@lingui/react/macro';

interface AuthBottomLinkProps {
	variant: 'login' | 'register';
	to: string;
}

export function AuthBottomLink({variant, to}: AuthBottomLinkProps) {
	const {i18n} = useLingui();
	return (
		<div className={styles.bottomLink} data-flx="auth.flow.auth-bottom-link.bottom-link">
			<span className={styles.bottomLinkText} data-flx="auth.flow.auth-bottom-link.bottom-link-text">
				{`${variant === 'login' ? i18n._(ALREADY_HAVE_ACCOUNT_DESCRIPTOR) : i18n._(NEED_ACCOUNT_DESCRIPTOR)} `}
			</span>
			<AuthRouterLink
				to={to}
				className={styles.bottomLinkAnchor}
				data-flx="auth.flow.auth-bottom-link.bottom-link-anchor"
			>
				{variant === 'login' ? i18n._(SIGN_IN_DESCRIPTOR) : i18n._(REGISTER_DESCRIPTOR)}
			</AuthRouterLink>
		</div>
	);
}

interface AuthBottomLinksProps {
	children: React.ReactNode;
}

export function AuthBottomLinks({children}: AuthBottomLinksProps) {
	return (
		<div className={styles.bottomLinks} data-flx="auth.flow.auth-bottom-link.auth-bottom-links.bottom-links">
			{children}
		</div>
	);
}
