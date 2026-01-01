/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans} from '@lingui/react/macro';
import styles from './AuthPageStyles.module.css';
import {AuthRouterLink} from './AuthRouterLink';

interface AuthBottomLinkProps {
	variant: 'login' | 'register';
	to: string;
}

export function AuthBottomLink({variant, to}: AuthBottomLinkProps) {
	return (
		<div className={styles.bottomLink}>
			<span className={styles.bottomLinkText}>
				{variant === 'login' ? <Trans>Already have an account?</Trans> : <Trans>Need an account?</Trans>}{' '}
			</span>
			<AuthRouterLink to={to} className={styles.bottomLinkAnchor}>
				{variant === 'login' ? <Trans>Log in</Trans> : <Trans>Register</Trans>}
			</AuthRouterLink>
		</div>
	);
}

interface AuthBottomLinksProps {
	children: React.ReactNode;
}

export function AuthBottomLinks({children}: AuthBottomLinksProps) {
	return <div className={styles.bottomLinks}>{children}</div>;
}
