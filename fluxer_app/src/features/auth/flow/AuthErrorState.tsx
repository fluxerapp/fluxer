// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/flow/AuthPageStyles.module.css';
import type {Icon} from '@phosphor-icons/react';
import {QuestionIcon} from '@phosphor-icons/react';

interface AuthErrorStateProps {
	icon?: Icon;
	title: React.ReactNode;
	text: React.ReactNode;
	action?: React.ReactNode;
}

export function AuthErrorState({icon: IconComponent = QuestionIcon, title, text, action}: AuthErrorStateProps) {
	return (
		<div className={styles.errorContainer} data-flx="auth.flow.auth-error-state.error-container">
			<div className={styles.errorIcon} data-flx="auth.flow.auth-error-state.error-icon">
				<IconComponent className={styles.errorIconSvg} data-flx="auth.flow.auth-error-state.error-icon-svg" />
			</div>
			<h1 className={styles.errorTitle} data-flx="auth.flow.auth-error-state.error-title">
				{title}
			</h1>
			<p className={styles.errorText} data-flx="auth.flow.auth-error-state.error-text">
				{text}
			</p>
			{action}
		</div>
	);
}
