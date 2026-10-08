// SPDX-License-Identifier: AGPL-3.0-or-later

import {isAuthRenderableNode} from '@app/features/auth/AuthRenderableNode';
import styles from '@app/features/auth/flow/auth_login_core/AuthLoginMethodPicker.module.css';
import {Button, type ButtonVariant} from '@app/features/ui/button/Button';
import {flxElementClassName} from '@app/lib/react';
import type {ReactElement, ReactNode} from 'react';

export interface AuthLoginMethodAction {
	readonly disabled: boolean;
	readonly icon: ReactNode;
	readonly id: string;
	readonly label: ReactNode;
	readonly onSelect: () => void;
	readonly stepFocus: boolean;
	readonly submitting: boolean;
	readonly variant: ButtonVariant;
}

interface AuthLoginMethodPickerProps {
	readonly footerAction: ReactNode;
	readonly primaryAction: AuthLoginMethodAction;
	readonly registerAction: ReactElement;
	readonly secondaryActions: ReadonlyArray<AuthLoginMethodAction>;
}

interface AuthLoginMethodButtonProps {
	readonly action: AuthLoginMethodAction;
}

function AuthLoginMethodButton({action}: AuthLoginMethodButtonProps) {
	if (action.stepFocus) {
		return (
			<Button
				type="button"
				variant={action.variant}
				fitContainer
				leftIcon={action.icon}
				onClick={action.onSelect}
				disabled={action.disabled}
				submitting={action.submitting}
				data-step-focus="true"
				data-flx={`auth.flow.auth-login-core.auth-login-method-picker.button.${action.id}`}
			>
				{action.label}
			</Button>
		);
	}
	return (
		<Button
			type="button"
			variant={action.variant}
			fitContainer
			leftIcon={action.icon}
			onClick={action.onSelect}
			disabled={action.disabled}
			submitting={action.submitting}
			data-flx={`auth.flow.auth-login-core.auth-login-method-picker.button.${action.id}`}
		>
			{action.label}
		</Button>
	);
}

export function AuthLoginMethodPicker({
	footerAction,
	primaryAction,
	registerAction,
	secondaryActions,
}: AuthLoginMethodPickerProps) {
	const hasFooterAction = isAuthRenderableNode(footerAction);
	const renderSecondaryActions = () => {
		if (secondaryActions.length === 0) {
			return null;
		}
		return (
			<flx-auth-login-method-picker-secondary-grid
				className={flxElementClassName(styles.methodSecondaryGrid)}
				data-flx="auth.flow.auth-login-core.auth-login-method-picker.render-secondary-actions.method-secondary-grid"
			>
				{secondaryActions.map((action) => (
					<AuthLoginMethodButton
						key={action.id}
						action={action}
						data-flx="auth.flow.auth-login-core.auth-login-method-picker.render-secondary-actions.auth-login-method-button"
					/>
				))}
			</flx-auth-login-method-picker-secondary-grid>
		);
	};
	const renderFooter = () => {
		if (!hasFooterAction) {
			return null;
		}
		return (
			<flx-auth-login-method-picker-footer
				className={flxElementClassName(styles.methodFooter)}
				data-flx="auth.flow.auth-login-core.auth-login-method-picker.render-footer.method-footer"
			>
				{footerAction}
			</flx-auth-login-method-picker-footer>
		);
	};
	return (
		<flx-auth-login-method-picker
			className={flxElementClassName(styles.methodList)}
			data-flx="auth.flow.auth-login-core.auth-login-method-picker.method-list"
		>
			<flx-auth-login-method-picker-primary
				className={flxElementClassName(styles.methodPrimary)}
				data-flx="auth.flow.auth-login-core.auth-login-method-picker.method-primary"
			>
				<AuthLoginMethodButton
					action={primaryAction}
					data-flx="auth.flow.auth-login-core.auth-login-method-picker.auth-login-method-button"
				/>
			</flx-auth-login-method-picker-primary>
			{renderSecondaryActions()}
			{registerAction}
			{renderFooter()}
		</flx-auth-login-method-picker>
	);
}
