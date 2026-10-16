// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	SIGN_IN_WITH_A_PASSKEY_DESCRIPTOR,
	SIGN_IN_WITH_BROWSER_DESCRIPTOR,
} from '@app/features/auth/AuthMessageDescriptors';
import loginStyles from '@app/features/auth/components/pages/LoginPage.module.css';
import {
	type AuthLoginMethodAction,
	AuthLoginMethodPicker,
} from '@app/features/auth/flow/auth_login_core/AuthLoginMethodPicker';
import styles from '@app/features/auth/flow/auth_login_core/AuthLoginMethodStep.module.css';
import type {LoginIdentifierField} from '@app/features/auth/utils/AccountSignInIdentifier';
import {CHANGE_INSTANCE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button, ButtonVariant} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, BrowserIcon, EnvelopeSimpleIcon, KeyIcon, UserIcon, UserPlusIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {cloneElement, type ReactElement, type ReactNode} from 'react';

const SIGN_IN_WITH_EMAIL_DESCRIPTOR = msg({
	message: 'Sign in with email',
	comment: 'Button label on the authentication method picker.',
});
const SIGN_IN_WITH_USERNAME_DESCRIPTOR = msg({
	message: 'Sign in with username',
	comment: 'Button label on the authentication method picker when the instance signs people in with a username.',
});
const REGISTER_ACCOUNT_DESCRIPTOR = msg({
	message: 'Register an account',
	comment: 'Link label on the authentication method picker.',
});

export interface AuthLoginSsoAction {
	readonly isStarting: boolean;
	readonly label: ReactNode;
	readonly onStart: () => void;
}

export function resolveShowPasskeyAction(showPasskeyOption: boolean, passkeyAvailableForInstance: boolean): boolean {
	return showPasskeyOption && passkeyAvailableForInstance;
}

interface AuthLoginMethodStepProps {
	readonly disabled: boolean;
	readonly extraTopContent: ReactNode;
	readonly identifierField: LoginIdentifierField;
	readonly leadingAction: ReactNode;
	readonly passkeyAvailableForInstance: boolean;
	readonly registerLink: ReactElement<Record<string, unknown>>;
	readonly showBrowserOption: boolean;
	readonly showPasskeyOption: boolean;
	readonly showTitle: boolean;
	readonly ssoAction: AuthLoginSsoAction | null;
	readonly switchError: string | null;
	readonly title: ReactNode;
	readonly onBrowserLogin: () => void;
	readonly onEmailLogin: () => void;
	readonly onPasskeyLogin: () => void;
	readonly onBackToInstance: (() => void) | null;
}

export function AuthLoginMethodStep({
	disabled,
	extraTopContent,
	identifierField,
	leadingAction,
	passkeyAvailableForInstance,
	registerLink,
	showBrowserOption,
	showPasskeyOption,
	showTitle,
	ssoAction,
	switchError,
	title,
	onBrowserLogin,
	onEmailLogin,
	onPasskeyLogin,
	onBackToInstance,
}: AuthLoginMethodStepProps) {
	const {i18n} = useLingui();
	const registerLinkProps = registerLink.props;
	const registerLinkClassNameValue = registerLinkProps['className'];
	let registerLinkClassName: string | null = null;
	if (typeof registerLinkClassNameValue === 'string') {
		registerLinkClassName = registerLinkClassNameValue;
	}
	const methodRegisterLink = cloneElement(registerLink, {
		className: clsx(styles.methodChoiceLink, registerLinkClassName),
		children: (
			<>
				<UserPlusIcon size={remFromPx(16)} data-flx="auth.flow.auth-login-core.auth-login-method-step.user-plus-icon" />
				<span data-flx="auth.flow.auth-login-core.auth-login-method-step.span">
					{i18n._(REGISTER_ACCOUNT_DESCRIPTOR)}
				</span>
			</>
		),
	});
	const secondaryActions: Array<AuthLoginMethodAction> = [];
	if (resolveShowPasskeyAction(showPasskeyOption, passkeyAvailableForInstance)) {
		secondaryActions.push({
			disabled,
			id: 'passkey',
			label: i18n._(SIGN_IN_WITH_A_PASSKEY_DESCRIPTOR),
			icon: <KeyIcon size={remFromPx(16)} data-flx="auth.flow.auth-login-core.auth-login-method-step.key-icon" />,
			variant: ButtonVariant.SECONDARY,
			onSelect: onPasskeyLogin,
			stepFocus: false,
			submitting: false,
		});
	}
	if (showBrowserOption) {
		secondaryActions.push({
			disabled,
			id: 'browser',
			label: i18n._(SIGN_IN_WITH_BROWSER_DESCRIPTOR),
			icon: (
				<BrowserIcon
					size={remFromPx(16)}
					data-flx="auth.flow.auth-login-core.auth-login-method-step.browser-sign-in-icon"
				/>
			),
			variant: ButtonVariant.SECONDARY,
			onSelect: onBrowserLogin,
			stepFocus: false,
			submitting: false,
		});
	}
	if (ssoAction != null) {
		secondaryActions.push({
			disabled,
			id: 'sso',
			label: ssoAction.label,
			icon: (
				<BrowserIcon size={remFromPx(16)} data-flx="auth.flow.auth-login-core.auth-login-method-step.browser-icon" />
			),
			variant: ButtonVariant.SECONDARY,
			onSelect: ssoAction.onStart,
			stepFocus: false,
			submitting: ssoAction.isStarting,
		});
	}
	const renderSwitchError = () => {
		if (switchError == null || switchError.length === 0) {
			return null;
		}
		return (
			<div
				className={loginStyles.loginNotice}
				role="alert"
				data-flx="auth.flow.auth-login-core.auth-login-method-step.render-switch-error.alert"
			>
				{switchError}
			</div>
		);
	};
	const renderFooterAction = () => {
		if (onBackToInstance == null) {
			return null;
		}
		return (
			<Button
				type="button"
				variant={ButtonVariant.SECONDARY}
				leftIcon={
					<ArrowLeftIcon
						size={remFromPx(16)}
						weight="bold"
						data-flx="auth.flow.auth-login-core.auth-login-method-step.render-footer-action.arrow-left-icon"
					/>
				}
				onClick={onBackToInstance}
				disabled={disabled}
				data-flx="auth.flow.auth-login-core.auth-login-method-step.button.back-to-instance"
			>
				{i18n._(CHANGE_INSTANCE_DESCRIPTOR)}
			</Button>
		);
	};
	return (
		<flx-auth-login-method-step
			className="flx-element"
			data-flx="auth.flow.auth-login-core.auth-login-method-step.flx-element"
		>
			{extraTopContent}
			{showTitle ? (
				<h1 className={loginStyles.title} data-flx="auth.flow.auth-login-core.auth-login-method-step.h1">
					{title}
				</h1>
			) : null}
			{renderSwitchError()}
			{leadingAction}
			<AuthLoginMethodPicker
				primaryAction={{
					disabled,
					id: 'email',
					label: i18n._(identifierField === 'login' ? SIGN_IN_WITH_USERNAME_DESCRIPTOR : SIGN_IN_WITH_EMAIL_DESCRIPTOR),
					icon:
						identifierField === 'login' ? (
							<UserIcon size={remFromPx(16)} data-flx="auth.flow.auth-login-core.auth-login-method-step.user-icon" />
						) : (
							<EnvelopeSimpleIcon
								size={remFromPx(16)}
								data-flx="auth.flow.auth-login-core.auth-login-method-step.envelope-simple-icon"
							/>
						),
					onSelect: onEmailLogin,
					stepFocus: true,
					submitting: false,
					variant: ButtonVariant.PRIMARY,
				}}
				secondaryActions={secondaryActions}
				registerAction={methodRegisterLink}
				footerAction={renderFooterAction()}
				data-flx="auth.flow.auth-login-core.auth-login-method-step.auth-login-method-picker"
			/>
		</flx-auth-login-method-step>
	);
}
