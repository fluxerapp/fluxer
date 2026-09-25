// SPDX-License-Identifier: AGPL-3.0-or-later

import {Button} from '@app/features/ui/button/Button';
import {Trans} from '@lingui/react/macro';
import {BrowserIcon, KeyIcon} from '@phosphor-icons/react';

interface AuthLoginDividerClasses {
	divider: string;
	dividerLine: string;
	dividerText: string;
}

export function AuthLoginDivider({
	classes,
	label = <Trans>or</Trans>,
}: {
	classes: AuthLoginDividerClasses;
	label?: React.ReactNode;
}) {
	return (
		<div
			className={classes.divider}
			data-flx="auth.flow.auth-login-core.auth-login-passkey-actions.auth-login-divider.div"
		>
			<div
				className={classes.dividerLine}
				data-flx="auth.flow.auth-login-core.auth-login-passkey-actions.auth-login-divider.div--2"
			/>
			<span
				className={classes.dividerText}
				data-flx="auth.flow.auth-login-core.auth-login-passkey-actions.auth-login-divider.span"
			>
				{label}
			</span>
			<div
				className={classes.dividerLine}
				data-flx="auth.flow.auth-login-core.auth-login-passkey-actions.auth-login-divider.div--3"
			/>
		</div>
	);
}

export interface AuthPasskeyClasses {
	wrapper?: string;
}

interface Props {
	classes?: AuthPasskeyClasses;
	disabled: boolean;
	onPasskeyLogin: () => void;
	showBrowserOption: boolean;
	onBrowserLogin?: () => void;
	primaryLabel?: React.ReactNode;
	browserLabel?: React.ReactNode;
}

export default function AuthLoginPasskeyActions({
	classes,
	disabled,
	onPasskeyLogin,
	showBrowserOption,
	onBrowserLogin,
	primaryLabel = <Trans>Sign in with a passkey</Trans>,
	browserLabel = <Trans>Sign in via browser</Trans>,
}: Props) {
	return (
		<div className={classes?.wrapper} data-flx="auth.flow.auth-login-core.auth-login-passkey-actions.div">
			<Button
				type="button"
				fitContainer
				variant="secondary"
				onClick={onPasskeyLogin}
				disabled={disabled}
				leftIcon={<KeyIcon size={16} data-flx="auth.flow.auth-login-core.auth-login-passkey-actions.key-icon" />}
				data-flx="auth.flow.auth-login-core.auth-login-passkey-actions.button.passkey-login"
			>
				{primaryLabel}
			</Button>
			{showBrowserOption && onBrowserLogin ? (
				<Button
					type="button"
					fitContainer
					variant="secondary"
					onClick={onBrowserLogin}
					disabled={disabled}
					leftIcon={
						<BrowserIcon size={16} data-flx="auth.flow.auth-login-core.auth-login-passkey-actions.browser-icon" />
					}
					data-flx="auth.flow.auth-login-core.auth-login-passkey-actions.button.browser-login"
				>
					{browserLabel}
				</Button>
			) : null}
		</div>
	);
}
