// SPDX-License-Identifier: AGPL-3.0-or-later

import {isPasskeyBridgeAvailable} from '@app/features/auth/utils/PasskeyBridge';
import {Button} from '@app/features/ui/button/Button';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PasswordIcon} from '@phosphor-icons/react';
import type React from 'react';

const USE_A_PASSWORD_MANAGER_PASSKEY_DESCRIPTOR = msg({
	message: 'Use a password manager passkey',
	comment:
		'Button that runs the passkey prompt in a small pop-up window on the old web address, so password manager extensions can find passkeys saved there.',
});
const PASSWORD_MANAGER_PASSKEY_SUGGESTION_DESCRIPTOR = msg({
	message:
		'Password managers may not offer your passkey on this web address. Try it in a pop-up window on the old address instead.',
	comment:
		'Shown after a passkey prompt fails, above the button that retries the passkey prompt in a pop-up window on the old web address.',
});

interface PasswordManagerPasskeyActionProps {
	suggested: boolean;
	disabled?: boolean;
	onClick: (event: React.MouseEvent<HTMLButtonElement> | React.KeyboardEvent<HTMLButtonElement>) => void;
}

export function PasswordManagerPasskeyAction({suggested, disabled, onClick}: PasswordManagerPasskeyActionProps) {
	const {i18n} = useLingui();
	if (!isPasskeyBridgeAvailable()) {
		return null;
	}
	const button = (
		<Button
			type="button"
			fitContainer
			variant={suggested ? 'primary' : 'secondary'}
			onClick={onClick}
			disabled={disabled}
			leftIcon={<PasswordIcon size={16} data-flx="auth.password-manager-passkey-action.icon" />}
			data-flx="auth.password-manager-passkey-action.button"
		>
			{i18n._(USE_A_PASSWORD_MANAGER_PASSKEY_DESCRIPTOR)}
		</Button>
	);
	if (!suggested) {
		return button;
	}
	return (
		<WarningAlert actions={button} data-flx="auth.password-manager-passkey-action.suggestion">
			{i18n._(PASSWORD_MANAGER_PASSKEY_SUGGESTION_DESCRIPTOR)}
		</WarningAlert>
	);
}
