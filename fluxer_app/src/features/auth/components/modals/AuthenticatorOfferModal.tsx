// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {startPasskeyRegistration} from '@app/features/auth/utils/PasskeyRegistrationFlow';
import * as WebAuthnUtils from '@app/features/auth/utils/WebAuthnUtils';
import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import Users from '@app/features/user/state/Users';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const OFFER_DISMISSED_KEY_PREFIX = 'fluxer.authenticator_offer_dismissed';

function offerDismissedKey(userId: string): string {
	return `${OFFER_DISMISSED_KEY_PREFIX}:${userId}`;
}

const SIGN_IN_FASTER_DESCRIPTOR = msg({
	message: 'Sign in faster next time',
	comment: 'Title of the optional, dismissible offer to add a passkey.',
});
const SIGN_IN_FASTER_BODY_DESCRIPTOR = msg({
	message:
		'Add a passkey to sign in with your fingerprint, face or screen lock. You can also do this later in settings.',
	comment: 'Body of the optional, dismissible offer to add a passkey.',
});
const ADD_PASSKEY_DESCRIPTOR = msg({
	message: 'Add passkey',
	comment: 'Button in the optional passkey offer.',
});
const NOT_NOW_DESCRIPTOR = msg({
	message: 'Not now',
	comment: 'Button that dismisses the optional passkey offer.',
});

function rememberDismissed(userId: string): void {
	try {
		getProtectedLocalStorage()?.setItem(offerDismissedKey(userId), '1');
	} catch {}
}

function wasAuthenticatorOfferDismissed(userId: string): boolean {
	try {
		return getProtectedLocalStorage()?.getItem(offerDismissedKey(userId)) === '1';
	} catch {
		return true;
	}
}

const AuthenticatorOfferModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<ConfirmModal
			title={i18n._(SIGN_IN_FASTER_DESCRIPTOR)}
			description={i18n._(SIGN_IN_FASTER_BODY_DESCRIPTOR)}
			primaryText={i18n._(ADD_PASSKEY_DESCRIPTOR)}
			primaryVariant="primary"
			secondaryText={i18n._(NOT_NOW_DESCRIPTOR)}
			onPrimary={() => {
				void startPasskeyRegistration(i18n);
			}}
			data-flx="auth.authenticator-offer-modal.confirm-modal"
		/>
	);
});

export async function offerAuthenticatorCalmly(): Promise<void> {
	const user = Users.currentUser;
	if (
		user == null ||
		wasAuthenticatorOfferDismissed(user.id) ||
		user.authenticatorTypes?.includes(UserAuthenticatorTypes.WEBAUTHN)
	) {
		return;
	}
	if (!(await WebAuthnUtils.isWebAuthnSupported())) {
		return;
	}
	rememberDismissed(user.id);
	ModalCommands.push(modal(() => <AuthenticatorOfferModal data-flx="auth.authenticator-offer-modal" />));
}
