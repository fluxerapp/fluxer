// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/ErrorFallback.module.css';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {
	type PasskeyBridgeSession,
	type PasskeyBridgeViewState,
	startPasskeyBridgeSession,
} from '@app/features/auth/passkey_bridge/PasskeyBridgeSession';
import {CONTINUE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {FluxerIcon} from '@app/features/ui/components/icons/FluxerIcon';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const USE_YOUR_PASSKEY_DESCRIPTOR = msg({
	message: 'Use your passkey',
	comment: 'Heading of the small pop-up window that runs a passkey prompt for the new web address.',
});
const CONTINUE_TO_SIGN_IN_DESCRIPTOR = msg({
	message: 'Continue with your passkey to sign in to {host}',
	comment:
		'Body of the passkey pop-up window when signing in or confirming identity. host is a web address such as fluxer.com.',
});
const CONTINUE_TO_CREATE_DESCRIPTOR = msg({
	message: 'Continue to create a passkey for {host}',
	comment: 'Body of the passkey pop-up window when adding a new passkey. host is a web address such as fluxer.com.',
});
const CLOSE_WINDOW_DESCRIPTOR = msg({
	message: 'Close window',
	comment: 'Button in the passkey pop-up window that closes it after an error.',
});
const BRIDGE_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Open this window from {productName} to use your passkey.',
	comment: 'Error in the passkey pop-up window when someone opens its address directly. productName is the app name.',
});
const BRIDGE_REJECTED_DESCRIPTOR = msg({
	message: 'This passkey request could not be verified. Close this window and try again.',
	comment: 'Error in the passkey pop-up window when the request it received is not valid.',
});
const BRIDGE_TIMED_OUT_DESCRIPTOR = msg({
	message: 'This passkey request timed out. Close this window and try again.',
	comment: 'Error in the passkey pop-up window after five minutes without finishing.',
});

interface PasskeyBridgeScreenProps {
	openerOrigin: string;
}

export const PasskeyBridgeScreen: React.FC<PasskeyBridgeScreenProps> = ({openerOrigin}) => {
	const {i18n} = useLingui();
	const [state, setState] = useState<PasskeyBridgeViewState>({status: 'waiting'});
	const sessionRef = useRef<PasskeyBridgeSession | null>(null);
	useEffect(() => {
		const session = startPasskeyBridgeSession(openerOrigin, setState);
		sessionRef.current = session;
		return () => {
			session.dispose();
			sessionRef.current = null;
		};
	}, [openerOrigin]);
	const handleContinue = useCallback(() => {
		sessionRef.current?.continueCeremony();
	}, []);
	const handleClose = useCallback(() => {
		window.close();
	}, []);
	const host = new URL(openerOrigin).host;
	const failure =
		state.status === 'unavailable'
			? i18n._(BRIDGE_UNAVAILABLE_DESCRIPTOR, {productName: PRODUCT_NAME})
			: state.status === 'rejected'
				? i18n._(BRIDGE_REJECTED_DESCRIPTOR)
				: state.status === 'timed_out'
					? i18n._(BRIDGE_TIMED_OUT_DESCRIPTOR)
					: null;
	const kind = state.status === 'ready' || state.status === 'running' ? state.kind : 'authenticate';
	return (
		<main className={styles.errorFallbackContainer} data-flx="auth.passkey-bridge-screen.container">
			<FluxerIcon className={styles.errorFallbackIcon} data-flx="auth.passkey-bridge-screen.icon" />
			<div className={styles.errorFallbackContent} data-flx="auth.passkey-bridge-screen.content">
				<h1 className={styles.errorFallbackTitle} data-flx="auth.passkey-bridge-screen.title">
					{i18n._(USE_YOUR_PASSKEY_DESCRIPTOR)}
				</h1>
				<p
					className={styles.errorFallbackDescription}
					role={failure === null ? undefined : 'alert'}
					data-flx="auth.passkey-bridge-screen.description"
				>
					{failure ??
						i18n._(kind === 'register' ? CONTINUE_TO_CREATE_DESCRIPTOR : CONTINUE_TO_SIGN_IN_DESCRIPTOR, {host})}
				</p>
			</div>
			<div className={styles.errorFallbackActions} data-flx="auth.passkey-bridge-screen.actions">
				{failure === null ? (
					<Button
						onClick={handleContinue}
						disabled={state.status !== 'ready'}
						submitting={state.status === 'waiting' || state.status === 'running'}
						autoFocus
						data-flx="auth.passkey-bridge-screen.button.continue"
					>
						{i18n._(CONTINUE_DESCRIPTOR)}
					</Button>
				) : state.status !== 'unavailable' ? (
					<Button variant="secondary" onClick={handleClose} data-flx="auth.passkey-bridge-screen.button.close">
						{i18n._(CLOSE_WINDOW_DESCRIPTOR)}
					</Button>
				) : null}
			</div>
		</main>
	);
};
