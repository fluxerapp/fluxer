// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/ErrorFallback.module.css';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import type {DomainMigrationSide} from '@app/features/app/domain_migration/DomainMigrationCore';
import {
	readPasskeyBridgePageLoginReturnPath,
	readPasskeyBridgeReturn,
} from '@app/features/auth/passkey_migration/PasskeyBridgeReturn';
import {
	CANCEL_DESCRIPTOR,
	CONTINUE_DESCRIPTOR,
	TRY_AGAIN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {FluxerIcon} from '@app/features/ui/components/icons/FluxerIcon';
import {PASSKEY_BRIDGE_CHANNEL} from '@fluxer/constants/src/PasskeyConstants';
import type {PasskeyBridgeFinishResponse} from '@fluxer/schema/src/domains/auth/PasskeyBridgeSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {type PublicKeyCredentialRequestOptionsJSON, startAuthentication} from '@simplewebauthn/browser';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';

const CEREMONY_ID_PATTERN = /^[A-Za-z0-9_-]{43}$/u;

const USE_YOUR_PASSKEY_DESCRIPTOR = msg({
	message: 'Use your passkey',
	comment: 'Heading of the page that asks the user to confirm with their passkey.',
});
const PRESS_CONTINUE_DESCRIPTOR = msg({
	message: 'Press Continue to use your passkey.',
	comment: 'Body of the page that asks the user to confirm with their passkey. Continue is the button label.',
});
const THAT_DIDN_T_WORK_DESCRIPTOR = msg({
	message: "That didn't work. Try again or cancel.",
	comment: 'Body of the passkey page after the passkey prompt failed or was dismissed. Keep plain.',
});
const REQUEST_EXPIRED_DESCRIPTOR = msg({
	message: 'This request has expired. Go back and try again.',
	comment: 'Body of the passkey page when the passkey request is no longer valid. Keep plain.',
});
const BACK_TO_PRODUCT_DESCRIPTOR = msg({
	message: 'Back to {productName}',
	comment: 'Button on the expired passkey page that returns to the app. productName is the app name.',
});
const YOU_CAN_CLOSE_THIS_TAB_DESCRIPTOR = msg({
	message: 'You can close this tab',
	comment: 'Heading of the page shown after a passkey confirmation finished in a separate tab.',
});
const GO_BACK_TO_CONTINUE_DESCRIPTOR = msg({
	message: 'Go back to {productName} to continue.',
	comment:
		'Body of the page shown after a passkey confirmation finished in a separate tab. productName is the app name.',
});

type LegacyBridgeState =
	| {status: 'loading'}
	| {status: 'ready'; options: PublicKeyCredentialRequestOptionsJSON}
	| {status: 'running'}
	| {status: 'failed'}
	| {status: 'expired'};

class PasskeyBridgeExpiredError extends Error {
	constructor() {
		super('Passkey bridge ceremony is unknown or expired');
		this.name = 'PasskeyBridgeExpiredError';
	}
}

async function postBridge<T>(path: string, body?: unknown): Promise<T> {
	const response = await fetch(`${window.location.origin}/api/v1${path}`, {
		method: 'POST',
		credentials: 'omit',
		...(body === undefined ? {} : {headers: {'Content-Type': 'application/json'}, body: JSON.stringify(body)}),
	});
	if (response.status === 404) {
		throw new PasskeyBridgeExpiredError();
	}
	if (!response.ok) {
		throw new Error(`Passkey bridge request failed with status ${response.status}`);
	}
	return (await response.json()) as T;
}

function leave(finish: PasskeyBridgeFinishResponse): void {
	if (finish.return_url === null) {
		throw new Error('Passkey bridge finished without a return address');
	}
	window.location.replace(finish.return_url);
}

const isDismissed = (error: unknown): boolean =>
	error instanceof Error && (error.name === 'NotAllowedError' || error.name === 'AbortError');

interface PasskeyBridgePageProps {
	side: DomainMigrationSide;
	hash: string;
	opensInOwnTab: boolean;
}

const LegacyPasskeyBridgePage: React.FC<PasskeyBridgePageProps> = ({side, hash, opensInOwnTab}) => {
	const {i18n} = useLingui();
	const ceremonyId = CEREMONY_ID_PATTERN.test(hash.slice(1)) ? hash.slice(1) : null;
	const [state, setState] = useState<LegacyBridgeState>(() =>
		ceremonyId === null ? {status: 'expired'} : {status: 'loading'},
	);
	const fail = useCallback((error: unknown) => {
		setState(error instanceof PasskeyBridgeExpiredError ? {status: 'expired'} : {status: 'failed'});
	}, []);
	const loadOptions = useCallback(() => {
		if (ceremonyId === null) {
			return;
		}
		setState({status: 'loading'});
		postBridge<{options: PublicKeyCredentialRequestOptionsJSON}>(Endpoints.AUTH_PASSKEY_BRIDGE_OPTIONS(ceremonyId))
			.then((response) => {
				setState({status: 'ready', options: response.options});
			})
			.catch(fail);
	}, [ceremonyId, fail]);
	useEffect(loadOptions, [loadOptions]);
	const cancel = useCallback(() => {
		if (ceremonyId === null) {
			return;
		}
		setState({status: 'running'});
		postBridge<PasskeyBridgeFinishResponse>(Endpoints.AUTH_PASSKEY_BRIDGE_CANCEL(ceremonyId))
			.then(leave)
			.catch(() => {
				setState({status: 'expired'});
			});
	}, [ceremonyId]);
	const handleContinue = useCallback(() => {
		if (ceremonyId === null || state.status !== 'ready') {
			return;
		}
		setState({status: 'running'});
		startAuthentication({optionsJSON: state.options}).then(
			(response) =>
				postBridge<PasskeyBridgeFinishResponse>(Endpoints.AUTH_PASSKEY_BRIDGE_COMPLETE(ceremonyId), {response})
					.then(leave)
					.catch((error: unknown) => {
						if (opensInOwnTab || error instanceof PasskeyBridgeExpiredError) {
							fail(error);
							return;
						}
						cancel();
					}),
			(error: unknown) => {
				if (!opensInOwnTab && isDismissed(error)) {
					cancel();
					return;
				}
				setState({status: 'failed'});
			},
		);
	}, [cancel, ceremonyId, fail, opensInOwnTab, state]);
	const handleBack = useCallback(() => {
		window.close();
		window.location.replace(`${side.target}/`);
	}, [side.target]);
	const expired = state.status === 'expired';
	const failed = state.status === 'failed';
	return (
		<main className={styles.errorFallbackContainer} data-flx="auth.passkey-bridge-page.container">
			<FluxerIcon className={styles.errorFallbackIcon} data-flx="auth.passkey-bridge-page.icon" />
			<div className={styles.errorFallbackContent} data-flx="auth.passkey-bridge-page.content">
				<h1 className={styles.errorFallbackTitle} data-flx="auth.passkey-bridge-page.title">
					{i18n._(USE_YOUR_PASSKEY_DESCRIPTOR)}
				</h1>
				<p
					className={styles.errorFallbackDescription}
					role={expired || failed ? 'alert' : undefined}
					data-flx="auth.passkey-bridge-page.description"
				>
					{i18n._(
						expired ? REQUEST_EXPIRED_DESCRIPTOR : failed ? THAT_DIDN_T_WORK_DESCRIPTOR : PRESS_CONTINUE_DESCRIPTOR,
					)}
				</p>
			</div>
			<div className={styles.errorFallbackActions} data-flx="auth.passkey-bridge-page.actions">
				{expired ? (
					<Button onClick={handleBack} autoFocus data-flx="auth.passkey-bridge-page.button.back">
						{i18n._(BACK_TO_PRODUCT_DESCRIPTOR, {productName: PRODUCT_NAME})}
					</Button>
				) : failed ? (
					<>
						<Button
							variant={opensInOwnTab ? 'secondary' : 'primary'}
							onClick={loadOptions}
							autoFocus={!opensInOwnTab}
							data-flx="auth.passkey-bridge-page.button.try-again"
						>
							{i18n._(TRY_AGAIN_DESCRIPTOR)}
						</Button>
						<Button
							variant={opensInOwnTab ? 'primary' : 'ghost'}
							onClick={cancel}
							autoFocus={opensInOwnTab}
							data-flx="auth.passkey-bridge-page.button.cancel"
						>
							{i18n._(CANCEL_DESCRIPTOR)}
						</Button>
					</>
				) : (
					<>
						<Button
							onClick={handleContinue}
							submitting={state.status !== 'ready'}
							autoFocus
							data-flx="auth.passkey-bridge-page.button.continue"
						>
							{i18n._(CONTINUE_DESCRIPTOR)}
						</Button>
						<Button
							variant="ghost"
							onClick={cancel}
							disabled={state.status === 'running'}
							data-flx="auth.passkey-bridge-page.button.cancel"
						>
							{i18n._(CANCEL_DESCRIPTOR)}
						</Button>
					</>
				)}
			</div>
		</main>
	);
};

const TargetPasskeyBridgePage: React.FC<{hash: string}> = ({hash}) => {
	const {i18n} = useLingui();
	const [bridgeReturn] = useState(() => readPasskeyBridgeReturn(hash));
	const [loginReturnPath] = useState(() =>
		bridgeReturn === null ? null : readPasskeyBridgePageLoginReturnPath(bridgeReturn.ceremonyId),
	);
	useEffect(() => {
		if (loginReturnPath !== null) {
			window.location.replace(`${window.location.origin}${loginReturnPath}${hash}`);
			return;
		}
		if (bridgeReturn !== null) {
			const channel = new BroadcastChannel(PASSKEY_BRIDGE_CHANNEL);
			channel.postMessage({ceremony_id: bridgeReturn.ceremonyId, completion_code: bridgeReturn.completionCode});
			channel.close();
		}
		window.close();
	}, [bridgeReturn, hash, loginReturnPath]);
	if (loginReturnPath !== null) {
		return null;
	}
	return (
		<main className={styles.errorFallbackContainer} data-flx="auth.passkey-bridge-page.container">
			<FluxerIcon className={styles.errorFallbackIcon} data-flx="auth.passkey-bridge-page.icon" />
			<div className={styles.errorFallbackContent} data-flx="auth.passkey-bridge-page.content">
				<h1 className={styles.errorFallbackTitle} data-flx="auth.passkey-bridge-page.title">
					{i18n._(YOU_CAN_CLOSE_THIS_TAB_DESCRIPTOR)}
				</h1>
				<p className={styles.errorFallbackDescription} data-flx="auth.passkey-bridge-page.description">
					{i18n._(GO_BACK_TO_CONTINUE_DESCRIPTOR, {productName: PRODUCT_NAME})}
				</p>
			</div>
		</main>
	);
};

export const PasskeyBridgePage: React.FC<PasskeyBridgePageProps> = ({side, hash, opensInOwnTab}) =>
	side.role === 'source' ? (
		<LegacyPasskeyBridgePage
			side={side}
			hash={hash}
			opensInOwnTab={opensInOwnTab}
			data-flx="auth.passkey-bridge-page.legacy"
		/>
	) : (
		<TargetPasskeyBridgePage hash={hash} data-flx="auth.passkey-bridge-page.target" />
	);
