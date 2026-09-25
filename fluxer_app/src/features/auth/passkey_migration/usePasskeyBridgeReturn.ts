// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	prunePasskeyBridgePageLogins,
	readPasskeyBridgeReturn,
	takePasskeyBridgePageLogin,
} from '@app/features/auth/passkey_migration/PasskeyBridgeReturn';
import {redeemPasskeyBridgePageLogin} from '@app/features/auth/passkey_migration/PasskeyLegacyCeremony';
import {writePasskeyLoginRoute} from '@app/features/auth/passkey_migration/PasskeyLoginRoute';
import {isPasskeyMigrationOrigin} from '@app/features/auth/passkey_migration/PasskeyMigrationOrigin';
import {type LoginSuccessPayload, type MfaChallenge, toLoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {useEffect, useRef, useState} from 'react';

const logger = new Logger('usePasskeyBridgeReturn');

interface PasskeyBridgeReturnOptions {
	redirectPath?: string;
	onLoginSuccess: (payload: LoginSuccessPayload) => Promise<void> | void;
	onRequireMfa: (challenge: MfaChallenge) => void;
	onFailure: () => void;
}

export function usePasskeyBridgeReturn(options: PasskeyBridgeReturnOptions): boolean {
	const [isRedeeming, setIsRedeeming] = useState(false);
	const handledRef = useRef(false);
	const optionsRef = useRef(options);
	optionsRef.current = options;
	useEffect(() => {
		if (handledRef.current || !isPasskeyMigrationOrigin()) {
			return;
		}
		handledRef.current = true;
		prunePasskeyBridgePageLogins(Date.now());
		const bridgeReturn = readPasskeyBridgeReturn(window.location.hash);
		if (bridgeReturn === null) {
			return;
		}
		window.history.replaceState(window.history.state, '', `${window.location.pathname}${window.location.search}`);
		const login = takePasskeyBridgePageLogin(bridgeReturn.ceremonyId);
		if (login === null) {
			optionsRef.current.onFailure();
			return;
		}
		const restoreMfa = () => {
			if (login.mfa !== null) {
				optionsRef.current.onRequireMfa(login.mfa);
			}
		};
		setIsRedeeming(true);
		redeemPasskeyBridgePageLogin(bridgeReturn.ceremonyId, login, bridgeReturn.completionCode)
			.then(async (result) => {
				if (result.status === 'cancelled') {
					writePasskeyLoginRoute('native');
					restoreMfa();
					return;
				}
				await optionsRef.current.onLoginSuccess(toLoginSuccessPayload(result));
				if (optionsRef.current.redirectPath) {
					RouterUtils.replaceWith(optionsRef.current.redirectPath);
				}
			})
			.catch((error: unknown) => {
				logger.error('Passkey sign-in on the previous address failed', error);
				restoreMfa();
				optionsRef.current.onFailure();
			})
			.finally(() => {
				setIsRedeeming(false);
			});
	}, []);
	return isRedeeming;
}
