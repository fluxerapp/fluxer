// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {rpIdMatchesPage} from '@app/features/auth/passkey_migration/PasskeyMigrationOrigin';
import {
	authenticateWithWebAuthn,
	getWebAuthnAuthenticationOptions,
	type LoginSuccessPayload,
} from '@app/features/auth/state/AuthFlow';
import {Platform} from '@app/features/platform/types/Platform';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {browserSupportsWebAuthnAutofill, startAuthentication, WebAuthnAbortService} from '@simplewebauthn/browser';
import {useEffect, useRef} from 'react';

const logger = new Logger('PasskeyAutofill');

export function usePasskeyAutofill(
	enabled: boolean,
	runtimeSnapshot: RuntimeConfigSnapshot,
	inviteCode: string | null,
	onSignedIn: (payload: LoginSuccessPayload) => Promise<void>,
): void {
	const onSignedInRef = useRef(onSignedIn);
	onSignedInRef.current = onSignedIn;
	useEffect(() => {
		if (!enabled || Platform.isElectron) return;
		let active = true;
		let pending = false;
		const start = async () => {
			if (!(await browserSupportsWebAuthnAutofill())) return;
			const options = await getWebAuthnAuthenticationOptions(runtimeSnapshot);
			if (!active || (options.rpId !== undefined && !rpIdMatchesPage(options.rpId))) return;
			pending = true;
			const credential = await startAuthentication({optionsJSON: options, useBrowserAutofill: true}).finally(() => {
				pending = false;
			});
			if (!active) return;
			const payload = await authenticateWithWebAuthn({
				response: credential,
				challenge: options.challenge,
				inviteCode: inviteCode ?? undefined,
				runtimeSnapshot,
			});
			await onSignedInRef.current(payload);
		};
		start().catch((error: unknown) => {
			if (active) logger.debug('Passkey autofill ended', error);
		});
		return () => {
			active = false;
			if (pending) WebAuthnAbortService.cancelCeremony();
		};
	}, [enabled, inviteCode, runtimeSnapshot]);
}
