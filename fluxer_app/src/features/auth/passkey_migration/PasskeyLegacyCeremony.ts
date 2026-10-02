// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {randomBase64Url, sha256Hex} from '@app/features/app/domain_migration/DomainMigrationCrypto';
import {
	type PasskeyBridgeLoginPurpose,
	type PasskeyBridgePageLogin,
	storePasskeyBridgePageLogin,
} from '@app/features/auth/passkey_migration/PasskeyBridgeReturn';
import type {MfaChallenge} from '@app/features/auth/state/AuthFlow';
import * as WebAuthnUtils from '@app/features/auth/utils/WebAuthnUtils';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Platform} from '@app/features/platform/types/Platform';
import {PASSKEY_BRIDGE_CHANNEL} from '@fluxer/constants/src/PasskeyConstants';
import type {
	PasskeyBridgeFinishResponse,
	PasskeyBridgeLoginRedeemResponse,
	PasskeyBridgeLoginStartRequest,
	PasskeyBridgeStartResponse,
	PasskeyBridgeSudoRedeemResponse,
} from '@fluxer/schema/src/domains/auth/PasskeyBridgeSchemas';
import type {AuthenticationResponseJSON, PublicKeyCredentialRequestOptionsJSON} from '@simplewebauthn/browser';

const NONCE_BYTES = 32;
const LINK_CEREMONY_RESTART_MS = 9 * 60 * 1000;

interface LegacyCeremony {
	id: string;
	nonce: string;
}

async function createNonce(): Promise<{nonce: string; nonceHash: string}> {
	const nonce = randomBase64Url(NONCE_BYTES);
	return {nonce, nonceHash: await sha256Hex(nonce)};
}

export function isPasskeyCeremonyDismissed(error: unknown): boolean {
	if (Platform.isElectron) {
		return !(error instanceof HttpError);
	}
	return error instanceof Error && (error.name === 'NotAllowedError' || error.name === 'AbortError');
}

async function startLogin(request: PasskeyBridgeLoginStartRequest): Promise<PasskeyBridgeStartResponse> {
	const response = await http.post<PasskeyBridgeStartResponse>(Endpoints.AUTH_PASSKEY_BRIDGE, {body: request});
	return response.body;
}

async function startSudo(runner: 'page' | 'native', nonceHash: string): Promise<PasskeyBridgeStartResponse> {
	const response = await http.post<PasskeyBridgeStartResponse>(Endpoints.USER_PASSKEY_BRIDGE, {
		body: {runner, nonce_hash: nonceHash},
	});
	return response.body;
}

async function redeemLogin(
	ceremony: LegacyCeremony,
	completionCode: string,
): Promise<PasskeyBridgeLoginRedeemResponse> {
	const response = await http.post<PasskeyBridgeLoginRedeemResponse>(
		Endpoints.AUTH_PASSKEY_BRIDGE_REDEEM(ceremony.id),
		{
			body: {nonce: ceremony.nonce, completion_code: completionCode},
		},
	);
	return response.body;
}

async function redeemSudo(ceremony: LegacyCeremony, completionCode: string): Promise<PasskeyBridgeSudoRedeemResponse> {
	const response = await http.post<PasskeyBridgeSudoRedeemResponse>(Endpoints.USER_PASSKEY_BRIDGE_REDEEM(ceremony.id), {
		body: {nonce: ceremony.nonce, completion_code: completionCode},
	});
	return response.body;
}

function requireCompletionCode(finish: PasskeyBridgeFinishResponse): string {
	if (finish.completion_code === null) {
		throw new Error('Passkey bridge finished without a completion code');
	}
	return finish.completion_code;
}

async function runNativeCeremony(ceremonyId: string): Promise<string> {
	const optionsResponse = await http.post<{options: PublicKeyCredentialRequestOptionsJSON}>(
		Endpoints.AUTH_PASSKEY_BRIDGE_OPTIONS(ceremonyId),
	);
	let credential: AuthenticationResponseJSON;
	try {
		credential = await WebAuthnUtils.performAuthentication(optionsResponse.body.options);
	} catch (error) {
		if (!isPasskeyCeremonyDismissed(error)) {
			throw error;
		}
		const cancelled = await http.post<PasskeyBridgeFinishResponse>(Endpoints.AUTH_PASSKEY_BRIDGE_CANCEL(ceremonyId));
		return requireCompletionCode(cancelled.body);
	}
	const completed = await http.post<PasskeyBridgeFinishResponse>(Endpoints.AUTH_PASSKEY_BRIDGE_COMPLETE(ceremonyId), {
		body: {response: credential},
	});
	return requireCompletionCode(completed.body);
}

export async function startPasskeyBridgePageLogin(mfa: MfaChallenge | null, returnPath: string): Promise<void> {
	const purpose: PasskeyBridgeLoginPurpose = mfa === null ? 'login' : 'login_mfa';
	const {nonce, nonceHash} = await createNonce();
	const started = await startLogin({
		purpose,
		runner: 'page',
		...(mfa === null ? {} : {ticket: mfa.ticket}),
		nonce_hash: nonceHash,
	});
	if (started.bridge_url === null) {
		throw new Error('Passkey bridge did not return a page');
	}
	storePasskeyBridgePageLogin(started.ceremony_id, {
		nonce,
		purpose,
		return_path: returnPath,
		mfa,
		created_at: Date.now(),
	});
	window.location.assign(started.bridge_url);
}

export function redeemPasskeyBridgePageLogin(
	ceremonyId: string,
	login: PasskeyBridgePageLogin,
	completionCode: string,
): Promise<PasskeyBridgeLoginRedeemResponse> {
	return redeemLogin({id: ceremonyId, nonce: login.nonce}, completionCode);
}

export async function runPasskeyBridgeNativeLogin(
	purpose: PasskeyBridgeLoginPurpose,
	ticket?: string,
): Promise<PasskeyBridgeLoginRedeemResponse> {
	const {nonce, nonceHash} = await createNonce();
	const started = await startLogin({
		purpose,
		runner: 'native',
		...(ticket === undefined ? {} : {ticket}),
		nonce_hash: nonceHash,
	});
	const completionCode = await runNativeCeremony(started.ceremony_id);
	return redeemLogin({id: started.ceremony_id, nonce}, completionCode);
}

export async function runPasskeyBridgeNativeSudo(): Promise<PasskeyBridgeSudoRedeemResponse> {
	const {nonce, nonceHash} = await createNonce();
	const started = await startSudo('native', nonceHash);
	const completionCode = await runNativeCeremony(started.ceremony_id);
	return redeemSudo({id: started.ceremony_id, nonce}, completionCode);
}

interface PasskeyBridgeSudoLinkHandlers {
	onLink(url: string | null): void;
	onCompleted(sudoToken: string): void;
	onError(error: unknown): void;
}

export class PasskeyBridgeSudoLink {
	private readonly channel = new BroadcastChannel(PASSKEY_BRIDGE_CHANNEL);
	private ceremony: LegacyCeremony | null = null;
	private restartTimer: ReturnType<typeof setTimeout> | null = null;
	private generation = 0;
	private disposed = false;

	constructor(private readonly handlers: PasskeyBridgeSudoLinkHandlers) {
		this.channel.onmessage = this.handleMessage;
	}

	async start(): Promise<void> {
		const generation = this.reset();
		this.handlers.onLink(null);
		try {
			const {nonce, nonceHash} = await createNonce();
			const started = await startSudo('page', nonceHash);
			if (generation !== this.generation) {
				return;
			}
			if (started.bridge_url === null) {
				throw new Error('Passkey bridge did not return a page');
			}
			this.ceremony = {id: started.ceremony_id, nonce};
			this.restartTimer = setTimeout(() => void this.start(), LINK_CEREMONY_RESTART_MS);
			this.handlers.onLink(started.bridge_url);
		} catch (error) {
			if (generation === this.generation) {
				this.handlers.onError(error);
			}
		}
	}

	dispose(): void {
		this.reset();
		this.disposed = true;
		this.channel.close();
	}

	private reset(): number {
		if (this.restartTimer !== null) {
			clearTimeout(this.restartTimer);
			this.restartTimer = null;
		}
		this.ceremony = null;
		this.generation += 1;
		return this.generation;
	}

	private readonly handleMessage = (event: MessageEvent<unknown>): void => {
		const ceremony = this.ceremony;
		const data = event.data as {ceremony_id?: unknown; completion_code?: unknown} | null;
		if (
			this.disposed ||
			ceremony === null ||
			data?.ceremony_id !== ceremony.id ||
			typeof data.completion_code !== 'string'
		) {
			return;
		}
		const generation = this.reset();
		redeemSudo(ceremony, data.completion_code).then(
			(result) => {
				if (generation !== this.generation) {
					return;
				}
				if (result.status === 'completed') {
					this.handlers.onCompleted(result.sudo_token);
					return;
				}
				void this.start();
			},
			(error: unknown) => {
				if (generation !== this.generation) {
					return;
				}
				this.handlers.onError(error);
				void this.start();
			},
		);
	};
}
