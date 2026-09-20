// SPDX-License-Identifier: AGPL-3.0-or-later

import SudoVerificationModal from '@app/features/auth/components/modals/SudoVerificationModal';
import Sudo from '@app/features/auth/state/AuthSudo';
import type {SudoVerificationPayload} from '@app/features/auth/types/AuthSudoTypes';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import type {HttpMethod} from '@app/features/platform/types/TransportTypes';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import Users from '@app/features/user/state/Users';
import WebAuthnCredentials from '@app/features/user/state/WebAuthnCredentials';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {MfaMethod, SudoPromptStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable, runInAction} from 'mobx';

interface SudoErrorPayload {
	has_mfa?: boolean;
	methods?: {
		totp?: boolean;
		webauthn?: boolean;
		backup_codes?: boolean;
	};
}

function extractSudoErrorPayload(error: unknown): SudoErrorPayload | null {
	if (!(error instanceof HttpError)) return null;
	const body = error.body;
	if (!body || typeof body !== 'object') return null;
	return body as SudoErrorPayload;
}

interface SudoRequestContext {
	method: string;
	path: string;
}

export enum SudoVerificationMethod {
	PASSWORD = 'password',
	TOTP = 'totp',
	WEBAUTHN = 'webauthn',
}

export function isAbortError(error: unknown): boolean {
	if (error instanceof DOMException && error.name === 'AbortError') {
		return true;
	}
	if (error instanceof Error && error.name === 'AbortError') {
		return true;
	}
	return false;
}

const SUDO_MODAL_KEY = 'sudo-verification-modal';

export interface AvailableMethods {
	password: boolean;
	totp: boolean;
	webauthn: boolean;
	backupCodes: boolean;
	hasMfa: boolean;
}

type StoredMfaMethod = NonNullable<SudoVerificationPayload['mfa_method']>;

const MFA_FROM_PROTO: Record<MfaMethod, StoredMfaMethod | null> = {
	[MfaMethod.UNSPECIFIED]: null,
	[MfaMethod.TOTP]: 'totp',
	[MfaMethod.WEBAUTHN]: 'webauthn',
};
const MFA_TO_PROTO: Record<StoredMfaMethod, MfaMethod> = {
	totp: MfaMethod.TOTP,
	webauthn: MfaMethod.WEBAUTHN,
};
const EMPTY_METHODS: AvailableMethods = {
	password: false,
	totp: false,
	webauthn: false,
	backupCodes: false,
	hasMfa: false,
};

function deriveMethodsFromCurrentUser(): AvailableMethods {
	const user = Users.currentUser;
	if (!user) return {...EMPTY_METHODS};
	const types = user.authenticatorTypes;
	const totp = types?.includes(UserAuthenticatorTypes.TOTP) ?? false;
	const webauthnIsSecondFactor = types?.includes(UserAuthenticatorTypes.WEBAUTHN) ?? false;
	const webauthn = WebAuthnCredentials.credentials.length > 0;
	const hasMfa = user.mfaEnabled ?? (totp || webauthnIsSecondFactor);
	return {
		password: !(totp || webauthnIsSecondFactor),
		totp,
		webauthn,
		backupCodes: false,
		hasMfa,
	};
}

class SudoPrompt {
	isOpen = false;
	isVerifying = false;
	verificationFailed = false;
	rawError: HttpError | null = null;
	currentRequest: SudoRequestContext | null = null;
	availableMethods: AvailableMethods = {...EMPTY_METHODS};
	lastUsedMfaMethod: SudoVerificationPayload['mfa_method'] | null = null;
	private resolver: ((payload: SudoVerificationPayload) => void) | null = null;
	private rejecter: ((reason?: unknown) => void) | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	init(): void {
		http.installSudo({
			tokenProvider: Sudo.getValidToken,
			tokenListener: (token) => {
				if (token) {
					Sudo.setToken(token);
				}
				this.handleTokenReceived(token);
			},
			invalidate: Sudo.clearToken,
			prompt: this.handleSudoPrompt,
			onFailure: this.onSudoVerificationFailed,
		});
		void makeSyncedField(this, {
			field: 'sudoPrompt',
			schema: SudoPromptStateSchema,
			persist: ['lastUsedMfaMethod'],
			toMessage: (s) => ({
				lastUsedMfaMethod: s.lastUsedMfaMethod != null ? MFA_TO_PROTO[s.lastUsedMfaMethod] : undefined,
			}),
			applyMessage: (s, m) => {
				if (m.lastUsedMfaMethod === undefined) {
					s.lastUsedMfaMethod = null;
					return;
				}
				const method = MFA_FROM_PROTO[m.lastUsedMfaMethod];
				s.lastUsedMfaMethod = method;
			},
		});
	}

	get hardMfaLock(): boolean {
		return this.availableMethods.hasMfa;
	}

	requestVerification(context: SudoRequestContext = {method: 'POST', path: ''}): Promise<SudoVerificationPayload> {
		return new Promise((resolve, reject) => {
			runInAction(() => {
				this.openPrompt(context, resolve, reject);
				this.availableMethods = deriveMethodsFromCurrentUser();
			});
		});
	}

	private openPrompt(
		context: SudoRequestContext,
		resolve: (payload: SudoVerificationPayload) => void,
		reject: (reason?: unknown) => void,
	): void {
		this.resetPromptState();
		this.currentRequest = context;
		this.isOpen = true;
		this.resolver = resolve;
		this.rejecter = reject;
		this.pushModal();
	}

	private pushModal(): void {
		ModalCommands.pushWithKey(
			modal(() => <SudoVerificationModal data-flx="auth.sudo-prompt.sudo-verification-modal" />),
			SUDO_MODAL_KEY,
		);
	}

	private resetPromptState(): void {
		this.availableMethods = {...EMPTY_METHODS};
		this.verificationFailed = false;
		this.rawError = null;
	}

	private mergeFromError(error: unknown): void {
		const payload = extractSudoErrorPayload(error);
		if (!payload) return;
		const baseline = deriveMethodsFromCurrentUser();
		const hasMfa = typeof payload.has_mfa === 'boolean' ? payload.has_mfa : baseline.hasMfa;
		const methods = payload.methods ?? {};
		const totp = methods.totp === true || (methods.totp === undefined && baseline.totp);
		const webauthn = methods.webauthn === true || (methods.webauthn === undefined && baseline.webauthn);
		const backupCodes = methods.backup_codes === true || (methods.backup_codes === undefined && baseline.backupCodes);
		this.availableMethods = {
			password: baseline.password,
			totp,
			webauthn,
			backupCodes,
			hasMfa,
		};
	}

	private async handleSudoPrompt(
		method: HttpMethod,
		path: string,
		triggeringFailure: unknown,
	): Promise<Record<string, unknown> | null> {
		const request: SudoRequestContext = {method, path};
		const payload = await new Promise<SudoVerificationPayload>((resolve, reject) => {
			runInAction(() => {
				if (this.isOpen || this.isVerifying) {
					this.currentRequest = request;
					this.isOpen = true;
					this.isVerifying = false;
					this.resolver = resolve;
					this.rejecter = reject;
				} else {
					this.openPrompt(request, resolve, reject);
				}
				this.availableMethods = deriveMethodsFromCurrentUser();
				this.mergeFromError(triggeringFailure);
			});
		});
		return payload;
	}

	submit(payload: SudoVerificationPayload): void {
		if (payload.mfa_method === 'totp' || payload.mfa_method === 'webauthn') {
			this.lastUsedMfaMethod = payload.mfa_method;
		}
		const resolver = this.resolver;
		if (resolver) {
			this.isVerifying = true;
			this.verificationFailed = false;
			this.rawError = null;
			this.resolver = null;
			this.rejecter = null;
			resolver(payload);
		}
	}

	reject(reason?: unknown): void {
		if (this.rejecter) {
			this.rejecter(reason);
		}
		this.cleanup();
	}

	handleTokenReceived(_token: string | null): void {
		if (!this.isOpen && !this.isVerifying) {
			return;
		}
		this.cleanup();
	}

	private onSudoVerificationFailed = (error: unknown): void => {
		runInAction(() => {
			this.isVerifying = false;
			if (isAbortError(error)) {
				this.cleanup();
				return;
			}
			const responseErr = error instanceof HttpError ? error : null;
			this.rawError = responseErr;
			this.mergeFromError(error);
			this.verificationFailed = true;
		});
	};

	private cleanup(): void {
		this.isOpen = false;
		this.isVerifying = false;
		this.verificationFailed = false;
		this.rawError = null;
		this.currentRequest = null;
		this.resolver = null;
		this.rejecter = null;
		this.availableMethods = {...EMPTY_METHODS};
		ModalCommands.popWithKey(SUDO_MODAL_KEY);
	}
}

export default new SudoPrompt();
