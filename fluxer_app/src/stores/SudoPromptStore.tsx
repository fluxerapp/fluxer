/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable, runInAction} from 'mobx';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import SudoVerificationModal from '~/components/modals/SudoVerificationModal';
import {Endpoints} from '~/Endpoints';
import HttpClient, {type HttpError, type HttpRequestConfig} from '~/lib/HttpClient';
import {makePersistent} from '~/lib/MobXPersistence';
import type {SudoVerificationPayload} from '~/types/Sudo';

interface SudoRequestContext {
	method: string;
	url: string;
}
export enum SudoVerificationMethod {
	PASSWORD = 'password',
	TOTP = 'totp',
	SMS = 'sms',
	WEBAUTHN = 'webauthn',
}

export const isAbortError = (error: unknown): boolean => {
	if (error instanceof DOMException && error.name === 'AbortError') {
		return true;
	}
	if (error instanceof Error && error.name === 'AbortError') {
		return true;
	}
	return false;
};

const SUDO_MODAL_KEY = 'sudo-verification-modal';

class SudoPromptStore {
	isOpen = false;
	isLoadingMethods = false;
	isVerifying = false;
	verificationError: string | null = null;
	rawError: HttpError | null = null;
	currentRequest: SudoRequestContext | null = null;
	availableMethods: {password: boolean; totp: boolean; sms: boolean; webauthn: boolean; has_mfa: boolean} = {
		password: true,
		totp: false,
		sms: false,
		webauthn: false,
		has_mfa: false,
	};
	lastUsedMfaMethod: SudoVerificationPayload['mfa_method'] | null = null;

	private resolver: ((payload: SudoVerificationPayload) => void) | null = null;
	private rejecter: ((reason?: unknown) => void) | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	init(): void {
		HttpClient.setSudoHandler(this.handleSudoRequest);
		HttpClient.setSudoFailureHandler(this.onSudoVerificationFailed);
		void makePersistent(this, 'SudoPromptStore', ['lastUsedMfaMethod']);
	}

	requestVerification(context: SudoRequestContext = {method: 'POST', url: ''}): Promise<SudoVerificationPayload> {
		return new Promise((resolve, reject) => {
			runInAction(() => {
				this.currentRequest = context;
				this.isOpen = true;
				this.resolver = resolve;
				this.rejecter = reject;
				this.pushModal();
			});
		});
	}

	private pushModal(): void {
		ModalActionCreators.pushWithKey(
			modal(() => <SudoVerificationModal />),
			SUDO_MODAL_KEY,
		);
	}

	private async handleSudoRequest(config: HttpRequestConfig): Promise<SudoVerificationPayload> {
		const request: SudoRequestContext = {method: config.method ?? 'GET', url: config.url};
		return await new Promise<SudoVerificationPayload>((resolve, reject) => {
			runInAction(() => {
				this.currentRequest = request;
				this.isOpen = true;
				this.resolver = resolve;
				this.rejecter = reject;
				this.pushModal();
			});
		});
	}

	async loadAvailableMethods(): Promise<void> {
		runInAction(() => {
			this.isLoadingMethods = true;
		});
		try {
			const response = await HttpClient.get<{totp: boolean; sms: boolean; webauthn: boolean; has_mfa: boolean}>({
				url: Endpoints.SUDO_MFA_METHODS,
			});
			runInAction(() => {
				const hasMfa = response.body.has_mfa;
				this.availableMethods = {
					password: !hasMfa,
					totp: response.body.totp,
					sms: response.body.sms,
					webauthn: response.body.webauthn,
					has_mfa: hasMfa,
				};
			});
		} catch (error) {
			console.error('Failed to load sudo MFA methods', error);
			runInAction(() => {
				this.availableMethods = {password: true, totp: false, sms: false, webauthn: false, has_mfa: false};
			});
		} finally {
			runInAction(() => {
				this.isLoadingMethods = false;
			});
		}
	}

	submit(payload: SudoVerificationPayload): void {
		if (payload.mfa_method === 'totp' || payload.mfa_method === 'sms' || payload.mfa_method === 'webauthn') {
			this.lastUsedMfaMethod = payload.mfa_method;
		}
		if (this.resolver) {
			this.isVerifying = true;
			this.verificationError = null;
			this.rawError = null;
			this.resolver(payload);
		}
	}

	reject(reason?: unknown): void {
		if (this.rejecter) {
			this.rejecter(reason);
		}
		this.cleanup();
	}

	handleTokenReceived(_token: string | null): void {
		if (!this.isVerifying) {
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

			const httpError = error as HttpError | null;
			this.rawError = httpError;

			const body = httpError?.body as {message?: string} | undefined;
			this.verificationError = body?.message ?? 'Verification failed';
		});
	};

	private cleanup(): void {
		this.isOpen = false;
		this.isVerifying = false;
		this.verificationError = null;
		this.rawError = null;
		this.currentRequest = null;
		this.resolver = null;
		this.rejecter = null;
		ModalActionCreators.popWithKey(SUDO_MODAL_KEY);
	}
}

export type {SudoVerificationPayload};

export default new SudoPromptStore();
