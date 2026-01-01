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

import type {AuthenticationResponseJSON, RegistrationResponseJSON} from '@simplewebauthn/server';
import {
	generateAuthenticationOptions,
	generateRegistrationOptions,
	type VerifiedAuthenticationResponse,
	type VerifiedRegistrationResponse,
	verifyAuthenticationResponse,
	verifyRegistrationResponse,
} from '@simplewebauthn/server';
import {createUserID, type UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {APIErrorCodes, UserAuthenticatorTypes} from '~/Constants';
import {
	FluxerAPIError,
	InputValidationError,
	PhoneRequiredForSmsMfaError,
	SmsMfaNotEnabledError,
	SmsMfaRequiresTotpError,
} from '~/Errors';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {ISMSService} from '~/infrastructure/ISMSService';
import {getMetricsService} from '~/infrastructure/MetricsService';
import {Logger} from '~/Logger';
import {getUserSearchService} from '~/Meilisearch';
import type {User} from '~/Models';
import type {BotMfaMirrorService} from '~/oauth/BotMfaMirrorService';
import type {IUserRepository} from '~/user/IUserRepository';
import {mapUserToPrivateResponse} from '~/user/UserModel';
import {TotpGenerator} from '~/utils/TotpGenerator';

const WEBAUTHN_CHALLENGE_TTL_SECONDS = 60 * 5;
type WebAuthnChallengeContext = 'registration' | 'discoverable' | 'mfa' | 'sudo';

interface SudoMfaVerificationParams {
	userId: UserID;
	method: 'totp' | 'sms' | 'webauthn';
	code?: string;
	webauthnResponse?: AuthenticationResponseJSON;
	webauthnChallenge?: string;
}

interface SudoMfaVerificationResult {
	success: boolean;
	error?: string;
}

interface VerifyMfaCodeParams {
	userId: UserID;
	mfaSecret: string;
	code: string;
	allowBackup?: boolean;
}

export class AuthMfaService {
	constructor(
		private repository: IUserRepository,
		private cacheService: ICacheService,
		private smsService: ISMSService,
		private gatewayService: IGatewayService,
		private botMfaMirrorService?: BotMfaMirrorService,
	) {}

	async verifyMfaCode({userId, mfaSecret, code, allowBackup = false}: VerifyMfaCodeParams): Promise<boolean> {
		try {
			const totp = new TotpGenerator(mfaSecret);
			const isValidTotp = await totp.validateTotp(code);

			if (isValidTotp) {
				if (Config.dev.testModeEnabled) {
					return true;
				}

				const reuseKey = `mfa-totp:${userId}:${code}`;
				const isCodeUsed = await this.cacheService.get<number>(reuseKey);
				if (!isCodeUsed) {
					await this.cacheService.set(reuseKey, 1, 30);
					return true;
				}
			}
		} catch {}

		if (allowBackup) {
			const backupCodes = await this.repository.listMfaBackupCodes(userId);
			const backupCode = backupCodes.find((bc) => bc.code === code && !bc.consumed);

			if (backupCode) {
				await this.repository.consumeMfaBackupCode(userId, code);
				return true;
			}
		}

		return false;
	}

	async enableSmsMfa(userId: UserID): Promise<void> {
		const user = await this.repository.findUniqueAssert(userId);

		if (!user.totpSecret) {
			throw new SmsMfaRequiresTotpError();
		}

		if (!user.phone) {
			throw new PhoneRequiredForSmsMfaError();
		}

		const authenticatorTypes = user.authenticatorTypes || new Set<number>();
		authenticatorTypes.add(UserAuthenticatorTypes.SMS);
		const updatedUser = await this.repository.patchUpsert(userId, {authenticator_types: authenticatorTypes});

		const userSearchService = getUserSearchService();
		if (userSearchService && updatedUser) {
			await userSearchService.updateUser(updatedUser).catch((error) => {
				Logger.error({userId, error}, 'Failed to update user in search');
			});
		}

		await this.gatewayService.dispatchPresence({
			userId,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(updatedUser!),
		});
		if (updatedUser) {
			await this.botMfaMirrorService?.syncAuthenticatorTypesForOwner(updatedUser);
		}
	}

	async disableSmsMfa(userId: UserID): Promise<void> {
		const user = await this.repository.findUniqueAssert(userId);

		const authenticatorTypes = user.authenticatorTypes || new Set<number>();
		authenticatorTypes.delete(UserAuthenticatorTypes.SMS);
		const updatedUser = await this.repository.patchUpsert(userId, {authenticator_types: authenticatorTypes});

		const userSearchService = getUserSearchService();
		if (userSearchService && updatedUser) {
			await userSearchService.updateUser(updatedUser).catch((error) => {
				Logger.error({userId, error}, 'Failed to update user in search');
			});
		}

		await this.gatewayService.dispatchPresence({
			userId,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(updatedUser!),
		});
		if (updatedUser) {
			await this.botMfaMirrorService?.syncAuthenticatorTypesForOwner(updatedUser);
		}
	}

	async sendSmsMfaCode(userId: UserID): Promise<void> {
		const user = await this.repository.findUniqueAssert(userId);

		if (!user.authenticatorTypes?.has(UserAuthenticatorTypes.SMS)) {
			throw new SmsMfaNotEnabledError();
		}

		if (!user.phone) {
			throw new PhoneRequiredForSmsMfaError();
		}

		await this.smsService.startVerification(user.phone);
	}

	async sendSmsMfaCodeForTicket(ticket: string): Promise<void> {
		const userId = await this.cacheService.get<string>(`mfa-ticket:${ticket}`);
		if (!userId) {
			throw InputValidationError.create('ticket', 'Session timeout. Please refresh the page and log in again.');
		}
		await this.sendSmsMfaCode(createUserID(BigInt(userId)));
	}

	async verifySmsMfaCode(userId: UserID, code: string): Promise<boolean> {
		const user = await this.repository.findUnique(userId);
		if (!user || !user.phone) {
			return false;
		}

		return await this.smsService.checkVerification(user.phone, code);
	}

	async generateWebAuthnRegistrationOptions(userId: UserID) {
		const user = await this.repository.findUniqueAssert(userId);
		const existingCredentials = await this.repository.listWebAuthnCredentials(userId);

		if (existingCredentials.length >= 10) {
			throw new FluxerAPIError({
				code: APIErrorCodes.WEBAUTHN_CREDENTIAL_LIMIT_REACHED,
				message: 'You have reached the maximum number of passkeys (10)',
				status: 400,
			});
		}

		const rpName = Config.auth.passkeys.rpName;
		const rpID = Config.auth.passkeys.rpId;

		const options = await generateRegistrationOptions({
			rpName,
			rpID,
			userID: new TextEncoder().encode(user.id.toString()),
			userName: user.username!,
			userDisplayName: user.username!,
			attestationType: 'none',
			excludeCredentials: existingCredentials.map((cred) => ({
				id: cred.credentialId,
				transports: cred.transports
					? (Array.from(cred.transports) as Array<'usb' | 'nfc' | 'ble' | 'internal' | 'cable' | 'hybrid'>)
					: undefined,
			})),
			authenticatorSelection: {
				residentKey: 'required',
				requireResidentKey: true,
				userVerification: 'required',
			},
		});

		await this.saveWebAuthnChallenge(options.challenge, {context: 'registration', userId});
		return options;
	}

	async verifyWebAuthnRegistration(
		userId: UserID,
		response: RegistrationResponseJSON,
		expectedChallenge: string,
		name: string,
	): Promise<void> {
		const user = await this.repository.findUniqueAssert(userId);
		const existingCredentials = await this.repository.listWebAuthnCredentials(userId);

		await this.consumeWebAuthnChallenge(expectedChallenge, 'registration', {userId});

		if (existingCredentials.length >= 10) {
			throw new FluxerAPIError({
				code: APIErrorCodes.WEBAUTHN_CREDENTIAL_LIMIT_REACHED,
				message: 'You have reached the maximum number of passkeys (10)',
				status: 400,
			});
		}

		const rpID = Config.auth.passkeys.rpId;
		const expectedOrigin = Config.auth.passkeys.allowedOrigins;

		let verification: VerifiedRegistrationResponse;
		try {
			verification = await verifyRegistrationResponse({
				response,
				expectedChallenge,
				expectedOrigin,
				expectedRPID: rpID,
			});
		} catch (_error) {
			throw new FluxerAPIError({
				code: APIErrorCodes.INVALID_WEBAUTHN_CREDENTIAL,
				message: 'Failed to verify WebAuthn credential',
				status: 400,
			});
		}

		if (!verification.verified || !verification.registrationInfo) {
			throw new FluxerAPIError({
				code: APIErrorCodes.INVALID_WEBAUTHN_CREDENTIAL,
				message: 'Failed to verify WebAuthn credential',
				status: 400,
			});
		}

		const {credential} = verification.registrationInfo;

		let publicKeyBuffer: Buffer;
		let counterBigInt: bigint;

		try {
			publicKeyBuffer = Buffer.from(credential.publicKey);
		} catch (_error) {
			throw new FluxerAPIError({
				code: APIErrorCodes.INVALID_WEBAUTHN_CREDENTIAL,
				message: 'Invalid credential public key format during registration',
				status: 400,
			});
		}

		try {
			if (credential.counter === undefined || credential.counter === null) {
				throw new Error('Counter value is undefined or null');
			}
			counterBigInt = BigInt(credential.counter);
		} catch (_error) {
			throw new FluxerAPIError({
				code: APIErrorCodes.INVALID_WEBAUTHN_CREDENTIAL,
				message: 'Invalid credential counter value during registration',
				status: 400,
			});
		}

		const responseObj = response as {response?: {transports?: Array<string>}};
		await this.repository.createWebAuthnCredential(
			userId,
			credential.id,
			publicKeyBuffer,
			counterBigInt,
			responseObj.response?.transports ? new Set(responseObj.response.transports) : null,
			name,
		);

		const authenticatorTypes = user.authenticatorTypes || new Set<number>();
		if (!authenticatorTypes.has(UserAuthenticatorTypes.WEBAUTHN)) {
			authenticatorTypes.add(UserAuthenticatorTypes.WEBAUTHN);
			const updatedUser = await this.repository.patchUpsert(userId, {authenticator_types: authenticatorTypes});

			const userSearchService = getUserSearchService();
			if (userSearchService && updatedUser) {
				await userSearchService.updateUser(updatedUser).catch((error) => {
					Logger.error({userId, error}, 'Failed to update user in search');
				});
			}

			await this.gatewayService.dispatchPresence({
				userId,
				event: 'USER_UPDATE',
				data: mapUserToPrivateResponse(updatedUser!),
			});
			if (updatedUser) {
				await this.botMfaMirrorService?.syncAuthenticatorTypesForOwner(updatedUser);
			}
		}
	}

	async deleteWebAuthnCredential(userId: UserID, credentialId: string): Promise<void> {
		const credential = await this.repository.getWebAuthnCredential(userId, credentialId);
		if (!credential) {
			throw new FluxerAPIError({
				code: APIErrorCodes.UNKNOWN_WEBAUTHN_CREDENTIAL,
				message: 'Credential not found',
				status: 404,
			});
		}

		await this.repository.deleteWebAuthnCredential(userId, credentialId);

		const remainingCredentials = await this.repository.listWebAuthnCredentials(userId);
		if (remainingCredentials.length === 0) {
			const user = await this.repository.findUniqueAssert(userId);
			const authenticatorTypes = user.authenticatorTypes || new Set<number>();
			authenticatorTypes.delete(UserAuthenticatorTypes.WEBAUTHN);
			const updatedUser = await this.repository.patchUpsert(userId, {authenticator_types: authenticatorTypes});

			const userSearchService = getUserSearchService();
			if (userSearchService && updatedUser) {
				await userSearchService.updateUser(updatedUser).catch((error) => {
					Logger.error({userId, error}, 'Failed to update user in search');
				});
			}

			await this.gatewayService.dispatchPresence({
				userId,
				event: 'USER_UPDATE',
				data: mapUserToPrivateResponse(updatedUser!),
			});
			if (updatedUser) {
				await this.botMfaMirrorService?.syncAuthenticatorTypesForOwner(updatedUser);
			}
		}
	}

	async renameWebAuthnCredential(userId: UserID, credentialId: string, name: string): Promise<void> {
		const credential = await this.repository.getWebAuthnCredential(userId, credentialId);
		if (!credential) {
			throw new FluxerAPIError({
				code: APIErrorCodes.UNKNOWN_WEBAUTHN_CREDENTIAL,
				message: 'Credential not found',
				status: 404,
			});
		}

		await this.repository.updateWebAuthnCredentialName(userId, credentialId, name);
	}

	async generateWebAuthnAuthenticationOptionsDiscoverable() {
		const rpID = Config.auth.passkeys.rpId;

		const options = await generateAuthenticationOptions({
			rpID,
			userVerification: 'required',
		});

		await this.saveWebAuthnChallenge(options.challenge, {context: 'discoverable'});
		return options;
	}

	async verifyWebAuthnAuthenticationDiscoverable(
		response: AuthenticationResponseJSON,
		expectedChallenge: string,
	): Promise<User> {
		const responseObj = response as {id: string};
		const credentialId = responseObj.id;

		const userId = await this.repository.getUserIdByCredentialId(credentialId);
		if (!userId) {
			getMetricsService().counter({
				name: 'auth.login.failure',
				dimensions: {reason: 'mfa_invalid'},
			});
			throw new FluxerAPIError({
				code: APIErrorCodes.PASSKEY_AUTHENTICATION_FAILED,
				message: 'Passkey authentication failed',
				status: 401,
			});
		}

		await this.verifyWebAuthnAuthentication(userId, response, expectedChallenge, 'discoverable');

		const user = await this.repository.findUniqueAssert(userId);
		return user;
	}

	async generateWebAuthnAuthenticationOptionsForMfa(ticket: string) {
		const userId = await this.cacheService.get<string>(`mfa-ticket:${ticket}`);
		if (!userId) {
			throw InputValidationError.create('ticket', 'Session timeout. Please refresh the page and log in again.');
		}

		const credentials = await this.repository.listWebAuthnCredentials(createUserID(BigInt(userId)));

		if (credentials.length === 0) {
			throw new FluxerAPIError({
				code: APIErrorCodes.INVALID_WEBAUTHN_CREDENTIAL,
				message: 'No passkeys registered',
				status: 400,
			});
		}

		const rpID = Config.auth.passkeys.rpId;

		const options = await generateAuthenticationOptions({
			rpID,
			allowCredentials: credentials.map((cred) => ({
				id: cred.credentialId,
				transports: cred.transports
					? (Array.from(cred.transports) as Array<'usb' | 'nfc' | 'ble' | 'internal' | 'cable' | 'hybrid'>)
					: undefined,
			})),
			userVerification: 'required',
		});

		await this.saveWebAuthnChallenge(options.challenge, {context: 'mfa', userId: createUserID(BigInt(userId)), ticket});
		return options;
	}

	async verifyWebAuthnAuthentication(
		userId: UserID,
		response: AuthenticationResponseJSON,
		expectedChallenge: string,
		context: WebAuthnChallengeContext = 'mfa',
		ticket?: string,
	): Promise<void> {
		await this.consumeWebAuthnChallenge(expectedChallenge, context, {userId, ticket});

		const responseObj = response as {id: string};
		const credentialId = responseObj.id;
		const credential = await this.repository.getWebAuthnCredential(userId, credentialId);

		if (!credential) {
			getMetricsService().counter({
				name: 'auth.login.failure',
				dimensions: {reason: 'mfa_invalid'},
			});
			throw new FluxerAPIError({
				code: APIErrorCodes.PASSKEY_AUTHENTICATION_FAILED,
				message: 'Passkey authentication failed',
				status: 401,
			});
		}

		const rpID = Config.auth.passkeys.rpId;
		const expectedOrigin = Config.auth.passkeys.allowedOrigins;

		let verification: VerifiedAuthenticationResponse;
		try {
			let publicKeyUint8Array: Uint8Array<ArrayBuffer>;
			try {
				const buffer = Buffer.from(credential.publicKey);
				const arrayBuffer: ArrayBuffer = buffer.buffer.slice(buffer.byteOffset, buffer.byteOffset + buffer.byteLength);
				publicKeyUint8Array = new Uint8Array(arrayBuffer);
			} catch (_error) {
				throw new FluxerAPIError({
					code: APIErrorCodes.INVALID_WEBAUTHN_CREDENTIAL,
					message: 'Invalid credential public key format',
					status: 400,
				});
			}

			verification = await verifyAuthenticationResponse({
				response,
				expectedChallenge,
				expectedOrigin,
				expectedRPID: rpID,
				credential: {
					id: credential.credentialId,
					publicKey: publicKeyUint8Array,
					counter: Number(credential.counter),
					transports: credential.transports
						? (Array.from(credential.transports) as Array<'usb' | 'nfc' | 'ble' | 'internal' | 'cable' | 'hybrid'>)
						: undefined,
				},
			});
		} catch (_error) {
			getMetricsService().counter({
				name: 'auth.login.failure',
				dimensions: {reason: 'mfa_invalid'},
			});
			throw new FluxerAPIError({
				code: APIErrorCodes.PASSKEY_AUTHENTICATION_FAILED,
				message: 'Passkey authentication failed',
				status: 401,
			});
		}

		if (!verification.verified) {
			getMetricsService().counter({
				name: 'auth.login.failure',
				dimensions: {reason: 'mfa_invalid'},
			});
			throw new FluxerAPIError({
				code: APIErrorCodes.PASSKEY_AUTHENTICATION_FAILED,
				message: 'Passkey authentication failed',
				status: 401,
			});
		}

		let newCounter: bigint;
		try {
			if (
				verification.authenticationInfo.newCounter === undefined ||
				verification.authenticationInfo.newCounter === null
			) {
				throw new Error('Counter value is undefined or null');
			}
			newCounter = BigInt(verification.authenticationInfo.newCounter);
		} catch (_error) {
			throw new FluxerAPIError({
				code: APIErrorCodes.GENERAL_ERROR,
				message: 'Invalid authentication counter value',
				status: 500,
			});
		}

		await this.repository.updateWebAuthnCredentialCounter(userId, credentialId, newCounter);
		await this.repository.updateWebAuthnCredentialLastUsed(userId, credentialId);
	}

	async generateWebAuthnOptionsForSudo(userId: UserID) {
		const credentials = await this.repository.listWebAuthnCredentials(userId);

		if (credentials.length === 0) {
			throw new FluxerAPIError({
				code: APIErrorCodes.INVALID_WEBAUTHN_CREDENTIAL,
				message: 'No passkeys registered',
				status: 400,
			});
		}

		const rpID = Config.auth.passkeys.rpId;

		const options = await generateAuthenticationOptions({
			rpID,
			allowCredentials: credentials.map((cred) => ({
				id: cred.credentialId,
				transports: cred.transports
					? (Array.from(cred.transports) as Array<'usb' | 'nfc' | 'ble' | 'internal' | 'cable' | 'hybrid'>)
					: undefined,
			})),
			userVerification: 'required',
		});

		await this.saveWebAuthnChallenge(options.challenge, {context: 'sudo', userId});
		return options;
	}

	async verifySudoMfa(params: SudoMfaVerificationParams): Promise<SudoMfaVerificationResult> {
		const {userId, method, code, webauthnResponse, webauthnChallenge} = params;

		const user = await this.repository.findUnique(userId);
		const hasMfa = (user?.authenticatorTypes?.size ?? 0) > 0;
		if (!user || !hasMfa) {
			return {success: false, error: 'MFA not enabled'};
		}

		switch (method) {
			case 'totp': {
				if (!code) {
					return {success: false, error: 'TOTP code is required'};
				}
				if (!user.totpSecret) {
					return {success: false, error: 'TOTP is not enabled'};
				}
				const isValid = await this.verifyMfaCode({
					userId,
					mfaSecret: user.totpSecret,
					code,
					allowBackup: true,
				});
				return {success: isValid, error: isValid ? undefined : 'Invalid TOTP code'};
			}

			case 'sms': {
				if (!code) {
					return {success: false, error: 'SMS code is required'};
				}
				if (!user.authenticatorTypes?.has(UserAuthenticatorTypes.SMS)) {
					return {success: false, error: 'SMS MFA is not enabled'};
				}
				const isValid = await this.verifySmsMfaCode(userId, code);
				return {success: isValid, error: isValid ? undefined : 'Invalid SMS code'};
			}

			case 'webauthn': {
				if (!webauthnResponse || !webauthnChallenge) {
					return {success: false, error: 'WebAuthn response and challenge are required'};
				}
				if (!user.authenticatorTypes?.has(UserAuthenticatorTypes.WEBAUTHN)) {
					return {success: false, error: 'WebAuthn is not enabled'};
				}
				try {
					await this.verifyWebAuthnAuthentication(userId, webauthnResponse, webauthnChallenge, 'sudo');
					return {success: true};
				} catch {
					return {success: false, error: 'WebAuthn verification failed'};
				}
			}

			default:
				return {success: false, error: 'Invalid MFA method'};
		}
	}

	async getAvailableMfaMethods(
		userId: UserID,
	): Promise<{totp: boolean; sms: boolean; webauthn: boolean; has_mfa: boolean}> {
		const user = await this.repository.findUnique(userId);
		if (!user) {
			return {totp: false, sms: false, webauthn: false, has_mfa: false};
		}

		const hasMfa = (user.authenticatorTypes?.size ?? 0) > 0;

		return {
			totp: user.totpSecret !== null,
			sms: user.authenticatorTypes?.has(UserAuthenticatorTypes.SMS) ?? false,
			webauthn: user.authenticatorTypes?.has(UserAuthenticatorTypes.WEBAUTHN) ?? false,
			has_mfa: hasMfa,
		};
	}

	private getWebAuthnChallengeCacheKey(challenge: string): string {
		return `webauthn:challenge:${challenge}`;
	}

	private async saveWebAuthnChallenge(
		challenge: string,
		entry: {context: WebAuthnChallengeContext; userId?: UserID; ticket?: string},
	): Promise<void> {
		const key = this.getWebAuthnChallengeCacheKey(challenge);
		await this.cacheService.set(
			key,
			{context: entry.context, userId: entry.userId?.toString(), ticket: entry.ticket},
			WEBAUTHN_CHALLENGE_TTL_SECONDS,
		);
	}

	private async consumeWebAuthnChallenge(
		challenge: string,
		expectedContext: WebAuthnChallengeContext,
		{userId, ticket}: {userId?: UserID; ticket?: string} = {},
	): Promise<void> {
		const key = this.getWebAuthnChallengeCacheKey(challenge);
		const cached = await this.cacheService.get<{context: WebAuthnChallengeContext; userId?: string; ticket?: string}>(
			key,
		);

		const challengeMatches =
			cached &&
			cached.context === expectedContext &&
			(userId === undefined || cached.userId === undefined || cached.userId === userId.toString()) &&
			(ticket === undefined || cached.ticket === undefined || cached.ticket === ticket);

		if (!challengeMatches) {
			throw this.createChallengeError(expectedContext);
		}

		await this.cacheService.delete(key);
	}

	private createChallengeError(context: WebAuthnChallengeContext): FluxerAPIError {
		const isRegistration = context === 'registration';
		return new FluxerAPIError({
			code: isRegistration ? APIErrorCodes.INVALID_WEBAUTHN_CREDENTIAL : APIErrorCodes.PASSKEY_AUTHENTICATION_FAILED,
			message: isRegistration ? 'Failed to verify WebAuthn credential' : 'Passkey authentication failed',
			status: isRegistration ? 400 : 401,
		});
	}
}
