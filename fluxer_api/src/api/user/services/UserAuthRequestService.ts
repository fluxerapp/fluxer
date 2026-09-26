// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import * as AuthMfa from '@app/api/auth/AuthMfa';
import * as AuthPhone from '@app/api/auth/AuthPhone';
import {requireEmailVerified} from '@app/api/auth/EmailVerificationUtils';
import {visibleWebAuthnCredentials} from '@app/api/auth/services/PasskeyRelyingParty';
import type {SudoVerificationResult} from '@app/api/auth/services/SudoVerificationService';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {User} from '@app/api/models/User';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import * as UserAuth from '@app/api/user/services/UserAuth';
import {mapUserToPrivateResponse, mapWebAuthnCredentialToResponse} from '@app/api/user/UserMappers';
import {GuildVerificationLevel} from '@fluxer/constants/src/GuildConstants';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {PhoneAddNotEligibleError} from '@fluxer/errors/src/domains/auth/PhoneAddNotEligibleError';
import type {
	DisableTotpRequest,
	EnableMfaTotpRequest,
	MfaBackupCodesRequest,
	MfaBackupCodesResponse,
	PhoneSendVerificationRequest,
	PhoneSendVerificationResponse,
	PhoneVerifyRequest,
	PhoneVerifyResponse,
	SudoMfaMethodsResponse,
	WebAuthnChallengeResponse,
	WebAuthnCredentialListResponse,
	WebAuthnCredentialUpdateRequest,
	WebAuthnRegisterRequest,
	WebAuthnTwoFactorRequest,
	WebAuthnTwoFactorResponse,
} from '@fluxer/schema/src/domains/auth/AuthSchemas';

interface UserAuthWithSudoRequest<T> {
	user: User;
	data: T;
	sudoContext: SudoVerificationResult;
}

interface UserAuthRequest<T> {
	user: User;
	data: T;
}

interface UserAuthWebAuthnUpdateRequest {
	user: User;
	credentialId: string;
	data: WebAuthnCredentialUpdateRequest;
}

interface UserAuthWebAuthnRegisterRequest {
	user: User;
	data: WebAuthnRegisterRequest;
}

interface UserAuthWebAuthnDeleteRequest {
	user: User;
	credentialId: string;
}

export class UserAuthRequestService {
	constructor(
		private apiContext: ApiContext,
		private userRepository: IUserRepository,
		private guildRepository: IGuildRepositoryAggregate,
	) {}

	private async assertPhoneEligible(user: User): Promise<void> {
		if (user.hasVerifiedPhone) {
			return;
		}
		if (user.authenticatorTypes.has(UserAuthenticatorTypes.TOTP)) {
			return;
		}
		if (user.suspiciousActivityFlags !== 0) {
			return;
		}
		const guildIds = await this.userRepository.getUserGuildIds(user.id);
		if (guildIds.length > 0) {
			const guilds = await this.guildRepository.listGuilds(guildIds);
			if (guilds.some((g) => g.verificationLevel >= GuildVerificationLevel.VERY_HIGH)) {
				return;
			}
		}
		throw new PhoneAddNotEligibleError();
	}

	async enableTotp({
		user,
		data,
		sudoContext,
	}: UserAuthWithSudoRequest<EnableMfaTotpRequest>): Promise<MfaBackupCodesResponse> {
		requireEmailVerified(user, 'mfa');
		const backupCodes = await UserAuth.enableMfaTotp(this.apiContext, {
			user,
			secret: data.secret,
			code: data.code,
			sudoContext,
		});
		return this.toBackupCodesResponse(backupCodes);
	}

	async disableTotp({user, data, sudoContext}: UserAuthWithSudoRequest<DisableTotpRequest>): Promise<void> {
		await UserAuth.disableMfaTotp(this.apiContext, {
			user,
			code: data.code,
			sudoContext,
		});
	}

	async getBackupCodes({
		user,
		data,
		sudoContext,
	}: UserAuthWithSudoRequest<MfaBackupCodesRequest>): Promise<MfaBackupCodesResponse> {
		const backupCodes = await UserAuth.getMfaBackupCodes(this.apiContext, {
			user,
			regenerate: data.regenerate,
			sudoContext,
		});
		return this.toBackupCodesResponse(backupCodes);
	}

	async sendPhoneVerificationCode({
		user,
		data,
		clientIp,
	}: UserAuthRequest<PhoneSendVerificationRequest> & {
		clientIp: string;
	}): Promise<PhoneSendVerificationResponse> {
		await this.assertPhoneEligible(user);
		const result = await AuthPhone.sendPhoneVerificationCode(this.apiContext, data.phone, user.id, {
			clientIp,
			channel: data.channel,
		});
		if (result.channel === 'inbound_challenge') {
			return {
				channel: 'inbound_challenge',
				challenge_code: result.challengeCode,
				our_number: result.ourNumber,
				expires_at: result.expiresAt.toISOString(),
				reason: result.reason,
			};
		}
		return {channel: result.channel};
	}

	async verifyPhoneCode({user, data}: UserAuthRequest<PhoneVerifyRequest>): Promise<PhoneVerifyResponse> {
		await this.assertPhoneEligible(user);
		await AuthPhone.verifyPhoneCode(this.apiContext, data.phone, data.code, user.id);
		return {verified: true};
	}

	async startInboundPhoneChallenge(user: User): Promise<{
		challenge_code: string;
		our_number: string;
		expires_at: string;
	}> {
		const issued = await AuthPhone.startInboundPhoneChallenge(this.apiContext, user.id);
		return {
			challenge_code: issued.challengeCode,
			our_number: issued.ourNumber,
			expires_at: issued.expiresAt.toISOString(),
		};
	}

	async forgetAuthorizedIps(user: User): Promise<void> {
		await this.userRepository.deleteAllAuthorizedIps(user.id);
	}

	async listWebAuthnCredentials(user: User): Promise<WebAuthnCredentialListResponse> {
		const credentials = await this.userRepository.listWebAuthnCredentials(user.id);
		const legacyRpId = this.apiContext.services.config.auth.passkeys.rpId;
		return visibleWebAuthnCredentials(credentials).map((cred) => mapWebAuthnCredentialToResponse(cred, legacyRpId));
	}

	async generateWebAuthnRegistrationOptions(
		user: User,
		origin: string | undefined,
	): Promise<WebAuthnChallengeResponse> {
		requireEmailVerified(user, 'mfa');
		const options = await AuthMfa.generateWebAuthnRegistrationOptions(this.apiContext, user.id, origin);
		return this.toWebAuthnChallengeResponse(options);
	}

	async registerWebAuthnCredential({user, data}: UserAuthWebAuthnRegisterRequest): Promise<void> {
		requireEmailVerified(user, 'mfa');
		await AuthMfa.verifyWebAuthnRegistration(this.apiContext, user.id, data.response, data.challenge, data.name);
	}

	async renameWebAuthnCredential({user, credentialId, data}: UserAuthWebAuthnUpdateRequest): Promise<void> {
		await AuthMfa.renameWebAuthnCredential(this.apiContext, user.id, credentialId, data.name);
	}

	async deleteWebAuthnCredential({user, credentialId}: UserAuthWebAuthnDeleteRequest): Promise<void> {
		await AuthMfa.deleteWebAuthnCredential(this.apiContext, user.id, credentialId);
	}

	async setWebAuthnTwoFactor({
		user,
		data,
	}: UserAuthRequest<WebAuthnTwoFactorRequest>): Promise<WebAuthnTwoFactorResponse> {
		if (data.enabled) {
			requireEmailVerified(user, 'mfa');
		}
		const result = await AuthMfa.setWebAuthnTwoFactor(this.apiContext, user.id, data.enabled);
		return {
			user: mapUserToPrivateResponse(result.user),
			backup_codes: result.backupCodes
				? result.backupCodes.map((backupCode) => ({code: backupCode.code, consumed: backupCode.consumed}))
				: null,
		};
	}

	async listSudoMfaMethods(user: User): Promise<SudoMfaMethodsResponse> {
		return AuthMfa.getAvailableMfaMethods(this.apiContext, user.id);
	}

	async getSudoWebAuthnOptions(user: User, origin: string | undefined): Promise<WebAuthnChallengeResponse> {
		const options = await AuthMfa.generateWebAuthnOptionsForSudo(this.apiContext, user.id, origin);
		return this.toWebAuthnChallengeResponse(options);
	}

	private toWebAuthnChallengeResponse(options: {challenge: string}): WebAuthnChallengeResponse {
		const response: Record<string, unknown> & {
			challenge: string;
		} = {
			...options,
			challenge: options.challenge,
		};
		return response;
	}

	private toBackupCodesResponse(
		backupCodes: Array<{
			code: string;
			consumed: boolean;
		}>,
	): MfaBackupCodesResponse {
		return {
			backup_codes: backupCodes.map((code) => ({
				code: code.code,
				consumed: code.consumed,
			})),
		};
	}
}
