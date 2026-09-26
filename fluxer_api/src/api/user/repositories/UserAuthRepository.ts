// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PhoneVerificationToken, UserID} from '@app/api/BrandedTypes';
import type {
	AuthSessionRow,
	EmailRevertTokenRow,
	EmailVerificationTokenRow,
	PasswordResetTokenRow,
	PhoneTokenRow,
} from '@app/api/database/types/AuthTypes';
import type {AuthSession, AuthSessionTombstone} from '@app/api/models/AuthSession';
import type {EmailRevertToken} from '@app/api/models/EmailRevertToken';
import type {EmailVerificationToken} from '@app/api/models/EmailVerificationToken';
import type {MfaBackupCode} from '@app/api/models/MfaBackupCode';
import type {PasswordResetToken} from '@app/api/models/PasswordResetToken';
import type {WebAuthnCredential} from '@app/api/models/WebAuthnCredential';
import {AuthSessionRepository} from '@app/api/user/repositories/auth/AuthSessionRepository';
import {IpAuthorizationRepository} from '@app/api/user/repositories/auth/IpAuthorizationRepository';
import {MfaBackupCodeRepository} from '@app/api/user/repositories/auth/MfaBackupCodeRepository';
import {TokenRepository} from '@app/api/user/repositories/auth/TokenRepository';
import {WebAuthnRepository} from '@app/api/user/repositories/auth/WebAuthnRepository';
import type {IUserAccountRepository} from '@app/api/user/repositories/IUserAccountRepository';
import type {IUserAuthRepository} from '@app/api/user/repositories/IUserAuthRepository';

export class UserAuthRepository implements IUserAuthRepository {
	private authSessionRepository: AuthSessionRepository;
	private mfaBackupCodeRepository: MfaBackupCodeRepository;
	private tokenRepository: TokenRepository;
	private ipAuthorizationRepository: IpAuthorizationRepository;
	private webAuthnRepository: WebAuthnRepository;

	constructor(userAccountRepository: IUserAccountRepository) {
		this.authSessionRepository = new AuthSessionRepository();
		this.mfaBackupCodeRepository = new MfaBackupCodeRepository();
		this.tokenRepository = new TokenRepository();
		this.ipAuthorizationRepository = new IpAuthorizationRepository(userAccountRepository);
		this.webAuthnRepository = new WebAuthnRepository();
	}

	async listAuthSessions(userId: UserID): Promise<Array<AuthSession>> {
		return this.authSessionRepository.listAuthSessions(userId);
	}

	async listAuthSessionTombstones(userId: UserID): Promise<Array<AuthSessionTombstone>> {
		return this.authSessionRepository.listAuthSessionTombstones(userId);
	}

	async getAuthSessionByToken(sessionIdHash: Buffer): Promise<AuthSession | null> {
		return this.authSessionRepository.getAuthSessionByToken(sessionIdHash);
	}

	async createAuthSession(sessionData: AuthSessionRow): Promise<AuthSession> {
		return this.authSessionRepository.createAuthSession(sessionData);
	}

	async updateAuthSessionLastUsed(sessionIdHash: Buffer): Promise<void> {
		const session = await this.getAuthSessionByToken(sessionIdHash);
		if (!session) return;
		await this.authSessionRepository.updateAuthSessionLastUsed(sessionIdHash);
	}

	async deleteAuthSessions(userId: UserID, sessionIdHashes: Array<Buffer>): Promise<void> {
		return this.authSessionRepository.deleteAuthSessions(userId, sessionIdHashes);
	}

	async deleteAllAuthSessions(userId: UserID): Promise<void> {
		return this.authSessionRepository.deleteAllAuthSessions(userId);
	}

	async recordCountrySighting(userId: UserID, country: string): Promise<void> {
		return this.authSessionRepository.recordCountrySighting(userId, country);
	}

	async hasCountrySightingOutsideSet(userId: UserID, countryCodes: Iterable<string>): Promise<boolean> {
		return this.authSessionRepository.hasCountrySightingOutsideSet(userId, countryCodes);
	}

	async listMfaBackupCodes(userId: UserID): Promise<Array<MfaBackupCode>> {
		return this.mfaBackupCodeRepository.listMfaBackupCodes(userId);
	}

	async createMfaBackupCodes(userId: UserID, codes: Array<string>): Promise<Array<MfaBackupCode>> {
		return this.mfaBackupCodeRepository.createMfaBackupCodes(userId, codes);
	}

	async clearMfaBackupCodes(userId: UserID): Promise<void> {
		return this.mfaBackupCodeRepository.clearMfaBackupCodes(userId);
	}

	async consumeMfaBackupCode(userId: UserID, code: string): Promise<void> {
		return this.mfaBackupCodeRepository.consumeMfaBackupCode(userId, code);
	}

	async deleteAllMfaBackupCodes(userId: UserID): Promise<void> {
		return this.mfaBackupCodeRepository.deleteAllMfaBackupCodes(userId);
	}

	async getEmailVerificationToken(token: string): Promise<EmailVerificationToken | null> {
		return this.tokenRepository.getEmailVerificationToken(token);
	}

	async createEmailVerificationToken(tokenData: EmailVerificationTokenRow): Promise<EmailVerificationToken> {
		return this.tokenRepository.createEmailVerificationToken(tokenData);
	}

	async deleteEmailVerificationToken(token: string): Promise<void> {
		return this.tokenRepository.deleteEmailVerificationToken(token);
	}

	async getPasswordResetToken(token: string): Promise<PasswordResetToken | null> {
		return this.tokenRepository.getPasswordResetToken(token);
	}

	async createPasswordResetToken(tokenData: PasswordResetTokenRow): Promise<PasswordResetToken> {
		return this.tokenRepository.createPasswordResetToken(tokenData);
	}

	async deletePasswordResetToken(token: string): Promise<void> {
		return this.tokenRepository.deletePasswordResetToken(token);
	}

	async deleteAllPasswordResetTokens(userId: UserID): Promise<void> {
		return this.tokenRepository.deleteAllPasswordResetTokens(userId);
	}

	async getEmailRevertToken(token: string): Promise<EmailRevertToken | null> {
		return this.tokenRepository.getEmailRevertToken(token);
	}

	async createEmailRevertToken(tokenData: EmailRevertTokenRow): Promise<EmailRevertToken> {
		return this.tokenRepository.createEmailRevertToken(tokenData);
	}

	async deleteEmailRevertToken(token: string): Promise<void> {
		return this.tokenRepository.deleteEmailRevertToken(token);
	}

	async createPhoneToken(token: PhoneVerificationToken, phone: string, userId: UserID | null): Promise<void> {
		return this.tokenRepository.createPhoneToken(token, phone, userId);
	}

	async getPhoneToken(token: PhoneVerificationToken): Promise<PhoneTokenRow | null> {
		return this.tokenRepository.getPhoneToken(token);
	}

	async deletePhoneToken(token: PhoneVerificationToken): Promise<void> {
		return this.tokenRepository.deletePhoneToken(token);
	}

	async checkIpAuthorized(userId: UserID, ip: string): Promise<boolean> {
		return this.ipAuthorizationRepository.checkIpAuthorized(userId, ip);
	}

	async createAuthorizedIp(userId: UserID, ip: string): Promise<void> {
		return this.ipAuthorizationRepository.createAuthorizedIp(userId, ip);
	}

	async createIpAuthorizationToken(userId: UserID, token: string, email: string): Promise<void> {
		return this.ipAuthorizationRepository.createIpAuthorizationToken(userId, token, email);
	}

	async authorizeIpByToken(token: string): Promise<{
		userId: UserID;
		email: string;
	} | null> {
		return this.ipAuthorizationRepository.authorizeIpByToken(token);
	}

	async getAuthorizedIps(userId: UserID): Promise<
		Array<{
			ip: string;
		}>
	> {
		return this.ipAuthorizationRepository.getAuthorizedIps(userId);
	}

	async deleteAllAuthorizedIps(userId: UserID): Promise<void> {
		return this.ipAuthorizationRepository.deleteAllAuthorizedIps(userId);
	}

	async listWebAuthnCredentials(userId: UserID): Promise<Array<WebAuthnCredential>> {
		return this.webAuthnRepository.listWebAuthnCredentials(userId);
	}

	async getWebAuthnCredential(userId: UserID, credentialId: string): Promise<WebAuthnCredential | null> {
		return this.webAuthnRepository.getWebAuthnCredential(userId, credentialId);
	}

	async createWebAuthnCredential(
		userId: UserID,
		credentialId: string,
		publicKey: Buffer,
		counter: bigint,
		transports: Set<string> | null,
		name: string,
		rpId: string | null,
	): Promise<void> {
		return this.webAuthnRepository.createWebAuthnCredential(
			userId,
			credentialId,
			publicKey,
			counter,
			transports,
			name,
			rpId,
		);
	}

	async updateWebAuthnCredentialCounter(userId: UserID, credentialId: string, counter: bigint): Promise<void> {
		return this.webAuthnRepository.updateWebAuthnCredentialCounter(userId, credentialId, counter);
	}

	async updateWebAuthnCredentialLastUsed(userId: UserID, credentialId: string): Promise<void> {
		return this.webAuthnRepository.updateWebAuthnCredentialLastUsed(userId, credentialId);
	}

	async updateWebAuthnCredentialName(userId: UserID, credentialId: string, name: string): Promise<void> {
		return this.webAuthnRepository.updateWebAuthnCredentialName(userId, credentialId, name);
	}

	async setWebAuthnCredentialSupersededBy(userId: UserID, credentialId: string, supersededBy: string): Promise<void> {
		return this.webAuthnRepository.setWebAuthnCredentialSupersededBy(userId, credentialId, supersededBy);
	}

	async deleteWebAuthnCredential(userId: UserID, credentialId: string): Promise<void> {
		return this.webAuthnRepository.deleteWebAuthnCredential(userId, credentialId);
	}

	async getUserIdByCredentialId(credentialId: string): Promise<UserID | null> {
		return this.webAuthnRepository.getUserIdByCredentialId(credentialId);
	}

	async deleteAllWebAuthnCredentials(userId: UserID): Promise<void> {
		return this.webAuthnRepository.deleteAllWebAuthnCredentials(userId);
	}
}
