// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PhoneVerificationToken, UserID} from '../../BrandedTypes';
import type {
	AuthSessionRow,
	EmailRevertTokenRow,
	EmailVerificationTokenRow,
	PasswordResetTokenRow,
	PhoneTokenRow,
} from '../../database/types/AuthTypes';
import type {AuthSession, AuthSessionTombstone} from '../../models/AuthSession';
import type {EmailRevertToken} from '../../models/EmailRevertToken';
import type {EmailVerificationToken} from '../../models/EmailVerificationToken';
import type {MfaBackupCode} from '../../models/MfaBackupCode';
import type {PasswordResetToken} from '../../models/PasswordResetToken';
import type {WebAuthnCredential} from '../../models/WebAuthnCredential';
import {AuthSessionRepository} from './auth/AuthSessionRepository';
import {IpAuthorizationRepository} from './auth/IpAuthorizationRepository';
import {MfaBackupCodeRepository} from './auth/MfaBackupCodeRepository';
import {TokenRepository} from './auth/TokenRepository';
import {WebAuthnRepository} from './auth/WebAuthnRepository';
import type {IUserAccountRepository} from './IUserAccountRepository';
import type {IUserAuthRepository} from './IUserAuthRepository';

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

	async revokeAuthSession(sessionIdHash: Buffer): Promise<void> {
		const session = await this.getAuthSessionByToken(sessionIdHash);
		if (!session) return;
		await this.deleteAuthSessions(session.userId, [sessionIdHash]);
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
	): Promise<void> {
		return this.webAuthnRepository.createWebAuthnCredential(userId, credentialId, publicKey, counter, transports, name);
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
