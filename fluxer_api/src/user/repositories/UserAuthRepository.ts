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

import type {PhoneVerificationToken, UserID} from '~/BrandedTypes';
import type {
	AuthSessionRow,
	EmailRevertTokenRow,
	EmailVerificationTokenRow,
	PasswordResetTokenRow,
	PhoneTokenRow,
} from '~/database/CassandraTypes';
import type {
	AuthSession,
	EmailRevertToken,
	EmailVerificationToken,
	MfaBackupCode,
	PasswordResetToken,
	WebAuthnCredential,
} from '~/Models';
import {AuthSessionRepository} from './auth/AuthSessionRepository';
import {IpAuthorizationRepository} from './auth/IpAuthorizationRepository';
import {MfaBackupCodeRepository} from './auth/MfaBackupCodeRepository';
import {PendingVerificationRepository} from './auth/PendingVerificationRepository';
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
	private pendingVerificationRepository: PendingVerificationRepository;

	constructor(userAccountRepository: IUserAccountRepository) {
		this.authSessionRepository = new AuthSessionRepository();
		this.mfaBackupCodeRepository = new MfaBackupCodeRepository();
		this.tokenRepository = new TokenRepository();
		this.ipAuthorizationRepository = new IpAuthorizationRepository(userAccountRepository);
		this.webAuthnRepository = new WebAuthnRepository();
		this.pendingVerificationRepository = new PendingVerificationRepository();
	}

	async listAuthSessions(userId: UserID): Promise<Array<AuthSession>> {
		return this.authSessionRepository.listAuthSessions(userId);
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

	async updateUserActivity(userId: UserID, clientIp: string): Promise<void> {
		return this.ipAuthorizationRepository.updateUserActivity(userId, clientIp);
	}

	async checkIpAuthorized(userId: UserID, ip: string): Promise<boolean> {
		return this.ipAuthorizationRepository.checkIpAuthorized(userId, ip);
	}

	async createAuthorizedIp(userId: UserID, ip: string): Promise<void> {
		return this.ipAuthorizationRepository.createAuthorizedIp(userId, ip);
	}

	async createIpAuthorizationToken(userId: UserID, token: string): Promise<void> {
		return this.ipAuthorizationRepository.createIpAuthorizationToken(userId, token);
	}

	async authorizeIpByToken(token: string): Promise<{userId: UserID; email: string} | null> {
		return this.ipAuthorizationRepository.authorizeIpByToken(token);
	}

	async getAuthorizedIps(userId: UserID): Promise<Array<{ip: string}>> {
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

	async createPendingVerification(userId: UserID, createdAt: Date, metadata: Map<string, string>): Promise<void> {
		return this.pendingVerificationRepository.createPendingVerification(userId, createdAt, metadata);
	}

	async deletePendingVerification(userId: UserID): Promise<void> {
		return this.pendingVerificationRepository.deletePendingVerification(userId);
	}
}
