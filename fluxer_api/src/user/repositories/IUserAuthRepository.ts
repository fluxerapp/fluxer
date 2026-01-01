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

export interface IUserAuthRepository {
	listAuthSessions(userId: UserID): Promise<Array<AuthSession>>;
	getAuthSessionByToken(sessionIdHash: Buffer): Promise<AuthSession | null>;
	createAuthSession(sessionData: AuthSessionRow): Promise<AuthSession>;
	updateAuthSessionLastUsed(sessionIdHash: Buffer): Promise<void>;
	deleteAuthSessions(userId: UserID, sessionIdHashes: Array<Buffer>): Promise<void>;
	revokeAuthSession(sessionIdHash: Buffer): Promise<void>;
	deleteAllAuthSessions(userId: UserID): Promise<void>;

	listMfaBackupCodes(userId: UserID): Promise<Array<MfaBackupCode>>;
	createMfaBackupCodes(userId: UserID, codes: Array<string>): Promise<Array<MfaBackupCode>>;
	clearMfaBackupCodes(userId: UserID): Promise<void>;
	consumeMfaBackupCode(userId: UserID, code: string): Promise<void>;
	deleteAllMfaBackupCodes(userId: UserID): Promise<void>;

	getEmailVerificationToken(token: string): Promise<EmailVerificationToken | null>;
	createEmailVerificationToken(tokenData: EmailVerificationTokenRow): Promise<EmailVerificationToken>;
	deleteEmailVerificationToken(token: string): Promise<void>;

	getPasswordResetToken(token: string): Promise<PasswordResetToken | null>;
	createPasswordResetToken(tokenData: PasswordResetTokenRow): Promise<PasswordResetToken>;
	deletePasswordResetToken(token: string): Promise<void>;

	getEmailRevertToken(token: string): Promise<EmailRevertToken | null>;
	createEmailRevertToken(tokenData: EmailRevertTokenRow): Promise<EmailRevertToken>;
	deleteEmailRevertToken(token: string): Promise<void>;

	createPhoneToken(token: PhoneVerificationToken, phone: string, userId: UserID | null): Promise<void>;
	getPhoneToken(token: PhoneVerificationToken): Promise<PhoneTokenRow | null>;
	deletePhoneToken(token: PhoneVerificationToken): Promise<void>;
	updateUserActivity(userId: UserID, clientIp: string): Promise<void>;
	checkIpAuthorized(userId: UserID, ip: string): Promise<boolean>;
	createAuthorizedIp(userId: UserID, ip: string): Promise<void>;
	createIpAuthorizationToken(userId: UserID, token: string): Promise<void>;
	authorizeIpByToken(token: string): Promise<{userId: UserID; email: string} | null>;
	deleteAllAuthorizedIps(userId: UserID): Promise<void>;

	listWebAuthnCredentials(userId: UserID): Promise<Array<WebAuthnCredential>>;
	getWebAuthnCredential(userId: UserID, credentialId: string): Promise<WebAuthnCredential | null>;
	createWebAuthnCredential(
		userId: UserID,
		credentialId: string,
		publicKey: Buffer,
		counter: bigint,
		transports: Set<string> | null,
		name: string,
	): Promise<void>;
	updateWebAuthnCredentialCounter(userId: UserID, credentialId: string, counter: bigint): Promise<void>;
	updateWebAuthnCredentialLastUsed(userId: UserID, credentialId: string): Promise<void>;
	updateWebAuthnCredentialName(userId: UserID, credentialId: string, name: string): Promise<void>;
	deleteWebAuthnCredential(userId: UserID, credentialId: string): Promise<void>;
	getUserIdByCredentialId(credentialId: string): Promise<UserID | null>;
	deleteAllWebAuthnCredentials(userId: UserID): Promise<void>;

	createPendingVerification(userId: UserID, createdAt: Date, metadata: Map<string, string>): Promise<void>;
	deletePendingVerification(userId: UserID): Promise<void>;
}
