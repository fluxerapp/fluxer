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

export interface IUserAuthRepository {
	listAuthSessions(userId: UserID): Promise<Array<AuthSession>>;
	listAuthSessionTombstones(userId: UserID): Promise<Array<AuthSessionTombstone>>;
	getAuthSessionByToken(sessionIdHash: Buffer): Promise<AuthSession | null>;
	createAuthSession(sessionData: AuthSessionRow): Promise<AuthSession>;
	updateAuthSessionLastUsed(sessionIdHash: Buffer): Promise<void>;
	deleteAuthSessions(userId: UserID, sessionIdHashes: Array<Buffer>): Promise<void>;
	revokeAuthSession(sessionIdHash: Buffer): Promise<void>;
	deleteAllAuthSessions(userId: UserID): Promise<void>;
	recordCountrySighting(userId: UserID, country: string): Promise<void>;
	hasCountrySightingOutsideSet(userId: UserID, countryCodes: Iterable<string>): Promise<boolean>;
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
	deleteAllPasswordResetTokens(userId: UserID): Promise<void>;
	getEmailRevertToken(token: string): Promise<EmailRevertToken | null>;
	createEmailRevertToken(tokenData: EmailRevertTokenRow): Promise<EmailRevertToken>;
	deleteEmailRevertToken(token: string): Promise<void>;
	createPhoneToken(token: PhoneVerificationToken, phone: string, userId: UserID | null): Promise<void>;
	getPhoneToken(token: PhoneVerificationToken): Promise<PhoneTokenRow | null>;
	deletePhoneToken(token: PhoneVerificationToken): Promise<void>;
	checkIpAuthorized(userId: UserID, ip: string): Promise<boolean>;
	createAuthorizedIp(userId: UserID, ip: string): Promise<void>;
	createIpAuthorizationToken(userId: UserID, token: string, email: string): Promise<void>;
	authorizeIpByToken(token: string): Promise<{
		userId: UserID;
		email: string;
	} | null>;
	getAuthorizedIps(userId: UserID): Promise<
		Array<{
			ip: string;
		}>
	>;
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
}
