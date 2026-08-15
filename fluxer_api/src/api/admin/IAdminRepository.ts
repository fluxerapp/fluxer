// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '../BrandedTypes';
import type {
	AdminAuditLogRow,
	BannedAvatarHashRow,
	BannedFileShaRow,
	BannedProfileSubstringRow,
	BannedProfileSubstringScope,
	BannedUrlDomainRow,
	BannedUrlRow,
} from '../database/types/AdminArchiveTypes';

export interface AdminAuditLog {
	logId: bigint;
	adminUserId: UserID;
	targetType: string;
	targetId: bigint;
	action: string;
	auditLogReason: string | null;
	metadata: Map<string, string>;
	createdAt: Date;
}

export type BannedIpKind = 'permanent' | 'temporary_24h';

export interface BannedIpEntry {
	ip: string;
	kind: BannedIpKind;
	reason: string | null;
	expiresAt: Date | null;
	createdAt: Date | null;
}

export abstract class IAdminRepository {
	abstract createAuditLog(log: AdminAuditLogRow): Promise<AdminAuditLog>;

	abstract getAuditLog(logId: bigint): Promise<AdminAuditLog | null>;

	abstract listAuditLogsByIds(logIds: Array<bigint>): Promise<Array<AdminAuditLog>>;

	abstract listAllAuditLogsPaginated(limit: number, lastLogId?: bigint): Promise<Array<AdminAuditLog>>;

	abstract isIpBanned(ip: string): Promise<boolean>;

	abstract banIp(ip: string): Promise<void>;

	abstract banIpTemp(ip: string, ttlSeconds: number): Promise<void>;

	abstract unbanIp(ip: string): Promise<void>;

	abstract loadAllBannedIpEntries(): Promise<Array<BannedIpEntry>>;

	abstract isEmailBanned(email: string): Promise<boolean>;

	abstract banEmail(email: string): Promise<void>;

	abstract unbanEmail(email: string): Promise<void>;

	abstract isEmailDomainSuspicious(domain: string): Promise<boolean>;

	abstract addSuspiciousEmailDomain(domain: string): Promise<void>;

	abstract removeSuspiciousEmailDomain(domain: string): Promise<void>;

	abstract isEmailDomainDisposable(domain: string): Promise<boolean>;

	abstract addDisposableEmailDomain(domain: string): Promise<void>;

	abstract removeDisposableEmailDomain(domain: string): Promise<void>;

	abstract listDisposableEmailDomains(limit?: number): Promise<Array<string>>;

	abstract isPhraseBanned(phrase: string): Promise<boolean>;

	abstract banPhrase(phrase: string): Promise<void>;

	abstract unbanPhrase(phrase: string): Promise<void>;

	abstract loadAllBannedPhrases(): Promise<Array<string>>;

	abstract loadAllBannedPhonePrefixes(): Promise<Array<string>>;

	abstract loadAllBannedIps(): Promise<Set<string>>;

	abstract isUrlBanned(url: string): Promise<boolean>;

	abstract banUrl(row: BannedUrlRow): Promise<void>;

	abstract unbanUrl(url: string): Promise<void>;

	abstract loadAllBannedUrls(): Promise<Array<BannedUrlRow>>;

	abstract isUrlDomainBanned(domain: string): Promise<boolean>;

	abstract banUrlDomain(row: BannedUrlDomainRow): Promise<void>;

	abstract unbanUrlDomain(domain: string): Promise<void>;

	abstract loadAllBannedUrlDomains(): Promise<Array<BannedUrlDomainRow>>;

	abstract isFileShaBanned(sha256Hex: string): Promise<boolean>;

	abstract banFileSha(row: BannedFileShaRow): Promise<void>;

	abstract unbanFileSha(sha256Hex: string): Promise<void>;

	abstract loadAllBannedFileShas(): Promise<Array<BannedFileShaRow>>;

	abstract isAvatarHashBanned(hashShort: string): Promise<boolean>;

	abstract banAvatarHash(row: BannedAvatarHashRow): Promise<void>;

	abstract unbanAvatarHash(hashShort: string): Promise<void>;

	abstract loadAllBannedAvatarHashes(): Promise<Array<BannedAvatarHashRow>>;

	abstract banProfileSubstring(row: BannedProfileSubstringRow): Promise<void>;

	abstract unbanProfileSubstring(scope: BannedProfileSubstringScope, substring: string): Promise<void>;

	abstract loadAllBannedProfileSubstrings(): Promise<Array<BannedProfileSubstringRow>>;
}
