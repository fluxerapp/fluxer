// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog, BannedIpEntry, BannedIpKind, IAdminRepository} from '@app/api/admin/IAdminRepository';
import {createUserID} from '@app/api/BrandedTypes';
import {isIpBanExempt} from '@app/api/ban/IpBanExemptions';
import {ContentBlocklistCategory} from '@app/api/constants/ContentModeration';
import {
	deleteOneOrMany,
	executeConditional,
	fetchMany,
	fetchOne,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import type {
	AdminAuditLogRow,
	BannedAvatarHashRow,
	BannedFileShaRow,
	BannedProfileSubstringRow,
	BannedProfileSubstringScope,
	BannedUrlDomainRow,
	BannedUrlRow,
} from '@app/api/database/types/AdminArchiveTypes';
import {
	AdminAuditLogs,
	BannedAvatarHashes,
	BannedEmails,
	BannedFileShas,
	BannedIps,
	BannedPhrases,
	BannedProfileSubstrings,
	BannedUrlDomains,
	BannedUrls,
} from '@app/api/Tables';
import {parseIpBanEntry, tryParseSingleIp} from '@app/api/utils/IpRangeUtils';
import {canonicalizeStoredPhrase} from '@app/api/utils/PhraseBlocklistNormalization';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';

const FETCH_AUDIT_LOG_BY_ID_QUERY = AdminAuditLogs.select({
	where: AdminAuditLogs.where.eq('log_id'),
});
const FETCH_AUDIT_LOGS_BY_IDS_QUERY = AdminAuditLogs.select({
	where: AdminAuditLogs.where.in('log_id', 'log_ids'),
});
const LOAD_ALL_BANNED_IPS_QUERY = BannedIps.select();
const IS_EMAIL_BANNED_QUERY = BannedEmails.select({
	where: BannedEmails.where.eq('email_lower'),
});
const LOAD_ALL_BANNED_EMAILS_QUERY = BannedEmails.select();

function getEmailBlocklistKeys(email: string): Array<string> {
	const emailLower = email.trim().toLowerCase();
	const atIndex = emailLower.lastIndexOf('@');
	if (atIndex <= 0) {
		return [emailLower];
	}
	const labels = emailLower.slice(atIndex + 1).split('.');
	const keys = [emailLower];
	for (let index = 0; index < labels.length - 1; index++) {
		keys.push(`@${labels.slice(index).join('.')}`);
	}
	return keys;
}
const LOAD_ALL_BANNED_PHRASES_QUERY = BannedPhrases.select();
const LOAD_ALL_BANNED_URLS_QUERY = BannedUrls.select();
const LOAD_ALL_BANNED_URL_DOMAINS_QUERY = BannedUrlDomains.select();
const LOAD_ALL_BANNED_FILE_SHAS_QUERY = BannedFileShas.select();
const LOAD_ALL_BANNED_AVATAR_HASHES_QUERY = BannedAvatarHashes.select();
const LOAD_ALL_BANNED_PROFILE_SUBSTRINGS_QUERY = BannedProfileSubstrings.select();
const createListAllAuditLogsPaginatedQuery = (limit: number) =>
	AdminAuditLogs.select({
		where: AdminAuditLogs.where.tokenGt('log_id', 'last_log_id'),
		limit,
	});
const createListAllAuditLogsFirstPageQuery = (limit: number) =>
	AdminAuditLogs.select({
		limit,
	});

function parseBannedIpKind(value: string | null | undefined): BannedIpKind {
	return value === 'temporary_24h' ? 'temporary_24h' : 'permanent';
}

function canonicalizeBannedIpEntry(value: string): string {
	return parseIpBanEntry(value)?.canonical ?? value;
}

export class AdminRepository implements IAdminRepository {
	async createAuditLog(log: AdminAuditLogRow): Promise<AdminAuditLog> {
		await upsertOne(AdminAuditLogs.insert(log));
		return this.mapRowToAuditLog(log);
	}

	async getAuditLog(logId: bigint): Promise<AdminAuditLog | null> {
		const row = await fetchOne<AdminAuditLogRow>(FETCH_AUDIT_LOG_BY_ID_QUERY.bind({log_id: logId}));
		return row ? this.mapRowToAuditLog(row) : null;
	}

	async listAuditLogsByIds(logIds: Array<bigint>): Promise<Array<AdminAuditLog>> {
		if (logIds.length === 0) {
			return [];
		}
		const rows = await fetchMany<AdminAuditLogRow>(FETCH_AUDIT_LOGS_BY_IDS_QUERY.bind({log_ids: logIds}));
		return rows.map((row) => this.mapRowToAuditLog(row));
	}

	async listAllAuditLogsPaginated(limit: number, lastLogId?: bigint): Promise<Array<AdminAuditLog>> {
		let rows: Array<AdminAuditLogRow>;
		if (lastLogId) {
			const query = createListAllAuditLogsPaginatedQuery(limit);
			rows = await fetchMany<AdminAuditLogRow>(query.bind({last_log_id: lastLogId}));
		} else {
			const query = createListAllAuditLogsFirstPageQuery(limit);
			rows = await fetchMany<AdminAuditLogRow>(query.bind({}));
		}
		return rows.map((row) => this.mapRowToAuditLog(row));
	}

	async isIpBanned(ip: string): Promise<boolean> {
		if (isIpBanExempt(ip)) {
			return false;
		}
		const candidate = tryParseSingleIp(ip);
		if (!candidate) {
			return false;
		}
		const sameIpDecisionKey = getSameIpDecisionKey(candidate.canonical);
		const entries = await this.loadAllBannedIpEntries();
		for (const entry of entries) {
			const parsed = parseIpBanEntry(entry.ip);
			if (!parsed) {
				continue;
			}
			if (parsed.type === 'single') {
				if (parsed.family === candidate.family && parsed.canonical === candidate.canonical) {
					return true;
				}
				if (sameIpDecisionKey && getSameIpDecisionKey(parsed.canonical) === sameIpDecisionKey) {
					return true;
				}
				continue;
			}
			if (parsed.family === candidate.family && candidate.value >= parsed.start && candidate.value <= parsed.end) {
				return true;
			}
		}
		return false;
	}

	async banIp(ip: string, ttlSeconds: number | null = null): Promise<void> {
		await this.writeIpBan(ip, 'platform_admin_enforcement', ttlSeconds);
	}

	async banIpTemp(ip: string, ttlSeconds: number): Promise<void> {
		await this.writeIpBan(ip, 'abusive_api_access_patterns', ttlSeconds);
	}

	private async writeIpBan(ip: string, reason: string, ttlSeconds: number | null): Promise<void> {
		if (ttlSeconds !== null && (!Number.isInteger(ttlSeconds) || ttlSeconds <= 0)) {
			throw new RangeError('Temporary IP ban TTL must be a positive integer');
		}
		if (isIpBanExempt(ip)) {
			return;
		}
		const canonicalIp = canonicalizeBannedIpEntry(ip);
		const createdAt = new Date();
		if (ttlSeconds === null) {
			await upsertOne(
				BannedIps.insert({
					ip: canonicalIp,
					ban_kind: 'permanent',
					reason,
					expires_at: null,
					created_at: createdAt,
				}),
			);
			return;
		}
		await upsertOne(
			BannedIps.insertWithTtl(
				{
					ip: canonicalIp,
					ban_kind: 'temporary_24h',
					reason,
					expires_at: new Date(createdAt.getTime() + ttlSeconds * 1000),
					created_at: createdAt,
				},
				ttlSeconds,
			),
		);
	}

	async unbanIp(ip: string): Promise<void> {
		const deleteKeys = new Set<string>([ip]);
		const parsed = parseIpBanEntry(ip);
		if (parsed) {
			deleteKeys.add(parsed.canonical);
			const entries = await this.loadAllBannedIpEntries();
			for (const entry of entries) {
				const stored = parseIpBanEntry(entry.ip);
				if (stored?.canonical === parsed.canonical) {
					deleteKeys.add(entry.ip);
				}
			}
		}
		await Promise.all(Array.from(deleteKeys).map((key) => deleteOneOrMany(BannedIps.deleteByPk({ip: key}))));
	}

	async loadAllBannedIps(): Promise<Set<string>> {
		const entries = await this.loadAllBannedIpEntries();
		return new Set(entries.map((row) => row.ip));
	}

	async loadAllBannedIpEntries(): Promise<Array<BannedIpEntry>> {
		const rows = await fetchMany<{
			ip: string;
			ban_kind?: string | null;
			reason?: string | null;
			expires_at?: Date | null;
			created_at?: Date | null;
		}>(LOAD_ALL_BANNED_IPS_QUERY.bind({}));
		const now = Date.now();
		return rows
			.filter((row) => !row.expires_at || row.expires_at.getTime() > now)
			.map((row) => ({
				ip: row.ip,
				kind: parseBannedIpKind(row.ban_kind),
				reason: row.reason ?? null,
				expiresAt: row.expires_at ?? null,
				createdAt: row.created_at ?? null,
			}));
	}

	async isEmailBanned(email: string): Promise<boolean> {
		for (const key of getEmailBlocklistKeys(email)) {
			const result = await fetchOne<{
				email_lower: string;
			}>(IS_EMAIL_BANNED_QUERY.bind({email_lower: key}));
			if (result) {
				return true;
			}
		}
		return false;
	}

	async banEmail(email: string): Promise<void> {
		const emailLower = email.toLowerCase();
		await upsertOne(BannedEmails.insert({email_lower: emailLower}));
	}

	async unbanEmail(email: string): Promise<void> {
		const emailLower = email.toLowerCase();
		await deleteOneOrMany(BannedEmails.deleteByPk({email_lower: emailLower}));
	}

	async loadAllBannedEmails(): Promise<Array<string>> {
		const rows = await fetchMany<{
			email_lower: string;
		}>(LOAD_ALL_BANNED_EMAILS_QUERY.bind({}));
		return rows.map((row) => row.email_lower);
	}

	async banPhrase(phrase: string): Promise<void> {
		const phraseLower = canonicalizeStoredPhrase(phrase);
		await upsertOne(BannedPhrases.insert({phrase: phraseLower}));
	}

	async unbanPhrase(phrase: string): Promise<void> {
		const phraseLower = canonicalizeStoredPhrase(phrase);
		await deleteOneOrMany(BannedPhrases.deleteByPk({phrase: phraseLower}));
	}

	async loadAllBannedPhrases(): Promise<Array<string>> {
		const rows = await fetchMany<{
			phrase: string;
		}>(LOAD_ALL_BANNED_PHRASES_QUERY.bind({}));
		return rows.map((row) => row.phrase);
	}

	async banUrl(row: BannedUrlRow): Promise<void> {
		await upsertOne(BannedUrls.insert({...row, url_canonical: row.url_canonical.toLowerCase()}));
	}

	async unbanUrl(url: string): Promise<void> {
		await deleteOneOrMany(BannedUrls.deleteByPk({url_canonical: url.toLowerCase()}));
	}

	async loadAllBannedUrls(): Promise<Array<BannedUrlRow>> {
		return fetchMany<BannedUrlRow>(LOAD_ALL_BANNED_URLS_QUERY.bind({}));
	}

	async banUrlDomain(row: BannedUrlDomainRow): Promise<void> {
		await upsertOne(BannedUrlDomains.insert({...row, domain: row.domain.toLowerCase()}));
	}

	async unbanUrlDomain(domain: string): Promise<void> {
		await deleteOneOrMany(BannedUrlDomains.deleteByPk({domain: domain.toLowerCase()}));
	}

	async loadAllBannedUrlDomains(): Promise<Array<BannedUrlDomainRow>> {
		return fetchMany<BannedUrlDomainRow>(LOAD_ALL_BANNED_URL_DOMAINS_QUERY.bind({}));
	}

	async banFileSha(row: BannedFileShaRow): Promise<void> {
		await upsertOne(BannedFileShas.insert({...row, sha256_hex: row.sha256_hex.toLowerCase()}));
	}

	async unbanFileSha(sha256Hex: string): Promise<void> {
		await deleteOneOrMany(BannedFileShas.deleteByPk({sha256_hex: sha256Hex.toLowerCase()}));
	}

	async unbanFeedFileSha(sha256Hex: string): Promise<boolean> {
		return executeConditional(
			BannedFileShas.conditionalDeleteByPk(
				{sha256_hex: sha256Hex.toLowerCase()},
				{added_by: null, category: ContentBlocklistCategory.MALWARE_BAZAAR},
			),
		);
	}

	async loadAllBannedFileShas(): Promise<Array<BannedFileShaRow>> {
		return fetchMany<BannedFileShaRow>(LOAD_ALL_BANNED_FILE_SHAS_QUERY.bind({}));
	}

	async banAvatarHash(row: BannedAvatarHashRow): Promise<void> {
		await upsertOne(BannedAvatarHashes.insert({...row, hash_short: row.hash_short.toLowerCase()}));
	}

	async unbanAvatarHash(hashShort: string): Promise<void> {
		await deleteOneOrMany(BannedAvatarHashes.deleteByPk({hash_short: hashShort.toLowerCase()}));
	}

	async loadAllBannedAvatarHashes(): Promise<Array<BannedAvatarHashRow>> {
		return fetchMany<BannedAvatarHashRow>(LOAD_ALL_BANNED_AVATAR_HASHES_QUERY.bind({}));
	}

	async banProfileSubstring(row: BannedProfileSubstringRow): Promise<void> {
		const canonical = canonicalizeStoredPhrase(row.substring);
		if (!canonical) return;
		await upsertOne(BannedProfileSubstrings.insert({...row, substring: canonical}));
	}

	async unbanProfileSubstring(scope: BannedProfileSubstringScope, substring: string): Promise<void> {
		const canonical = canonicalizeStoredPhrase(substring);
		if (!canonical) return;
		await deleteOneOrMany(BannedProfileSubstrings.deleteByPk({scope, substring: canonical}));
	}

	async loadAllBannedProfileSubstrings(): Promise<Array<BannedProfileSubstringRow>> {
		return fetchMany<BannedProfileSubstringRow>(LOAD_ALL_BANNED_PROFILE_SUBSTRINGS_QUERY.bind({}));
	}

	private mapRowToAuditLog(row: AdminAuditLogRow): AdminAuditLog {
		return {
			logId: row.log_id,
			adminUserId: createUserID(row.admin_user_id),
			targetType: row.target_type,
			targetId: row.target_id,
			action: row.action,
			auditLogReason: row.audit_log_reason,
			metadata: row.metadata || new Map(),
			createdAt: row.created_at,
		};
	}
}
