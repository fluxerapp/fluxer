// SPDX-License-Identifier: AGPL-3.0-or-later

import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import {createUserID} from '../BrandedTypes';
import {deleteOneOrMany, fetchMany, fetchOne, fetchPage, upsertOne} from '../database/CassandraQueryExecution';
import type {
	AdminAuditLogRow,
	BannedAvatarHashRow,
	BannedFileShaRow,
	BannedProfileSubstringRow,
	BannedProfileSubstringScope,
	BannedUrlDomainRow,
	BannedUrlRow,
} from '../database/types/AdminArchiveTypes';
import {isAccountPolicyContactDomainReputationExempt} from '../risk/AccountPolicyService';
import {isIpBanExempt} from '../risk/IpBanExemptions';
import {
	AdminAuditLogs,
	BannedAvatarHashes,
	BannedEmails,
	BannedFileShas,
	BannedIps,
	BannedPhonePrefixes,
	BannedPhrases,
	BannedProfileSubstrings,
	BannedUrlDomains,
	BannedUrls,
	DisposableEmailDomains,
	SuspiciousEmailDomains,
} from '../Tables';
import {parseIpBanEntry, tryParseSingleIp} from '../utils/IpRangeUtils';
import {canonicalizeStoredPhrase} from '../utils/PhraseBlocklistNormalization';
import type {
	AdminAuditLog,
	BannedIpEntry,
	BannedIpKind,
	DisposableEmailDomainPage,
	IAdminRepository,
} from './IAdminRepository';

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
const IS_EMAIL_DOMAIN_SUSPICIOUS_QUERY = SuspiciousEmailDomains.select({
	where: SuspiciousEmailDomains.where.eq('domain'),
});
const createLoadSuspiciousEmailDomainsQuery = (limit?: number) =>
	limit ? SuspiciousEmailDomains.select({limit}) : SuspiciousEmailDomains.select();
const IS_EMAIL_DOMAIN_DISPOSABLE_QUERY = DisposableEmailDomains.select({
	where: DisposableEmailDomains.where.eq('domain'),
});
const createLoadDisposableEmailDomainsQuery = (limit?: number) =>
	limit ? DisposableEmailDomains.select({limit}) : DisposableEmailDomains.select();
const IS_PHRASE_BANNED_QUERY = BannedPhrases.select({
	where: BannedPhrases.where.eq('phrase'),
});
const LOAD_ALL_BANNED_PHRASES_QUERY = BannedPhrases.select();
const LOAD_ALL_BANNED_PHONE_PREFIXES_QUERY = BannedPhonePrefixes.select();
const IS_URL_BANNED_QUERY = BannedUrls.select({
	where: BannedUrls.where.eq('url_canonical'),
});
const LOAD_ALL_BANNED_URLS_QUERY = BannedUrls.select();
const IS_URL_DOMAIN_BANNED_QUERY = BannedUrlDomains.select({
	where: BannedUrlDomains.where.eq('domain'),
});
const LOAD_ALL_BANNED_URL_DOMAINS_QUERY = BannedUrlDomains.select();
const IS_FILE_SHA_BANNED_QUERY = BannedFileShas.select({
	where: BannedFileShas.where.eq('sha256_hex'),
});
const LOAD_ALL_BANNED_FILE_SHAS_QUERY = BannedFileShas.select();
const IS_AVATAR_HASH_BANNED_QUERY = BannedAvatarHashes.select({
	where: BannedAvatarHashes.where.eq('hash_short'),
});
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

	async banIp(ip: string): Promise<void> {
		if (isIpBanExempt(ip)) {
			return;
		}
		const canonicalIp = canonicalizeBannedIpEntry(ip);
		await upsertOne(
			BannedIps.insert({
				ip: canonicalIp,
				ban_kind: 'permanent',
				reason: 'platform_admin_enforcement',
				expires_at: null,
				created_at: new Date(),
			}),
		);
	}

	async banIpTemp(ip: string, ttlSeconds: number): Promise<void> {
		if (!Number.isInteger(ttlSeconds) || ttlSeconds <= 0) {
			throw new RangeError('Temporary IP ban TTL must be a positive integer');
		}
		if (isIpBanExempt(ip)) {
			return;
		}
		const canonicalIp = canonicalizeBannedIpEntry(ip);
		await upsertOne(
			BannedIps.insertWithTtl(
				{
					ip: canonicalIp,
					ban_kind: 'temporary_24h',
					reason: 'abusive_api_access_patterns',
					expires_at: new Date(Date.now() + ttlSeconds * 1000),
					created_at: new Date(),
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
		return rows.map((row) => ({
			ip: row.ip,
			kind: parseBannedIpKind(row.ban_kind),
			reason: row.reason ?? null,
			expiresAt: row.expires_at ?? null,
			createdAt: row.created_at ?? null,
		}));
	}

	async isEmailBanned(email: string): Promise<boolean> {
		const emailLower = email.toLowerCase();
		const result = await fetchOne<{
			email_lower: string;
		}>(IS_EMAIL_BANNED_QUERY.bind({email_lower: emailLower}));
		return !!result;
	}

	async banEmail(email: string): Promise<void> {
		const emailLower = email.toLowerCase();
		await upsertOne(BannedEmails.insert({email_lower: emailLower}));
	}

	async unbanEmail(email: string): Promise<void> {
		const emailLower = email.toLowerCase();
		await deleteOneOrMany(BannedEmails.deleteByPk({email_lower: emailLower}));
	}

	async isEmailDomainSuspicious(domain: string): Promise<boolean> {
		const domainLower = domain.toLowerCase();
		if (isAccountPolicyContactDomainReputationExempt(domainLower)) return false;
		const result = await fetchOne<{
			domain: string;
		}>(IS_EMAIL_DOMAIN_SUSPICIOUS_QUERY.bind({domain: domainLower}));
		return !!result;
	}

	async addSuspiciousEmailDomain(domain: string): Promise<void> {
		const domainLower = domain.toLowerCase();
		await upsertOne(SuspiciousEmailDomains.insert({domain: domainLower}));
	}

	async removeSuspiciousEmailDomain(domain: string): Promise<void> {
		const domainLower = domain.toLowerCase();
		await deleteOneOrMany(SuspiciousEmailDomains.deleteByPk({domain: domainLower}));
	}

	async listSuspiciousEmailDomains(limit?: number): Promise<Array<string>> {
		const rows = await fetchMany<{
			domain: string;
		}>(createLoadSuspiciousEmailDomainsQuery(limit).bind({}));
		return rows.map((row) => row.domain);
	}

	async isEmailDomainDisposable(domain: string): Promise<boolean> {
		const domainLower = domain.toLowerCase();
		if (isAccountPolicyContactDomainReputationExempt(domainLower)) return false;
		const result = await fetchOne<{
			domain: string;
		}>(IS_EMAIL_DOMAIN_DISPOSABLE_QUERY.bind({domain: domainLower}));
		return !!result;
	}

	async addDisposableEmailDomain(domain: string): Promise<void> {
		const domainLower = domain.toLowerCase();
		await upsertOne(DisposableEmailDomains.insert({domain: domainLower}));
	}

	async removeDisposableEmailDomain(domain: string): Promise<void> {
		const domainLower = domain.toLowerCase();
		await deleteOneOrMany(DisposableEmailDomains.deleteByPk({domain: domainLower}));
	}

	async listDisposableEmailDomains(limit?: number): Promise<Array<string>> {
		const rows = await fetchMany<{
			domain: string;
		}>(createLoadDisposableEmailDomainsQuery(limit).bind({}));
		return rows.map((row) => row.domain);
	}

	async listDisposableEmailDomainsPage(limit: number, pageState?: string | null): Promise<DisposableEmailDomainPage> {
		const page = await fetchPage<{
			domain: string;
		}>(createLoadDisposableEmailDomainsQuery().bind({}), undefined, {
			pageSize: limit,
			pageState,
		});
		return {
			domains: page.rows.map((row) => row.domain),
			pageState: page.pageState,
		};
	}

	async isPhraseBanned(phrase: string): Promise<boolean> {
		const phraseLower = canonicalizeStoredPhrase(phrase);
		const result = await fetchOne<{
			phrase: string;
		}>(IS_PHRASE_BANNED_QUERY.bind({phrase: phraseLower}));
		return !!result;
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

	async loadAllBannedPhonePrefixes(): Promise<Array<string>> {
		const rows = await fetchMany<{
			prefix: string;
		}>(LOAD_ALL_BANNED_PHONE_PREFIXES_QUERY.bind({}));
		return rows.map((row) => row.prefix);
	}

	async isUrlBanned(url: string): Promise<boolean> {
		const canonical = url.toLowerCase();
		const result = await fetchOne<{
			url_canonical: string;
		}>(IS_URL_BANNED_QUERY.bind({url_canonical: canonical}));
		return !!result;
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

	async isUrlDomainBanned(domain: string): Promise<boolean> {
		const d = domain.toLowerCase();
		const result = await fetchOne<{
			domain: string;
		}>(IS_URL_DOMAIN_BANNED_QUERY.bind({domain: d}));
		return !!result;
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

	async isFileShaBanned(sha256Hex: string): Promise<boolean> {
		const h = sha256Hex.toLowerCase();
		const result = await fetchOne<{
			sha256_hex: string;
		}>(IS_FILE_SHA_BANNED_QUERY.bind({sha256_hex: h}));
		return !!result;
	}

	async banFileSha(row: BannedFileShaRow): Promise<void> {
		await upsertOne(BannedFileShas.insert({...row, sha256_hex: row.sha256_hex.toLowerCase()}));
	}

	async unbanFileSha(sha256Hex: string): Promise<void> {
		await deleteOneOrMany(BannedFileShas.deleteByPk({sha256_hex: sha256Hex.toLowerCase()}));
	}

	async loadAllBannedFileShas(): Promise<Array<BannedFileShaRow>> {
		return fetchMany<BannedFileShaRow>(LOAD_ALL_BANNED_FILE_SHAS_QUERY.bind({}));
	}

	async isAvatarHashBanned(hashShort: string): Promise<boolean> {
		const h = hashShort.toLowerCase();
		const result = await fetchOne<{
			hash_short: string;
		}>(IS_AVATAR_HASH_BANNED_QUERY.bind({hash_short: h}));
		return !!result;
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
