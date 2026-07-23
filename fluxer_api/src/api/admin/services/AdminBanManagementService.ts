// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {NotFoundError} from '@fluxer/errors/src/domains/core/NotFoundError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {IpInfoLookupResult, IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import type {ApiContext} from '../../ApiContext';
import {createUserID, type UserID} from '../../BrandedTypes';
import {
	BANNED_AVATAR_HASHES_REFRESH_CHANNEL,
	BANNED_FILE_SHAS_REFRESH_CHANNEL,
	BANNED_PHRASES_REFRESH_CHANNEL,
	BANNED_PROFILE_SUBSTRINGS_REFRESH_CHANNEL,
	BANNED_URL_DOMAINS_REFRESH_CHANNEL,
	BANNED_URLS_REFRESH_CHANNEL,
	ContentBlocklistCategory,
	ContentBlocklistSeverity,
} from '../../constants/ContentModeration';
import {IP_BAN_REFRESH_CHANNEL} from '../../constants/IpBan';
import type {BannedProfileSubstringScope} from '../../database/types/AdminArchiveTypes';
import {Logger} from '../../Logger';
import {bannedAvatarHashCache} from '../../middleware/BannedAvatarHashCache';
import {fileShaCache} from '../../middleware/FileShaCache';
import {ipBanCache} from '../../middleware/IpBanMiddleware';
import {phraseBlocklistCache} from '../../middleware/PhraseBlocklistCache';
import {profileSubstringBlocklistCache} from '../../middleware/ProfileSubstringBlocklistCache';
import {urlBlocklistCache} from '../../middleware/UrlBlocklistCache';
import {
	getSuspiciousIpSkipReason,
	hasHighCgnatBlastRadiusRisk,
	isSingleIpBanCandidate,
} from '../../risk/IpBanCgnatGuard';
import {isIpBanExempt} from '../../risk/IpBanExemptions';
import type {ISuspiciousIpRepository} from '../../risk/SuspiciousIpRepository';
import {tryParseSingleIp} from '../../utils/IpRangeUtils';
import {canonicalizeStoredPhrase} from '../../utils/PhraseBlocklistNormalization';
import {canonicalizeUrl} from '../../utils/UrlNormalizer';
import type {IAdminRepository} from '../IAdminRepository';
import type {AdminAuditService} from './AdminAuditService';

interface AdminBanManagementServiceDeps {
	apiContext: ApiContext;
	adminRepository: IAdminRepository;
	auditService: AdminAuditService;
	ipInfoService: IpInfoService;
	suspiciousIpRepository: ISuspiciousIpRepository;
}

interface AdminBlocklistAuditParams {
	adminUserId: UserID;
	auditLogReason: string | null;
	targetType: string;
	action: string;
	metadata: Map<string, string>;
}

function stripAvatarAnimationPrefix(hash: string): string {
	return hash.startsWith('a_') ? hash.substring(2) : hash;
}

function normalizeAvatarHashes(hashes: Array<string>): Array<string> {
	return Array.from(new Set(hashes.map((hash) => stripAvatarAnimationPrefix(hash.toLowerCase()))));
}

function withReasonMetadata(entries: Array<[string, string]>, reason: string | undefined): Map<string, string> {
	if (!reason) {
		return new Map(entries);
	}
	const reasonEntry: [string, string] = ['reason', reason];
	return new Map([...entries, reasonEntry]);
}

export class AdminBanManagementService {
	constructor(private readonly deps: AdminBanManagementServiceDeps) {}

	async banIp(
		data: {
			ip: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository, auditService} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		if (isIpBanExempt(data.ip)) {
			await auditService.createAuditLog({
				adminUserId,
				targetType: 'ip',
				targetId: BigInt(0),
				action: 'ban_ip_skipped_exempt',
				auditLogReason,
				metadata: new Map([['ip', data.ip]]),
			});
			return;
		}
		if (await this.shouldSkipIpBanForCgnat(data.ip)) {
			await auditService.createAuditLog({
				adminUserId,
				targetType: 'ip',
				targetId: BigInt(0),
				action: 'ban_ip_skipped_cgnat',
				auditLogReason,
				metadata: new Map([['ip', data.ip]]),
			});
			return;
		}
		await adminRepository.banIp(data.ip);
		ipBanCache.ban(data.ip);
		await cacheService.publish(IP_BAN_REFRESH_CHANNEL, 'refresh');
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'ip',
			action: 'ban_ip',
			auditLogReason,
			metadata: new Map([['ip', data.ip]]),
		});
	}

	async unbanIp(
		data: {
			ip: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		await adminRepository.unbanIp(data.ip);
		ipBanCache.unban(data.ip);
		await cacheService.publish(IP_BAN_REFRESH_CHANNEL, 'refresh');
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'ip',
			action: 'unban_ip',
			auditLogReason,
			metadata: new Map([['ip', data.ip]]),
		});
	}

	async checkIpBan(data: {ip: string}): Promise<{
		banned: boolean;
	}> {
		const banned = ipBanCache.isBanned(data.ip);
		return {banned};
	}

	async markSuspiciousIpForScheduledDeletion(
		data: {
			ip: string;
			sourceUserId: UserID;
			deletionReasonCode: number;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<void> {
		const parsed = tryParseSingleIp(data.ip);
		if (!parsed) {
			await this.deps.auditService.createAuditLog({
				adminUserId,
				targetType: 'ip',
				targetId: BigInt(0),
				action: 'mark_suspicious_ip_skipped_invalid',
				auditLogReason,
				metadata: new Map([['ip', data.ip]]),
			});
			return;
		}
		let info: IpInfoLookupResult;
		try {
			info = await this.deps.ipInfoService.lookup(parsed.canonical, {
				source: 'admin.scheduled_deletion_suspicious_ip',
				reason: 'pre_write_suspicious_ip_guard',
				metadata: {
					source_user_id: data.sourceUserId.toString(),
					deletion_reason_code: data.deletionReasonCode,
				},
			});
		} catch (error) {
			Logger.warn({error, ip: parsed.canonical}, 'IPInfo guard failed while marking suspicious IP');
			return;
		}
		const skipReason = getSuspiciousIpSkipReason(info);
		if (skipReason) {
			Logger.info({ip: parsed.canonical, skipReason}, 'Skipping suspicious IP marker from scheduled deletion');
			await this.deps.auditService.createAuditLog({
				adminUserId,
				targetType: 'ip',
				targetId: BigInt(0),
				action: `mark_suspicious_ip_skipped_${skipReason}`,
				auditLogReason,
				metadata: new Map([
					['ip', parsed.canonical],
					['source_user_id', data.sourceUserId.toString()],
					['deletion_reason_code', data.deletionReasonCode.toString()],
					['provider_name', info.anonymous.providerName ?? ''],
				]),
			});
			return;
		}
		await this.deps.suspiciousIpRepository.markSuspiciousIp({
			ip: parsed.canonical,
			source: 'scheduled_deletion',
			reason: 'account_scheduled_for_deletion',
			sourceUserId: data.sourceUserId,
			deletionReasonCode: data.deletionReasonCode,
			providerName: info.anonymous.providerName,
			asn: info.asn.number,
			asnName: info.asn.name,
			asnType: info.asn.type,
			riskNote: info.riskNote,
		});
		await this.deps.auditService.createAuditLog({
			adminUserId,
			targetType: 'ip',
			targetId: BigInt(0),
			action: 'mark_suspicious_ip',
			auditLogReason,
			metadata: new Map([
				['ip', parsed.canonical],
				['source_user_id', data.sourceUserId.toString()],
				['deletion_reason_code', data.deletionReasonCode.toString()],
				['provider_name', info.anonymous.providerName ?? ''],
			]),
		});
	}

	private async shouldSkipIpBanForCgnat(ip: string): Promise<boolean> {
		if (!isSingleIpBanCandidate(ip)) {
			return false;
		}
		try {
			const highRisk = await hasHighCgnatBlastRadiusRisk(ip, this.deps.ipInfoService, {
				source: 'admin.ip_ban',
				reason: 'pre_write_cgnat_guard',
			});
			if (highRisk) {
				Logger.warn({ip}, 'Skipping IP ban because IPInfo indicates high CGNAT blast-radius risk');
			}
			return highRisk;
		} catch (error) {
			Logger.warn({error, ip}, 'IPInfo CGNAT guard failed while adding IP ban');
			return false;
		}
	}

	async banEmail(
		data: {
			email: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		await adminRepository.banEmail(data.email);
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'email',
			action: 'ban_email',
			auditLogReason,
			metadata: new Map([['email', data.email]]),
		});
	}

	async unbanEmail(
		data: {
			email: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		await adminRepository.unbanEmail(data.email);
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'email',
			action: 'unban_email',
			auditLogReason,
			metadata: new Map([['email', data.email]]),
		});
	}

	async checkEmailBan(data: {email: string}): Promise<{
		banned: boolean;
	}> {
		const {adminRepository} = this.deps;
		const banned = await adminRepository.isEmailBanned(data.email);
		return {banned};
	}

	async addSuspiciousEmailDomain(
		data: {
			domain: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		await adminRepository.addSuspiciousEmailDomain(data.domain);
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'email_domain',
			action: 'add_suspicious_email_domain',
			auditLogReason,
			metadata: new Map([['domain', data.domain]]),
		});
	}

	async removeSuspiciousEmailDomain(
		data: {
			domain: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		await adminRepository.removeSuspiciousEmailDomain(data.domain);
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'email_domain',
			action: 'remove_suspicious_email_domain',
			auditLogReason,
			metadata: new Map([['domain', data.domain]]),
		});
	}

	async checkSuspiciousEmailDomain(data: {domain: string}): Promise<{
		banned: boolean;
	}> {
		const {adminRepository} = this.deps;
		const banned = await adminRepository.isEmailDomainSuspicious(data.domain);
		return {banned};
	}

	async banPhrase(
		data: {
			phrase: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		await adminRepository.banPhrase(data.phrase);
		phraseBlocklistCache.add(data.phrase);
		await cacheService.publish(BANNED_PHRASES_REFRESH_CHANNEL, 'refresh');
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'phrase',
			action: 'ban_phrase',
			auditLogReason,
			metadata: new Map([['phrase', data.phrase]]),
		});
	}

	async unbanPhrase(
		data: {
			phrase: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		await adminRepository.unbanPhrase(data.phrase);
		phraseBlocklistCache.remove(data.phrase);
		await cacheService.publish(BANNED_PHRASES_REFRESH_CHANNEL, 'refresh');
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'phrase',
			action: 'unban_phrase',
			auditLogReason,
			metadata: new Map([['phrase', data.phrase]]),
		});
	}

	async checkPhraseBan(data: {phrase: string}): Promise<{
		banned: boolean;
	}> {
		return {banned: phraseBlocklistCache.isPhraseBanned(data.phrase)};
	}

	async banUrl(
		data: {
			url: string;
			category?: string;
			severity?: number;
			source_url?: string;
			notes?: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		const canonical = canonicalizeUrl(data.url);
		if (!canonical) throw new Error('URL could not be canonicalized');
		await adminRepository.banUrl({
			url_canonical: canonical,
			category: data.category ?? ContentBlocklistCategory.MANUAL,
			severity: data.severity ?? ContentBlocklistSeverity.BLOCK,
			source_url: data.source_url ?? null,
			added_at: new Date(),
			added_by: adminUserId,
			notes: data.notes ?? null,
		});
		urlBlocklistCache.addExactUrl(canonical);
		await cacheService.publish(BANNED_URLS_REFRESH_CHANNEL, 'refresh');
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'url',
			action: 'ban_url',
			auditLogReason,
			metadata: new Map([
				['url', canonical],
				['category', data.category ?? ContentBlocklistCategory.MANUAL],
			]),
		});
	}

	async unbanUrl(
		data: {
			url: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		const canonical = canonicalizeUrl(data.url);
		if (!canonical) throw new Error('URL could not be canonicalized');
		await adminRepository.unbanUrl(canonical);
		urlBlocklistCache.removeExactUrl(canonical);
		await cacheService.publish(BANNED_URLS_REFRESH_CHANNEL, 'refresh');
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'url',
			action: 'unban_url',
			auditLogReason,
			metadata: new Map([['url', canonical]]),
		});
	}

	async checkUrlBan(data: {url: string}): Promise<{
		banned: boolean;
	}> {
		return {banned: urlBlocklistCache.isUrlBanned(data.url)};
	}

	async banUrlDomain(
		data: {
			domain: string;
			match_subdomains?: boolean;
			category?: string;
			severity?: number;
			source_url?: string;
			notes?: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		const d = data.domain.toLowerCase();
		const matchSubs = data.match_subdomains ?? true;
		await adminRepository.banUrlDomain({
			domain: d,
			match_subdomains: matchSubs,
			category: data.category ?? ContentBlocklistCategory.MANUAL,
			severity: data.severity ?? ContentBlocklistSeverity.BLOCK,
			source_url: data.source_url ?? null,
			added_at: new Date(),
			added_by: adminUserId,
			notes: data.notes ?? null,
		});
		urlBlocklistCache.addDomain(d);
		await cacheService.publish(BANNED_URL_DOMAINS_REFRESH_CHANNEL, 'refresh');
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'url_domain',
			action: 'ban_url_domain',
			auditLogReason,
			metadata: new Map([
				['domain', d],
				['match_subdomains', String(matchSubs)],
			]),
		});
	}

	async unbanUrlDomain(
		data: {
			domain: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		const d = data.domain.toLowerCase();
		await adminRepository.unbanUrlDomain(d);
		urlBlocklistCache.removeDomain(d);
		await cacheService.publish(BANNED_URL_DOMAINS_REFRESH_CHANNEL, 'refresh');
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'url_domain',
			action: 'unban_url_domain',
			auditLogReason,
			metadata: new Map([['domain', d]]),
		});
	}

	async checkUrlDomainBan(data: {domain: string}): Promise<{
		banned: boolean;
	}> {
		return {banned: urlBlocklistCache.isHostnameBanned(data.domain)};
	}

	async banFileSha(
		data: {
			sha256_hex: string;
			category?: string;
			severity?: number;
			content_type?: string;
			source_url?: string;
			notes?: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		const hex = data.sha256_hex.toLowerCase();
		await adminRepository.banFileSha({
			sha256_hex: hex,
			category: data.category ?? ContentBlocklistCategory.MANUAL,
			severity: data.severity ?? ContentBlocklistSeverity.BLOCK,
			content_type: data.content_type ?? null,
			source_url: data.source_url ?? null,
			added_at: new Date(),
			added_by: adminUserId,
			notes: data.notes ?? null,
		});
		fileShaCache.add(hex);
		await cacheService.publish(BANNED_FILE_SHAS_REFRESH_CHANNEL, 'refresh');
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'file_sha',
			action: 'ban_file_sha',
			auditLogReason,
			metadata: new Map([['sha256', hex]]),
		});
	}

	async unbanFileSha(
		data: {
			sha256_hex: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		const hex = data.sha256_hex.toLowerCase();
		await adminRepository.unbanFileSha(hex);
		fileShaCache.remove(hex);
		await cacheService.publish(BANNED_FILE_SHAS_REFRESH_CHANNEL, 'refresh');
		await this.createBlocklistAuditLog({
			adminUserId,
			targetType: 'file_sha',
			action: 'unban_file_sha',
			auditLogReason,
			metadata: new Map([['sha256', hex]]),
		});
	}

	async checkFileShaBan(data: {sha256_hex: string}): Promise<{
		banned: boolean;
	}> {
		return {banned: fileShaCache.isBanned(data.sha256_hex)};
	}

	async banAvatarHash(
		data: {
			hashes: Array<string>;
			category?: string;
			severity?: number;
			source_url?: string;
			reason?: string;
			notes?: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		const normalized = normalizeAvatarHashes(data.hashes);
		for (const hash of normalized) {
			await adminRepository.banAvatarHash({
				hash_short: hash,
				category: data.category ?? ContentBlocklistCategory.MANUAL,
				severity: data.severity ?? ContentBlocklistSeverity.BLOCK,
				source_url: data.source_url ?? null,
				added_at: new Date(),
				added_by: adminUserId,
				notes: data.notes ?? null,
			});
			bannedAvatarHashCache.add(hash);
			await this.createBlocklistAuditLog({
				adminUserId,
				targetType: 'avatar_hash',
				action: 'ban_avatar_hash',
				auditLogReason,
				metadata: withReasonMetadata([['hash_short', hash]], data.reason),
			});
		}
		await cacheService.publish(BANNED_AVATAR_HASHES_REFRESH_CHANNEL, 'refresh');
	}

	async unbanAvatarHash(
		data: {
			hashes: Array<string>;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		const normalized = normalizeAvatarHashes(data.hashes);
		for (const hash of normalized) {
			await adminRepository.unbanAvatarHash(hash);
			bannedAvatarHashCache.remove(hash);
			await this.createBlocklistAuditLog({
				adminUserId,
				targetType: 'avatar_hash',
				action: 'unban_avatar_hash',
				auditLogReason,
				metadata: new Map([['hash_short', hash]]),
			});
		}
		await cacheService.publish(BANNED_AVATAR_HASHES_REFRESH_CHANNEL, 'refresh');
	}

	async checkAvatarHashBan(data: {hashes: Array<string>}): Promise<{
		banned: boolean;
	}> {
		for (const hash of data.hashes) {
			if (bannedAvatarHashCache.contains(stripAvatarAnimationPrefix(hash.toLowerCase()))) {
				return {banned: true};
			}
		}
		return {banned: false};
	}

	async banUserAvatar(
		data: {
			user_id: string;
			reason?: string;
			notes?: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<{
		hash_short: string;
	}> {
		const {users: userRepository} = this.deps.apiContext.services;
		const userId = createUserID(BigInt(data.user_id));
		const user = await userRepository.findUnique(userId);
		if (!user) throw new UnknownUserError();
		const avatar = user.avatarHash;
		if (!avatar) {
			throw new NotFoundError({code: APIErrorCodes.NOT_FOUND});
		}
		const hashShort = stripAvatarAnimationPrefix(avatar.toLowerCase());
		await this.banAvatarHash(
			{
				hashes: [hashShort],
				reason: data.reason,
				notes: data.notes ?? `banned via user shortcut user_id=${data.user_id}`,
			},
			adminUserId,
			auditLogReason,
		);
		return {hash_short: hashShort};
	}

	async banProfileSubstring(
		data: {
			scope: BannedProfileSubstringScope;
			substrings: Array<string>;
			reason?: string;
			notes?: string;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		for (const raw of data.substrings) {
			const canonical = canonicalizeStoredPhrase(raw);
			if (!canonical) continue;
			await adminRepository.banProfileSubstring({
				scope: data.scope,
				substring: canonical,
				added_at: new Date(),
				added_by: adminUserId,
				notes: data.notes ?? null,
			});
			profileSubstringBlocklistCache.add(data.scope, canonical);
			await this.createBlocklistAuditLog({
				adminUserId,
				targetType: 'profile_substring',
				action: 'ban_profile_substring',
				auditLogReason,
				metadata: withReasonMetadata(
					[
						['scope', data.scope],
						['substring', canonical],
					],
					data.reason,
				),
			});
		}
		await cacheService.publish(BANNED_PROFILE_SUBSTRINGS_REFRESH_CHANNEL, 'refresh');
	}

	async unbanProfileSubstring(
		data: {
			scope: BannedProfileSubstringScope;
			substrings: Array<string>;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {adminRepository} = this.deps;
		const {cache: cacheService} = this.deps.apiContext.services;
		for (const raw of data.substrings) {
			const canonical = canonicalizeStoredPhrase(raw);
			if (!canonical) continue;
			await adminRepository.unbanProfileSubstring(data.scope, canonical);
			profileSubstringBlocklistCache.remove(data.scope, canonical);
			await this.createBlocklistAuditLog({
				adminUserId,
				targetType: 'profile_substring',
				action: 'unban_profile_substring',
				auditLogReason,
				metadata: new Map([
					['scope', data.scope],
					['substring', canonical],
				]),
			});
		}
		await cacheService.publish(BANNED_PROFILE_SUBSTRINGS_REFRESH_CHANNEL, 'refresh');
	}

	async checkProfileSubstringBan(data: {scope: BannedProfileSubstringScope; substrings: Array<string>}): Promise<{
		banned: boolean;
	}> {
		for (const raw of data.substrings) {
			if (profileSubstringBlocklistCache.isSubstringBanned(data.scope, raw)) {
				return {banned: true};
			}
		}
		return {banned: false};
	}

	private async createBlocklistAuditLog({
		adminUserId,
		auditLogReason,
		targetType,
		action,
		metadata,
	}: AdminBlocklistAuditParams): Promise<void> {
		await this.deps.auditService.createAuditLog({
			adminUserId,
			targetType,
			targetId: BigInt(0),
			action,
			auditLogReason,
			metadata,
		});
	}
}
