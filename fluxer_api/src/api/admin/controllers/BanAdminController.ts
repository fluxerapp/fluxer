// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import {MissingACLError} from '@fluxer/errors/src/domains/core/MissingACLError';
import {
	AdminBlocklistAvatarHashUpdateRequest,
	AdminBlocklistBulkDeleteRequest,
	AdminBlocklistEntryCreateRequest,
	AdminBlocklistEntryListQuery,
	AdminBlocklistEntryListResponse,
	AdminBlocklistEntryUpdateRequest,
	AdminBlocklistFileShaUpdateRequest,
	type AdminBlocklistListType,
	AdminBlocklistProfileSubstringUpdateRequest,
	AdminBlocklistScopeQuery,
	AdminBlocklistTypeListResponse,
	AdminBlocklistUrlDomainUpdateRequest,
	AdminBlocklistUrlUpdateRequest,
	BlocklistEntryParam,
	BlocklistTypeParam,
} from '@fluxer/schema/src/domains/admin/AdminBlocklistSchemas';
import {
	BanAvatarHashRequest,
	BanCheckResponseSchema,
	BanEmailRequest,
	BanFileShaRequest,
	BanIpRequest,
	BanPhraseRequest,
	BanProfileSubstringRequest,
	BanUrlDomainRequest,
	BanUrlRequest,
	BanUserAvatarRequest,
	BanUserAvatarResponseSchema,
	BulkBanFileShasRequest,
	BulkJobResponse,
	CheckAvatarHashRequest,
	SuspiciousEmailDomainRequest,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import type {ZodTypeAny, z} from 'zod';
import {requireAdminACL, requireAnyAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {getWorkerService} from '../../middleware/ServiceRegistry';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {requireRequestJsonBody} from '../../utils/RequestJsonBody';
import {inputValidationErrorFromZodIssues, Validator} from '../../Validator';

type ProfileSubstringScope = BanProfileSubstringRequest['scope'];

const BLOCKLIST_CATALOG = [
	{
		list_type: 'ip' as const,
		description:
			'IPv4/IPv6 addresses and CIDR ranges denied service. Applies to live connections and can be applied retroactively.',
		value_field: 'ip',
		fields: [],
		scoped: false,
		supports_bulk_create: false,
		supports_bulk_delete: false,
		supports_update: false,
	},
	{
		list_type: 'email' as const,
		description: 'Email addresses that cannot be used to register or be set on an account.',
		value_field: 'email',
		fields: [],
		scoped: false,
		supports_bulk_create: false,
		supports_bulk_delete: false,
		supports_update: false,
	},
	{
		list_type: 'email-domain-suspicious' as const,
		description:
			'Email domains flagged as suspicious. Registration is not blocked, but new accounts using the domain must verify a phone number before they can act on the platform. The list itself is not exposed to users.',
		value_field: 'domain',
		fields: [],
		scoped: false,
		supports_bulk_create: false,
		supports_bulk_delete: false,
		supports_update: false,
	},
	{
		list_type: 'phrase' as const,
		description:
			'Phrases blocked in content. Matching is case-insensitive and normalizes common bypass tricks such as inserted whitespace, punctuation, invisible characters, and compatibility glyphs.',
		value_field: 'phrase',
		fields: [],
		scoped: false,
		supports_bulk_create: false,
		supports_bulk_delete: false,
		supports_update: false,
	},
	{
		list_type: 'url' as const,
		description: 'Absolute http(s) URLs blocked from being posted. Values are canonicalized before storage.',
		value_field: 'url',
		fields: ['category', 'severity', 'source_url', 'notes'],
		scoped: false,
		supports_bulk_create: false,
		supports_bulk_delete: false,
		supports_update: true,
	},
	{
		list_type: 'url-domain' as const,
		description: 'Domains blocked from being linked, optionally covering every subdomain rooted at the domain.',
		value_field: 'domain',
		fields: ['match_subdomains', 'category', 'severity', 'source_url', 'notes'],
		scoped: false,
		supports_bulk_create: false,
		supports_bulk_delete: false,
		supports_update: true,
	},
	{
		list_type: 'file-sha' as const,
		description: 'SHA-256 hashes of files rejected on upload.',
		value_field: 'sha256_hex',
		fields: ['category', 'severity', 'content_type', 'source_url', 'notes'],
		scoped: false,
		supports_bulk_create: true,
		supports_bulk_delete: false,
		supports_update: true,
	},
	{
		list_type: 'avatar-hash' as const,
		description: '8-char MD5-prefix avatar hashes rejected on upload.',
		value_field: 'hashes',
		fields: ['category', 'severity', 'source_url', 'reason', 'notes'],
		scoped: false,
		supports_bulk_create: false,
		supports_bulk_delete: true,
		supports_update: true,
	},
	{
		list_type: 'profile-substring' as const,
		description:
			'Substrings blocked within one profile field. Matching reuses the phrase blocklist normalization, so a scope must accompany every operation.',
		value_field: 'substrings',
		fields: ['scope', 'reason', 'notes'],
		scoped: true,
		supports_bulk_create: false,
		supports_bulk_delete: true,
		supports_update: true,
	},
];

const BLOCKLIST_TYPE_ACLS: Record<AdminBlocklistListType, {add: string; check: string; remove: string}> = {
	ip: {add: AdminACLs.BAN_IP_ADD, check: AdminACLs.BAN_IP_CHECK, remove: AdminACLs.BAN_IP_REMOVE},
	email: {add: AdminACLs.BAN_EMAIL_ADD, check: AdminACLs.BAN_EMAIL_CHECK, remove: AdminACLs.BAN_EMAIL_REMOVE},
	'email-domain-suspicious': {
		add: AdminACLs.SUSPICIOUS_EMAIL_DOMAIN_ADD,
		check: AdminACLs.SUSPICIOUS_EMAIL_DOMAIN_CHECK,
		remove: AdminACLs.SUSPICIOUS_EMAIL_DOMAIN_REMOVE,
	},
	phrase: {add: AdminACLs.BAN_PHRASE_ADD, check: AdminACLs.BAN_PHRASE_CHECK, remove: AdminACLs.BAN_PHRASE_REMOVE},
	url: {add: AdminACLs.BAN_URL_ADD, check: AdminACLs.BAN_URL_CHECK, remove: AdminACLs.BAN_URL_REMOVE},
	'url-domain': {
		add: AdminACLs.BAN_URL_DOMAIN_ADD,
		check: AdminACLs.BAN_URL_DOMAIN_CHECK,
		remove: AdminACLs.BAN_URL_DOMAIN_REMOVE,
	},
	'file-sha': {
		add: AdminACLs.BAN_FILE_SHA_ADD,
		check: AdminACLs.BAN_FILE_SHA_CHECK,
		remove: AdminACLs.BAN_FILE_SHA_REMOVE,
	},
	'avatar-hash': {
		add: AdminACLs.BAN_AVATAR_HASH_ADD,
		check: AdminACLs.BAN_AVATAR_HASH_CHECK,
		remove: AdminACLs.BAN_AVATAR_HASH_REMOVE,
	},
	'profile-substring': {
		add: AdminACLs.BAN_PROFILE_SUBSTRING_ADD,
		check: AdminACLs.BAN_PROFILE_SUBSTRING_CHECK,
		remove: AdminACLs.BAN_PROFILE_SUBSTRING_REMOVE,
	},
};

type BlocklistVerb = 'add' | 'check' | 'remove';

const BLOCKLIST_ACLS_BY_VERB: Record<BlocklistVerb, Array<string>> = {
	add: Object.values(BLOCKLIST_TYPE_ACLS).map((acls) => acls.add),
	check: Object.values(BLOCKLIST_TYPE_ACLS).map((acls) => acls.check),
	remove: Object.values(BLOCKLIST_TYPE_ACLS).map((acls) => acls.remove),
};

function requireBlocklistACL(adminAcls: Set<string>, listType: AdminBlocklistListType, verb: BlocklistVerb): void {
	const requiredAcl = BLOCKLIST_TYPE_ACLS[listType][verb];
	if (!adminAcls.has(requiredAcl) && !adminAcls.has(AdminACLs.WILDCARD)) {
		throw new MissingACLError(requiredAcl);
	}
}

function unsupportedForBlocklist(): never {
	throw new BadRequestError({
		code: APIErrorCodes.INVALID_FORM_BODY,
		message: 'This blocklist does not support this operation',
	});
}

function assertBlocklistScopeAllowed(listType: AdminBlocklistListType, scope: ProfileSubstringScope | undefined): void {
	if (listType !== 'profile-substring' && scope != null) {
		throw new BadRequestError({
			code: APIErrorCodes.INVALID_FORM_BODY,
			message: 'This blocklist does not accept a scope',
		});
	}
}

function requireProfileSubstringScope(scope: ProfileSubstringScope | undefined): ProfileSubstringScope {
	if (scope == null) {
		throw new BadRequestError({
			code: APIErrorCodes.INVALID_FORM_BODY,
			message: 'The profile-substring blocklist requires a scope',
		});
	}
	return scope;
}

async function parseBlocklistBody<T extends ZodTypeAny>(schema: T, value: unknown): Promise<z.infer<T>> {
	const result = await schema.safeParseAsync(value);
	if (!result.success) {
		throw inputValidationErrorFromZodIssues(result.error.issues);
	}
	return result.data;
}

export function BanAdminController(app: HonoApp) {
	app.get(
		'/admin/blocklists',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAnyAdminACL(BLOCKLIST_ACLS_BY_VERB.check),
		OpenAPI({
			operationId: 'list_admin_blocklist_types',
			summary: 'List blocklists',
			responseSchema: AdminBlocklistTypeListResponse,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'List every blocklist this instance maintains, the request field that carries an entry value, the extra fields its entries accept, and which of the bulk and update operations it supports.',
		}),
		async (ctx) => {
			return ctx.json({items: BLOCKLIST_CATALOG});
		},
	);
	app.get(
		'/admin/blocklists/:list_type/entries',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAnyAdminACL(BLOCKLIST_ACLS_BY_VERB.check),
		Validator('param', BlocklistTypeParam),
		Validator('query', AdminBlocklistEntryListQuery),
		OpenAPI({
			operationId: 'list_admin_blocklist_entries',
			summary: 'List blocklist entries',
			responseSchema: AdminBlocklistEntryListResponse,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Page through the entries of a blocklist, ordered by value. Pass the next_after cursor of the previous page as after to fetch the next page. The profile-substring blocklist requires a scope.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {list_type: listType} = ctx.req.valid('param');
			requireBlocklistACL(ctx.get('adminUserAcls'), listType, 'check');
			const {limit, after, scope} = ctx.req.valid('query');
			assertBlocklistScopeAllowed(listType, scope);
			return ctx.json(
				await adminService.banManagementService.listBlocklistEntries({
					listType,
					limit,
					after: after ?? null,
					scope: listType === 'profile-substring' ? requireProfileSubstringScope(scope) : null,
				}),
			);
		},
	);
	app.post(
		'/admin/blocklists/:list_type/entries',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAnyAdminACL(BLOCKLIST_ACLS_BY_VERB.add),
		Validator('param', BlocklistTypeParam),
		OpenAPI({
			operationId: 'create_admin_blocklist_entry',
			summary: 'Add blocklist entry',
			responseSchema: null,
			statusCode: 204,
			security: ['adminApiKey'],
			tags: ['Admin'],
			requestSchema: AdminBlocklistEntryCreateRequest,
			description:
				'Add a value to a blocklist. The request body is the shape the blocklist named by list_type accepts, and the value is validated and canonicalized for that blocklist. Adding an IP address that is on the instance exemption list, or that IPInfo reports as a high blast-radius carrier NAT, is refused with 400 IP_BAN_DECLINED and recorded in the audit log.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {list_type: listType} = ctx.req.valid('param');
			requireBlocklistACL(ctx.get('adminUserAcls'), listType, 'add');
			const bans = adminService.banManagementService;
			const raw = await requireRequestJsonBody(ctx.req);
			switch (listType) {
				case 'ip':
					await bans.banIp(await parseBlocklistBody(BanIpRequest, raw), adminUserId, auditLogReason);
					break;
				case 'email':
					await bans.banEmail(await parseBlocklistBody(BanEmailRequest, raw), adminUserId, auditLogReason);
					break;
				case 'email-domain-suspicious':
					await bans.addSuspiciousEmailDomain(
						await parseBlocklistBody(SuspiciousEmailDomainRequest, raw),
						adminUserId,
						auditLogReason,
					);
					break;
				case 'phrase':
					await bans.banPhrase(await parseBlocklistBody(BanPhraseRequest, raw), adminUserId, auditLogReason);
					break;
				case 'url':
					await bans.banUrl(await parseBlocklistBody(BanUrlRequest, raw), adminUserId, auditLogReason);
					break;
				case 'url-domain':
					await bans.banUrlDomain(await parseBlocklistBody(BanUrlDomainRequest, raw), adminUserId, auditLogReason);
					break;
				case 'file-sha':
					await bans.banFileSha(await parseBlocklistBody(BanFileShaRequest, raw), adminUserId, auditLogReason);
					break;
				case 'avatar-hash':
					await bans.banAvatarHash(await parseBlocklistBody(BanAvatarHashRequest, raw), adminUserId, auditLogReason);
					break;
				case 'profile-substring':
					await bans.banProfileSubstring(
						await parseBlocklistBody(BanProfileSubstringRequest, raw),
						adminUserId,
						auditLogReason,
					);
					break;
			}
			return ctx.body(null, 204);
		},
	);
	app.put(
		'/admin/blocklists/:list_type/entries',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAnyAdminACL(BLOCKLIST_ACLS_BY_VERB.add),
		Validator('param', BlocklistTypeParam),
		Validator('json', BulkBanFileShasRequest),
		OpenAPI({
			operationId: 'bulk_create_admin_blocklist_entries',
			summary: 'Bulk-add blocklist entries',
			responseSchema: BulkJobResponse,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Enqueue a background job that adds many entries to a blocklist at once. Returns a job_id immediately; observe progress at /admin/jobs/:job_id. Only the file-sha blocklist accepts this operation, reported as supports_bulk_create by GET /admin/blocklists.',
		}),
		async (ctx) => {
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {list_type: listType} = ctx.req.valid('param');
			requireBlocklistACL(ctx.get('adminUserAcls'), listType, 'add');
			if (listType !== 'file-sha') {
				unsupportedForBlocklist();
			}
			const body = ctx.req.valid('json');
			const workerService = getWorkerService();
			const jobId = await workerService.addJob(
				'bulkBanFileShas',
				{
					sha256_list: body.sha256_list,
					admin_user_id: adminUserId.toString(),
					audit_log_reason: auditLogReason,
				},
				{requestedByUserId: adminUserId, requireLedger: true, ...(auditLogReason && {auditLogReason})},
			);
			return ctx.json({job_id: jobId.toString()});
		},
	);
	app.delete(
		'/admin/blocklists/:list_type/entries',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAnyAdminACL(BLOCKLIST_ACLS_BY_VERB.remove),
		Validator('param', BlocklistTypeParam),
		OpenAPI({
			operationId: 'bulk_delete_admin_blocklist_entries',
			summary: 'Bulk-remove blocklist entries',
			responseSchema: null,
			statusCode: 204,
			security: ['adminApiKey'],
			tags: ['Admin'],
			requestSchema: AdminBlocklistBulkDeleteRequest,
			description:
				'Remove several entries from a blocklist in one request. The request body is the shape the blocklist named by list_type accepts. Only the avatar-hash and profile-substring blocklists accept this operation, reported as supports_bulk_delete by GET /admin/blocklists.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {list_type: listType} = ctx.req.valid('param');
			requireBlocklistACL(ctx.get('adminUserAcls'), listType, 'remove');
			const bans = adminService.banManagementService;
			const raw = await requireRequestJsonBody(ctx.req);
			switch (listType) {
				case 'avatar-hash':
					await bans.unbanAvatarHash(
						await parseBlocklistBody(CheckAvatarHashRequest, raw),
						adminUserId,
						auditLogReason,
					);
					break;
				case 'profile-substring':
					await bans.unbanProfileSubstring(
						await parseBlocklistBody(BanProfileSubstringRequest, raw),
						adminUserId,
						auditLogReason,
					);
					break;
				default:
					unsupportedForBlocklist();
			}
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/admin/blocklists/:list_type/entries/:entry_value',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAnyAdminACL(BLOCKLIST_ACLS_BY_VERB.check),
		Validator('param', BlocklistEntryParam),
		Validator('query', AdminBlocklistScopeQuery),
		OpenAPI({
			operationId: 'get_admin_blocklist_entry',
			summary: 'Check blocklist entry',
			responseSchema: BanCheckResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Report whether a value is currently blocked by a blocklist. The value is percent-encoded in the path. An IP address can still match a broader stored CIDR entry, and a URL can match a banned domain. The profile-substring blocklist requires a scope.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {list_type: listType, entry_value: entryValue} = ctx.req.valid('param');
			requireBlocklistACL(ctx.get('adminUserAcls'), listType, 'check');
			const {scope} = ctx.req.valid('query');
			assertBlocklistScopeAllowed(listType, scope);
			const bans = adminService.banManagementService;
			switch (listType) {
				case 'ip':
					return ctx.json(await bans.checkIpBan({ip: entryValue}));
				case 'email':
					return ctx.json(await bans.checkEmailBan({email: entryValue}));
				case 'email-domain-suspicious':
					return ctx.json(await bans.checkSuspiciousEmailDomain({domain: entryValue}));
				case 'phrase':
					return ctx.json(await bans.checkPhraseBan({phrase: entryValue}));
				case 'url':
					return ctx.json(await bans.checkUrlBan({url: entryValue}));
				case 'url-domain':
					return ctx.json(await bans.checkUrlDomainBan({domain: entryValue}));
				case 'file-sha':
					return ctx.json(await bans.checkFileShaBan({sha256_hex: entryValue}));
				case 'avatar-hash':
					return ctx.json(await bans.checkAvatarHashBan({hashes: [entryValue]}));
				case 'profile-substring':
					return ctx.json(
						await bans.checkProfileSubstringBan({
							scope: requireProfileSubstringScope(scope),
							substrings: [entryValue],
						}),
					);
			}
		},
	);
	app.patch(
		'/admin/blocklists/:list_type/entries/:entry_value',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAnyAdminACL(BLOCKLIST_ACLS_BY_VERB.add),
		Validator('param', BlocklistEntryParam),
		OpenAPI({
			operationId: 'update_admin_blocklist_entry',
			summary: 'Update blocklist entry',
			responseSchema: null,
			statusCode: 204,
			security: ['adminApiKey'],
			tags: ['Admin'],
			requestSchema: AdminBlocklistEntryUpdateRequest,
			description:
				'Rewrite the stored fields of a blocklist entry without removing and re-adding it. The stored metadata is replaced by the supplied fields, so fields left out fall back to their defaults. Only blocklists whose entries carry fields accept this operation, reported as supports_update by GET /admin/blocklists.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {list_type: listType, entry_value: entryValue} = ctx.req.valid('param');
			requireBlocklistACL(ctx.get('adminUserAcls'), listType, 'add');
			const bans = adminService.banManagementService;
			const raw = await requireRequestJsonBody(ctx.req);
			switch (listType) {
				case 'url': {
					const body = await parseBlocklistBody(AdminBlocklistUrlUpdateRequest, raw);
					await bans.banUrl({url: entryValue, ...body}, adminUserId, auditLogReason);
					break;
				}
				case 'url-domain': {
					const body = await parseBlocklistBody(AdminBlocklistUrlDomainUpdateRequest, raw);
					await bans.banUrlDomain({domain: entryValue, ...body}, adminUserId, auditLogReason);
					break;
				}
				case 'file-sha': {
					const body = await parseBlocklistBody(AdminBlocklistFileShaUpdateRequest, raw);
					await bans.banFileSha({sha256_hex: entryValue, ...body}, adminUserId, auditLogReason);
					break;
				}
				case 'avatar-hash': {
					const body = await parseBlocklistBody(AdminBlocklistAvatarHashUpdateRequest, raw);
					await bans.banAvatarHash({hashes: [entryValue], ...body}, adminUserId, auditLogReason);
					break;
				}
				case 'profile-substring': {
					const body = await parseBlocklistBody(AdminBlocklistProfileSubstringUpdateRequest, raw);
					await bans.banProfileSubstring({substrings: [entryValue], ...body}, adminUserId, auditLogReason);
					break;
				}
				default:
					unsupportedForBlocklist();
			}
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/admin/blocklists/:list_type/entries/:entry_value',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAnyAdminACL(BLOCKLIST_ACLS_BY_VERB.remove),
		Validator('param', BlocklistEntryParam),
		Validator('query', AdminBlocklistScopeQuery),
		OpenAPI({
			operationId: 'delete_admin_blocklist_entry',
			summary: 'Remove blocklist entry',
			responseSchema: null,
			statusCode: 204,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Remove an entry from a blocklist. The value is percent-encoded in the path and is canonicalized the same way it was on add, so an IP covered only by a broader CIDR entry cannot be removed through the narrower address. The profile-substring blocklist requires a scope.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {list_type: listType, entry_value: entryValue} = ctx.req.valid('param');
			requireBlocklistACL(ctx.get('adminUserAcls'), listType, 'remove');
			const {scope} = ctx.req.valid('query');
			assertBlocklistScopeAllowed(listType, scope);
			const bans = adminService.banManagementService;
			switch (listType) {
				case 'ip':
					await bans.unbanIp({ip: entryValue}, adminUserId, auditLogReason);
					break;
				case 'email':
					await bans.unbanEmail({email: entryValue}, adminUserId, auditLogReason);
					break;
				case 'email-domain-suspicious':
					await bans.removeSuspiciousEmailDomain({domain: entryValue}, adminUserId, auditLogReason);
					break;
				case 'phrase':
					await bans.unbanPhrase({phrase: entryValue}, adminUserId, auditLogReason);
					break;
				case 'url':
					await bans.unbanUrl({url: entryValue}, adminUserId, auditLogReason);
					break;
				case 'url-domain':
					await bans.unbanUrlDomain({domain: entryValue}, adminUserId, auditLogReason);
					break;
				case 'file-sha':
					await bans.unbanFileSha({sha256_hex: entryValue}, adminUserId, auditLogReason);
					break;
				case 'avatar-hash':
					await bans.unbanAvatarHash({hashes: [entryValue]}, adminUserId, auditLogReason);
					break;
				case 'profile-substring':
					await bans.unbanProfileSubstring(
						{scope: requireProfileSubstringScope(scope), substrings: [entryValue]},
						adminUserId,
						auditLogReason,
					);
					break;
			}
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/admin/users/:user_id/avatar-block',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAdminACL(AdminACLs.BAN_AVATAR_HASH_ADD),
		Validator('param', UserIdParam),
		Validator('json', BanUserAvatarRequest),
		OpenAPI({
			operationId: 'ban_admin_user_avatar',
			summary: "Ban this user's current avatar",
			responseSchema: BanUserAvatarResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				"Reads the user's current avatar_hash, strips any animation prefix, and adds the 8-char hash to the avatar-hash blocklist. Returns the banned hash.",
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const userId = ctx.req.valid('param').user_id.toString();
			const body = ctx.req.valid('json');
			return ctx.json(
				await adminService.banManagementService.banUserAvatar({user_id: userId, ...body}, adminUserId, auditLogReason),
			);
		},
	);
}
