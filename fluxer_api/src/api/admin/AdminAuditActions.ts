// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import type {AdminAuditAccess} from '@fluxer/schema/src/domains/admin/AdminSchemas';

export const AdminAuditReadActions = {
	CHECK_BLOCKLIST_ENTRY: 'check_blocklist_entry',
	GET_ADMIN_API_KEY: 'get_admin_api_key',
	GET_APPLICATION: 'get_application',
	GET_ARCHIVE: 'get_archive',
	GET_ARCHIVE_DOWNLOAD_URL: 'get_archive_download_url',
	GET_AUDIT_LOG: 'get_audit_log',
	GET_GATEWAY_STATS: 'get_gateway_stats',
	GET_GUILD: 'get_guild',
	GET_INSTANCE_CONFIG: 'get_instance_config',
	GET_LIMIT_CONFIG: 'get_limit_config',
	GET_MESSAGE: 'get_message',
	GET_MESSAGE_SHRED_STATUS: 'get_message_shred_status',
	GET_REPORT: 'get_report',
	GET_USER: 'get_user',
	GET_VOICE_REGION: 'get_voice_region',
	GET_VOICE_SERVER: 'get_voice_server',
	GET_VOICE_STATE_COUNTS: 'get_voice_state_counts',
	LIST_ADMIN_ACLS: 'list_admin_acls',
	LIST_ADMIN_API_KEYS: 'list_admin_api_keys',
	LIST_ARCHIVES: 'list_archives',
	LIST_AUDIT_LOGS: 'list_audit_logs',
	LIST_BLOCKLIST_ENTRIES: 'list_blocklist_entries',
	LIST_BLOCKLISTS: 'list_blocklists',
	LIST_CHANNEL_MESSAGES: 'list_channel_messages',
	LIST_DISCOVERY_APPLICATIONS: 'list_discovery_applications',
	LIST_DISCOVERY_CATEGORIES: 'list_discovery_categories',
	LIST_DISCOVERY_CATEGORY_LISTINGS: 'list_discovery_category_listings',
	LIST_DISCOVERY_LISTINGS: 'list_discovery_listings',
	LIST_GUILD_APPLICATIONS: 'list_guild_applications',
	LIST_GUILD_AUDIT_LOGS: 'list_guild_audit_logs',
	LIST_GUILD_EMOJIS: 'list_guild_emojis',
	LIST_GUILD_MEMBERS: 'list_guild_members',
	LIST_GUILD_MEMORY_STATS: 'list_guild_memory_stats',
	LIST_GUILD_STICKERS: 'list_guild_stickers',
	LIST_USER_APPLICATIONS: 'list_user_applications',
	LIST_USER_CHANGE_LOG: 'list_user_change_log',
	LIST_USER_DM_CHANNELS: 'list_user_dm_channels',
	LIST_USER_GUILDS: 'list_user_guilds',
	LIST_USER_RELATIONSHIPS: 'list_user_relationships',
	LIST_USER_SESSIONS: 'list_user_sessions',
	LIST_VOICE_REGIONS: 'list_voice_regions',
	LIST_VOICE_SERVERS: 'list_voice_servers',
	LIST_WEBAUTHN_CREDENTIALS: 'list_webauthn_credentials',
	SEARCH_AUDIT_LOGS: 'search_audit_logs',
	SEARCH_GUILDS: 'search_guilds',
	SEARCH_MESSAGES: 'search_messages',
	SEARCH_REPORTS: 'search_reports',
	SEARCH_USERS: 'search_users',
} as const;

export type AdminAuditReadAction = ValueOf<typeof AdminAuditReadActions>;

export const ADMIN_AUDIT_READ_ACTIONS: ReadonlyArray<AdminAuditReadAction> = Object.values(AdminAuditReadActions);

const READ_ACTION_SET: ReadonlySet<string> = new Set(ADMIN_AUDIT_READ_ACTIONS);

export function getAdminAuditAccess(action: string): AdminAuditAccess {
	return READ_ACTION_SET.has(action) ? 'read' : 'write';
}
