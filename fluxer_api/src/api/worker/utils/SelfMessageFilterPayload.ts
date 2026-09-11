import {
	type BulkDeleteSelfMessagesFilter,
	BulkDeleteSelfMessagesGuildFilterMode,
	BulkDeleteSelfMessagesScope,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import {z} from 'zod';
import type {SelfMessageFilter} from '../../channel/services/message/SelfMessageFilter';

export const SelfMessageFilterPayload = z.object({
	scope: BulkDeleteSelfMessagesScope,
	includeDms: z.boolean(),
	includeDmsClosed: z.boolean(),
	includeGroupDms: z.boolean(),
	includeGuilds: z.boolean(),
	guildFilterMode: BulkDeleteSelfMessagesGuildFilterMode.default('exclude'),
	excludedGuildIds: z.array(z.string()),
	includedGuildIds: z.array(z.string()).default([]),
	startTimestamp: z.number().nullable(),
	endTimestamp: z.number().nullable(),
});

export type SelfMessageFilterPayload = z.infer<typeof SelfMessageFilterPayload>;

export function serializeSelfMessageFilter(filter: BulkDeleteSelfMessagesFilter): SelfMessageFilterPayload {
	return {
		scope: filter.scope,
		includeDms: filter.include_dms,
		includeDmsClosed: filter.include_dms_closed,
		includeGroupDms: filter.include_group_dms,
		includeGuilds: filter.include_guilds,
		guildFilterMode: filter.guild_filter_mode,
		excludedGuildIds: filter.excluded_guild_ids.map((id) => id.toString()),
		includedGuildIds: filter.included_guild_ids.map((id) => id.toString()),
		startTimestamp: filter.start_date ? new Date(filter.start_date).getTime() : null,
		endTimestamp: filter.end_date ? new Date(filter.end_date).getTime() : null,
	};
}

export function deserializeSelfMessageFilter(payload: SelfMessageFilterPayload): SelfMessageFilter {
	return {
		...payload,
		excludedGuildIds: new Set(payload.excludedGuildIds),
		includedGuildIds: new Set(payload.includedGuildIds),
	};
}
