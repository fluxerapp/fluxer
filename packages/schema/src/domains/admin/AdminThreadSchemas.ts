// SPDX-License-Identifier: AGPL-3.0-or-later

import {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import {z} from 'zod';

export const ListGuildThreadsResponse = z.object({
	threads: z.array(ThreadChannelResponse).describe('Every thread of the guild, active and archived, newest first'),
});

export type ListGuildThreadsResponse = z.infer<typeof ListGuildThreadsResponse>;
