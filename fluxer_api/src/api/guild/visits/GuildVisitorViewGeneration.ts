// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID} from '@app/api/BrandedTypes';
import type {GuildRow} from '@app/api/database/types/GuildTypes';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';

const VISITOR_VIEW_FIELDS = [
	'features',
	'nsfw',
	'nsfw_level',
	'message_history_cutoff',
	'disabled_operations',
] as const satisfies ReadonlyArray<keyof GuildRow>;

function guildVisitorViewGenerationKey(guildId: GuildID): string {
	return `guild:visitor_view_gen:${guildId}`;
}

export async function readGuildVisitorViewGeneration(guildId: GuildID): Promise<string> {
	return (await getKVClient().get(guildVisitorViewGenerationKey(guildId))) ?? '0';
}

export async function bumpGuildVisitorViewGeneration(guildId: GuildID): Promise<void> {
	await getKVClient().incr(guildVisitorViewGenerationKey(guildId));
}

function sameValue(left: unknown, right: unknown): boolean {
	if (left instanceof Set && right instanceof Set) {
		return left.size === right.size && [...left].every((value) => right.has(value));
	}
	if (left instanceof Date && right instanceof Date) return left.getTime() === right.getTime();
	return (left ?? null) === (right ?? null);
}

export function touchesGuildVisitorView(patch: Partial<GuildRow>, previous: GuildRow | null | undefined): boolean {
	return VISITOR_VIEW_FIELDS.some(
		(field) => field in patch && (previous == null || !sameValue(patch[field], previous[field])),
	);
}
