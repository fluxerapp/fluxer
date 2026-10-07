// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

export const ReportProfileAssetSnapshot = z.object({
	hash: z.string(),
	key: z.string().nullable(),
});

export type ReportProfileAssetSnapshot = z.infer<typeof ReportProfileAssetSnapshot>;

export const ReportProfileUserSnapshot = z.object({
	id: z.string(),
	username: z.string().nullable(),
	discriminator: z.number().int().nullable(),
	global_name: z.string().nullable(),
	bio: z.string().nullable(),
	pronouns: z.string().nullable(),
	avatar: ReportProfileAssetSnapshot.nullable(),
	banner: ReportProfileAssetSnapshot.nullable(),
});

export type ReportProfileUserSnapshot = z.infer<typeof ReportProfileUserSnapshot>;

export const ReportProfileMemberSnapshot = z.object({
	guild_id: z.string(),
	nick: z.string().nullable(),
	bio: z.string().nullable(),
	pronouns: z.string().nullable(),
	joined_at: z.string().nullable(),
	avatar: ReportProfileAssetSnapshot.nullable(),
	banner: ReportProfileAssetSnapshot.nullable(),
});

export type ReportProfileMemberSnapshot = z.infer<typeof ReportProfileMemberSnapshot>;

export const ReportProfileGuildSnapshot = z.object({
	id: z.string(),
	name: z.string().nullable(),
	vanity_url_code: z.string().nullable(),
	icon: ReportProfileAssetSnapshot.nullable(),
	banner: ReportProfileAssetSnapshot.nullable(),
	splash: ReportProfileAssetSnapshot.nullable(),
});

export type ReportProfileGuildSnapshot = z.infer<typeof ReportProfileGuildSnapshot>;

export const ReportProfileSnapshot = z.object({
	captured_at: z.string(),
	user: ReportProfileUserSnapshot.nullable(),
	member: ReportProfileMemberSnapshot.nullable(),
	guild: ReportProfileGuildSnapshot.nullable(),
});

export type ReportProfileSnapshot = z.infer<typeof ReportProfileSnapshot>;

export function parseReportProfileSnapshot(json: string | null | undefined): ReportProfileSnapshot | null {
	if (!json) {
		return null;
	}
	let parsed: unknown;
	try {
		parsed = JSON.parse(json);
	} catch {
		return null;
	}
	const result = ReportProfileSnapshot.safeParse(parsed);
	return result.success ? result.data : null;
}

export function serializeReportProfileSnapshot(snapshot: ReportProfileSnapshot): string {
	return JSON.stringify(ReportProfileSnapshot.parse(snapshot));
}

export function listReportProfileSnapshotAssets(
	snapshot: ReportProfileSnapshot | null,
): Array<ReportProfileAssetSnapshot> {
	if (!snapshot) {
		return [];
	}
	return [
		snapshot.user?.avatar,
		snapshot.user?.banner,
		snapshot.member?.avatar,
		snapshot.member?.banner,
		snapshot.guild?.icon,
		snapshot.guild?.banner,
		snapshot.guild?.splash,
	].filter((asset): asset is ReportProfileAssetSnapshot => asset != null);
}
