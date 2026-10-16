// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	listReportProfileSnapshotAssets,
	parseReportProfileSnapshot,
	type ReportProfileSnapshot,
	serializeReportProfileSnapshot,
} from '@fluxer/schema/src/domains/report/ReportProfileSnapshotSchemas';
import {describe, expect, it} from 'vitest';

const fullSnapshot: ReportProfileSnapshot = {
	captured_at: '2026-10-04T12:00:00.000Z',
	user: {
		id: '1427410010101010101',
		username: 'target',
		discriminator: 1,
		global_name: 'Target',
		bio: 'Original bio',
		pronouns: 'they/them',
		avatar: {hash: 'a_user', key: 'reports/1/profile/avatar/a_user'},
		banner: {hash: 'user-banner', key: null},
	},
	member: {
		guild_id: '1427410010101010102',
		nick: 'Nick',
		bio: null,
		pronouns: null,
		joined_at: '2026-01-01T00:00:00.000Z',
		avatar: {hash: 'member-avatar', key: 'reports/1/profile/avatar/member-avatar'},
		banner: null,
	},
	guild: {
		id: '1427410010101010102',
		name: 'Community',
		vanity_url_code: null,
		icon: {hash: 'icon', key: 'reports/1/profile/icon/icon'},
		banner: null,
		splash: {hash: 'splash', key: 'reports/1/profile/splash/splash'},
	},
};

describe('ReportProfileSnapshot', () => {
	it('round-trips a full snapshot', () => {
		expect(parseReportProfileSnapshot(serializeReportProfileSnapshot(fullSnapshot))).toEqual(fullSnapshot);
	});

	it('accepts a snapshot with only some parts', () => {
		const userOnly: ReportProfileSnapshot = {
			captured_at: '2026-10-04T12:00:00.000Z',
			user: {
				id: '1',
				username: null,
				discriminator: null,
				global_name: null,
				bio: null,
				pronouns: null,
				avatar: null,
				banner: null,
			},
			member: null,
			guild: null,
		};
		expect(parseReportProfileSnapshot(JSON.stringify(userOnly))).toEqual(userOnly);
	});

	it('drops unknown keys', () => {
		const parsed = parseReportProfileSnapshot(JSON.stringify({...fullSnapshot, extra: true}));
		expect(parsed).toEqual(fullSnapshot);
	});

	it.each([
		['null', null],
		['undefined', undefined],
		['an empty string', ''],
		['bad JSON', '{"captured_at":'],
		['a JSON array', '[]'],
		['a JSON string', '"snapshot"'],
		['a missing part', JSON.stringify({captured_at: '2026-10-04T12:00:00.000Z', user: null, member: null})],
		['a wrong field type', JSON.stringify({...fullSnapshot, user: {...fullSnapshot.user, discriminator: '0001'}})],
		['an asset without a hash', JSON.stringify({...fullSnapshot, guild: {...fullSnapshot.guild, icon: {key: null}}})],
	])('reads %s as null', (_label, json) => {
		expect(parseReportProfileSnapshot(json)).toBeNull();
	});

	it('refuses to serialize an invalid snapshot', () => {
		expect(() =>
			serializeReportProfileSnapshot({...fullSnapshot, captured_at: 1} as unknown as ReportProfileSnapshot),
		).toThrow();
	});

	it('lists every stored asset', () => {
		expect(listReportProfileSnapshotAssets(fullSnapshot)).toEqual([
			{hash: 'a_user', key: 'reports/1/profile/avatar/a_user'},
			{hash: 'user-banner', key: null},
			{hash: 'member-avatar', key: 'reports/1/profile/avatar/member-avatar'},
			{hash: 'icon', key: 'reports/1/profile/icon/icon'},
			{hash: 'splash', key: 'reports/1/profile/splash/splash'},
		]);
		expect(listReportProfileSnapshotAssets(null)).toEqual([]);
	});
});
