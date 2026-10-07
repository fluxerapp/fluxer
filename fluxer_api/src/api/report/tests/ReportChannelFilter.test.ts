// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, sendChannelMessage, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface ReportResponse {
	report_id: string;
}

interface AdminReportList {
	reports: Array<{
		report_id: string;
		reported_channel_id: string | null;
	}>;
	total: number;
	offset: number;
	limit: number;
}

interface ChannelReports {
	admin: TestAccount;
	firstChannelId: string;
	secondChannelId: string;
	emptyChannelId: string;
	firstReportId: string;
	secondReportId: string;
	userReportId: string;
}

async function reportMessage(
	harness: ApiTestHarness,
	reporter: TestAccount,
	author: TestAccount,
	channelId: string,
	category: string,
): Promise<string> {
	const message = await sendChannelMessage(harness, author.token, channelId, `Reported in ${channelId}`);
	const report = await createBuilder<ReportResponse>(harness, reporter.token)
		.post('/reports/message')
		.body({channel_id: channelId, message_id: message.id, category})
		.expect(HTTP_STATUS.OK)
		.execute();
	return report.report_id;
}

async function setupChannelReports(harness: ApiTestHarness): Promise<ChannelReports> {
	const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
	const author = members[0];
	const second = await createChannel(harness, owner.token, guild.id, 'second');
	const empty = await createChannel(harness, owner.token, guild.id, 'empty');
	const firstReportId = await reportMessage(harness, owner, author, systemChannel.id, 'spam');
	const secondReportId = await reportMessage(harness, owner, author, second.id, 'harassment');
	const userReport = await createBuilder<ReportResponse>(harness, owner.token)
		.post('/reports/user')
		.body({user_id: author.userId, category: 'harassment'})
		.expect(HTTP_STATUS.OK)
		.execute();
	const admin = await setUserACLs(harness, await createTestAccount(harness), ['admin:authenticate', 'report:view']);
	return {
		admin,
		firstChannelId: systemChannel.id,
		secondChannelId: second.id,
		emptyChannelId: empty.id,
		firstReportId,
		secondReportId,
		userReportId: userReport.report_id,
	};
}

function listReports(harness: ApiTestHarness, admin: TestAccount, query: string): Promise<AdminReportList> {
	return createBuilder<AdminReportList>(harness, admin.token)
		.get(`/admin/reports?${query}`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

function reportIds(list: AdminReportList): Array<string> {
	return list.reports.map((report) => report.report_id).sort();
}

describe('Admin report channel filter', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});

	afterEach(async () => {
		await harness.shutdown();
	});

	test('each channel filter returns only the report filed in that channel', async () => {
		const world = await setupChannelReports(harness);
		const unfiltered = await listReports(harness, world.admin, 'limit=50');
		expect(reportIds(unfiltered)).toEqual([world.firstReportId, world.secondReportId, world.userReportId].sort());

		const first = await listReports(harness, world.admin, `reported_channel_id=${world.firstChannelId}`);
		expect(reportIds(first)).toEqual([world.firstReportId]);
		expect(first.total).toBe(1);
		expect(first.reports[0].reported_channel_id).toBe(world.firstChannelId);

		const second = await listReports(harness, world.admin, `reported_channel_id=${world.secondChannelId}`);
		expect(reportIds(second)).toEqual([world.secondReportId]);
		expect(second.total).toBe(1);
		expect(second.reports[0].reported_channel_id).toBe(world.secondChannelId);
	});

	test('a channel without reports returns an empty page', async () => {
		const world = await setupChannelReports(harness);
		const empty = await listReports(harness, world.admin, `reported_channel_id=${world.emptyChannelId}`);
		expect(empty.reports).toEqual([]);
		expect(empty.total).toBe(0);
	});

	test('the channel filter combines with the other filters', async () => {
		const world = await setupChannelReports(harness);
		const pending = await listReports(
			harness,
			world.admin,
			`status=pending&reported_channel_id=${world.secondChannelId}`,
		);
		expect(reportIds(pending)).toEqual([world.secondReportId]);
		expect(pending.total).toBe(1);

		const resolved = await listReports(
			harness,
			world.admin,
			`status=resolved&reported_channel_id=${world.secondChannelId}`,
		);
		expect(resolved.reports).toEqual([]);
		expect(resolved.total).toBe(0);

		const matchingCategory = await listReports(
			harness,
			world.admin,
			`category=harassment&reported_channel_id=${world.secondChannelId}`,
		);
		expect(reportIds(matchingCategory)).toEqual([world.secondReportId]);

		const otherCategory = await listReports(
			harness,
			world.admin,
			`category=harassment&reported_channel_id=${world.firstChannelId}`,
		);
		expect(otherCategory.reports).toEqual([]);
		expect(otherCategory.total).toBe(0);

		const messageType = await listReports(
			harness,
			world.admin,
			`report_type=message&reported_channel_id=${world.firstChannelId}`,
		);
		expect(reportIds(messageType)).toEqual([world.firstReportId]);

		const userType = await listReports(
			harness,
			world.admin,
			`report_type=user&reported_channel_id=${world.firstChannelId}`,
		);
		expect(userType.reports).toEqual([]);
	});
});
