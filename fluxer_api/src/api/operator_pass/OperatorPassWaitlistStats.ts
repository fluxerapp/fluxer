// SPDX-License-Identifier: AGPL-3.0-or-later

import type {OperatorPassWaitlistRow} from '@app/api/database/types/OperatorPassTypes';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import type {
	AdminOperatorPassWaitlistFollowupTextEntry,
	AdminOperatorPassWaitlistStatsResponse,
} from '@fluxer/schema/src/domains/admin/AdminOperatorPassSchemas';
import {
	OPERATOR_PASS_CURRENCIES,
	OPERATOR_PASS_PRICE_FEEDBACK,
	OPERATOR_PASS_WAITLIST_ABOUT_KEYS,
	OPERATOR_PASS_WAITLIST_FOLLOWUP_KEYS,
	type OperatorPassWaitlistAboutKey,
	type OperatorPassWaitlistFollowupKey,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';

const STATS_PAGE_SIZE = 500;

type WaitlistStatsRow = Pick<OperatorPassWaitlistRow, 'status' | 'price_feedback' | 'currency'> &
	Partial<
		Pick<OperatorPassWaitlistRow, 'followup_picks' | 'followup_about' | 'followup_other_text' | 'followup_answered_at'>
	>;

type WaitlistStats = Omit<AdminOperatorPassWaitlistStatsResponse, 'standard_price_set'>;

function createEmptyStats(): WaitlistStats {
	return {
		total: 0,
		by_status: {pending: 0, confirmed: 0, unsubscribed: 0, bounced: 0},
		unanswered: {confirmed: 0, all: 0},
		price_feedback: OPERATOR_PASS_CURRENCIES.map((currency) => ({
			currency,
			confirmed: 0,
			all: 0,
			answers: OPERATOR_PASS_PRICE_FEEDBACK.map((feedback) => ({feedback, confirmed: 0, all: 0})),
		})),
		followup: {
			answered: {confirmed: 0, all: 0},
			picks: OPERATOR_PASS_WAITLIST_FOLLOWUP_KEYS.map((key) => ({key, confirmed: 0, all: 0})),
			about: OPERATOR_PASS_WAITLIST_ABOUT_KEYS.map((key) => ({key, confirmed: 0, all: 0})),
			other_text: {confirmed: 0, all: 0},
		},
	};
}

function followupKeys(row: Pick<WaitlistStatsRow, 'followup_picks'>): Array<OperatorPassWaitlistFollowupKey> {
	return OPERATOR_PASS_WAITLIST_FOLLOWUP_KEYS.filter((key) => row.followup_picks?.has(key) === true);
}

function aboutKeys(row: Pick<WaitlistStatsRow, 'followup_about'>): Array<OperatorPassWaitlistAboutKey> {
	return OPERATOR_PASS_WAITLIST_ABOUT_KEYS.filter((key) => row.followup_about?.has(key) === true);
}

function countFollowup(stats: WaitlistStats, row: WaitlistStatsRow, confirmed: number): void {
	if (!row.followup_answered_at) return;
	stats.followup.answered.all += 1;
	stats.followup.answered.confirmed += confirmed;
	for (const key of followupKeys(row)) {
		const count = stats.followup.picks.find((entry) => entry.key === key);
		if (!count) continue;
		count.all += 1;
		count.confirmed += confirmed;
	}
	for (const key of aboutKeys(row)) {
		const count = stats.followup.about.find((entry) => entry.key === key);
		if (!count) continue;
		count.all += 1;
		count.confirmed += confirmed;
	}
	if (row.followup_other_text) {
		stats.followup.other_text.all += 1;
		stats.followup.other_text.confirmed += confirmed;
	}
}

function countRow(stats: WaitlistStats, row: WaitlistStatsRow): void {
	const confirmed = row.status === 'confirmed' ? 1 : 0;
	stats.total += 1;
	stats.by_status[row.status] += 1;
	countFollowup(stats, row, confirmed);
	const distribution = stats.price_feedback.find((entry) => entry.currency === row.currency);
	const count = distribution?.answers.find((entry) => entry.feedback === row.price_feedback);
	if (!distribution || !count) {
		stats.unanswered.all += 1;
		stats.unanswered.confirmed += confirmed;
		return;
	}
	distribution.all += 1;
	distribution.confirmed += confirmed;
	count.all += 1;
	count.confirmed += confirmed;
}

async function scanWaitlist(
	repository: OperatorPassRepository,
	visit: (row: OperatorPassWaitlistRow) => void,
): Promise<void> {
	let pageState: string | null = null;
	do {
		const page = await repository.scanWaitlistPage(STATS_PAGE_SIZE, pageState);
		for (const row of page.entries) {
			visit(row);
		}
		pageState = page.pageState;
	} while (pageState);
}

export async function getOperatorPassWaitlistStats(repository: OperatorPassRepository): Promise<WaitlistStats> {
	const stats = createEmptyStats();
	await scanWaitlist(repository, (row) => countRow(stats, row));
	return stats;
}

export async function listOperatorPassWaitlistFollowupTexts(
	repository: OperatorPassRepository,
): Promise<Array<AdminOperatorPassWaitlistFollowupTextEntry>> {
	const entries: Array<AdminOperatorPassWaitlistFollowupTextEntry> = [];
	await scanWaitlist(repository, (row) => {
		if (!row.followup_other_text || !row.followup_answered_at) return;
		entries.push({
			other_text: row.followup_other_text,
			picks: followupKeys(row),
			about: aboutKeys(row),
			status: row.status,
			answered_at: row.followup_answered_at.toISOString(),
		});
	});
	return entries.sort((left, right) => right.answered_at.localeCompare(left.answered_at));
}
