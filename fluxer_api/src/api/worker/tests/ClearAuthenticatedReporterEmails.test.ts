// SPDX-License-Identifier: AGPL-3.0-or-later

import {createReportID, type ReportID, type UserID} from '@app/api/BrandedTypes';
import {IAR_SUBMISSION_COLUMNS, type IARSubmissionRow} from '@app/api/database/types/ReportTypes';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {clearAuthenticatedReporterEmails} from '@app/api/worker/tasks/ClearAuthenticatedReporterEmails';
import {beforeEach, describe, expect, it} from 'vitest';

let sequence = 1_490_000_000_000_000_000n;

function nextId(): bigint {
	sequence += 1n;
	return sequence;
}

describe('clearAuthenticatedReporterEmails', () => {
	let repository: ReportRepository;

	beforeEach(() => {
		repository = new ReportRepository();
	});

	async function seed(reporterId: bigint | null, reporterEmail: string | null): Promise<ReportID> {
		const id = nextId();
		await repository.createReport({
			...Object.fromEntries(IAR_SUBMISSION_COLUMNS.map((column) => [column, null])),
			report_id: id,
			reporter_id: reporterId,
			reporter_email: reporterEmail,
			reporter_full_legal_name: reporterId === null ? 'Notifier Name' : null,
			reported_at: new Date(),
			status: 0,
			report_type: 1,
			category: 'harassment',
		} as IARSubmissionRow);
		return createReportID(id);
	}

	async function storedEmail(reportId: ReportID): Promise<string | null | undefined> {
		return (await repository.getReport(reportId))?.reporterEmail;
	}

	it('clears the stored address on account reports and keeps it on notices filed without an account', async () => {
		const reporterId = nextId();
		const accountReports = [];
		for (let index = 0; index < 5; index++) {
			accountReports.push(await seed(reporterId, `reporter${index}@example.com`));
		}
		const withoutEmail = await seed(nextId(), null);
		const notices = [await seed(null, 'notifier@example.com'), await seed(null, 'other-notifier@example.com')];

		const summary = await clearAuthenticatedReporterEmails(repository, 2);

		expect(summary).toEqual({scanned: 8, cleared: 5, skipped: 0, failed: 0});
		for (const reportId of accountReports) {
			expect(await storedEmail(reportId)).toBeNull();
		}
		expect(await storedEmail(withoutEmail)).toBeNull();
		expect(await storedEmail(notices[0]!)).toBe('notifier@example.com');
		expect(await storedEmail(notices[1]!)).toBe('other-notifier@example.com');
		const notice = await repository.getReport(notices[0]!);
		expect(notice?.reporterFullLegalName).toBe('Notifier Name');
	});

	it('changes nothing on a second run', async () => {
		await seed(nextId(), 'reporter@example.com');
		await seed(null, 'notifier@example.com');
		await clearAuthenticatedReporterEmails(repository);

		const summary = await clearAuthenticatedReporterEmails(repository);

		expect(summary).toEqual({scanned: 2, cleared: 0, skipped: 0, failed: 0});
	});

	it('keeps the other fields of the report', async () => {
		const reporterId = nextId();
		const reportId = await seed(reporterId, 'reporter@example.com');
		const before = await repository.getReport(reportId);

		await clearAuthenticatedReporterEmails(repository);

		expect(await repository.getReport(reportId)).toEqual({...before, reporterEmail: null});
	});

	it('counts a report that could not be updated and moves on to the next', async () => {
		const failing = await seed(nextId(), 'first@example.com');
		const other = await seed(nextId(), 'second@example.com');
		const failingRepository = Object.create(repository) as ReportRepository;
		failingRepository.clearReporterEmail = async (reportId: ReportID, reporterId: UserID) => {
			if (reportId === failing) throw new Error('write timeout');
			return repository.clearReporterEmail(reportId, reporterId);
		};

		const summary = await clearAuthenticatedReporterEmails(failingRepository);

		expect(summary).toEqual({scanned: 2, cleared: 1, skipped: 0, failed: 1});
		expect(await storedEmail(failing)).toBe('first@example.com');
		expect(await storedEmail(other)).toBeNull();
	});

	it('counts a report deleted while the backfill ran as skipped', async () => {
		const deleted = await seed(nextId(), 'gone@example.com');
		const racingRepository = Object.create(repository) as ReportRepository;
		racingRepository.clearReporterEmail = async (reportId: ReportID, reporterId: UserID) => {
			await repository.deleteReport(reportId);
			return repository.clearReporterEmail(reportId, reporterId);
		};

		const summary = await clearAuthenticatedReporterEmails(racingRepository);

		expect(summary).toEqual({scanned: 1, cleared: 0, skipped: 1, failed: 0});
		expect(await repository.getReport(deleted)).toBeNull();
	});
});
