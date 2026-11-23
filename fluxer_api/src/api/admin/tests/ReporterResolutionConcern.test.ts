// SPDX-License-Identifier: AGPL-3.0-or-later

import {reportReference} from '@app/api/admin/services/ReporterResolutionNotifier';
import type {IARSubmission} from '@app/api/report/ReportModels';
import {describe, expect, it} from 'vitest';

function report(category: string, reason: string | null): IARSubmission {
	return {category, reason, reportType: -1, reportedAt: new Date('2026-11-15T00:00:00Z')} as unknown as IARSubmission;
}

describe('report outcome concern', () => {
	it('uses the self-harm copy for a wellbeing concern, whatever the category', () => {
		expect(reportReference(report('self_harm', 'wellbeing_concern')).concern).toBe('self_harm');
		expect(reportReference(report('other', 'wellbeing_concern')).concern).toBe('self_harm');
	});

	it('uses the general copy for reports of self-harm promotion', () => {
		expect(reportReference(report('self_harm', 'self_harm_promotion')).concern).toBe('other');
	});

	it('falls back to the category for reports without a reason', () => {
		expect(reportReference(report('self_harm', null)).concern).toBe('self_harm');
		expect(reportReference(report('spam', null)).concern).toBe('other');
	});
});
