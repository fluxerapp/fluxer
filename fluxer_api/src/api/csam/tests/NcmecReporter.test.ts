// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildNcmecFileDetailsXml,
	buildNcmecReportXml,
	createNcmecApiConfig,
	NcmecReporter,
} from '@app/api/csam/NcmecReporter';
import {INVALID_REPORT_XML, MALFORMED_REPORT_XML} from '@app/api/test/fixtures/ncmec/NcmecXmlFixtures';
import {fakeNcmecServer} from '@app/api/test/Setup';
import {describe, expect, test} from 'vitest';

function reporter(): NcmecReporter {
	return new NcmecReporter({config: createNcmecApiConfig(), fetch});
}

function validReportXml(overrides: {priorNcmecReportIds?: Array<string>} = {}): string {
	return buildNcmecReportXml({
		attachmentUrl: 'https://cdn.example.com/x.png',
		reportedAt: new Date('2026-04-15T12:00:00Z'),
		reporterFullName: 'Lilith Example',
		reporterEmail: 'admin@example.com',
		reportedUser: {
			id: 42n,
			screenName: 'evil-user',
			displayName: 'Evil User',
			espService: 'Fluxer',
			permanentlyDisabledAt: null,
			person: {
				emails: [{address: 'evil-user@example.com', type: 'Home', verified: true}],
				phones: [{number: '+15551234567', type: 'Mobile'}],
				dateOfBirth: '1999-02-03',
			},
			ipCaptureEvents: [
				{
					ipAddress: '203.0.113.10',
					eventName: 'Upload',
					dateTime: new Date('2026-04-15T11:55:00Z'),
				},
			],
		},
		priorNcmecReportIds: overrides.priorNcmecReportIds ?? [],
		additionalInfo: 'Test report.',
	});
}

describe('NcmecReporter', () => {
	test('submitReport returns a numeric report ID', async () => {
		const reportId = await reporter().submitReport(validReportXml());
		expect(reportId).toMatch(/^\d+$/);
		expect(fakeNcmecServer.getReport(reportId)).not.toBeNull();
	});
	test('uploadEvidence returns the server-computed md5', async () => {
		const r = reporter();
		const reportId = await r.submitReport(validReportXml());
		const upload = await r.uploadEvidence(reportId, new Uint8Array([1, 2, 3]), 'evidence.png');
		expect(upload.fileId).toMatch(/^file-\d+$/);
		expect(upload.md5).toMatch(/^[0-9a-f]{32}$/);
	});
	test('submitFileDetails marks the file as having details submitted', async () => {
		const r = reporter();
		const reportId = await r.submitReport(validReportXml());
		const {fileId} = await r.uploadEvidence(reportId, new Uint8Array([4, 5, 6]), 'evidence.png');
		await r.submitFileDetails(
			buildNcmecFileDetailsXml({
				reportId,
				fileId,
				filename: 'evidence.png',
				uploadedToEspTimestamp: new Date('2026-04-15T11:55:00Z'),
				fileViewedByEsp: true,
				ipCaptureEvent: {
					ipAddress: '203.0.113.10',
					eventName: 'Upload',
					dateTime: new Date('2026-04-15T11:55:00Z'),
				},
				additionalInfo: 'context',
			}),
		);
		const record = fakeNcmecServer.getReport(reportId)!;
		expect(record.files.find((f) => f.fileId === fileId)?.submittedDetails).toBe(true);
	});
	test('submitFileDetails throws when file details fail documented validation', async () => {
		await expect(
			reporter().submitFileDetails(`<?xml version="1.0" encoding="UTF-8"?>
<fileDetails xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xsi:noNamespaceSchemaLocation="https://report.cybertip.org/ispws/xsd">
    <originalFileName>missing-ids.jpg</originalFileName>
</fileDetails>`),
		).rejects.toThrow('responseCode 4100');
	});
	test('finish returns every uploaded file id', async () => {
		const r = reporter();
		const reportId = await r.submitReport(validReportXml());
		const a = await r.uploadEvidence(reportId, new Uint8Array([7]), 'a.png');
		const b = await r.uploadEvidence(reportId, new Uint8Array([8]), 'b.png');
		const result = await r.finish(reportId);
		expect(result.reportId).toBe(reportId);
		expect(result.fileIds).toEqual([a.fileId, b.fileId]);
	});
	test('retract marks the report as retracted', async () => {
		const r = reporter();
		const reportId = await r.submitReport(validReportXml());
		await r.retract(reportId);
		expect(fakeNcmecServer.getReport(reportId)!.retracted).toBe(true);
	});
	test('submitReport throws when the server rejects the XML', async () => {
		await expect(reporter().submitReport(INVALID_REPORT_XML)).rejects.toThrow('responseCode 4100');
	});
	test('submitReport surfaces the documented malformed-xml response code', async () => {
		await expect(reporter().submitReport(MALFORMED_REPORT_XML)).rejects.toThrow('responseCode 4110');
	});
	test('submitReport throws when the server returns 401', async () => {
		fakeNcmecServer.setFailure('unauthorized');
		await expect(reporter().submitReport(validReportXml())).rejects.toThrow(/http 401/);
		fakeNcmecServer.setFailure('none');
	});
	test('buildNcmecReportXml emits personOrUserReported with priorCTReports and espIdentifier', () => {
		const xml = validReportXml({priorNcmecReportIds: ['NCMEC-100', 'NCMEC-200']});
		expect(xml).toContain('<personOrUserReported>');
		expect(xml).toContain('<personOrUserReportedPerson>');
		expect(xml).toContain('<espIdentifier>42</espIdentifier>');
		expect(xml).toContain('<espService>Fluxer</espService>');
		expect(xml).toContain('<screenName>evil-user</screenName>');
		expect(xml).toContain('<displayName>Evil User</displayName>');
		expect(xml).toContain('<email type="Home" verified="true">evil-user@example.com</email>');
		expect(xml).toContain('<phone type="Mobile">+15551234567</phone>');
		expect(xml).toContain('<dateOfBirth>1999-02-03</dateOfBirth>');
		expect(xml).toContain('<ipAddress>203.0.113.10</ipAddress>');
		expect(xml).toContain('<eventName>Upload</eventName>');
		expect(xml).toContain('<priorCTReports>NCMEC-100</priorCTReports>');
		expect(xml).toContain('<priorCTReports>NCMEC-200</priorCTReports>');
		expect(xml).toContain('<allEmailsReported>true</allEmailsReported>');
		expect(xml).toContain('<incidentType>Child Pornography (possession, manufacture, and distribution)</incidentType>');
		expect(xml).toContain('<platform>Fluxer</platform>');
	});
	test('buildNcmecReportXml emits incident additionalInfo at the report root', () => {
		const xml = validReportXml();
		expect(xml).toContain('<additionalInfo>Test report.</additionalInfo>');
		expect(xml).toMatch(/<\/personOrUserReported>\s*<additionalInfo>Test report\.<\/additionalInfo>\s*<\/report>/);
	});
	test('buildNcmecReportXml emits accountPermanentlyDisabled when the user was banned', () => {
		const xml = buildNcmecReportXml({
			attachmentUrl: 'https://cdn.example.com/x.png',
			reportedAt: new Date('2026-04-15T12:00:00Z'),
			reporterFullName: 'Lilith Example',
			reporterEmail: 'admin@example.com',
			reportedUser: {
				id: 42n,
				screenName: null,
				displayName: null,
				espService: null,
				permanentlyDisabledAt: new Date('2026-04-14T08:00:00Z'),
				person: null,
				ipCaptureEvents: [],
			},
			priorNcmecReportIds: [],
			additionalInfo: null,
		});
		expect(xml).toContain('<accountPermanentlyDisabled disabledDate="2026-04-14T08:00:00.000Z"');
	});
	test('buildNcmecFileDetailsXml emits upload metadata for the uploaded file', () => {
		const xml = buildNcmecFileDetailsXml({
			reportId: 'NCMEC-100',
			fileId: 'file-100',
			filename: 'evidence.png',
			uploadedToEspTimestamp: new Date('2026-04-15T11:55:00Z'),
			fileViewedByEsp: true,
			ipCaptureEvent: {
				ipAddress: '203.0.113.10',
				eventName: 'Upload',
				dateTime: new Date('2026-04-15T11:55:00Z'),
			},
			additionalInfo: 'Uploaded from Fluxer',
		});
		expect(xml).toContain('<uploadedToEspTimestamp>2026-04-15T11:55:00.000Z</uploadedToEspTimestamp>');
		expect(xml).toContain('<fileViewedByEsp>true</fileViewedByEsp>');
		expect(xml).toContain('<ipCaptureEvent>');
		expect(xml).toContain('<ipAddress>203.0.113.10</ipAddress>');
		expect(xml).toContain('<additionalInfo>Uploaded from Fluxer</additionalInfo>');
	});
	test('buildNcmecReportXml requires a reporter email address', () => {
		expect(() =>
			buildNcmecReportXml({
				attachmentUrl: 'https://cdn.example.com/x.png',
				reportedAt: new Date('2026-04-15T12:00:00Z'),
				reporterFullName: 'Lilith Example',
				reporterEmail: '' as string,
				reportedUser: {
					id: 42n,
					screenName: null,
					displayName: null,
					espService: null,
					permanentlyDisabledAt: null,
					person: null,
					ipCaptureEvents: [],
				},
				priorNcmecReportIds: [],
				additionalInfo: null,
			}),
		).toThrow('NCMEC reporter email');
	});
});
