// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {XMLParser, XMLValidator} from 'fast-xml-parser';
import {HttpResponse, http, type RequestHandler} from 'msw';

type FakeNcmecFailure = 'none' | 'submit' | 'upload' | 'fileinfo' | 'finish' | 'retract' | 'unauthorized';

interface FakeNcmecFileRecord {
	fileId: string;
	filename: string;
	size: number;
	md5: string;
	submittedDetails: boolean;
	detailsXml: string | null;
}

interface FakeNcmecReportRecord {
	reportId: string;
	reportXml: string;
	files: Array<FakeNcmecFileRecord>;
	finished: boolean;
	retracted: boolean;
	authUsername: string;
}

const BASE_URL = 'https://ncmec.test/ispws';
const USERNAME = 'usr123';
const PASSWORD = 'pswd123';
const xmlParser = new XMLParser({ignoreAttributes: false, parseTagValue: false, trimValues: true});

class MswNcmecServer {
	readonly baseUrl = BASE_URL;
	private reportCounter = 1000000;
	private fileCounter = 1;
	private requestCounter = 1;
	private readonly reports = new Map<string, FakeNcmecReportRecord>();
	private failureMode: FakeNcmecFailure = 'none';

	reset(): void {
		this.reports.clear();
		this.reportCounter = 1000000;
		this.fileCounter = 1;
		this.requestCounter = 1;
		this.failureMode = 'none';
	}

	setFailure(mode: FakeNcmecFailure): void {
		this.failureMode = mode;
	}

	getReports(): Array<FakeNcmecReportRecord> {
		return [...this.reports.values()];
	}

	getReport(reportId: string): FakeNcmecReportRecord | null {
		return this.reports.get(reportId) ?? null;
	}

	createHandlers(): Array<RequestHandler> {
		return [
			http.get(`${BASE_URL}/status`, ({request}) => this.handleStatus(request)),
			http.get(`${BASE_URL}/xsd`, () => this.text(200, '<schema></schema>', 'text/plain; charset=utf-8')),
			http.post(`${BASE_URL}/submit`, ({request}) => this.handleSubmit(request)),
			http.post(`${BASE_URL}/upload`, ({request}) => this.handleUpload(request)),
			http.post(`${BASE_URL}/fileinfo`, ({request}) => this.handleFileInfo(request)),
			http.post(`${BASE_URL}/finish`, ({request}) => this.handleFinish(request)),
			http.post(`${BASE_URL}/retract`, ({request}) => this.handleRetract(request)),
		];
	}

	private handleStatus(request: Request): HttpResponse<string> {
		const unauthorized = this.maybeUnauthorized(request);
		if (unauthorized) return unauthorized;
		return this.xml(
			200,
			ok(`<responseDescription>Remote User : ${USERNAME}, Remote Ip : 127.0.0.1</responseDescription>`),
		);
	}

	private async handleSubmit(request: Request): Promise<HttpResponse<string>> {
		const failure = this.effectiveFailure(request);
		const unauthorized = this.maybeUnauthorized(request, failure);
		if (unauthorized) return unauthorized;
		if (failure === 'submit') return this.xml(200, errorXml(1100, 'Save failed'));
		if (!isXmlContentType(request)) return this.xml(200, errorXml(4000, 'Invalid request'));
		const body = await request.text();
		if (XMLValidator.validate(body) !== true) return this.xml(200, errorXml(4110, 'Malformed XML submittal'));
		const report = parseReportDocument(body);
		if (!hasRequiredReportFields(report)) return this.xml(200, errorXml(4100, 'Validation failed'));
		const reportId = String(this.reportCounter++);
		this.reports.set(reportId, {
			reportId,
			reportXml: body,
			files: [],
			finished: false,
			retracted: false,
			authUsername: USERNAME,
		});
		return this.xml(200, ok(`<reportId>${reportId}</reportId>`));
	}

	private async handleUpload(request: Request): Promise<HttpResponse<string>> {
		const failure = this.effectiveFailure(request);
		const unauthorized = this.maybeUnauthorized(request, failure);
		if (unauthorized) return unauthorized;
		if (failure === 'upload') return this.xml(200, errorXml(1111, 'File upload failed'));
		const form = await request.formData();
		const reportId = String(form.get('id') ?? '');
		const file = form.get('file');
		const record = this.reports.get(reportId);
		if (!reportId || !record) return this.xml(200, errorXml(5001, 'Report does not exist'));
		if (record.retracted) return this.xml(200, errorXml(5101, 'Report already retracted'));
		if (record.finished) return this.xml(200, errorXml(5102, 'Report already finished'));
		if (!(file instanceof Blob)) return this.xml(200, errorXml(4200, 'Malformed file submittal'));
		const buffer = Buffer.from(await file.arrayBuffer());
		const md5 = crypto.createHash('md5').update(buffer).digest('hex');
		const fileId = `file-${this.fileCounter++}`;
		const fileRecord: FakeNcmecFileRecord = {
			fileId,
			filename: (file as File).name || 'upload.bin',
			size: buffer.byteLength,
			md5,
			submittedDetails: false,
			detailsXml: null,
		};
		record.files.push(fileRecord);
		return this.xml(200, ok(`<reportId>${reportId}</reportId><fileId>${fileId}</fileId><hash>${md5}</hash>`));
	}

	private async handleFileInfo(request: Request): Promise<HttpResponse<string>> {
		const failure = this.effectiveFailure(request);
		const unauthorized = this.maybeUnauthorized(request, failure);
		if (unauthorized) return unauthorized;
		if (failure === 'fileinfo') return this.xml(200, errorXml(1300, 'Update failed'));
		if (!isXmlContentType(request)) return this.xml(200, errorXml(4000, 'Invalid request'));
		const body = await request.text();
		if (XMLValidator.validate(body) !== true) return this.xml(200, errorXml(4110, 'Malformed XML submittal'));
		const fileDetails = parseFileDetailsDocument(body);
		if (!hasFileDetailsFields(fileDetails)) return this.xml(200, errorXml(4100, 'Validation failed'));
		const reportId = nonBlankString(fileDetails?.reportId);
		const fileId = nonBlankString(fileDetails?.fileId);
		const record = reportId ? this.reports.get(reportId) : null;
		if (!record) return this.xml(200, errorXml(5001, 'Report does not exist'));
		const file = record.files.find((candidate) => candidate.fileId === fileId);
		if (!file) return this.xml(200, errorXml(5002, 'File does not exist'));
		file.submittedDetails = true;
		file.detailsXml = body;
		return this.xml(200, ok(`<reportId>${reportId}</reportId>`));
	}

	private async handleFinish(request: Request): Promise<HttpResponse<string>> {
		const failure = this.effectiveFailure(request);
		const unauthorized = this.maybeUnauthorized(request, failure);
		if (unauthorized) return unauthorized;
		if (failure === 'finish') return this.xml(200, errorXml(1300, 'Update failed'));
		const form = await request.formData();
		const reportId = String(form.get('id') ?? '');
		const record = this.reports.get(reportId);
		if (!record) return this.xml(200, errorXml(5001, 'Report does not exist'));
		record.finished = true;
		const files = record.files.map((file) => `    <fileId>${file.fileId}</fileId>`).join('\n');
		return this.xml(
			200,
			`<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<reportDoneResponse>
    <responseCode>0</responseCode>
    <reportId>${reportId}</reportId>
    <files>
${files}
    </files>
</reportDoneResponse>`,
		);
	}

	private async handleRetract(request: Request): Promise<HttpResponse<string>> {
		const failure = this.effectiveFailure(request);
		const unauthorized = this.maybeUnauthorized(request, failure);
		if (unauthorized) return unauthorized;
		if (failure === 'retract') return this.xml(200, errorXml(1300, 'Update failed'));
		const form = await request.formData();
		const reportId = String(form.get('id') ?? '');
		const record = this.reports.get(reportId);
		if (!record) return this.xml(200, errorXml(5001, 'Report does not exist'));
		record.retracted = true;
		return this.xml(200, ok(`<reportId>${reportId}</reportId>`));
	}

	private effectiveFailure(request: Request): FakeNcmecFailure {
		return ((request.headers.get('x-test-fail') ?? '') || this.failureMode) as FakeNcmecFailure;
	}

	private maybeUnauthorized(request: Request, failure = this.failureMode): HttpResponse<string> | null {
		if (failure === 'unauthorized') return this.text(401, '');
		const auth = request.headers.get('authorization') ?? '';
		if (!auth.toLowerCase().startsWith('basic ')) return this.text(401, '');
		const decoded = Buffer.from(auth.slice(6), 'base64').toString('utf-8');
		return decoded === `${USERNAME}:${PASSWORD}` ? null : this.text(401, '');
	}

	private xml(status: number, body: string): HttpResponse<string> {
		return this.text(status, body, 'application/xml; charset=utf-8');
	}

	private text(status: number, body: string, contentType = 'text/plain; charset=utf-8'): HttpResponse<string> {
		return new HttpResponse(body, {
			status,
			headers: {
				'content-type': contentType,
				'Request-ID': `req-${this.requestCounter++}`,
			},
		});
	}
}

function ok(extra: string): string {
	return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<reportResponse>
    <responseCode>0</responseCode>
    <responseDescription>Success</responseDescription>
    ${extra}
</reportResponse>`;
}

function errorXml(code: number, description: string): string {
	return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<reportResponse>
    <responseCode>${code}</responseCode>
    <responseDescription>${description}</responseDescription>
</reportResponse>`;
}

function isXmlContentType(request: Request): boolean {
	return (request.headers.get('content-type') ?? '').toLowerCase().includes('text/xml');
}

function parseReportDocument(xml: string): Record<string, unknown> | null {
	const parsed = xmlParser.parse(xml) as Record<string, unknown>;
	return objectField(parsed, 'report');
}

function parseFileDetailsDocument(xml: string): Record<string, unknown> | null {
	const parsed = xmlParser.parse(xml) as Record<string, unknown>;
	return objectField(parsed, 'fileDetails');
}

function hasRequiredReportFields(report: Record<string, unknown> | null): boolean {
	const incidentSummary = objectField(report, 'incidentSummary');
	const reporter = objectField(report, 'reporter');
	const reportingPerson = objectField(reporter, 'reportingPerson');
	return Boolean(
		nonBlankString(incidentSummary?.incidentType) &&
			nonBlankString(incidentSummary?.incidentDateTime) &&
			nonBlankString(reportingPerson?.email),
	);
}

function hasFileDetailsFields(fileDetails: Record<string, unknown> | null): boolean {
	return Boolean(nonBlankString(fileDetails?.reportId) && nonBlankString(fileDetails?.fileId));
}

function objectField(value: unknown, key: string): Record<string, unknown> | null {
	if (!value || typeof value !== 'object' || Array.isArray(value)) return null;
	const field = (value as Record<string, unknown>)[key];
	if (!field || typeof field !== 'object' || Array.isArray(field)) return null;
	return field as Record<string, unknown>;
}

function nonBlankString(value: unknown): string | null {
	if (typeof value === 'string') {
		const trimmed = value.trim();
		return trimmed || null;
	}
	if (typeof value === 'number' || typeof value === 'bigint') {
		return String(value);
	}
	return null;
}

export const fakeNcmecServer = new MswNcmecServer();

export function createNcmecHandlers(): Array<RequestHandler> {
	return fakeNcmecServer.createHandlers();
}
