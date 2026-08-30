// SPDX-License-Identifier: AGPL-3.0-or-later

import {ms} from 'itty-time';
import {Config} from '../Config';
import {Logger} from '../Logger';
import {EXTERNAL_RESPONSE_LIMITS} from '../utils/ExternalResponseLimits';
import * as FetchUtils from '../utils/FetchUtils';

const NCMEC_REQUEST_TIMEOUT_MS = ms('2 minutes');

type NcmecOperation = 'report' | 'evidence' | 'fileinfo' | 'finish' | 'retract';
type NcmecApiConfig =
	| {
			enabled: true;
			baseUrl: string;
			username: string;
			password: string;
	  }
	| {
			enabled: false;
			baseUrl: null;
			username: null;
			password: null;
	  };

interface NcmecApiDeps {
	config: NcmecApiConfig;
	fetch: typeof fetch;
}

class NcmecRequestError extends Error {
	constructor(
		message: string,
		public readonly httpStatus: number,
		public readonly responseCode: number | null,
		public readonly responseDescription: string | null,
		public readonly requestId: string | null,
		public readonly body: string | null,
	) {
		super(message);
	}
}

interface NcmecResponseMeta {
	responseCode: number | null;
	responseDescription: string | null;
	requestId: string | null;
}

export interface NcmecApiClient {
	submitReport(reportXml: string): Promise<string>;
	uploadEvidence(
		reportId: string,
		body: Uint8Array,
		filename: string,
	): Promise<{
		fileId: string;
		md5: string | null;
	}>;
	submitFileDetails(fileDetailsXml: string): Promise<void>;
	finish(reportId: string): Promise<{
		reportId: string;
		fileIds: Array<string>;
	}>;
	retract(reportId: string): Promise<void>;
}

export class NcmecReporter implements NcmecApiClient {
	constructor(private readonly deps: NcmecApiDeps) {}

	async submitReport(reportXml: string): Promise<string> {
		return this.telemetered('report', async () => {
			const {res, text} = await this.postXml('/submit', reportXml);
			this.assertOk(res, text);
			const reportId = takeTag(text, 'reportId');
			if (!reportId) throw this.fail('NCMEC /submit returned no reportId.', res, text);
			return reportId;
		});
	}

	async uploadEvidence(
		reportId: string,
		body: Uint8Array,
		filename: string,
	): Promise<{
		fileId: string;
		md5: string | null;
	}> {
		return this.telemetered('evidence', async () => {
			const form = new FormData();
			form.append('id', reportId);
			const blobBytes = new Uint8Array(body.byteLength);
			blobBytes.set(body);
			form.append('file', new Blob([blobBytes]), filename);
			const {res, text} = await this.postForm('/upload', form);
			this.assertOk(res, text);
			const fileId = takeTag(text, 'fileId');
			if (!fileId) throw this.fail('NCMEC /upload returned no fileId.', res, text);
			return {fileId, md5: takeTag(text, 'hash')};
		});
	}

	async submitFileDetails(fileDetailsXml: string): Promise<void> {
		await this.telemetered('fileinfo', async () => {
			const {res, text} = await this.postXml('/fileinfo', fileDetailsXml);
			this.assertOk(res, text);
		});
	}

	async finish(reportId: string): Promise<{
		reportId: string;
		fileIds: Array<string>;
	}> {
		return this.telemetered('finish', async () => {
			const form = new FormData();
			form.append('id', reportId);
			const {res, text} = await this.postForm('/finish', form);
			this.assertOk(res, text);
			const returned = takeTag(text, 'reportId');
			if (!returned) throw this.fail('NCMEC /finish returned no reportId.', res, text);
			return {reportId: returned, fileIds: takeTags(text, 'fileId')};
		});
	}

	async retract(reportId: string): Promise<void> {
		await this.telemetered('retract', async () => {
			const form = new FormData();
			form.append('id', reportId);
			const {res, text} = await this.postForm('/retract', form);
			this.assertOk(res, text);
		});
	}

	private postXml(
		path: string,
		body: string,
	): Promise<{
		res: Response;
		text: string;
	}> {
		return this.post(path, {body, headers: {'Content-Type': 'text/xml; charset=utf-8'}});
	}

	private postForm(
		path: string,
		form: FormData,
	): Promise<{
		res: Response;
		text: string;
	}> {
		return this.post(path, {body: form});
	}

	private async post(
		path: string,
		init: RequestInit,
	): Promise<{
		res: Response;
		text: string;
	}> {
		const cfg = this.requireEnabled();
		const res = await this.deps.fetch(`${cfg.baseUrl}${path}`, {
			method: 'POST',
			...init,
			headers: {
				Authorization: basicAuth(cfg.username, cfg.password),
				...(init.headers ?? {}),
			},
			signal: AbortSignal.timeout(NCMEC_REQUEST_TIMEOUT_MS),
		});
		const text = await FetchUtils.streamToStringWithLimit(res.body, {
			maxBytes: EXTERNAL_RESPONSE_LIMITS.ncmecResponseBytes,
			headers: res.headers,
			url: res.url,
			description: 'NCMEC response',
		}).catch(() => '');
		return {res, text};
	}

	private assertOk(res: Response, body: string): void {
		const meta = parseResponseMeta(res, body);
		if (res.ok && meta.responseCode === 0) return;
		throw this.fail(
			meta.responseCode === null
				? `NCMEC request failed (http ${res.status}).`
				: `NCMEC request failed (http ${res.status}, responseCode ${meta.responseCode}).`,
			res,
			body,
			meta,
		);
	}

	private fail(message: string, res: Response, body: string, meta = parseResponseMeta(res, body)): NcmecRequestError {
		Logger.warn(
			{
				status: res.status,
				responseCode: meta.responseCode,
				description: meta.responseDescription,
				body: body || '<no body>',
			},
			'NCMEC request failed',
		);
		return new NcmecRequestError(
			message,
			res.status,
			meta.responseCode,
			meta.responseDescription,
			meta.requestId,
			body || null,
		);
	}

	private requireEnabled(): Extract<
		NcmecApiConfig,
		{
			enabled: true;
		}
	> {
		if (!this.deps.config.enabled) throw new Error('NCMEC reporting is disabled.');
		return this.deps.config;
	}

	private async telemetered<T>(operation: NcmecOperation, fn: () => Promise<T>): Promise<T> {
		if (!this.deps.config.enabled) {
			throw new Error('NCMEC reporting is disabled.');
		}
		try {
			const out = await fn();
			return out;
		} catch (error) {
			Logger.error({error, operation}, 'NCMEC operation failed');
			throw error;
		}
	}
}

export function createNcmecApiConfig(): NcmecApiConfig {
	if (!Config.ncmec.enabled) return {enabled: false, baseUrl: null, username: null, password: null};
	return {
		enabled: true,
		baseUrl: normaliseBaseUrl(Config.ncmec.baseUrl),
		username: requireConfigValue(Config.ncmec.username, 'NCMEC username'),
		password: requireConfigValue(Config.ncmec.password, 'NCMEC password'),
	};
}

function basicAuth(user: string, pass: string): string {
	return `Basic ${Buffer.from(`${user}:${pass}`, 'utf-8').toString('base64')}`;
}

function normaliseBaseUrl(raw: string | undefined | null): string {
	const trimmed = (raw ?? '').trim();
	if (!trimmed) throw new Error('NCMEC base URL is required when reporting is enabled.');
	return trimmed.replace(/\/+$/, '');
}

function requireConfigValue(value: string | undefined | null, label: string): string {
	const trimmed = (value ?? '').trim();
	if (!trimmed) throw new Error(`${label} is required when reporting is enabled.`);
	return trimmed;
}

function requireNonBlank(value: string | undefined | null, label: string): string {
	const trimmed = (value ?? '').trim();
	if (!trimmed) throw new Error(`${label} is required.`);
	return trimmed;
}

function takeTag(xml: string, tag: string): string | null {
	if (!xml) return null;
	const match = xml.match(new RegExp(`<${tag}>([\\s\\S]*?)</${tag}>`, 'i'));
	return match?.[1]?.trim() || null;
}

function takeTags(xml: string, tag: string): Array<string> {
	if (!xml) return [];
	return [...xml.matchAll(new RegExp(`<${tag}>([\\s\\S]*?)</${tag}>`, 'gi'))]
		.map((match) => match[1]?.trim() ?? '')
		.filter(Boolean);
}

function parseResponseCode(value: string | null): number | null {
	if (!value) return null;
	const parsed = Number.parseInt(value, 10);
	return Number.isFinite(parsed) ? parsed : null;
}

function parseResponseMeta(res: Response, body: string): NcmecResponseMeta {
	return {
		responseCode: parseResponseCode(takeTag(body, 'responseCode')),
		responseDescription: takeTag(body, 'responseDescription'),
		requestId: res.headers.get('Request-ID'),
	};
}

interface NcmecReportedUser {
	id: bigint | null;
	screenName: string | null;
	displayName: string | null;
	espService: string | null;
	permanentlyDisabledAt: Date | null;
	person: NcmecReportedUserPerson | null;
	ipCaptureEvents: ReadonlyArray<NcmecIpCaptureEvent>;
}

interface NcmecReportedUserPerson {
	emails: ReadonlyArray<NcmecContactEmail>;
	phones: ReadonlyArray<NcmecContactPhone>;
	dateOfBirth: string | null;
}

interface NcmecContactEmail {
	address: string;
	type?: 'Home' | 'Work' | 'Business' | 'Recovery';
	verified?: boolean | null;
}

interface NcmecContactPhone {
	number: string;
	type?: 'Mobile' | 'Home' | 'Business' | 'Work' | 'Fax' | 'Internet' | 'Recovery';
	verified?: boolean | null;
}

interface NcmecIpCaptureEvent {
	ipAddress: string;
	eventName?: 'Login' | 'Registration' | 'Purchase' | 'Upload' | 'Other' | 'Unknown';
	dateTime?: Date | null;
	possibleProxy?: boolean | null;
}

interface NcmecReportXmlInput {
	attachmentUrl: string;
	reportedAt: Date;
	reporterFullName: string;
	reporterEmail: string;
	reportedUser: NcmecReportedUser;
	priorNcmecReportIds: ReadonlyArray<string>;
	additionalInfo: string | null;
}

interface NcmecFileDetailsXmlInput {
	reportId: string;
	fileId: string;
	filename: string;
	uploadedToEspTimestamp: Date | null;
	fileViewedByEsp?: boolean;
	ipCaptureEvent: NcmecIpCaptureEvent | null;
	additionalInfo: string | null;
}

export function buildNcmecReportXml(input: NcmecReportXmlInput): string {
	const schemaLocation = `${schemaRoot()}/xsd`;
	const {firstName, lastName} = splitName(input.reporterFullName);
	const reporterEmail = requireNonBlank(input.reporterEmail, 'NCMEC reporter email');
	const screenName = input.reportedUser.screenName
		? `        <screenName>${escapeXml(input.reportedUser.screenName)}</screenName>\n`
		: '';
	const displayName = input.reportedUser.displayName
		? `        <displayName>${escapeXml(input.reportedUser.displayName)}</displayName>\n`
		: '';
	const reportedUserPerson = renderReportedUserPerson(input.reportedUser.person ?? null);
	const espIdentifier =
		input.reportedUser.id !== null
			? `        <espIdentifier>${escapeXml(input.reportedUser.id.toString())}</espIdentifier>\n`
			: '';
	const espService = input.reportedUser.espService
		? `        <espService>${escapeXml(input.reportedUser.espService)}</espService>\n`
		: '';
	const ipCaptureEvents = (input.reportedUser.ipCaptureEvents ?? [])
		.map((event) => renderIpCaptureEvent(event, '        '))
		.join('');
	const priorReports = input.priorNcmecReportIds
		.map((id) => `        <priorCTReports>${escapeXml(id)}</priorCTReports>\n`)
		.join('');
	const disabled = input.reportedUser.permanentlyDisabledAt
		? `        <accountPermanentlyDisabled disabledDate="${escapeXml(input.reportedUser.permanentlyDisabledAt.toISOString())}" userNotified="false">true</accountPermanentlyDisabled>\n`
		: '';
	const allEmailsReported =
		input.reportedUser.person && input.reportedUser.person.emails.length > 0
			? '        <allEmailsReported>true</allEmailsReported>\n'
			: '';
	const reportAdditionalInfo = input.additionalInfo
		? `    <additionalInfo>${escapeXml(input.additionalInfo)}</additionalInfo>\n`
		: '';
	return `<?xml version="1.0" encoding="UTF-8"?>
<report xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="${escapeXml(schemaLocation)}">
    <incidentSummary>
        <incidentType>Child Pornography (possession, manufacture, and distribution)</incidentType>
        <platform>Fluxer</platform>
        <incidentDateTime>${escapeXml(input.reportedAt.toISOString())}</incidentDateTime>
    </incidentSummary>
    <internetDetails>
        <webPageIncident>
            <url>${escapeXml(input.attachmentUrl)}</url>
        </webPageIncident>
    </internetDetails>
    <reporter>
        <reportingPerson>
            <firstName>${escapeXml(firstName)}</firstName>
            <lastName>${escapeXml(lastName)}</lastName>
            <email>${escapeXml(reporterEmail)}</email>
        </reportingPerson>
    </reporter>
    <personOrUserReported>
${reportedUserPerson}${espIdentifier}${espService}${screenName}${displayName}${ipCaptureEvents}${priorReports}${disabled}${allEmailsReported}    </personOrUserReported>
${reportAdditionalInfo}</report>`;
}

export function buildNcmecFileDetailsXml(input: NcmecFileDetailsXmlInput): string {
	const schemaLocation = `${schemaRoot()}/xsd`;
	const uploadedToEspTimestamp = input.uploadedToEspTimestamp
		? `    <uploadedToEspTimestamp>${escapeXml(input.uploadedToEspTimestamp.toISOString())}</uploadedToEspTimestamp>\n`
		: '';
	const fileViewedByEsp = input.fileViewedByEsp === true ? '    <fileViewedByEsp>true</fileViewedByEsp>\n' : '';
	const ipCaptureEvent = input.ipCaptureEvent ? renderIpCaptureEvent(input.ipCaptureEvent, '    ') : '';
	const additionalInfo = input.additionalInfo
		? `    <additionalInfo>${escapeXml(input.additionalInfo)}</additionalInfo>\n`
		: '';
	return `<?xml version="1.0" encoding="UTF-8"?>
<fileDetails xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="${escapeXml(schemaLocation)}">
    <reportId>${escapeXml(input.reportId)}</reportId>
    <fileId>${escapeXml(input.fileId)}</fileId>
    <originalFileName>${escapeXml(input.filename)}</originalFileName>
${uploadedToEspTimestamp}${fileViewedByEsp}${ipCaptureEvent}${additionalInfo}</fileDetails>`;
}

function schemaRoot(): string {
	const trimmed = (Config.ncmec.baseUrl ?? '').trim().replace(/\/+$/, '');
	return trimmed || 'https://report.cybertip.org/ispws';
}

function splitName(fullName: string): {
	firstName: string;
	lastName: string;
} {
	const parts = fullName.trim().split(/\s+/).filter(Boolean);
	if (parts.length === 0) return {firstName: 'Unknown', lastName: 'Reporter'};
	if (parts.length === 1) return {firstName: parts[0]!, lastName: 'Reporter'};
	return {firstName: parts[0]!, lastName: parts.slice(1).join(' ')};
}

function renderReportedUserPerson(input: NcmecReportedUserPerson | null): string {
	if (!input) return '';
	const phones = input.phones.map((phone) => renderPhone(phone)).join('');
	const emails = input.emails.map((email) => renderEmail(email)).join('');
	const dateOfBirth = input.dateOfBirth
		? `            <dateOfBirth>${escapeXml(input.dateOfBirth)}</dateOfBirth>\n`
		: '';
	if (!phones && !emails && !dateOfBirth) {
		return '';
	}
	return `        <personOrUserReportedPerson>\n${phones}${emails}${dateOfBirth}        </personOrUserReportedPerson>\n`;
}

function renderPhone(phone: NcmecContactPhone): string {
	const attrs: Array<string> = [];
	if (phone.type) {
		attrs.push(`type="${escapeXml(phone.type)}"`);
	}
	if (typeof phone.verified === 'boolean') {
		attrs.push(`verified="${phone.verified ? 'true' : 'false'}"`);
	}
	const attrText = attrs.length > 0 ? ` ${attrs.join(' ')}` : '';
	return `            <phone${attrText}>${escapeXml(phone.number)}</phone>\n`;
}

function renderEmail(email: NcmecContactEmail): string {
	const attrs: Array<string> = [];
	if (email.type) {
		attrs.push(`type="${escapeXml(email.type)}"`);
	}
	if (typeof email.verified === 'boolean') {
		attrs.push(`verified="${email.verified ? 'true' : 'false'}"`);
	}
	const attrText = attrs.length > 0 ? ` ${attrs.join(' ')}` : '';
	return `            <email${attrText}>${escapeXml(email.address)}</email>\n`;
}

function renderIpCaptureEvent(event: NcmecIpCaptureEvent, indent: string): string {
	const eventName = event.eventName ? `${indent}    <eventName>${escapeXml(event.eventName)}</eventName>\n` : '';
	const dateTime = event.dateTime
		? `${indent}    <dateTime>${escapeXml(event.dateTime.toISOString())}</dateTime>\n`
		: '';
	const possibleProxy =
		typeof event.possibleProxy === 'boolean'
			? `${indent}    <possibleProxy>${event.possibleProxy ? 'true' : 'false'}</possibleProxy>\n`
			: '';
	return `${indent}<ipCaptureEvent>\n${indent}    <ipAddress>${escapeXml(event.ipAddress)}</ipAddress>\n${eventName}${dateTime}${possibleProxy}${indent}</ipCaptureEvent>\n`;
}

function escapeXml(value: string): string {
	return value
		.replaceAll('&', '&amp;')
		.replaceAll('<', '&lt;')
		.replaceAll('>', '&gt;')
		.replaceAll('"', '&quot;')
		.replaceAll("'", '&apos;');
}
