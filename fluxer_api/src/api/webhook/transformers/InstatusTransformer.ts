// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	RICH_EMBED_DESCRIPTION_MAX_LENGTH,
	RICH_EMBED_FIELD_VALUE_MAX_LENGTH,
	RICH_EMBED_FOOTER_TEXT_MAX_LENGTH,
	RICH_EMBED_TITLE_MAX_LENGTH,
	type RichEmbedRequest,
} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {InstatusWebhook} from '@fluxer/schema/src/domains/webhook/InstatusWebhookSchemas';
import {parseString, safeUrl} from '../../utils/StringUtils';

type EmbedField = NonNullable<RichEmbedRequest['fields']>[number];
type UpdateLike = {
	created_at?: string | null | undefined;
	markdown?: string | null | undefined;
};
type ComponentLike = {name?: string | null | undefined; status?: string | null | undefined};

const COLOUR_OPERATIONAL = 0x3ecf8e as const;
const COLOUR_DEGRADED = 0xf5a623 as const;
const COLOUR_PARTIAL_OUTAGE = 0xf97316 as const;
const COLOUR_MAJOR_OUTAGE = 0xe23c39 as const;
const COLOUR_MAINTENANCE = 0x3b82f6 as const;
const COLOUR_NEUTRAL = 0x5c6bc0 as const;

const STATUS_LABELS: Record<string, string> = {
	// Page.
	UP: 'All systems operational',
	HASISSUES: 'Has issues',
	// Component.
	OPERATIONAL: 'Operational',
	UNDERMAINTENANCE: 'Under maintenance',
	DEGRADEDPERFORMANCE: 'Degraded performance',
	PARTIALOUTAGE: 'Partial outage',
	MAJOROUTAGE: 'Major outage',
	// Incident.
	INVESTIGATING: 'Investigating',
	IDENTIFIED: 'Identified',
	MONITORING: 'Monitoring',
	RESOLVED: 'Resolved',
	// Maintenance.
	/**
	 * @remarks Documented but not observable. May have swapped to `PLANNED` at some point.
	 */
	NOTSTARTEDYET: 'Scheduled',
	/**
	 * @remarks Undocumented but observable. May be `NOTSTARTEDYET`'s replacement.
	 */
	PLANNED: 'Planned',
	INPROGRESS: 'In progress',
	COMPLETED: 'Completed',
};

/**
 * @remarks Instatus is inconsistent about casing. `page.status_indicator` arrives as
 * SCREAMINGCASE ("UP") whilst incident and maintenance statuses arrive in title case.
 */
function normaliseStatusToken(status: string): string {
	return status.toUpperCase().replace(/[^A-Z]/g, '');
}

function humaniseStatus(status: string): string {
	return STATUS_LABELS[normaliseStatusToken(status)] ?? status;
}

const STATUS_COLOURS: Record<string, number> = {
	// Component.
	OPERATIONAL: COLOUR_OPERATIONAL,
	UNDERMAINTENANCE: COLOUR_MAINTENANCE,
	DEGRADEDPERFORMANCE: COLOUR_DEGRADED,
	PARTIALOUTAGE: COLOUR_PARTIAL_OUTAGE,
	MAJOROUTAGE: COLOUR_MAJOR_OUTAGE,
	// Incident.
	INVESTIGATING: COLOUR_MAJOR_OUTAGE,
	IDENTIFIED: COLOUR_PARTIAL_OUTAGE,
	MONITORING: COLOUR_DEGRADED,
	RESOLVED: COLOUR_OPERATIONAL,
	// Maintenance.
	NOTSTARTEDYET: COLOUR_MAINTENANCE,
	PLANNED: COLOUR_MAINTENANCE,
	INPROGRESS: COLOUR_MAINTENANCE,
	COMPLETED: COLOUR_OPERATIONAL,
};

function statusColour(status: string): number {
	return STATUS_COLOURS[normaliseStatusToken(status)] ?? COLOUR_NEUTRAL;
}

function nonEmpty(value: string | null | undefined): string | undefined {
	if (typeof value !== 'string') return undefined;
	const trimmed = value.trim();
	return trimmed.length > 0 ? trimmed : undefined;
}

function toDate(value: string | null | undefined): Date | undefined {
	if (typeof value !== 'string' || value.length === 0) return undefined;
	const date = new Date(value);
	return Number.isNaN(date.getTime()) ? undefined : date;
}

function pickLatestUpdate(updates: ReadonlyArray<UpdateLike> | null | undefined): UpdateLike | undefined {
	if (!updates || updates.length === 0) return undefined;
	return updates.reduce((latest, update) =>
		new Date(update.created_at ?? 0).getTime() > new Date(latest.created_at ?? 0).getTime() ? update : latest,
	);
}

function statusField(status: string | null | undefined): EmbedField | undefined {
	const value = nonEmpty(status);
	if (!value) return undefined;
	return {
		name: 'Status',
		value: parseString(humaniseStatus(value), RICH_EMBED_FIELD_VALUE_MAX_LENGTH),
		inline: true,
	};
}

function affectedField(components: ReadonlyArray<ComponentLike> | null | undefined): EmbedField | undefined {
	if (!components || components.length === 0) return undefined;
	const parts: Array<string> = [];
	for (const component of components) {
		const name = nonEmpty(component.name);
		if (!name) continue;
		const status = nonEmpty(component.status);
		const statusLabel = status ? humaniseStatus(status).toLowerCase() : undefined;
		parts.push(statusLabel ? `${name} (${statusLabel})` : name);
	}
	if (parts.length === 0) return undefined;
	const value = parts.length === 1 ? parts[0] : parts.map((part) => `- ${part}`).join('\n');
	return {
		name: 'Affected components',
		value: parseString(value, RICH_EMBED_FIELD_VALUE_MAX_LENGTH),
		inline: false,
	};
}

function timestampMarkup(value: string | null | undefined): string | undefined {
	const date = toDate(value);
	return date ? `<t:${Math.floor(date.getTime() / 1000)}:F>` : undefined;
}

function windowField(startDate: string | null | undefined, endDate: string | null | undefined): EmbedField | undefined {
	const start = timestampMarkup(startDate);
	const end = timestampMarkup(endDate);
	const value = start && end ? `${start}-${end}` : (start ?? end);
	if (!value) return undefined;
	return {name: 'Window', value, inline: false};
}

function footer(page: InstatusWebhook['page'], backfilled?: boolean | null): RichEmbedRequest['footer'] {
	const indicator = nonEmpty(page?.status_indicator);
	const base = nonEmpty(page?.status_description) ?? (indicator ? humaniseStatus(indicator) : undefined);
	const parts: Array<string> = [];
	if (base) parts.push(base);
	if (backfilled) parts.push('Backfilled');
	if (parts.length === 0) return undefined;
	return {text: parseString(parts.join(' | '), RICH_EMBED_FOOTER_TEXT_MAX_LENGTH)};
}

function addFields(fields: Array<EmbedField>, entries: ReadonlyArray<EmbedField | undefined>): void {
	for (const entry of entries) {
		if (entry !== undefined) fields.push(entry);
	}
}

function transformIncident(body: InstatusWebhook): RichEmbedRequest | null {
	const incident = body.incident;
	if (!incident) return null;
	const name = nonEmpty(incident.name);
	if (!name) return null;
	const status = nonEmpty(incident.status);
	const latest = pickLatestUpdate(incident.incident_updates);
	const markdown = nonEmpty(latest?.markdown);
	const fields: Array<EmbedField> = [];
	addFields(fields, [statusField(status), affectedField(incident.affected_components)]);
	return {
		title: parseString(name, RICH_EMBED_TITLE_MAX_LENGTH),
		url: safeUrl(incident.url) ?? safeUrl(body.page?.url),
		color: status ? statusColour(status) : COLOUR_NEUTRAL,
		description: markdown ? parseString(markdown, RICH_EMBED_DESCRIPTION_MAX_LENGTH) : undefined,
		fields: fields.length > 0 ? fields : undefined,
		footer: footer(body.page, incident.backfilled),
		timestamp: toDate(latest?.created_at) ?? toDate(incident.updated_at) ?? toDate(incident.created_at),
	};
}

function transformMaintenance(body: InstatusWebhook): RichEmbedRequest | null {
	const maintenance = body.maintenance;
	if (!maintenance) return null;
	const name = nonEmpty(maintenance.name);
	if (!name) return null;
	const status = nonEmpty(maintenance.status);
	const latest = pickLatestUpdate(maintenance.maintenance_updates);
	const markdown = nonEmpty(latest?.markdown);
	const fields: Array<EmbedField> = [];
	addFields(fields, [
		statusField(status),
		windowField(maintenance.maintenance_start_date, maintenance.maintenance_end_date),
		affectedField(maintenance.affected_components),
	]);
	return {
		title: parseString(name, RICH_EMBED_TITLE_MAX_LENGTH),
		url: safeUrl(maintenance.url) ?? safeUrl(body.page?.url),
		color: status ? statusColour(status) : COLOUR_MAINTENANCE,
		description: markdown ? parseString(markdown, RICH_EMBED_DESCRIPTION_MAX_LENGTH) : undefined,
		fields: fields.length > 0 ? fields : undefined,
		footer: footer(body.page, maintenance.backfilled),
		timestamp: toDate(latest?.created_at) ?? toDate(maintenance.updated_at) ?? toDate(maintenance.created_at),
	};
}

function transformComponentUpdate(body: InstatusWebhook): RichEmbedRequest | null {
	const update = body.component_update;
	const component = body.component;
	if (!update && !component) return null;
	const newStatus = nonEmpty(update?.new_status) ?? nonEmpty(component?.status);
	const componentName = nonEmpty(component?.name) ?? 'A component';
	const statusLabel = newStatus ? humaniseStatus(newStatus).toLowerCase() : 'updated';
	return {
		title: parseString(`${componentName} - ${statusLabel}`, RICH_EMBED_TITLE_MAX_LENGTH),
		url: safeUrl(body.page?.url),
		color: newStatus ? statusColour(newStatus) : COLOUR_NEUTRAL,
		footer: footer(body.page),
		timestamp: toDate(update?.created_at) ?? toDate(component?.created_at),
	};
}

export function transformInstatusWebhook(body: InstatusWebhook): RichEmbedRequest | null {
	if (body.incident) return transformIncident(body);
	if (body.maintenance) return transformMaintenance(body);
	if (body.component_update || body.component) return transformComponentUpdate(body);
	return null;
}
