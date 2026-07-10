// SPDX-License-Identifier: AGPL-3.0-or-later

import {createStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const InstatusMeta = z.object({
	unsubscribe: createStringType(0, 2048).nullish(),
	documentation: createStringType(0, 2048).nullish(),
});

const InstatusPage = z.object({
	id: createStringType(0, 256).nullish(),
	status_indicator: createStringType(0, 256).nullish(),
	status_description: createStringType(0, 1024).nullish(),
	url: createStringType(0, 2048).nullish(),
});

const InstatusAffectedComponent = z.object({
	id: createStringType(0, 256).nullish(),
	name: createStringType(0, 1024).nullish(),
	status: createStringType(0, 256).nullish(),
});

const InstatusComponentUpdateComponent = InstatusAffectedComponent.extend({
	created_at: createStringType(0, 64).nullish(),
});

const InstatusIncidentIncidentUpdate = z.object({
	id: createStringType(0, 256).nullish(),
	incident_id: createStringType(0, 256).nullish(),
	markdown: createStringType(0, 65536).nullish(),
	status: createStringType(0, 256).nullish(),
	created_at: createStringType(0, 64).nullish(),
	updated_at: createStringType(0, 64).nullish(),
});

const InstatusIncident = z.object({
	id: createStringType(0, 256).nullish(),
	name: createStringType(0, 1024).nullish(),
	url: createStringType(0, 2048).nullish(),
	status: createStringType(0, 256).nullish(),
	backfilled: z.boolean().nullish(),
	created_at: createStringType(0, 64).nullish(),
	updated_at: createStringType(0, 64).nullish(),
	resolved_at: createStringType(0, 64).nullish(),
	incident_updates: z.array(InstatusIncidentIncidentUpdate).nullish(),
	/**
	 * @remarks Undocumented.
	 */
	affected_components: z.array(InstatusAffectedComponent).nullish(),
});

const InstatusMaintenanceMaintenanceUpdate = z.object({
	id: createStringType(0, 256).nullish(),
	maintenance_id: createStringType(0, 256).nullish(),
	markdown: createStringType(0, 65536).nullish(),
	created_at: createStringType(0, 64).nullish(),
	updated_at: createStringType(0, 64).nullish(),
});

const InstatusMaintenance = z.object({
	id: createStringType(0, 256).nullish(),
	name: createStringType(0, 1024).nullish(),
	url: createStringType(0, 2048).nullish(),
	status: createStringType(0, 256).nullish(),
	maintenance_start_date: createStringType(0, 64).nullish(),
	maintenance_end_date: createStringType(0, 64).nullish(),
	backfilled: z.boolean().nullish(),
	created_at: createStringType(0, 64).nullish(),
	updated_at: createStringType(0, 64).nullish(),
	resolved_at: createStringType(0, 64).nullish(),
	maintenance_updates: z.array(InstatusMaintenanceMaintenanceUpdate).nullish(),
	affected_components: z.array(InstatusAffectedComponent).nullish(),
});

const InstatusComponentUpdate = z.object({
	created_at: createStringType(0, 64).nullish(),
	new_status: createStringType(0, 256).nullish(),
	component_id: createStringType(0, 256).nullish(),
});

export const InstatusWebhook = z.object({
	meta: InstatusMeta.nullish(),
	page: InstatusPage.nullish(),
	incident: InstatusIncident.nullish(),
	maintenance: InstatusMaintenance.nullish(),
	component_update: InstatusComponentUpdate.nullish(),
	component: InstatusComponentUpdateComponent.nullish(),
});

export type InstatusWebhook = z.infer<typeof InstatusWebhook>;
