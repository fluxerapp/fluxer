// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerAdminControllers} from '@app/api/admin/controllers/index';
import {AdminApiKeyAdminAuditCases} from '@app/api/admin/tests/audit_coverage/AdminApiKeyAdminAuditCases';
import {adminAuditCaseShape} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {ApplicationAdminAuditCases} from '@app/api/admin/tests/audit_coverage/ApplicationAdminAuditCases';
import {ArchiveAdminAuditCases} from '@app/api/admin/tests/audit_coverage/ArchiveAdminAuditCases';
import {AssetAdminAuditCases} from '@app/api/admin/tests/audit_coverage/AssetAdminAuditCases';
import {AuditLogAdminAuditCases} from '@app/api/admin/tests/audit_coverage/AuditLogAdminAuditCases';
import {BanAdminAuditCases} from '@app/api/admin/tests/audit_coverage/BanAdminAuditCases';
import {BulkAdminAuditCases} from '@app/api/admin/tests/audit_coverage/BulkAdminAuditCases';
import {CodesAdminAuditCases} from '@app/api/admin/tests/audit_coverage/CodesAdminAuditCases';
import {DiscoveryAdminAuditCases} from '@app/api/admin/tests/audit_coverage/DiscoveryAdminAuditCases';
import {GatewayAdminAuditCases} from '@app/api/admin/tests/audit_coverage/GatewayAdminAuditCases';
import {GuildAdminAuditCases} from '@app/api/admin/tests/audit_coverage/GuildAdminAuditCases';
import {InstanceConfigAdminAuditCases} from '@app/api/admin/tests/audit_coverage/InstanceConfigAdminAuditCases';
import {JobsAdminAuditCases} from '@app/api/admin/tests/audit_coverage/JobsAdminAuditCases';
import {LimitConfigAdminAuditCases} from '@app/api/admin/tests/audit_coverage/LimitConfigAdminAuditCases';
import {MessageAdminAuditCases} from '@app/api/admin/tests/audit_coverage/MessageAdminAuditCases';
import {ReportAdminAuditCases} from '@app/api/admin/tests/audit_coverage/ReportAdminAuditCases';
import {SearchAdminAuditCases} from '@app/api/admin/tests/audit_coverage/SearchAdminAuditCases';
import {SystemDmAdminAuditCases} from '@app/api/admin/tests/audit_coverage/SystemDmAdminAuditCases';
import {UserAdminAuditCases} from '@app/api/admin/tests/audit_coverage/UserAdminAuditCases';
import {UserWriteAdminAuditCases} from '@app/api/admin/tests/audit_coverage/UserWriteAdminAuditCases';
import {VoiceAdminAuditCases} from '@app/api/admin/tests/audit_coverage/VoiceAdminAuditCases';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {Hono} from 'hono';
import {describe, expect, test} from 'vitest';

const ALL_CASES = [
	...AdminApiKeyAdminAuditCases,
	...ApplicationAdminAuditCases,
	...ArchiveAdminAuditCases,
	...AssetAdminAuditCases,
	...AuditLogAdminAuditCases,
	...BanAdminAuditCases,
	...BulkAdminAuditCases,
	...CodesAdminAuditCases,
	...DiscoveryAdminAuditCases,
	...GatewayAdminAuditCases,
	...GuildAdminAuditCases,
	...InstanceConfigAdminAuditCases,
	...JobsAdminAuditCases,
	...LimitConfigAdminAuditCases,
	...MessageAdminAuditCases,
	...ReportAdminAuditCases,
	...SearchAdminAuditCases,
	...SystemDmAdminAuditCases,
	...UserAdminAuditCases,
	...UserWriteAdminAuditCases,
	...VoiceAdminAuditCases,
];

const EXEMPT_ROUTES = [
	'GET /admin/jobs',
	'GET /admin/jobs/:job_id',
	'GET /admin/jobs/active',
	'GET /admin/search/index-refreshes/:job_id',
	'GET /admin/users/@me',
];

function registeredAdminRoutes(): Array<string> {
	const app = new Hono<HonoEnv>();
	registerAdminControllers(app);
	return [
		...new Set(app.routes.filter((route) => route.method !== 'ALL').map((route) => `${route.method} ${route.path}`)),
	].sort();
}

describe('Admin audit route coverage', () => {
	test('every registered admin route has an audit coverage case', () => {
		expect([...new Set(ALL_CASES.map(adminAuditCaseShape))].sort()).toEqual(registeredAdminRoutes());
	});

	test('only the identity and polled status reads are exempt from auditing', () => {
		const exempt = ALL_CASES.filter((coverageCase) => 'exempt' in coverageCase).map(adminAuditCaseShape);
		expect([...new Set(exempt)].sort()).toEqual(EXEMPT_ROUTES);
	});
});
