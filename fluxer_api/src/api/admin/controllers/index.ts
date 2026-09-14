// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminApiKeyAdminController} from '@app/api/admin/controllers/AdminApiKeyAdminController';
import {ApplicationAdminController} from '@app/api/admin/controllers/ApplicationAdminController';
import {ArchiveAdminController} from '@app/api/admin/controllers/ArchiveAdminController';
import {AssetAdminController} from '@app/api/admin/controllers/AssetAdminController';
import {AuditLogAdminController} from '@app/api/admin/controllers/AuditLogAdminController';
import {BanAdminController} from '@app/api/admin/controllers/BanAdminController';
import {BulkAdminController} from '@app/api/admin/controllers/BulkAdminController';
import {CodesAdminController} from '@app/api/admin/controllers/CodesAdminController';
import {DiscoveryAdminController} from '@app/api/admin/controllers/DiscoveryAdminController';
import {GatewayAdminController} from '@app/api/admin/controllers/GatewayAdminController';
import {GuildAdminController} from '@app/api/admin/controllers/GuildAdminController';
import {InstanceConfigAdminController} from '@app/api/admin/controllers/InstanceConfigAdminController';
import {JobsAdminController} from '@app/api/admin/controllers/JobsAdminController';
import {LimitConfigAdminController} from '@app/api/admin/controllers/LimitConfigAdminController';
import {MessageAdminController} from '@app/api/admin/controllers/MessageAdminController';
import {ReportAdminController} from '@app/api/admin/controllers/ReportAdminController';
import {SearchAdminController} from '@app/api/admin/controllers/SearchAdminController';
import {SystemAdminController} from '@app/api/admin/controllers/SystemAdminController';
import {SystemDmAdminController} from '@app/api/admin/controllers/SystemDmAdminController';
import {UserAdminController} from '@app/api/admin/controllers/UserAdminController';
import {VoiceAdminController} from '@app/api/admin/controllers/VoiceAdminController';
import type {HonoApp} from '@app/api/types/HonoEnv';

export function registerAdminControllers(app: HonoApp) {
	AdminApiKeyAdminController(app);
	ApplicationAdminController(app);
	UserAdminController(app);
	CodesAdminController(app);
	GuildAdminController(app);
	AssetAdminController(app);
	BanAdminController(app);
	InstanceConfigAdminController(app);
	LimitConfigAdminController(app);
	MessageAdminController(app);
	BulkAdminController(app);
	AuditLogAdminController(app);
	ArchiveAdminController(app);
	ReportAdminController(app);
	VoiceAdminController(app);
	GatewayAdminController(app);
	SearchAdminController(app);
	DiscoveryAdminController(app);
	SystemDmAdminController(app);
	SystemAdminController(app);
	JobsAdminController(app);
}
