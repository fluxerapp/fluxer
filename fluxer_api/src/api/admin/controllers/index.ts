// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoApp} from '../../types/HonoEnv';
import {AdminApiKeyAdminController} from './AdminApiKeyAdminController';
import {ApplicationAdminController} from './ApplicationAdminController';
import {ArchiveAdminController} from './ArchiveAdminController';
import {AssetAdminController} from './AssetAdminController';
import {AuditLogAdminController} from './AuditLogAdminController';
import {BanAdminController} from './BanAdminController';
import {BulkAdminController} from './BulkAdminController';
import {CodesAdminController} from './CodesAdminController';
import {DiscoveryAdminController} from './DiscoveryAdminController';
import {GatewayAdminController} from './GatewayAdminController';
import {GuildAdminController} from './GuildAdminController';
import {InstanceConfigAdminController} from './InstanceConfigAdminController';
import {JobsAdminController} from './JobsAdminController';
import {LimitConfigAdminController} from './LimitConfigAdminController';
import {MessageAdminController} from './MessageAdminController';
import {ReportAdminController} from './ReportAdminController';
import {SearchAdminController} from './SearchAdminController';
import {SystemAdminController} from './SystemAdminController';
import {SystemDmAdminController} from './SystemDmAdminController';
import {UserAdminController} from './UserAdminController';
import {VoiceAdminController} from './VoiceAdminController';

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
