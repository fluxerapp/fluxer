// SPDX-License-Identifier: AGPL-3.0-or-later

import {describeAdminAuditCoverage} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {BanAdminAuditCases} from '@app/api/admin/tests/audit_coverage/BanAdminAuditCases';

describeAdminAuditCoverage('BanAdminController', BanAdminAuditCases);
