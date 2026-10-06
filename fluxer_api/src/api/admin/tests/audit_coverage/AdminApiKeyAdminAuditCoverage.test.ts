// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminApiKeyAdminAuditCases} from '@app/api/admin/tests/audit_coverage/AdminApiKeyAdminAuditCases';
import {describeAdminAuditCoverage} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';

describeAdminAuditCoverage('AdminApiKeyAdminController', AdminApiKeyAdminAuditCases);
