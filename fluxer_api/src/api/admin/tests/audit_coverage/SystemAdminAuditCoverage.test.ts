// SPDX-License-Identifier: AGPL-3.0-or-later

import {describeAdminAuditCoverage} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {SystemAdminAuditCases} from '@app/api/admin/tests/audit_coverage/SystemAdminAuditCases';

describeAdminAuditCoverage('SystemAdminController', SystemAdminAuditCases);
