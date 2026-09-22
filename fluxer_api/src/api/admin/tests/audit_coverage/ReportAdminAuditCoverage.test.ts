// SPDX-License-Identifier: AGPL-3.0-or-later

import {describeAdminAuditCoverage} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {ReportAdminAuditCases} from '@app/api/admin/tests/audit_coverage/ReportAdminAuditCases';

describeAdminAuditCoverage('ReportAdminController', ReportAdminAuditCases);
