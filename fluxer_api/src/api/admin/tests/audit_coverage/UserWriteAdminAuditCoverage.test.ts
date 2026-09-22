// SPDX-License-Identifier: AGPL-3.0-or-later

import {describeAdminAuditCoverage} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {UserWriteAdminAuditCases} from '@app/api/admin/tests/audit_coverage/UserWriteAdminAuditCases';

describeAdminAuditCoverage('UserAdminController writes', UserWriteAdminAuditCases);
