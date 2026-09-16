// SPDX-License-Identifier: AGPL-3.0-or-later

import {describeAdminAuditCoverage} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {CodesAdminAuditCases} from '@app/api/admin/tests/audit_coverage/CodesAdminAuditCases';

describeAdminAuditCoverage('CodesAdminController', CodesAdminAuditCases);
