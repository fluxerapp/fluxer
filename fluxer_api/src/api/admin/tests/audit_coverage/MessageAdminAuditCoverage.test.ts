// SPDX-License-Identifier: AGPL-3.0-or-later

import {describeAdminAuditCoverage} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {MessageAdminAuditCases} from '@app/api/admin/tests/audit_coverage/MessageAdminAuditCases';

describeAdminAuditCoverage('MessageAdminController', MessageAdminAuditCases);
