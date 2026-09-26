// SPDX-License-Identifier: AGPL-3.0-or-later

import {describeAdminAuditCoverage} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {ArchiveAdminAuditCases} from '@app/api/admin/tests/audit_coverage/ArchiveAdminAuditCases';

describeAdminAuditCoverage('ArchiveAdminController', ArchiveAdminAuditCases);
