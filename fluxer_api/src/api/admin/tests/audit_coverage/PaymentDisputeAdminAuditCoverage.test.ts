// SPDX-License-Identifier: AGPL-3.0-or-later

import {describeAdminAuditCoverage} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {PaymentDisputeAdminAuditCases} from '@app/api/admin/tests/audit_coverage/PaymentDisputeAdminAuditCases';

describeAdminAuditCoverage('PaymentDisputeAdminController', PaymentDisputeAdminAuditCases);
