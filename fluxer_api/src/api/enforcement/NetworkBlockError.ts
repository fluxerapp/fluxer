// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BannedIpKind} from '@app/api/admin/AdminRepository';
import {Config} from '@app/api/Config';
import {describeRule} from '@app/api/enforcement/EnforcementNotice';
import {formatNoticeTimeUtc} from '@app/api/enforcement/NoticeDates';
import {resolveContactEmails} from '@app/api/instance/ContactEmails';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import {ENFORCEMENT_RULES, enforcementReasonOf} from '@fluxer/constants/src/EnforcementReasons';
import {IpBannedError} from '@fluxer/errors/src/domains/moderation/IpBannedError';

export interface NetworkBlock {
	ipAddress: string;
	kind: BannedIpKind;
	expiresAt: Date | null;
	reasonCode: number | null;
}

function shownReason(reasonCode: number | null, locale: string) {
	const reason = enforcementReasonOf(reasonCode);
	return reason && ENFORCEMENT_RULES[reason.rule].showOnNetworkScreen ? describeRule(reason.rule, locale) : null;
}

export function createNetworkBlockError(block: NetworkBlock, locale: string): IpBannedError {
	const contacts = resolveContactEmails();
	const reason = shownReason(block.reasonCode, locale);
	return new IpBannedError({
		ipAddress: block.ipAddress,
		kind: block.kind,
		expiresAt: block.expiresAt,
		expiresAtText: block.expiresAt ? formatNoticeTimeUtc(block.expiresAt, locale) : null,
		appealEmail: reason ? contacts.appealsEmail : contacts.supportEmail,
		appealsSupported: !Config.instance.selfHosted,
		reason,
		productName: getInstanceProductName(),
	});
}
