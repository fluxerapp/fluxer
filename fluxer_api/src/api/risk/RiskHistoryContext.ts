// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LatestRiskContextRecord} from '@app/api/risk/RiskHistoryTypes';
import {getSubnet} from '@fluxer/ip_utils/src/IpAddress';

export function deriveLatestRiskContext(params: {
	userId: string;
	email: string | null;
	clientIp: string;
	asn: number | null;
	updatedAt: Date;
}): LatestRiskContextRecord {
	const emailDomain = params.email?.split('@')[1]?.toLowerCase() ?? null;
	return {
		userId: params.userId,
		ip: params.clientIp,
		subnet: getSubnet(params.clientIp),
		emailDomain,
		asn: params.asn,
		updatedAt: params.updatedAt,
	};
}
