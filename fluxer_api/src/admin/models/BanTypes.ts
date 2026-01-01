/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createStringType, EmailType, PhoneNumberType, z} from '~/Schema';
import {isValidIpOrRange} from '~/utils/IpRangeUtils';

export const BanIpRequest = z.object({
	ip: createStringType(1, 45).refine(
		(value) => isValidIpOrRange(value),
		'Must be a valid IPv4/IPv6 address or CIDR range',
	),
});

export type BanIpRequest = z.infer<typeof BanIpRequest>;

export const BanEmailRequest = z.object({
	email: EmailType,
});

export type BanEmailRequest = z.infer<typeof BanEmailRequest>;

export const BanPhoneRequest = z.object({
	phone: PhoneNumberType,
});

export type BanPhoneRequest = z.infer<typeof BanPhoneRequest>;
