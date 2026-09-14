// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {AuditLogActionTypeSchema} from '@fluxer/schema/src/primitives/AuditLogValidators';
import {describe, expect, it} from 'vitest';

describe('AuditLogActionTypeSchema', () => {
	it.each(
		Object.values(AuditLogActionType).filter((value) => typeof value === 'number'),
	)('accepts registered action %i', (value) => {
		expect(AuditLogActionTypeSchema.parse(value)).toBe(value);
	});

	it.each([0, 9, 999, -1, 1.5, '1', 'GUILD_UPDATE', null])('rejects unknown or nonnumeric action %j', (value) => {
		expect(AuditLogActionTypeSchema.safeParse(value).success).toBe(false);
	});
});
