// SPDX-License-Identifier: AGPL-3.0-or-later

import {ERROR_I18N_MESSAGES} from '@fluxer/errors/src/i18n/ErrorI18nMessages';
import {describe, expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: unknown) => descriptor}));

const {NETWORK_BLOCK_LEAD_DESCRIPTOR, NETWORK_BLOCK_LEAD_WITH_REASON_DESCRIPTOR, SHARED_NETWORK_DESCRIPTOR} =
	await import('@app/features/auth/AuthMessageDescriptors');

function asErrorCatalogText(descriptor: unknown): string {
	return (descriptor as {message: string}).message.replaceAll('{productName}', '{product_name}');
}

describe('network block copy matches the error catalog', () => {
	test.each(['permissions.global_ip_banned', 'permissions.global_ip_temporarily_banned'] as const)('%s', (key) => {
		const message = ERROR_I18N_MESSAGES[key];
		expect(message).toContain(
			`{reason, select, shown {${asErrorCatalogText(NETWORK_BLOCK_LEAD_WITH_REASON_DESCRIPTOR)}} other {${asErrorCatalogText(NETWORK_BLOCK_LEAD_DESCRIPTOR)}}}`,
		);
		expect(message).toContain(` ${asErrorCatalogText(SHARED_NETWORK_DESCRIPTOR)}{appealEmail, select,`);
	});
});
