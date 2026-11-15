// SPDX-License-Identifier: AGPL-3.0-or-later

import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';
import {describe, expect, it} from 'vitest';

function render<T extends Parameters<typeof getEmailTemplate>[0]>(
	key: T,
	locale: string,
	variables: Parameters<typeof getEmailTemplate<T>>[2],
	timeZone: string | null,
) {
	const result = getEmailTemplate(key, locale, variables, 'Fluxer', timeZone);
	expect(result.ok).toBe(true);
	if (!result.ok) throw new Error(result.error.message);
	return result.value;
}

describe('email dates and times', () => {
	it('words the code expiry relative to now', () => {
		const template = render('sign_in_code', 'sv-SE', {minutes: 10}, null);
		expect(template.expiry).toBe('Koden går ut om 10 minuter.');
		expect(render('reauth_code', 'en-US', {minutes: 1}, null).expiry).toBe('This code expires in 1 minute.');
	});
});
